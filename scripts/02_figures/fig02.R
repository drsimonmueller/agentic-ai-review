#!/usr/bin/env Rscript
# Figure 2: Master benchmark evaluation (N = 36 primary cohort)
# Unified ggplot2 / patchwork design matching chr20 & crispore-seq visual grammar:
#   a: Per-arm causal recall (mean +/- s.d. bars, beeswarm points, UpSet experimental matrix)
#   b: Causal-chain node recovery by evidence tier (horizontal stacked barplot)
#   c: Control-trap audit outcomes across FP1-FP7 (horizontal stacked barplot)
#   d: Blinded quality-control hypothesis falsification rate across models (bar + beeswarm)
#   e: Wall-clock runtime vs. causal recall and speed-recall Pareto frontier (log2 scale)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(ggbeeswarm)
  library(patchwork)
  library(scales)
  library(grid)
})

repo_root_init <- Sys.getenv("AGENTIC_REPO_ROOT", unset = ".")
source(file.path(repo_root_init, "config", "paths.R"))
source(file.path(repo_root, "R", "colors.R"))
source(file.path(repo_root, "R", "theme.R"))
source(file.path(repo_root, "R", "stats_utils.R"))
source(file.path(repo_root, "R", "plot_utils.R"))

set.seed(GLOBAL_SEED)

fig2_dir <- ensure_dir(file.path(path_figures, "Figure_2"))

# Typography & shared neutral/model palette (col_* & model_colors from R/colors.R)
TAG_SIZE     <- 9.0
TEXT_SIZE    <- 6.0
GG_TEXT_SIZE <- TEXT_SIZE / ggplot2::.pt

model_order <- model_display_order
model_map <- c(
  "Claude Opus 5.5"   = "Claude Opus 5.0*",
  "Claude Sonnet 5.5" = "Claude Sonnet 5.5",
  "Gemini 4 Hydrogen" = "Gemini 4 Hydrogen",
  "Gemini 4 Neon"     = "Gemini 4 Neon"
)
fig2_model_cols <- model_colors[model_display_order]

panel_header <- function(plt, tag_lbl, title_txt = NULL, tag_size = TAG_SIZE, title_size = TEXT_SIZE) {
  plt +
    ggplot2::labs(tag = tag_lbl, title = NULL) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(
        family = "Helvetica", face = "bold", size = tag_size, color = "black"
      ),
      plot.tag.position = c(0.01, 0.99),
      plot.title = ggplot2::element_blank()
    )
}

# ---------------------------------------------------------------------------
# 1. Load and validate inputs (N = 36 primary cohort)
# ---------------------------------------------------------------------------
sc_path  <- require_file(file.path(path_sd_benchmark, "scored_rubric_N36.csv"))
tel_path <- require_file(file.path(path_sd_benchmark, "run_telemetry_N36.csv"))

sc  <- readr::read_csv(sc_path, show_col_types = FALSE)
tel <- readr::read_csv(tel_path, show_col_types = FALSE)

require_columns(sc, c(
  "model_slug", "model_label", "actual_ceo_model", "ceo_biosafety_fallback",
  "condition", "replicate", "run_path",
  paste0("N", 1:7, "_score"), paste0("N", 1:7, "_tier"),
  "causal_node_score_sum", "causal_node_recall_pct",
  paste0("FP", 1:7, "_status"), "n_non_chain_discoveries", "in_primary_N36"
), "scored_rubric_N36.csv")

require_columns(tel, c(
  "model_slug", "model_label", "actual_ceo_model", "condition", "replicate",
  "run_path", "wall_clock_min", "n_confirmed_findings", "n_discarded_hypotheses",
  "ceo_safeguard_events", "subagent_safety_interrupts", "in_primary_N36"
), "run_telemetry_N36.csv")

stopifnot(nrow(sc) == 36L, nrow(tel) == 36L)

n36 <- sc %>%
  dplyr::inner_join(
    tel %>% dplyr::select(
      run_path, wall_clock_min, slurm_job_union_min, active_agent_min,
      n_confirmed_findings, n_discarded_hypotheses,
      ceo_safeguard_events, subagent_safety_interrupts
    ),
    by = "run_path"
  ) %>%
  dplyr::mutate(
    model_f       = factor(model_map[model_label], levels = model_order),
    framing       = factor(dplyr::if_else(stringr::str_detect(condition, "^cold"), "Cold", "Seeded"),
                           levels = c("Seeded", "Cold")),
    lit_access    = factor(dplyr::if_else(stringr::str_detect(condition, "lit$"), "With literature", "Without literature"),
                           levels = c("Without literature", "With literature")),
    is_fallback   = as.logical(ceo_biosafety_fallback),
    has_sub_stop  = subagent_safety_interrupts > 0,
    discard_total = n_confirmed_findings + n_discarded_hypotheses,
    discard_pct   = dplyr::if_else(discard_total > 0, 100 * n_discarded_hypotheses / discard_total, 0),
    run_id        = paste(model_slug, condition, replicate, sep = "/")
  )

stopifnot(nrow(n36) == 36L, all(n36$n_discarded_hypotheses > 0))

# Exact Wilcoxon invariance checks
matched24 <- n36 %>% dplyr::filter(model_label %in% c("Claude Sonnet 5.5", "Gemini 4 Hydrogen"))
cold_vs_seeded <- stats::wilcox.test(
  n36$causal_node_score_sum[n36$framing == "Cold"],
  n36$causal_node_score_sum[n36$framing == "Seeded"],
  exact = FALSE
)
lit_vs_nolit <- stats::wilcox.test(
  matched24$causal_node_score_sum[matched24$lit_access == "With literature"],
  matched24$causal_node_score_sum[matched24$lit_access == "Without literature"],
  exact = FALSE
)
stopifnot(
  abs(cold_vs_seeded$p.value - 0.114) < 0.01,
  abs(lit_vs_nolit$p.value - 0.151) < 0.01
)

# ---------------------------------------------------------------------------
# Panel a: Aligned condition panels (Seeded | Cold | Seeded + literature | Cold + literature)
# ---------------------------------------------------------------------------
panel_levels <- c("Seeded", "Cold", "Seeded + literature", "Cold + literature")

arm_meta <- tibble::tribble(
  ~arm_id,             ~panel_grp,            ~bar_lbl,          ~model_f,            ~cond_key,
  "opus_seeded",       "Seeded",              "Opus 5.0*",       "Claude Opus 5.0*",  "seeded",
  "sonnet_seeded",     "Seeded",              "Sonnet 5.5**",    "Claude Sonnet 5.5", "seeded",
  "hydro_seeded",      "Seeded",              "Hydrogen",        "Gemini 4 Hydrogen", "seeded",
  "neon_seeded",       "Seeded",              "Neon",            "Gemini 4 Neon",     "seeded",
  "opus_cold",         "Cold",                "Opus 5.0*",       "Claude Opus 5.0*",  "cold",
  "sonnet_cold",       "Cold",                "Sonnet 5.5**",    "Claude Sonnet 5.5", "cold",
  "hydro_cold",        "Cold",                "Hydrogen",        "Gemini 4 Hydrogen", "cold",
  "neon_cold",         "Cold",                "Neon",            "Gemini 4 Neon",     "cold",
  "sonnet_seeded_lit", "Seeded + literature", "Sonnet 5.5",      "Claude Sonnet 5.5", "seeded_lit",
  "hydro_seeded_lit",  "Seeded + literature", "Hydrogen",        "Gemini 4 Hydrogen", "seeded_lit",
  "sonnet_cold_lit",   "Cold + literature",   "Sonnet 5.5",      "Claude Sonnet 5.5", "cold_lit",
  "hydro_cold_lit",    "Cold + literature",   "Hydrogen",        "Gemini 4 Hydrogen", "cold_lit"
)

bar_order_all <- c("Opus 5.0*", "Sonnet 5.5**", "Sonnet 5.5", "Hydrogen", "Neon")

all_pts_a <- n36 %>%
  dplyr::inner_join(
    arm_meta %>% dplyr::select(arm_id, panel_grp, bar_lbl, model_f, cond_key),
    by = c("model_f", "condition" = "cond_key")
  ) %>%
  dplyr::transmute(
    arm_id,
    panel_grp = factor(panel_grp, levels = panel_levels),
    bar_lbl   = factor(bar_lbl, levels = bar_order_all),
    model_f,
    score     = causal_node_score_sum
  )

sum_a <- all_pts_a %>%
  dplyr::group_by(arm_id, panel_grp, bar_lbl) %>%
  dplyr::summarise(
    mean_rec = mean(score),
    sd_rec   = stats::sd(score),
    low_rec  = pmax(0, mean_rec - sd_rec),
    high_rec = pmin(7, mean_rec + sd_rec),
    .groups  = "drop"
  )

# Pooled means invariance assertions (reported in text)
stopifnot(
  abs(mean(n36$causal_node_score_sum[n36$framing == "Cold"]) - 6.39) < 0.01,
  abs(mean(n36$causal_node_score_sum[n36$framing == "Seeded"]) - 5.89) < 0.01,
  abs(mean(matched24$causal_node_score_sum[matched24$lit_access == "With literature"]) - 6.33) < 0.01,
  abs(mean(matched24$causal_node_score_sum[matched24$lit_access == "Without literature"]) - 6.08) < 0.01
)

p2a_raw <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = sum_a,
    ggplot2::aes(x = bar_lbl, y = mean_rec),
    width = 0.66, fill = col_bar_gray, color = NA
  ) +
  ggplot2::geom_hline(yintercept = 7, linetype = "dashed", color = col_divider, linewidth = LW_025) +
  ggplot2::geom_errorbar(
    data = sum_a,
    ggplot2::aes(x = bar_lbl, ymin = low_rec, ymax = high_rec),
    width = 0, linewidth = LW_025, color = "black"
  ) +
  ggbeeswarm::geom_quasirandom(
    data = all_pts_a,
    ggplot2::aes(x = bar_lbl, y = score, color = model_f),
    shape = 16, size = 1.75, alpha = 0.90, width = 0.18
  ) +
  ggplot2::facet_grid(. ~ panel_grp, scales = "free_x", space = "free_x") +
  ggplot2::scale_color_manual(
    values = fig2_model_cols,
    name = NULL,
    guide = ggplot2::guide_legend(
      nrow = 1,
      override.aes = list(shape = 16, size = 2.0, alpha = 1)
    )
  ) +
  ggplot2::scale_y_continuous(
    breaks = 0:7,
    expand = c(0, 0)
  ) +
  ggplot2::coord_cartesian(ylim = c(0, 7.35), expand = FALSE, clip = "off") +
  ggplot2::labs(x = NULL, y = "Causal nodes recovered (n/7)") +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    legend.position      = "top",
    legend.direction     = "horizontal",
    legend.background    = ggplot2::element_blank(),
    legend.key           = ggplot2::element_blank(),
    legend.key.size      = grid::unit(0.20, "cm"),
    legend.spacing.x     = grid::unit(0.14, "cm"),
    legend.margin        = ggplot2::margin(t = 0, b = 1),
    legend.box.margin    = ggplot2::margin(b = 1),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    strip.background     = ggplot2::element_rect(fill = col_stripe, color = NA),
    strip.text           = ggplot2::element_text(
      family = "Helvetica", face = "plain", size = TEXT_SIZE, color = "black",
      margin = ggplot2::margin(t = 2.5, b = 2.5)
    ),
    panel.spacing.x      = grid::unit(7.0, "pt"),
    axis.ticks.x         = ggplot2::element_blank(),
    axis.text.x          = ggplot2::element_text(size = TEXT_SIZE, color = "black", lineheight = 0.90),
    axis.text.y          = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.y         = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    plot.margin          = ggplot2::margin(2, 6, 4, 4)
  )

p2a_stack <- panel_header(p2a_raw, "a")
save_panel_pair(p2a_stack, file.path(fig2_dir, "fig2a_recall_by_arm.pdf"), width = 7.20, height = 2.35, dpi = 300L)

# ---------------------------------------------------------------------------
# Panel b: Causal-chain node recovery (crispore-seq fig3b horizontal stacked)
# ---------------------------------------------------------------------------
node_labels <- c(
  "N1  Essential-gene recall deficit",
  "N2  Sparse lncRNA hit calls",
  "N3  Pol III poly-T termination",
  "N4  Poly-T functional depletion loss",
  "N5  Direct-repeat recombination",
  "N6  Effective cellular coverage loss",
  "N7  Statistical power collapse"
)

node_counts_df <- purrr::map_dfr(seq_len(7L), function(i) {
  sc_col <- paste0("N", i, "_score")
  tr_col <- paste0("N", i, "_tier")
  tibble::tibble(
    node_lbl = node_labels[i],
    n_e1 = sum(n36[[sc_col]] > 0 & n36[[tr_col]] == "E1", na.rm = TRUE),
    n_e2 = sum(n36[[sc_col]] > 0 & n36[[tr_col]] != "E1", na.rm = TRUE)
  )
})

stopifnot(
  identical(
    as.integer(node_counts_df$n_e1 + node_counts_df$n_e2),
    c(36L, 36L, 34L, 34L, 33L, 12L, 36L)
  )
)

node_long_b <- node_counts_df %>%
  tidyr::pivot_longer(cols = c(n_e1, n_e2), names_to = "tier", values_to = "n_runs") %>%
  dplyr::mutate(
    node_f = factor(node_lbl, levels = rev(node_labels)),
    y_num  = as.numeric(node_f),
    pct    = 100 * n_runs / nrow(n36),
    tier_f = factor(
      dplyr::if_else(tier == "n_e1", "Primary raw data (E1)", "Supplementary tables (E2)"),
      levels = c("Supplementary tables (E2)", "Primary raw data (E1)")
    )
  )

stripe_7 <- tibble::tibble(y_num = c(1, 3, 5, 7))

p2b_raw <- ggplot2::ggplot(node_long_b, ggplot2::aes(x = pct, y = node_f, fill = tier_f)) +
  ggplot2::geom_rect(
    data = stripe_7,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = y_num - 0.5, ymax = y_num + 0.5),
    inherit.aes = FALSE, fill = col_stripe
  ) +
  ggplot2::geom_col(position = "stack", width = 0.62, color = NA) +
  ggplot2::scale_fill_manual(
    values = c("Primary raw data (E1)" = pal_blue, "Supplementary tables (E2)" = pal_blue_light),
    breaks = c("Primary raw data (E1)", "Supplementary tables (E2)"),
    name = NULL,
    guide = ggplot2::guide_legend(nrow = 1)
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 102),
    breaks = c(0, 25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  ggplot2::labs(x = "Runs recovering node (%)", y = NULL) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line.y      = ggplot2::element_blank(),
    axis.ticks.y     = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.text.x      = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.x     = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    legend.position  = "top",
    legend.justification = "right",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.18, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    legend.spacing.x = grid::unit(0.10, "cm"),
    legend.margin    = ggplot2::margin(0, 0, -3, 0),
    plot.margin      = ggplot2::margin(2, 6, 4, 4)
  )

p2b <- panel_header(p2b_raw, "b", "Causal-chain node recovery")
save_panel_pair(p2b, file.path(fig2_dir, "fig2b_causal_node_recovery.pdf"), width = 3.60, height = 2.25, dpi = 300L)

# ---------------------------------------------------------------------------
# Panel c: Control-trap audit outcomes across FP1-FP7 (twin horizontal bars)
# ---------------------------------------------------------------------------
fp_labels <- c(
  "FP1  Read-count mapping error",
  "FP2  Non-targeting off-target effect",
  "FP3  CasRx collateral RNA cleavage",
  "FP4  Guide GC-content bias",
  "FP5  Plasmid skew or folding confound",
  "FP6  Non-essential control specificity",
  "FP7  Dismissal of all 206 lncRNA hits"
)

fp_counts_df <- purrr::map_dfr(seq_len(7L), function(i) {
  col_nm <- paste0("FP", i, "_status")
  v <- n36[[col_nm]]
  tibble::tibble(
    fp_lbl    = fp_labels[i],
    n_qc      = sum(v == "Caught_in_Discard_Log", na.rm = TRUE),
    n_revised = sum(v == "Refined_by_Data", na.rm = TRUE),
    n_flag    = sum(v == "Triggered_in_Final", na.rm = TRUE)
  )
})

stopifnot(
  sum(fp_counts_df$n_flag[1:6]) == 0L,
  fp_counts_df$n_revised[3] == 6L,
  fp_counts_df$n_flag[7] == 13L,
  fp_counts_df$n_qc[5] == 35L
)

fp_long_c <- fp_counts_df %>%
  tidyr::pivot_longer(cols = c(n_qc, n_revised, n_flag), names_to = "status", values_to = "n_runs") %>%
  dplyr::mutate(
    fp_f = factor(fp_lbl, levels = rev(fp_labels)),
    pct  = 100 * n_runs / nrow(n36),
    status_f = factor(
      dplyr::case_when(
        status == "n_qc"      ~ "Refuted in QC",
        status == "n_revised" ~ "Empirically revised",
        status == "n_flag"    ~ "Editorial flag"
      ),
      levels = c("Editorial flag", "Empirically revised", "Refuted in QC")
    )
  )

p2c_raw <- ggplot2::ggplot(fp_long_c, ggplot2::aes(x = pct, y = fp_f, fill = status_f)) +
  ggplot2::geom_rect(
    data = stripe_7,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = y_num - 0.5, ymax = y_num + 0.5),
    inherit.aes = FALSE, fill = col_stripe
  ) +
  ggplot2::geom_col(position = "stack", width = 0.62, color = NA) +
  ggplot2::scale_fill_manual(
    values = c(
      "Refuted in QC"       = col_qc_gray,
      "Empirically revised" = pal_teal,
      "Editorial flag"      = pal_coral
    ),
    breaks = c("Refuted in QC", "Empirically revised", "Editorial flag"),
    labels = c("Refuted in QC", "Revised (FP3)", "Flagged (FP7)"),
    name = NULL,
    guide = ggplot2::guide_legend(nrow = 1)
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 102),
    breaks = c(0, 25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  ggplot2::labs(x = "Runs engaging control trap (%)", y = NULL) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line.y      = ggplot2::element_blank(),
    axis.ticks.y     = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.text.x      = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.x     = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    legend.position  = "top",
    legend.justification = "right",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.18, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    legend.spacing.x = grid::unit(0.10, "cm"),
    legend.margin    = ggplot2::margin(0, 0, -3, 0),
    plot.margin      = ggplot2::margin(2, 6, 4, 4)
  )

p2c <- panel_header(p2c_raw, "c", "Control-trap audit outcomes")
save_panel_pair(p2c, file.path(fig2_dir, "fig2c_control_trap_audit.pdf"), width = 3.60, height = 2.25, dpi = 300L)

# ---------------------------------------------------------------------------
# Panel d: Blinded QC hypothesis falsification rate (chr20 fig2d bar+beeswarm)
# ---------------------------------------------------------------------------
d_model_short <- c(
  "Claude Opus 5.0*"  = "Claude\nOpus 5.0*",
  "Claude Sonnet 5.5" = "Claude\nSonnet 5.5",
  "Gemini 4 Hydrogen" = "Gemini 4\nHydrogen",
  "Gemini 4 Neon"     = "Gemini 4\nNeon"
)

d_pts <- n36 %>%
  dplyr::mutate(
    model_short = factor(unname(d_model_short[as.character(model_f)]),
                         levels = unname(d_model_short))
  )

d_sum <- d_pts %>%
  dplyr::group_by(model_f, model_short) %>%
  dplyr::summarise(
    med_pct = stats::median(discard_pct),
    q25_pct = stats::quantile(discard_pct, 0.25),
    q75_pct = stats::quantile(discard_pct, 0.75),
    .groups = "drop"
  )

overall_med_disc <- stats::median(n36$discard_pct)
stopifnot(
  abs(overall_med_disc - 48.8) < 0.2,
  sum(n36$n_discarded_hypotheses) == 488L,
  sum(n36$n_confirmed_findings) == 465L
)

p2d_raw <- ggplot2::ggplot(d_sum, ggplot2::aes(x = model_short, y = med_pct)) +
  ggplot2::geom_col(width = 0.62, fill = col_bar_gray, color = NA) +
  ggplot2::annotate(
    "segment",
    x = 0.45, xend = 4.38, y = overall_med_disc, yend = overall_med_disc,
    linetype = "dashed", linewidth = LW_025, color = col_dark
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = q25_pct, ymax = q75_pct),
    width = 0, linewidth = LW_025, color = "black"
  ) +
  ggbeeswarm::geom_quasirandom(
    data = d_pts,
    ggplot2::aes(x = model_short, y = discard_pct, color = model_f),
    inherit.aes = FALSE,
    shape = 16, size = 1.75, alpha = 0.90, width = 0.18
  ) +
  ggplot2::annotate(
    "text", x = 4.46, y = overall_med_disc,
    label = sprintf("Median\n%.1f%%", overall_med_disc),
    hjust = 0, vjust = 0.5, family = "Helvetica", size = GG_TEXT_SIZE,
    color = col_dark, lineheight = 0.90
  ) +
  ggplot2::scale_color_manual(values = fig2_model_cols, guide = "none") +
  ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.55, 0.95))) +
  ggplot2::scale_y_continuous(
    limits = c(0, 78),
    breaks = c(0, 25, 50, 75),
    expand = c(0, 0)
  ) +
  ggplot2::labs(x = NULL, y = "Hypotheses refuted in blinded QC (%)") +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x  = ggplot2::element_text(size = TEXT_SIZE, color = "black", lineheight = 0.90),
    axis.text.y  = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.y = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    plot.margin  = ggplot2::margin(8, 6, 4, 8)
  )

p2d <- panel_header(p2d_raw, "d", "Blinded quality-control falsification")
save_panel_pair(p2d, file.path(fig2_dir, "fig2d_qc_falsification_rate.pdf"), width = 3.60, height = 2.25, dpi = 300L)

# ---------------------------------------------------------------------------
# Panel e: Wall-clock runtime vs. causal recall & Pareto frontier (log2 scale)
# ---------------------------------------------------------------------------
n36_e <- n36 %>%
  dplyr::arrange(wall_clock_min, dplyr::desc(causal_node_score_sum))

is_pareto <- logical(nrow(n36_e))
best_so_far <- -Inf
for (i in seq_len(nrow(n36_e))) {
  if (n36_e$causal_node_score_sum[i] > best_so_far) {
    is_pareto[i] <- TRUE
    best_so_far <- n36_e$causal_node_score_sum[i]
  }
}

n36_e <- n36_e %>%
  dplyr::mutate(
    is_pareto = is_pareto,
    pt_label = dplyr::if_else(is_pareto, sprintf("%.0f min", wall_clock_min), "")
  )

n36_e_dom <- n36_e %>%
  dplyr::filter(!is_pareto) %>%
  dplyr::group_by(causal_node_score_sum) %>%
  dplyr::arrange(wall_clock_min, .by_group = TRUE) %>%
  dplyr::mutate(
    y_plot = causal_node_score_sum + if (dplyr::n() <= 1L) 0 else rep(c(-0.10, 0.10, -0.05, 0.05, 0), length.out = dplyr::n())
  ) %>%
  dplyr::ungroup()

pareto_pts <- n36_e %>%
  dplyr::filter(is_pareto) %>%
  dplyr::arrange(wall_clock_min)

pareto_line <- dplyr::bind_rows(
  pareto_pts %>% dplyr::select(wall_clock_min, causal_node_score_sum),
  tibble::tibble(wall_clock_min = 255, causal_node_score_sum = max(pareto_pts$causal_node_score_sum))
)

label_df <- n36_e %>%
  dplyr::filter(is_pareto) %>%
  dplyr::mutate(
    lbl_x = dplyr::case_when(
      run_id == "claude_opus_5_5/cold/run_03"         ~ 46,
      run_id == "claude_sonnet_5_5/seeded_lit/run_02" ~ 52,
      run_id == "google_gemini_4_neon/cold/run_02"    ~ 48
    ),
    lbl_y = dplyr::case_when(
      run_id == "claude_opus_5_5/cold/run_03"         ~ 4.45,
      run_id == "claude_sonnet_5_5/seeded_lit/run_02" ~ 5.42,
      run_id == "google_gemini_4_neon/cold/run_02"    ~ 7.46
    )
  )

med_runtime <- stats::median(n36_e$wall_clock_min)

p2e_raw <- ggplot2::ggplot() +
  ggplot2::geom_vline(
    xintercept = med_runtime, linetype = "dashed", color = col_divider, linewidth = LW_025
  ) +
  ggplot2::annotate(
    "text", x = med_runtime * 1.05, y = 3.00,
    label = sprintf("Median %.1f min", med_runtime),
    hjust = 0, vjust = 0.5, family = "Helvetica", size = GG_TEXT_SIZE, color = col_dark
  ) +
  ggplot2::geom_step(
    data = pareto_line,
    ggplot2::aes(x = wall_clock_min, y = causal_node_score_sum),
    direction = "hv", color = col_dark, linewidth = LW_050
  ) +
  ggplot2::geom_point(
    data = n36_e_dom,
    ggplot2::aes(x = wall_clock_min, y = y_plot, color = model_f),
    shape = 16, size = 1.75, alpha = 0.88
  ) +
  ggplot2::geom_point(
    data = pareto_pts,
    ggplot2::aes(x = wall_clock_min, y = causal_node_score_sum, color = model_f),
    shape = 16, size = 2.05
  ) +
  ggplot2::geom_point(
    data = pareto_pts,
    ggplot2::aes(x = wall_clock_min, y = causal_node_score_sum),
    shape = 21, size = 3.15, fill = NA, color = "black", stroke = 0.40
  ) +
  ggplot2::geom_segment(
    data = label_df,
    ggplot2::aes(
      x = wall_clock_min, y = causal_node_score_sum,
      xend = ifelse(lbl_y > 7, wall_clock_min * 0.90, lbl_x * 0.97),
      yend = ifelse(lbl_y > 7, lbl_y - 0.08, lbl_y + 0.04)
    ),
    color = col_dark, linewidth = LW_025, inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = label_df,
    ggplot2::aes(x = lbl_x, y = lbl_y, label = pt_label),
    hjust = 0, vjust = 0.5, family = "Helvetica", size = GG_TEXT_SIZE,
    color = "black", inherit.aes = FALSE
  ) +
  ggplot2::scale_color_manual(values = fig2_model_cols, guide = "none") +
  ggplot2::scale_x_continuous(
    trans  = "log2",
    limits = c(28, 270),
    breaks = c(30, 60, 120, 240),
    labels = c("30", "60", "120", "240"),
    expand = c(0.01, 0)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(2.65, 7.75),
    breaks = 3:7,
    labels = sprintf("%d/7", 3:7),
    expand = c(0, 0)
  ) +
  ggplot2::labs(x = "Wall-clock runtime (min, log2 scale)", y = "Causal nodes recovered") +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text   = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title  = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    plot.margin = ggplot2::margin(2, 6, 4, 4)
  )

p2e <- panel_header(p2e_raw, "e", "Runtime and speed-recall Pareto frontier")
save_panel_pair(p2e, file.path(fig2_dir, "fig2e_runtime_vs_recall.pdf"), width = 3.60, height = 2.25, dpi = 300L)

# ---------------------------------------------------------------------------
# Assemble single-page Nature composite: a / (b | c) / (d | e)
# ---------------------------------------------------------------------------
p2a_wrapped <- patchwork::wrap_elements(full = ggplot2::ggplotGrob(p2a_stack))
row_mid     <- patchwork::wrap_elements(full = patchwork::patchworkGrob((p2b | p2c) + patchwork::plot_layout(widths = c(1, 1))))
row_bot     <- patchwork::wrap_elements(full = patchwork::patchworkGrob((p2d | p2e) + patchwork::plot_layout(widths = c(1, 1))))

fig2_composite <- (p2a_wrapped / row_mid / row_bot) +
  patchwork::plot_layout(heights = c(1.06, 1.00, 1.00)) +
  patchwork::plot_annotation(
    caption = "*Assigned Claude Opus 5.5 (6/6 automatic bio-safety fallback to Claude Opus 5.0). **Subagent bio-safety halts in 4/6 standard runs (0/6 with literature).",
    theme = ggplot2::theme(
      plot.caption = ggplot2::element_text(
        family = "Helvetica", face = "plain", size = TEXT_SIZE,
        color = col_dark, hjust = 0, margin = ggplot2::margin(t = 2, l = 4)
      )
    )
  )

composite_pdf <- file.path(fig2_dir, "Figure_2_composite.pdf")
save_panel_pair(fig2_composite, composite_pdf, width = 7.20, height = 7.25, dpi = 300L)

cat(
  sprintf(
    "Figure 2 complete:\n  Pareto runs (%d): %s\n  Blinded QC discard median: %.1f%% (range %.1f-%.1f%%)\n  Composite: %s\n",
    nrow(pareto_pts),
    paste(sprintf("%s %.1f min %d/7", pareto_pts$actual_ceo_model, pareto_pts$wall_clock_min, pareto_pts$causal_node_score_sum), collapse = "; "),
    overall_med_disc, min(n36$discard_pct), max(n36$discard_pct),
    composite_pdf
  )
)
