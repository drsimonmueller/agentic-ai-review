#!/usr/bin/env Rscript
# Extended Data Figure 3: Telemetry, runtime decomposition, scripts vs Slurm jobs, tokens & blinded QC counts (N = 36)
# Standardized to 7.20 in width, Helvetica Bold 9 pt tags, Helvetica Plain 6 pt body,
# 0.25 pt lines, and canonical 4-model colors from R/colors.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(ggrepel)
  library(patchwork)
})

source(file.path("config", "paths.R"))
source(file.path("R", "colors.R"))
source(file.path("R", "theme.R"))
source(file.path("R", "stats_utils.R"))
source(file.path("R", "plot_utils.R"))

set.seed(GLOBAL_SEED)

out_dir <- ensure_dir(file.path(path_figures, "Extended_Data_3"))

TAG_SIZE     <- 9.0
TEXT_SIZE    <- 6.0
GG_TEXT_SIZE <- TEXT_SIZE / ggplot2::.pt

tel36 <- require_file(file.path(path_sd_benchmark, "run_telemetry_N36.csv")) %>%
  readr::read_csv(show_col_types = FALSE, progress = FALSE)

require_columns(
  tel36,
  c(
    "provider", "model_slug", "actual_ceo_model", "condition", "replicate", "run_path",
    "wall_clock_min", "slurm_job_union_min", "slurm_queue_union_min", "active_agent_min",
    "n_slurm_jobs", "n_scripts_executed", "tokens_input_uncached", "tokens_cache_read",
    "tokens_cache_write", "tokens_output", "n_confirmed_findings", "n_discarded_hypotheses"
  ),
  "run_telemetry_N36.csv"
)

model_map_ed3 <- c(
  "Claude Opus 5.0"   = "Claude Opus 5.0*",
  "Claude Sonnet 5.5" = "Claude Sonnet 5.5",
  "Gemini 4 Hydrogen" = "Gemini 4 Hydrogen",
  "Gemini 4 Neon"     = "Gemini 4 Neon"
)

cond_order <- c("seeded", "cold", "seeded_lit", "cold_lit")
cond_pretty <- c(
  "seeded"     = "Seeded",
  "cold"       = "Cold",
  "seeded_lit" = "Seeded, lit.",
  "cold_lit"   = "Cold, lit."
)
model_short_y <- c(
  "Claude Opus 5.0*"  = "Opus 5.0*",
  "Claude Sonnet 5.5" = "Sonnet 5.5",
  "Gemini 4 Hydrogen" = "Hydrogen",
  "Gemini 4 Neon"     = "Neon"
)

ed3_model_cols <- model_colors[model_display_order]

tel_ord <- tel36 %>%
  dplyr::mutate(
    model_f        = factor(model_map_ed3[actual_ceo_model], levels = model_display_order),
    cond_ord       = factor(condition, levels = cond_order),
    rep_idx        = sub("run_0", "", replicate),
    arm_group      = factor(dplyr::if_else(condition %in% c("seeded_lit", "cold_lit"), "With literature", "Without literature"),
                            levels = c("Without literature", "With literature")),
    run_label      = sprintf("%s | %s (%s)", model_short_y[as.character(model_f)], cond_pretty[condition], rep_idx),
    slurm_exec_min = pmax(0, slurm_job_union_min - slurm_queue_union_min)
  ) %>%
  dplyr::arrange(model_f, cond_ord, replicate)

run_levels <- rev(tel_ord$run_label)
tel_ord <- tel_ord %>%
  dplyr::mutate(
    run_label = factor(run_label, levels = run_levels),
    y_num     = as.numeric(run_label)
  )

# Canonical model strip data for 36-run vertical panels
model_dot_36 <- tel_ord %>%
  dplyr::transmute(run_label, y_num, model_f)

# ---------------------------------------------------------------------------
# Panel a: Runtime decomposition (active agent + Slurm compute + queue wait)
# ---------------------------------------------------------------------------
rt_long <- tel_ord %>%
  dplyr::select(
    run_label,
    `Active agent reasoning` = active_agent_min,
    `Slurm compute`          = slurm_exec_min,
    `Slurm queue wait`       = slurm_queue_union_min
  ) %>%
  tidyr::pivot_longer(cols = -run_label, names_to = "component", values_to = "minutes") %>%
  dplyr::mutate(
    component = factor(
      component,
      levels = c("Slurm queue wait", "Slurm compute", "Active agent reasoning")
    )
  )

rt_pal <- c(
  "Active agent reasoning" = pal_blue,
  "Slurm compute"          = col_qc_gray,
  "Slurm queue wait"       = col_bar_gray
)

p_ed3a <- ggplot2::ggplot(rt_long, ggplot2::aes(x = minutes, y = run_label, fill = component)) +
  ggplot2::geom_col(width = 0.72, color = NA) +
  ggplot2::geom_point(
    data = model_dot_36,
    ggplot2::aes(x = -6.0, y = run_label, color = model_f),
    inherit.aes = FALSE, shape = 15, size = 1.60, show.legend = FALSE
  ) +
  ggplot2::scale_color_manual(values = ed3_model_cols, guide = "none") +
  ggplot2::geom_text(
    data = tel_ord,
    ggplot2::aes(x = wall_clock_min + 2.5, y = run_label, label = sprintf("%.0f min", wall_clock_min)),
    inherit.aes = FALSE,
    hjust = 0, size = 4.6 / ggplot2::.pt, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_fill_manual(
    values = rt_pal,
    breaks = c("Active agent reasoning", "Slurm compute", "Slurm queue wait"),
    name = NULL
  ) +
  ggplot2::scale_x_continuous(
    limits = c(-10, max(tel_ord$wall_clock_min) * 1.15),
    breaks = seq(0, 240, by = 60),
    expand = c(0, 0)
  ) +
  ggplot2::labs(x = "Wall-clock runtime (min)", y = NULL) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line.y      = ggplot2::element_blank(),
    axis.ticks.y     = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = 5.2, color = "black"),
    legend.position  = "bottom",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.18, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.margin    = ggplot2::margin(t = -3),
    plot.margin      = ggplot2::margin(12, 6, 3, 3)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

p_ed3a_titled <- apply_panel_header(
  p_ed3a, "a",
  "Wall-clock runtime decomposition (N = 36)",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
)
save_panel_pair(p_ed3a_titled, file.path(out_dir, "ed3a_runtime_decomposition.pdf"), width = 3.90, height = 4.80)

# ---------------------------------------------------------------------------
# Panel b: Slurm jobs vs scripts executed (colored by canonical 4-model palette)
# ---------------------------------------------------------------------------
ct_b <- stats::cor.test(tel_ord$n_scripts_executed, tel_ord$n_slurm_jobs, method = "spearman", exact = FALSE)

tel_ord <- tel_ord %>%
  dplyr::mutate(
    is_outlier = n_scripts_executed >= 43 | n_slurm_jobs >= 29 | n_slurm_jobs <= 4,
    short_tag = sprintf("%s %s (%s)", model_short_y[as.character(model_f)], cond_pretty[condition], rep_idx)
  )

label_pts <- tel_ord %>% dplyr::filter(is_outlier)

p_ed3b <- ggplot2::ggplot(
  tel_ord,
  ggplot2::aes(x = n_scripts_executed, y = n_slurm_jobs, color = model_f, shape = arm_group)
) +
  ggplot2::geom_smooth(
    data = tel_ord,
    ggplot2::aes(x = n_scripts_executed, y = n_slurm_jobs),
    inherit.aes = FALSE,
    method = "lm", se = TRUE, color = col_dark, fill = col_bar_gray,
    linewidth = LW_025, alpha = 0.60, show.legend = FALSE
  ) +
  ggplot2::geom_point(size = 1.85, alpha = 0.90) +
  ggrepel::geom_text_repel(
    data = label_pts,
    ggplot2::aes(label = short_tag),
    size = 4.8 / ggplot2::.pt, family = "Helvetica",
    box.padding = 0.22, point.padding = 0.16, min.segment.length = 0.1,
    segment.size = 0.25, max.overlaps = 15, show.legend = FALSE
  ) +
  ggplot2::annotate(
    "text",
    x = 78, y = 2.0,
    label = sprintf("Spearman r = %.2f, %s", unname(ct_b$estimate), format_pval(ct_b$p.value)),
    hjust = 1, vjust = 0, size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_color_manual(values = ed3_model_cols, name = NULL) +
  ggplot2::scale_shape_manual(values = c("Without literature" = 16, "With literature" = 17), name = NULL) +
  ggplot2::scale_x_continuous(limits = c(10, 80), breaks = seq(15, 75, by = 15)) +
  ggplot2::scale_y_continuous(limits = c(0, 46), breaks = seq(0, 45, by = 10)) +
  ggplot2::labs(x = "Scripts executed (n)", y = "Slurm jobs submitted (n)") +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    legend.position  = "bottom",
    legend.box       = "vertical",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.20, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE),
    legend.spacing.y = grid::unit(0.16, "cm"),
    legend.box.spacing = grid::unit(0.12, "cm"),
    legend.margin    = ggplot2::margin(t = 1, b = 1),
    plot.margin      = ggplot2::margin(2, 6, 3, 4)
  ) +
  ggplot2::guides(
    color = ggplot2::guide_legend(order = 1, nrow = 2, byrow = TRUE, override.aes = list(size = 1.8)),
    shape = ggplot2::guide_legend(order = 2, nrow = 1, override.aes = list(size = 1.8))
  )

p_ed3b_titled <- apply_panel_header(
  p_ed3b, "b",
  "Slurm jobs versus analysis scripts per run (N = 36)",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
)
save_panel_pair(p_ed3b_titled, file.path(out_dir, "ed3b_slurm_and_scripts_footprint.pdf"), width = 3.20, height = 3.60)

# ---------------------------------------------------------------------------
# Panel c: Anthropic token usage (n = 18; Google & cost not available)
# ---------------------------------------------------------------------------
anth_ord <- tel_ord %>%
  dplyr::filter(provider == "Anthropic") %>%
  dplyr::mutate(run_label = droplevels(run_label))

anth_noncache <- anth_ord %>%
  dplyr::transmute(
    run_label = run_label,
    panel = "Non-cache tokens (millions)",
    `Uncached input` = tokens_input_uncached / 1e6,
    `Cache write`    = tokens_cache_write / 1e6,
    `Output`         = tokens_output / 1e6
  ) %>%
  tidyr::pivot_longer(
    cols = c("Uncached input", "Cache write", "Output"),
    names_to = "token_type",
    values_to = "millions"
  )

anth_cacheread <- anth_ord %>%
  dplyr::transmute(
    run_label = run_label,
    panel = "Cache-read tokens (millions)",
    token_type = "Cache read",
    millions = tokens_cache_read / 1e6
  )

tok_df <- dplyr::bind_rows(anth_noncache, anth_cacheread) %>%
  dplyr::mutate(
    panel = factor(panel, levels = c("Non-cache tokens (millions)", "Cache-read tokens (millions)")),
    token_type = factor(token_type, levels = c("Uncached input", "Cache write", "Output", "Cache read"))
  )

tok_pal <- c(
  "Output"         = pal_blue_dark,
  "Cache write"    = pal_blue,
  "Uncached input" = pal_blue_light,
  "Cache read"     = col_qc_gray
)

note_df <- tibble::tibble(
  panel = factor("Cache-read tokens (millions)", levels = c("Non-cache tokens (millions)", "Cache-read tokens (millions)")),
  x = 198,
  y = 1.0,
  label = "USD cost & Google (n = 18): NA\nUncached input: 0.3k-2.1k"
)

p_ed3c <- ggplot2::ggplot(tok_df, ggplot2::aes(x = millions, y = run_label, fill = token_type)) +
  ggplot2::geom_col(width = 0.70, color = NA) +
  ggplot2::geom_text(
    data = note_df,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1, vjust = 0, size = 4.8 / ggplot2::.pt, family = "Helvetica", color = col_dark, lineheight = 0.92
  ) +
  ggplot2::facet_wrap(~ panel, scales = "free_x", ncol = 2) +
  ggplot2::scale_fill_manual(
    values = tok_pal,
    breaks = c("Output", "Cache write", "Uncached input", "Cache read"),
    name = NULL
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
  ggplot2::labs(x = "Tokens (millions)", y = NULL) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line.y      = ggplot2::element_blank(),
    axis.ticks.y     = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = 5.2, color = "black"),
    strip.text       = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    legend.position  = "bottom",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.18, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.margin    = ggplot2::margin(t = -3),
    plot.margin      = ggplot2::margin(2, 6, 3, 4)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

p_ed3c_titled <- apply_panel_header(
  p_ed3c, "c",
  "Anthropic token consumption (n = 18)",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
)
save_panel_pair(p_ed3c_titled, file.path(out_dir, "ed3c_anthropic_tokens_cost.pdf"), width = 4.00, height = 3.60)

# ---------------------------------------------------------------------------
# Panel d: Blinded QC confirmed findings vs refuted hypotheses (N = 36)
# ---------------------------------------------------------------------------
qc_long <- tel_ord %>%
  dplyr::select(
    run_label,
    `Confirmed findings`    = n_confirmed_findings,
    `Refuted in blinded QC` = n_discarded_hypotheses
  ) %>%
  tidyr::pivot_longer(cols = -run_label, names_to = "outcome", values_to = "count") %>%
  dplyr::mutate(
    outcome = factor(outcome, levels = c("Refuted in blinded QC", "Confirmed findings"))
  )

qc_tot <- tel_ord %>%
  dplyr::transmute(
    run_label  = run_label,
    total_eval = n_confirmed_findings + n_discarded_hypotheses,
    pct_disc   = 100 * n_discarded_hypotheses / total_eval,
    lbl        = sprintf("%d/%d (%.0f%%)", n_discarded_hypotheses, total_eval, pct_disc)
  )

p_ed3d <- ggplot2::ggplot(qc_long, ggplot2::aes(x = count, y = run_label, fill = outcome)) +
  ggplot2::geom_col(width = 0.72, color = NA) +
  ggplot2::geom_text(
    data = qc_tot,
    ggplot2::aes(x = total_eval + 0.8, y = run_label, label = lbl),
    inherit.aes = FALSE,
    hjust = 0, size = 4.6 / ggplot2::.pt, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_fill_manual(
    values = c("Confirmed findings" = pal_blue, "Refuted in blinded QC" = col_qc_gray),
    breaks = c("Confirmed findings", "Refuted in blinded QC"),
    name = NULL
  ) +
  ggplot2::scale_x_continuous(limits = c(0, max(qc_tot$total_eval) * 1.28), expand = c(0, 0)) +
  ggplot2::labs(x = "Tested hypotheses (n; refuted/total, %)", y = NULL) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line.y      = ggplot2::element_blank(),
    axis.ticks.y     = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(size = 5.2, color = "black"),
    legend.position  = "bottom",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.18, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.margin    = ggplot2::margin(t = -3),
    plot.margin      = ggplot2::margin(12, 6, 3, 4)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

p_ed3d_titled <- apply_panel_header(
  p_ed3d, "d",
  "Blinded quality-control hypothesis filtering (N = 36)",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
)
save_panel_pair(p_ed3d_titled, file.path(out_dir, "ed3d_tier4_qc_discard_yield.pdf"), width = 3.90, height = 4.80)

# ---------------------------------------------------------------------------
# Composite Extended Data Fig. 3 (7.20 in Nature width)
# ---------------------------------------------------------------------------
p_ed3d_comp <- apply_panel_header(
  p_ed3d + ggplot2::theme(axis.text.y = ggplot2::element_blank(), plot.margin = ggplot2::margin(12, 6, 3, 18)),
  "d",
  "Blinded quality-control falsification yield",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
) +
  ggplot2::theme(plot.tag.position = c(-0.04, 0.995))

top_row_3 <- patchwork::wrap_elements(
  full = patchwork::patchworkGrob(patchwork::wrap_plots(p_ed3a_titled, p_ed3d_comp, ncol = 2, widths = c(1.42, 1.00)))
)
bot_row_3 <- patchwork::wrap_elements(
  full = patchwork::patchworkGrob(patchwork::wrap_plots(p_ed3b_titled, p_ed3c_titled, ncol = 2, widths = c(1.00, 1.38)))
)
ed3_comp <- patchwork::wrap_plots(top_row_3, bot_row_3, ncol = 1, heights = c(1.36, 1.00))

save_panel_pair(ed3_comp, file.path(out_dir, "Extended_Data_3_composite.pdf"), width = 7.20, height = 8.40)
message("Extended Data Fig. 3 complete: ", out_dir)
