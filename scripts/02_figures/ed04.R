#!/usr/bin/env Rscript
# Extended Data Figure 4: Matched Without vs With literature ablation (Sonnet 5.5 & Hydrogen, N = 24)
# Standardized to 7.20 in width, Helvetica Bold 9 pt tags, Helvetica Plain 6 pt body,
# 0.25 pt lines, Figure 2 bar + beeswarm grammar, and canonical model colors from R/colors.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(ggbeeswarm)
  library(patchwork)
})

source(file.path("config", "paths.R"))
source(file.path("R", "colors.R"))
source(file.path("R", "theme.R"))
source(file.path("R", "stats_utils.R"))
source(file.path("R", "plot_utils.R"))

set.seed(GLOBAL_SEED)

out_dir <- ensure_dir(file.path(path_figures, "Extended_Data_4"))

TAG_SIZE     <- 9.0
TEXT_SIZE    <- 6.0
GG_TEXT_SIZE <- TEXT_SIZE / ggplot2::.pt

sc36 <- require_file(file.path(path_sd_benchmark, "scored_rubric_N36.csv")) %>%
  readr::read_csv(show_col_types = FALSE, progress = FALSE)
tel36 <- require_file(file.path(path_sd_benchmark, "run_telemetry_N36.csv")) %>%
  readr::read_csv(show_col_types = FALSE, progress = FALSE)

require_columns(
  sc36,
  c("model_slug", "actual_ceo_model", "condition", "replicate", "run_path",
    paste0("N", 1:7, "_score"), "causal_node_score_sum", "n_non_chain_discoveries"),
  "scored_rubric_N36.csv"
)
require_columns(
  tel36,
  c("run_path", "wall_clock_min", "n_discarded_hypotheses", "subagent_safety_interrupts"),
  "run_telemetry_N36.csv"
)

abl <- sc36 %>%
  dplyr::inner_join(
    tel36 %>% dplyr::select(run_path, wall_clock_min, n_discarded_hypotheses, subagent_safety_interrupts),
    by = "run_path"
  ) %>%
  dplyr::filter(model_slug %in% c("claude_sonnet_5_5", "google_gemini_4_hydrogen")) %>%
  dplyr::mutate(
    model_short = factor(
      dplyr::if_else(model_slug == "claude_sonnet_5_5", "Claude Sonnet 5.5", "Gemini 4 Hydrogen"),
      levels = c("Claude Sonnet 5.5", "Gemini 4 Hydrogen")
    ),
    lit_arm = factor(
      dplyr::if_else(condition %in% c("seeded_lit", "cold_lit"), "With literature", "Without literature"),
      levels = c("Without literature", "With literature")
    ),
    framing = factor(
      dplyr::if_else(condition %in% c("seeded", "seeded_lit"), "Seeded", "Cold"),
      levels = c("Seeded", "Cold")
    ),
    arm_detail = factor(
      dplyr::case_when(
        condition == "seeded"     ~ "Seeded, without",
        condition == "seeded_lit" ~ "Seeded, with lit.",
        condition == "cold"       ~ "Cold, without",
        condition == "cold_lit"   ~ "Cold, with lit."
      ),
      levels = c("Seeded, without", "Seeded, with lit.", "Cold, without", "Cold, with lit.")
    ),
    has_sub_stop = as.integer(subagent_safety_interrupts > 0),
    pt_col = unname(model_colors[as.character(model_short)]),
    pt_fill = dplyr::if_else(framing == "Cold", pt_col, "white")
  )

stopifnot(nrow(abl) == 24L)

abl_model_cols <- model_colors[c("Claude Sonnet 5.5", "Gemini 4 Hydrogen")]

# ---------------------------------------------------------------------------
# Panel a: Recall (n/7) & per-node recovery heatmap across Without vs With literature
# ---------------------------------------------------------------------------
recall_sum <- abl %>%
  dplyr::group_by(model_short, lit_arm) %>%
  dplyr::summarise(
    mean_n = mean(causal_node_score_sum),
    sd_n   = stats::sd(causal_node_score_sum),
    low_n  = pmax(0, mean_n - sd_n),
    high_n = pmin(7, mean_n + sd_n),
    .groups = "drop"
  )

recall_p <- abl %>%
  dplyr::group_by(model_short) %>%
  dplyr::summarise(
    p_val = suppressWarnings(stats::wilcox.test(
      causal_node_score_sum[lit_arm == "With literature"],
      causal_node_score_sum[lit_arm == "Without literature"]
    )$p.value),
    .groups = "drop"
  ) %>%
  dplyr::mutate(p_lbl = format_pval(p_val))

p_ed4a_left <- ggplot2::ggplot(abl, ggplot2::aes(x = lit_arm, y = causal_node_score_sum)) +
  ggplot2::geom_col(
    data = recall_sum,
    ggplot2::aes(x = lit_arm, y = mean_n),
    width = 0.54, fill = col_bar_gray, color = NA
  ) +
  ggplot2::geom_errorbar(
    data = recall_sum,
    ggplot2::aes(x = lit_arm, y = mean_n, ymin = low_n, ymax = high_n),
    width = 0, linewidth = LW_025, color = "black"
  ) +
  ggbeeswarm::geom_quasirandom(
    ggplot2::aes(fill = pt_fill, color = model_short, shape = framing),
    size = 1.65, stroke = 0.45, width = 0.14
  ) +
  ggplot2::geom_text(
    data = recall_sum,
    ggplot2::aes(x = lit_arm, y = 0.45, label = sprintf("%.2f/7", mean_n)),
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::geom_text(
    data = recall_p,
    ggplot2::aes(x = 1.5, y = 7.45, label = p_lbl),
    inherit.aes = FALSE,
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::facet_wrap(~ model_short, ncol = 2) +
  ggplot2::scale_color_manual(values = abl_model_cols, name = NULL) +
  ggplot2::scale_fill_identity() +
  ggplot2::scale_shape_manual(values = c("Seeded" = 21, "Cold" = 24), name = "Prompt") +
  ggplot2::scale_y_continuous(limits = c(0, 7.85), breaks = 0:7, expand = c(0, 0)) +
  ggplot2::labs(x = NULL, y = "Causal nodes recovered (n/7)") +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    strip.text       = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.text.x      = ggplot2::element_text(size = TEXT_SIZE, color = "black", angle = 15, hjust = 1),
    legend.position  = "bottom",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.18, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE),
    legend.title     = ggplot2::element_text(size = TEXT_SIZE),
    legend.margin    = ggplot2::margin(t = -3),
    plot.margin      = ggplot2::margin(2, 4, 3, 4)
  )

node_heat <- abl %>%
  dplyr::select(model_short, arm_detail, dplyr::all_of(paste0("N", 1:7, "_score"))) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(paste0("N", 1:7, "_score")),
    names_to = "node",
    values_to = "score"
  ) %>%
  dplyr::mutate(node = factor(sub("_score$", "", node), levels = paste0("N", 1:7))) %>%
  dplyr::group_by(model_short, arm_detail, node) %>%
  dplyr::summarise(
    k = sum(score == 1),
    n = dplyr::n(),
    pct = 100 * k / n,
    lbl = sprintf("%d/%d", k, n),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    arm_detail = factor(arm_detail, levels = rev(levels(abl$arm_detail)))
  )

p_ed4a_right <- ggplot2::ggplot(node_heat, ggplot2::aes(x = node, y = arm_detail, fill = pct)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = lbl,
      color = dplyr::if_else(pct >= 67, "white", "black")
    ),
    size = GG_TEXT_SIZE, family = "Helvetica"
  ) +
  ggplot2::facet_wrap(~ model_short, ncol = 1) +
  ggplot2::scale_fill_gradientn(
    colours = c(col_stripe, pal_blue_light, pal_blue, pal_blue_dark),
    values  = c(0, 0.33, 0.67, 1.0),
    limits  = c(0, 100),
    name    = "Recovery (%)"
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_x_discrete(position = "top") +
  ggplot2::labs(x = NULL, y = NULL) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line       = ggplot2::element_blank(),
    axis.ticks      = ggplot2::element_blank(),
    strip.text      = ggplot2::element_text(size = TEXT_SIZE, hjust = 0, color = "black"),
    axis.text.y     = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.text.x     = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    legend.position = "bottom",
    legend.key.height = grid::unit(0.18, "cm"),
    legend.key.width  = grid::unit(0.50, "cm"),
    legend.text     = ggplot2::element_text(size = TEXT_SIZE),
    legend.title    = ggplot2::element_text(size = TEXT_SIZE),
    legend.margin   = ggplot2::margin(t = -3),
    plot.margin     = ggplot2::margin(2, 6, 3, 6)
  )

p_ed4a_left_tagged <- apply_panel_header(
  p_ed4a_left, "a",
  "Literature ablation causal recall and node recovery (N = 24)",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
)
p_ed4a_comp <- patchwork::wrap_plots(p_ed4a_left_tagged, p_ed4a_right, ncol = 2, widths = c(1.0, 1.15))
save_panel_pair(p_ed4a_comp, file.path(out_dir, "ed4a_ablation_recall_and_nodes.pdf"), width = 7.20, height = 3.00)

# ---------------------------------------------------------------------------
# Panel b: Runtime, QC refutations, beyond-rubric discoveries & subagent halts
# ---------------------------------------------------------------------------
cont_df <- abl %>%
  dplyr::select(
    model_short, lit_arm, framing, pt_fill,
    `Wall-clock runtime (min)`      = wall_clock_min,
    `Hypotheses refuted (n)`        = n_discarded_hypotheses,
    `Beyond-rubric discoveries (n)` = n_non_chain_discoveries
  ) %>%
  tidyr::pivot_longer(
    cols = c("Wall-clock runtime (min)", "Hypotheses refuted (n)", "Beyond-rubric discoveries (n)"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    metric = factor(
      metric,
      levels = c("Wall-clock runtime (min)", "Hypotheses refuted (n)", "Beyond-rubric discoveries (n)")
    ),
    group_x = factor(
      paste0(
        dplyr::if_else(model_short == "Claude Sonnet 5.5", "Sonnet\n", "Hydrogen\n"),
        dplyr::if_else(lit_arm == "Without literature", "No lit.", "With lit.")
      ),
      levels = c("Sonnet\nNo lit.", "Sonnet\nWith lit.", "Hydrogen\nNo lit.", "Hydrogen\nWith lit.")
    )
  )

cont_sum <- cont_df %>%
  dplyr::group_by(metric, model_short, lit_arm, group_x) %>%
  dplyr::summarise(
    mean_v = mean(value),
    sd_v   = stats::sd(value),
    low_v  = pmax(0, mean_v - sd_v),
    high_v = mean_v + sd_v,
    .groups = "drop"
  )

cont_p <- cont_df %>%
  dplyr::group_by(metric, model_short) %>%
  dplyr::summarise(
    p_val = suppressWarnings(stats::wilcox.test(
      value[lit_arm == "With literature"],
      value[lit_arm == "Without literature"]
    )$p.value),
    y_max = max(value),
    .groups = "drop"
  ) %>%
  dplyr::group_by(metric) %>%
  dplyr::mutate(
    y_pos = max(y_max) * 1.13,
    x_pos = dplyr::if_else(model_short == "Claude Sonnet 5.5", 1.5, 3.5),
    p_lbl = format_pval(p_val)
  ) %>%
  dplyr::ungroup()

p_ed4b_cont <- ggplot2::ggplot(cont_df, ggplot2::aes(x = group_x, y = value)) +
  ggplot2::geom_col(
    data = cont_sum,
    ggplot2::aes(x = group_x, y = mean_v),
    width = 0.56, fill = col_bar_gray, color = NA
  ) +
  ggplot2::geom_errorbar(
    data = cont_sum,
    ggplot2::aes(x = group_x, y = mean_v, ymin = low_v, ymax = high_v),
    width = 0, linewidth = LW_025, color = "black"
  ) +
  ggbeeswarm::geom_quasirandom(
    ggplot2::aes(fill = pt_fill, color = model_short, shape = framing),
    size = 1.55, stroke = 0.40, width = 0.14
  ) +
  ggplot2::geom_vline(xintercept = 2.5, linetype = "dashed", color = col_divider, linewidth = LW_025) +
  ggplot2::geom_text(
    data = cont_p,
    ggplot2::aes(x = x_pos, y = y_pos, label = p_lbl),
    inherit.aes = FALSE,
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::facet_wrap(~ metric, scales = "free_y", ncol = 3) +
  ggplot2::scale_color_manual(values = abl_model_cols, name = NULL) +
  ggplot2::scale_fill_identity() +
  ggplot2::scale_shape_manual(values = c("Seeded" = 21, "Cold" = 24), name = "Prompt") +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.22))) +
  ggplot2::labs(x = NULL, y = NULL) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x      = ggplot2::element_text(size = 5.4, lineheight = 0.88),
    strip.text       = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    legend.position  = "bottom",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.18, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE),
    legend.title     = ggplot2::element_text(size = TEXT_SIZE),
    legend.margin    = ggplot2::margin(t = -3),
    plot.margin      = ggplot2::margin(2, 4, 3, 4)
  )

# 4th sub-panel: runs with subagent safety halts (%)
safe_df <- abl %>%
  dplyr::group_by(model_short, lit_arm) %>%
  dplyr::summarise(
    k = sum(has_sub_stop),
    n = dplyr::n(),
    pct = 100 * k / n,
    events_tot = sum(subagent_safety_interrupts),
    lbl = dplyr::if_else(
      k > 0,
      sprintf("%d/%d\n(%.0f%%)", k, n, pct),
      sprintf("0/%d", n)
    ),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    group_x = factor(
      paste0(
        dplyr::if_else(model_short == "Claude Sonnet 5.5", "Sonnet\n", "Hydrogen\n"),
        dplyr::if_else(lit_arm == "Without literature", "No lit.", "With lit.")
      ),
      levels = c("Sonnet\nNo lit.", "Sonnet\nWith lit.", "Hydrogen\nNo lit.", "Hydrogen\nWith lit.")
    )
  )

ft_sonnet <- stats::fisher.test(matrix(c(4, 2, 0, 6), nrow = 2))

p_ed4b_safe <- ggplot2::ggplot(safe_df, ggplot2::aes(x = group_x, y = pct, fill = lit_arm)) +
  ggplot2::geom_col(width = 0.56, color = NA) +
  ggplot2::geom_text(
    ggplot2::aes(y = pct + 3.5, label = lbl),
    vjust = 0, size = 5.0 / ggplot2::.pt, family = "Helvetica", color = "black", lineheight = 0.90
  ) +
  ggplot2::geom_vline(xintercept = 2.5, linetype = "dashed", color = col_divider, linewidth = LW_025) +
  ggplot2::annotate(
    "text", x = 1.5, y = 104,
    label = sprintf("Subagent pool pinned\nFisher %s", format_pval(ft_sonnet$p.value)),
    size = GG_TEXT_SIZE, family = "Helvetica", color = pal_coral_dark, lineheight = 0.92
  ) +
  ggplot2::scale_fill_manual(
    values = c("Without literature" = pal_coral, "With literature" = col_bar_gray),
    guide = "none"
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 118), breaks = seq(0, 100, by = 25), expand = c(0, 0)) +
  ggplot2::labs(x = NULL, y = "Runs with subagent halt (%)") +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 5.4, lineheight = 0.88),
    plot.margin = ggplot2::margin(2, 6, 3, 4)
  )

p_ed4b_cont_tagged <- apply_panel_header(
  p_ed4b_cont, "b",
  "Runtime, blinded QC refutations, discoveries, and Sonnet subagent halts",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
)
p_ed4b_comp <- patchwork::wrap_plots(p_ed4b_cont_tagged, p_ed4b_safe, ncol = 2, widths = c(2.65, 1.00))
save_panel_pair(p_ed4b_comp, file.path(out_dir, "ed4b_ablation_runtime_and_subagents.pdf"), width = 7.20, height = 3.00)

# ---------------------------------------------------------------------------
# Composite Extended Data Fig. 4 (7.20 in Nature width)
# ---------------------------------------------------------------------------
ed4_comp <- patchwork::wrap_plots(
  patchwork::wrap_elements(full = patchwork::patchworkGrob(p_ed4a_comp)),
  patchwork::wrap_elements(full = patchwork::patchworkGrob(p_ed4b_comp)),
  ncol = 1, heights = c(1.04, 1.00)
)
save_panel_pair(ed4_comp, file.path(out_dir, "Extended_Data_4_composite.pdf"), width = 7.20, height = 6.00)
message("Extended Data Fig. 4 complete: ", out_dir)
