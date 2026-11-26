#!/usr/bin/env Rscript
# Extended Data Figure 1: Full 6-line human reference audit of Montero et al.
# Standardized to 7.20 in width, Helvetica Bold 9 pt tags, Helvetica Plain 6 pt body, 0.25 pt lines.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

source(file.path("config", "paths.R"))
source(file.path("R", "colors.R"))
source(file.path("R", "theme.R"))
source(file.path("R", "stats_utils.R"))
source(file.path("R", "plot_utils.R"))

set.seed(GLOBAL_SEED)

out_dir <- ensure_dir(file.path(path_figures, "Extended_Data_1"))

TAG_SIZE     <- 9.0
TEXT_SIZE    <- 6.0
GG_TEXT_SIZE <- TEXT_SIZE / ggplot2::.pt

# ---------------------------------------------------------------------------
# Panel a: 6-line AE recall at FDR < 0.25 & < 0.05 vs DepMap 22Q2 + ROC AUC
# ---------------------------------------------------------------------------
ae_df <- require_file(file.path(path_sd_forensics, "ae_recall_vs_depmap_6lines.csv")) %>%
  readr::read_csv(show_col_types = FALSE, progress = FALSE)

require_columns(
  ae_df,
  c(
    "cell_line", "display_name",
    "depmap_cas9_recall_lt_neg05_pct", "depmap_cas9_recall_lt_neg10_pct",
    "casrx_ae_recall_fdr25_pct", "casrx_ae_recall_fdr05_pct",
    "casrx_roc_auc_ae_vs_ne"
  ),
  "ae_recall_vs_depmap_6lines.csv"
)

line_order_a <- c("LN-18", "KP-4", "MIA PaCa-2", "NCI-H460", "A549", "LN-229", "6-Line Union")

ae_long <- ae_df %>%
  dplyr::mutate(display_name = factor(display_name, levels = line_order_a)) %>%
  dplyr::select(
    display_name,
    `DepMap Cas9 (Chronos < -0.5)` = depmap_cas9_recall_lt_neg05_pct,
    `DepMap Cas9 (Chronos < -1.0)` = depmap_cas9_recall_lt_neg10_pct,
    `CasRx (FDR < 0.25)`           = casrx_ae_recall_fdr25_pct,
    `CasRx (FDR < 0.05)`           = casrx_ae_recall_fdr05_pct
  ) %>%
  tidyr::pivot_longer(
    cols = -display_name,
    names_to = "metric",
    values_to = "recall_pct"
  ) %>%
  dplyr::mutate(
    metric = factor(
      metric,
      levels = c(
        "DepMap Cas9 (Chronos < -0.5)",
        "DepMap Cas9 (Chronos < -1.0)",
        "CasRx (FDR < 0.25)",
        "CasRx (FDR < 0.05)"
      )
    )
  )

auc_labels <- ae_df %>%
  dplyr::mutate(
    display_name = factor(display_name, levels = line_order_a),
    auc_lbl = sprintf("AUC %.2f", casrx_roc_auc_ae_vs_ne)
  )

metric_pal <- c(
  "DepMap Cas9 (Chronos < -0.5)" = col_qc_gray,
  "DepMap Cas9 (Chronos < -1.0)" = col_bar_gray,
  "CasRx (FDR < 0.25)"           = pal_blue,
  "CasRx (FDR < 0.05)"           = pal_blue_dark
)

p_ed1a <- ggplot2::ggplot(ae_long, ggplot2::aes(x = display_name, y = recall_pct, fill = metric)) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.78),
    width = 0.68,
    color = NA
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.0f", recall_pct), y = recall_pct + 2.0),
    position = ggplot2::position_dodge(width = 0.78),
    size = 4.8 / ggplot2::.pt,
    family = "Helvetica",
    color = "black",
    vjust = 0
  ) +
  ggplot2::geom_vline(xintercept = 6.5, linetype = "dashed", color = col_divider, linewidth = LW_025) +
  ggplot2::geom_text(
    data = auc_labels,
    ggplot2::aes(x = display_name, y = 110.0, label = auc_lbl),
    inherit.aes = FALSE,
    size = GG_TEXT_SIZE,
    family = "Helvetica",
    color = pal_blue_dark
  ) +
  ggplot2::scale_fill_manual(values = metric_pal, name = NULL) +
  ggplot2::scale_y_continuous(
    limits = c(0, 118),
    breaks = seq(0, 100, by = 20),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Always-essential gene recall (%)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x      = ggplot2::element_text(angle = 22, hjust = 1, vjust = 1, size = TEXT_SIZE),
    legend.position  = "bottom",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.18, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE),
    legend.spacing.x = grid::unit(0.10, "cm"),
    legend.margin    = ggplot2::margin(t = -3),
    plot.margin      = ggplot2::margin(2, 6, 3, 4)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

p_ed1a_titled <- apply_panel_header(
  p_ed1a, "a",
  "Essential-gene recall across significance thresholds (n = 300)",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
)
save_panel_pair(p_ed1a_titled, file.path(out_dir, "ed1a_ae_recall_fdr_thresholds.pdf"), width = 4.20, height = 2.85)

# ---------------------------------------------------------------------------
# Panel c: Plasmid read counts & 522 BsmBI dropouts vs intact library
# ---------------------------------------------------------------------------
con_gz <- gzfile(require_file(file.path(path_sd_forensics, "albarossa_arrays_qc.tsv.gz")), "rt")
arr_df <- read.delim(con_gz, check.names = FALSE, stringsAsFactors = FALSE)
close(con_gz)

require_columns(
  arr_df,
  c(
    "category", "has_T4_any", "has_bsmbi_rev_or_junc", "plasmid_count",
    "LN18_lfc", "LN229_lfc", "A549_lfc", "NCIH460_lfc", "KP4_lfc", "MIAPACA2_lfc",
    "LN18wo_lfc", "NCIH460wo_lfc", "MIAPACA2wo_lfc"
  ),
  "albarossa_arrays_qc.tsv.gz"
)

bsmbi_df <- arr_df %>%
  dplyr::transmute(
    plasmid_count = plasmid_count,
    log10_pc = log10(plasmid_count + 1),
    group = dplyr::if_else(
      has_bsmbi_rev_or_junc == 1L,
      "BsmBI defect (n = 522)",
      "Intact arrays (n = 77,603)"
    )
  ) %>%
  dplyr::mutate(
    group = factor(group, levels = c("BsmBI defect (n = 522)", "Intact arrays (n = 77,603)"))
  )

med_bsmbi     <- stats::median(arr_df$plasmid_count[arr_df$has_bsmbi_rev_or_junc == 1L])
med_intact    <- stats::median(arr_df$plasmid_count[arr_df$has_bsmbi_rev_or_junc == 0L])
n_intact_lt30 <- sum(arr_df$has_bsmbi_rev_or_junc == 0L & arr_df$plasmid_count < 30)
wt_bsmbi      <- stats::wilcox.test(
  arr_df$plasmid_count[arr_df$has_bsmbi_rev_or_junc == 1L],
  arr_df$plasmid_count[arr_df$has_bsmbi_rev_or_junc == 0L]
)

annot_c <- tibble::tibble(
  group = factor(
    c("BsmBI defect (n = 522)", "Intact arrays (n = 77,603)"),
    levels = c("BsmBI defect (n = 522)", "Intact arrays (n = 77,603)")
  ),
  x     = c(log10(31) + 0.12, log10(31) - 0.10),
  y     = c(155, 13000),
  hjust = c(0, 1),
  label = c(
    sprintf("522/522 < 30 reads\n180 zeros; median = %d\n(max = 17)", med_bsmbi),
    sprintf("Median = %d reads\nOnly %d (0.15%%) < 30\n%s", med_intact, n_intact_lt30, format_pval(wt_bsmbi$p.value))
  )
)

cutoff_lbl_c <- tibble::tibble(
  group = factor("BsmBI defect (n = 522)", levels = c("BsmBI defect (n = 522)", "Intact arrays (n = 77,603)")),
  x = log10(31) + 0.06,
  y = 195,
  label = "30-read QC cutoff"
)

med_lines_c <- tibble::tibble(
  group = factor(
    c("BsmBI defect (n = 522)", "Intact arrays (n = 77,603)"),
    levels = c("BsmBI defect (n = 522)", "Intact arrays (n = 77,603)")
  ),
  xint = log10(c(med_bsmbi, med_intact) + 1)
)

pc_breaks <- c(0, 3, 10, 30, 100, 300, 1000)

p_ed1c <- ggplot2::ggplot(bsmbi_df, ggplot2::aes(x = log10_pc, fill = group)) +
  ggplot2::geom_histogram(bins = 42, color = "white", linewidth = 0.15) +
  ggplot2::geom_vline(
    xintercept = log10(30 + 1),
    linetype = "dashed", color = pal_coral_dark, linewidth = LW_025
  ) +
  ggplot2::geom_vline(
    data = med_lines_c,
    ggplot2::aes(xintercept = xint),
    linetype = "dotted", color = "black", linewidth = LW_025
  ) +
  ggplot2::geom_text(
    data = annot_c,
    ggplot2::aes(x = x, y = y, label = label, hjust = hjust),
    inherit.aes = FALSE,
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black", vjust = 1, lineheight = 0.92
  ) +
  ggplot2::geom_text(
    data = cutoff_lbl_c,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = GG_TEXT_SIZE, family = "Helvetica", color = pal_coral_dark
  ) +
  ggplot2::facet_wrap(~ group, ncol = 1, scales = "free_y") +
  ggplot2::scale_fill_manual(
    values = c(
      "BsmBI defect (n = 522)"     = pal_coral,
      "Intact arrays (n = 77,603)" = col_qc_gray
    ),
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = log10(pc_breaks + 1),
    labels = as.character(pc_breaks)
  ) +
  ggplot2::coord_cartesian(xlim = c(-0.08, log10(1200))) +
  ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.14))) +
  ggplot2::labs(
    x = "Plasmid read count (log10[count + 1] scale)",
    y = "Array count"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    strip.text  = ggplot2::element_text(size = TEXT_SIZE, hjust = 0, color = "black"),
    plot.margin = ggplot2::margin(2, 6, 3, 4)
  )

p_ed1c_titled <- apply_panel_header(
  p_ed1c, "c",
  "BsmBI cloning dropouts (<30 reads)",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
)
save_panel_pair(p_ed1c_titled, file.path(out_dir, "ed1c_plasmid_and_bsmbi_dropouts.pdf"), width = 3.00, height = 2.85)

# ---------------------------------------------------------------------------
# Panel b: Per-line T>=4 vs No-T>=4 AE LFC across 6 CasRx + 3 No-nuclease lines
# ---------------------------------------------------------------------------
ae_only <- arr_df %>% dplyr::filter(category == "AE")

line_meta <- tibble::tribble(
  ~col_name,        ~line_label,               ~casrx_group,
  "LN18_lfc",       "LN-18",                   "CasRx (+)",
  "KP4_lfc",        "KP-4",                    "CasRx (+)",
  "MIAPACA2_lfc",   "MIA PaCa-2",              "CasRx (+)",
  "NCIH460_lfc",    "NCI-H460",                "CasRx (+)",
  "A549_lfc",       "A549",                    "CasRx (+)",
  "LN229_lfc",      "LN-229",                  "CasRx (+)",
  "LN18wo_lfc",     "LN-18\n(no-nuclease)",    "No-nuclease",
  "NCIH460wo_lfc",  "NCI-H460\n(no-nuclease)", "No-nuclease",
  "MIAPACA2wo_lfc", "MIA PaCa-2\n(no-nuclease)","No-nuclease"
)

lfc_long <- ae_only %>%
  dplyr::select(has_T4_any, dplyr::all_of(line_meta$col_name)) %>%
  tidyr::pivot_longer(cols = -has_T4_any, names_to = "col_name", values_to = "lfc") %>%
  dplyr::left_join(line_meta, by = "col_name") %>%
  dplyr::mutate(
    t4_group = dplyr::case_when(
      casrx_group == "CasRx (+)" & has_T4_any == 0L ~ "CasRx: No poly-T (n = 765)",
      casrx_group == "CasRx (+)" & has_T4_any == 1L ~ "CasRx: Poly-T (n = 135)",
      casrx_group == "No-nuclease" & has_T4_any == 0L ~ "No-nuclease: No poly-T",
      TRUE ~ "No-nuclease: Poly-T"
    ),
    t4_group = factor(
      t4_group,
      levels = c(
        "CasRx: No poly-T (n = 765)",
        "CasRx: Poly-T (n = 135)",
        "No-nuclease: No poly-T",
        "No-nuclease: Poly-T"
      )
    ),
    line_label  = factor(line_label, levels = line_meta$line_label),
    casrx_group = factor(casrx_group, levels = c("CasRx (+)", "No-nuclease"))
  )

stats_b <- lfc_long %>%
  dplyr::group_by(line_label, casrx_group) %>%
  dplyr::summarise(
    med_no = stats::median(lfc[has_T4_any == 0L], na.rm = TRUE),
    med_t4 = stats::median(lfc[has_T4_any == 1L], na.rm = TRUE),
    shift  = med_t4 - med_no,
    p_val  = stats::wilcox.test(lfc[has_T4_any == 1L], lfc[has_T4_any == 0L])$p.value,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    p_txt = dplyr::if_else(
      p_val < 0.05,
      sprintf("%+.2f\n%s", shift, format_pval(p_val)),
      sprintf("%+.2f\nNS", shift)
    )
  )

p_ed1b <- ggplot2::ggplot(lfc_long, ggplot2::aes(x = line_label, y = lfc, fill = t4_group, color = t4_group)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = col_divider, linewidth = LW_025) +
  ggplot2::geom_vline(xintercept = 6.5, linetype = "dashed", color = col_divider, linewidth = LW_025) +
  ggplot2::geom_boxplot(
    position = ggplot2::position_dodge(width = 0.72),
    width = 0.62,
    outlier.size = 0.45,
    outlier.alpha = 0.45,
    linewidth = LW_025,
    median.linewidth = LW_050
  ) +
  ggplot2::geom_text(
    data = stats_b,
    ggplot2::aes(x = line_label, y = 2.65, label = p_txt),
    inherit.aes = FALSE,
    size = 5.0 / ggplot2::.pt,
    family = "Helvetica",
    color = "black",
    vjust = 0,
    lineheight = 0.92
  ) +
  ggplot2::annotate(
    "text", x = 3.5, y = 4.00, label = "CasRx screens (6 lines)",
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::annotate(
    "text", x = 8.0, y = 4.00, label = "No-nuclease controls (3 lines)",
    size = GG_TEXT_SIZE, family = "Helvetica", color = col_dark
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "CasRx: No poly-T (n = 765)" = col_bar_gray,
      "CasRx: Poly-T (n = 135)"    = pal_coral,
      "No-nuclease: No poly-T"     = col_stripe,
      "No-nuclease: Poly-T"        = col_qc_gray
    ),
    name = NULL
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "CasRx: No poly-T (n = 765)" = col_dark,
      "CasRx: Poly-T (n = 135)"    = pal_coral_dark,
      "No-nuclease: No poly-T"     = col_divider,
      "No-nuclease: Poly-T"        = col_dark
    ),
    name = NULL
  ) +
  ggplot2::scale_y_continuous(limits = c(-5.3, 4.3), breaks = seq(-4, 2, by = 2)) +
  ggplot2::labs(
    x = NULL,
    y = "Essential-array fold change (log2)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x      = ggplot2::element_text(size = TEXT_SIZE, lineheight = 0.88),
    legend.position  = "bottom",
    legend.key       = ggplot2::element_blank(),
    legend.key.size  = grid::unit(0.18, "cm"),
    legend.text      = ggplot2::element_text(size = TEXT_SIZE),
    legend.spacing.x = grid::unit(0.10, "cm"),
    legend.margin    = ggplot2::margin(t = -3),
    plot.margin      = ggplot2::margin(2, 6, 3, 4)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

p_ed1b_titled <- apply_panel_header(
  p_ed1b, "b",
  "Internal Pol III poly-T termination attenuates essential-gene depletion across all six CasRx lines",
  tag_size = TAG_SIZE, title_size = TEXT_SIZE
)
save_panel_pair(p_ed1b_titled, file.path(out_dir, "ed1b_t4_lfc_distribution_6lines.pdf"), width = 7.20, height = 2.95)

# ---------------------------------------------------------------------------
# Composite Extended Data Fig. 1 (7.20 in Nature width)
# ---------------------------------------------------------------------------
top_row <- patchwork::wrap_plots(p_ed1a_titled, p_ed1c_titled, ncol = 2, widths = c(1.36, 1.00))
ed1_comp <- patchwork::wrap_plots(top_row, p_ed1b_titled, ncol = 1, heights = c(1.00, 1.04))

save_panel_pair(ed1_comp, file.path(out_dir, "Extended_Data_1_composite.pdf"), width = 7.20, height = 5.80)
message("Extended Data Fig. 1 complete: ", out_dir)
