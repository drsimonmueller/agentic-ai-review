#!/usr/bin/env Rscript
# Figure 4: Round 2 residual discovery and beyond-human findings across N = 36 and Montero et al. primary data
# Standardized to Nature 7.20 in width, Helvetica Bold 9 pt tags, Helvetica Plain 6 pt text,
# 0.25 pt hairline strokes, and high-level peer-review-ready descriptions.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(forcats)
  library(patchwork)
  library(scales)
})

repo_root_init <- Sys.getenv("AGENTIC_REPO_ROOT", unset = ".")
source(file.path(repo_root_init, "config", "paths.R"))
source(file.path(repo_root, "R", "colors.R"))
source(file.path(repo_root, "R", "theme.R"))
source(file.path(repo_root, "R", "stats_utils.R"))
source(file.path(repo_root, "R", "plot_utils.R"))

set.seed(GLOBAL_SEED)

path_fig4 <- ensure_dir(file.path(path_figures, "Figure_4"))

# Nature typography & shared structural constants (R/colors.R)
TAG_SIZE     <- 9.0
TEXT_SIZE    <- 6.0
GG_TEXT_SIZE <- TEXT_SIZE / ggplot2::.pt

gray_bar_med <- col_qc_gray
gray_bar_lt  <- col_bar_gray
gray_pt_dark <- col_dark
gray_line    <- col_divider

style_panel_title <- function(p, tag, title_txt = NULL) {
  p +
    ggplot2::labs(tag = tag, title = NULL) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(
        family = "Helvetica", face = "bold", size = TAG_SIZE, color = "black"
      ),
      plot.tag.position = c(0.005, 0.99),
      plot.title = ggplot2::element_blank()
    )
}

make_header_strip <- function(tag, title = NULL) {
  ggplot2::ggplot() +
    ggplot2::labs(tag = tag, title = NULL) +
    ggplot2::theme_void(base_size = TEXT_SIZE, base_family = "Helvetica") +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(
        family = "Helvetica", face = "bold", size = TAG_SIZE, color = "black"
      ),
      plot.tag.position = c(0.005, 0.55),
      plot.title = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(0, 2, 0, 2)
    )
}

# ------------------------------------------------------------------------------
# Load and validate source datasets
# ------------------------------------------------------------------------------
file_beyond  <- require_file(file.path(path_sd_benchmark, "beyond_rubric_discoveries_N36.csv"))
file_rubric  <- require_file(file.path(path_sd_benchmark, "scored_rubric_N36.csv"))
file_res_ae  <- require_file(file.path(path_sd_forensics, "residual_ae_predictors.csv"))
file_lnc_col <- require_file(file.path(path_sd_forensics, "lncrna_expression_and_collateral.csv"))

df_beyond <- readr::read_csv(file_beyond, show_col_types = FALSE)
require_columns(
  df_beyond,
  c("discovery_key", "discovery_label", "category", "n_runs_N36", "pct_of_N36",
    "n_cold_18", "n_seeded_18", "n_opus_5_0_6", "n_sonnet_5_5_12",
    "n_gemini_neon_6", "n_gemini_hydrogen_12"),
  "beyond_rubric_discoveries_N36.csv"
)

df_rubric <- readr::read_csv(file_rubric, show_col_types = FALSE)
require_columns(
  df_rubric,
  c("condition", "non_chain_discoveries", "in_primary_N36"),
  "scored_rubric_N36.csv"
)

df_res <- readr::read_csv(file_res_ae, show_col_types = FALSE)
require_columns(
  df_res,
  c("section", "stratum_or_feature", "cell_line", "n", "recall_or_frac_pct",
    "median_lfc", "effect_or_rho_or_or", "ci_lo", "ci_hi", "p_value", "notes"),
  "residual_ae_predictors.csv"
)

df_lnc <- readr::read_csv(file_lnc_col, show_col_types = FALSE)
require_columns(
  df_lnc,
  c("section", "library", "category", "cell_line_or_bin", "n_items",
    "pct_tpm_lt_001", "pct_tpm_lt_1", "pct_t7_reads_eq_0", "pct_t7_reads_lt_10",
    "median_casrx_lfc", "median_wocasrx_lfc", "spearman_rho_vs_expr", "p_value", "notes"),
  "lncrna_expression_and_collateral.csv"
)

# ------------------------------------------------------------------------------
# Panel a: 19 recurring beyond-rubric discoveries (N = 36) + 4-model dot strip
# ------------------------------------------------------------------------------
featured_steel <- c(
  "BsmBI_strand_and_junction_bug",
  "In_silico_rescue_tradeoff",
  "Called_lncRNA_hits_unexpressed"
)
featured_teal <- c(
  "Target_RNA_collateral_masked_by_silent_NE"
)

highlight_colors <- c(
  "Featured finding"          = pal_blue,
  "Collateral revision"       = pal_teal,
  "Other recurring discovery" = gray_bar_med
)

short_labels <- c(
  "BsmBI_strand_and_junction_bug"            = "Unidirectional BsmBI cloning filter omission (522 dropouts)",
  "In_silico_rescue_tradeoff"                = "Post-hoc defect filtering reduces statistical power",
  "Code_repo_and_ENA_defects"                = "Analysis script defects & swapped public read files",
  "RNAfold_efficacy_null"                    = "RNA folding & design scores fail to predict residual inertness",
  "NT_baseline_drift_and_unscored_RRA"       = "Non-targeting guide baseline drift & scoring omission",
  "Target_expression_AE_gradient"            = "Target transcript abundance drives residual depletion",
  "Called_lncRNA_hits_unexpressed"           = "Called lncRNA hits lack expression in target cell lines",
  "RRA_2of3_cliff_and_power"                 = "Two-of-three array power cliff & control-list omission",
  "Albarossa_NE_unexpressed"                 = "Albarossa non-essential controls 97-99% unexpressed (<1 TPM)",
  "Table13_redundancy_and_Fig4f_205_vs_206"  = "Published hit-table transcript redundancy (187 vs 206 entries)",
  "Background_lncRNA_library_unexpressed"    = "Background lncRNA library ~45-50% unexpressed (0 TPM)",
  "Target_region_3UTR_vs_CDS"                = "3'UTR versus coding-sequence targeting explains inertness",
  "CRISPRi_benchmark_reversal"               = "CRISPRi positive-control benchmark reversal & missing metrics",
  "Subcellular_nuclear_localization_NLS"     = "Nuclear-enriched essential transcripts deplete more strongly",
  "DR_A7T_substitution_and_predictR_mismatch"= "Direct-repeat substitution & guide-length code mismatch",
  "Target_RNA_collateral_masked_by_silent_NE"= "Collateral RNA cleavage masked by silent non-essential controls",
  "BsmBI_chronometer_precloning_proof"       = "BsmBI cleavage proves recombination preceded library cloning",
  "In_cell_RT_deletion_and_chimera_killing"  = "Proviral recombination deletion & chimeric transcript depletion",
  "BH_pool_contamination_206_to_51"          = "Pooled false-discovery dilution by positive controls (206 to 51 hits)"
)

df_a <- df_beyond %>%
  dplyr::mutate(
    highlight_group = dplyr::case_when(
      discovery_key %in% featured_steel ~ "Featured finding",
      discovery_key %in% featured_teal  ~ "Collateral revision",
      TRUE                              ~ "Other recurring discovery"
    ),
    highlight_group = factor(highlight_group, levels = names(highlight_colors)),
    short_label = dplyr::if_else(
      discovery_key %in% names(short_labels),
      unname(short_labels[discovery_key]),
      discovery_label
    ),
    label_ordered = forcats::fct_reorder(short_label, n_runs_N36 + (nrow(.) - dplyr::row_number()) * 1e-3),
    count_pct_lbl = sprintf("%d/36 (%.0f%%)", n_runs_N36, pct_of_N36)
  )

model_cols_x <- c(
  "Claude Opus 5.0*"  = 49.5,
  "Claude Sonnet 5.5" = 52.8,
  "Gemini 4 Hydrogen" = 56.1,
  "Gemini 4 Neon"     = 59.4
)

df_a_dots <- df_a %>%
  dplyr::select(label_ordered, n_opus_5_0_6, n_sonnet_5_5_12, n_gemini_hydrogen_12, n_gemini_neon_6) %>%
  tidyr::pivot_longer(
    cols = c(n_opus_5_0_6, n_sonnet_5_5_12, n_gemini_hydrogen_12, n_gemini_neon_6),
    names_to = "model_col",
    values_to = "n_present"
  ) %>%
  dplyr::mutate(
    model_full = dplyr::recode(
      model_col,
      "n_opus_5_0_6"         = "Claude Opus 5.0*",
      "n_sonnet_5_5_12"      = "Claude Sonnet 5.5",
      "n_gemini_hydrogen_12" = "Gemini 4 Hydrogen",
      "n_gemini_neon_6"      = "Gemini 4 Neon"
    ),
    x_pos     = unname(model_cols_x[model_full]),
    present   = n_present > 0,
    dot_color = dplyr::if_else(present, unname(model_colors[model_full]), gray_bar_lt)
  )

df_a_headers <- tibble::tibble(
  model_full = names(model_cols_x),
  x_pos = unname(model_cols_x),
  y_pos = 19.75
)

stripe_a <- tibble::tibble(y_num = seq(1, nrow(df_a), by = 2))

p_a <- ggplot2::ggplot(df_a, ggplot2::aes(x = n_runs_N36, y = label_ordered)) +
  ggplot2::geom_rect(
    data = stripe_a,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = y_num - 0.5, ymax = y_num + 0.5),
    inherit.aes = FALSE, fill = col_stripe
  ) +
  ggplot2::geom_col(ggplot2::aes(fill = highlight_group), width = 0.65, color = NA) +
  ggplot2::geom_text(
    ggplot2::aes(x = n_runs_N36 + 0.65, label = count_pct_lbl),
    hjust = 0, vjust = 0.5, size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::geom_point(
    data = df_a_dots,
    ggplot2::aes(x = x_pos, y = label_ordered),
    color = df_a_dots$dot_color,
    shape = 16,
    size = dplyr::if_else(df_a_dots$present, 1.70, 1.15)
  ) +
  ggplot2::geom_text(
    data = df_a_headers,
    ggplot2::aes(x = x_pos, y = y_pos, label = model_full),
    angle = 30, hjust = 0, vjust = 0, size = GG_TEXT_SIZE, family = "Helvetica", color = "black",
    inherit.aes = FALSE
  ) +
  ggplot2::annotate("segment", x = 0, xend = 36, y = 0.4, yend = 0.4, color = "black", linewidth = LW_025) +
  ggplot2::scale_fill_manual(values = highlight_colors, name = NULL) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 9, 18, 27, 36),
    limits = c(0, 69.5),
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.6, 1.2))) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    x = "Completed benchmark runs (out of N = 36)",
    y = NULL
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line.x     = ggplot2::element_blank(),
    axis.line.y     = ggplot2::element_blank(),
    axis.ticks.y    = ggplot2::element_blank(),
    axis.text.y     = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.text.x     = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.x    = ggplot2::element_text(size = TEXT_SIZE, hjust = 0.24),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = grid::unit(0.20, "cm"),
    legend.text     = ggplot2::element_text(size = TEXT_SIZE),
    legend.margin   = ggplot2::margin(t = -3, b = 0),
    plot.margin     = ggplot2::margin(t = 24, r = 16, b = 2, l = 4)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

p_a_ind <- style_panel_title(p_a, "a", "Recurring beyond-rubric discoveries across N = 36 benchmark audits")
save_panel_pair(p_a_ind, file.path(path_fig4, "fig4a_beyond_rubric_discoveries.pdf"), width = 7.20, height = 3.00)

# ------------------------------------------------------------------------------
# Panel b: Biological vs computational predictors of residual inertness (3 columns)
# ------------------------------------------------------------------------------
drv_keep <- c(
  "log1p_t7_nuc"                  = "Target nuclear RNA (CPM)",
  "log1p_t7_casrx"                = "Target CasRx RNA (CPM)",
  "log1p_iso_TPM"                 = "Target isoform TPM",
  "depmap_all_lines_mean_chronos" = "DepMap Chronos score",
  "g2_max_homopol"                = "gRNA2 max homopolymer",
  "g1_pos1_G"                     = "gRNA1 position 1 = G",
  "t7_cyt_nuc_lfc"                = "Nuclear/cytoplasmic ratio"
)
null_keep <- c(
  "wessels_rf_mean"               = "Cas13design score",
  "cr2_mfe"                       = "crRNA-2 folding energy",
  "cr1_mfe"                       = "crRNA-1 folding energy",
  "pre_mfe"                       = "Pre-crRNA folding energy"
)

df_b_uni <- dplyr::bind_rows(
  df_res %>%
    dplyr::filter(section == "clean_ae_residual_drivers", stratum_or_feature %in% names(drv_keep)) %>%
    dplyr::mutate(
      label = unname(drv_keep[stratum_or_feature]),
      group = "Biological feature"
    ),
  df_res %>%
    dplyr::filter(section == "null_rnafold_and_cas13design", stratum_or_feature %in% names(null_keep)) %>%
    dplyr::mutate(
      label = unname(null_keep[stratum_or_feature]),
      group = "Sequence or folding"
    )
) %>%
  dplyr::mutate(
    abs_rho   = abs(effect_or_rho_or_or),
    sig_class = factor(group, levels = c("Biological feature", "Sequence or folding")),
    val_lbl   = sprintf("r = %+.2f", effect_or_rho_or_or),
    label_ord = forcats::fct_reorder(label, abs_rho)
  )

pred_colors <- c(
  "Biological feature"  = pal_blue,
  "Sequence or folding" = gray_bar_med
)

q_summ <- df_res %>%
  dplyr::filter(section == "expression_quartiles_6lines") %>%
  dplyr::mutate(q = sub(".*__", "", stratum_or_feature)) %>%
  dplyr::group_by(q) %>%
  dplyr::summarise(mean_recall = mean(recall_or_frac_pct), .groups = "drop")

stripe_b1 <- tibble::tibble(y_num = seq(1, nrow(df_b_uni), by = 2))

p_b1 <- ggplot2::ggplot(df_b_uni, ggplot2::aes(x = abs_rho, y = label_ord, fill = sig_class)) +
  ggplot2::geom_rect(
    data = stripe_b1,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = y_num - 0.5, ymax = y_num + 0.5),
    inherit.aes = FALSE, fill = col_stripe
  ) +
  ggplot2::geom_col(width = 0.64, color = NA) +
  ggplot2::geom_text(
    ggplot2::aes(x = abs_rho + 0.012, label = val_lbl),
    hjust = 0, vjust = 0.5, size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_fill_manual(values = pred_colors, name = NULL) +
  ggplot2::scale_x_continuous(
    limits = c(0, 0.58),
    breaks = c(0, 0.2, 0.4),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::labs(
    x = "|Spearman correlation| (n = 757 arrays)",
    y = NULL
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line.y     = ggplot2::element_blank(),
    axis.ticks.y    = ggplot2::element_blank(),
    axis.text.y     = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.text.x     = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.x    = ggplot2::element_text(size = TEXT_SIZE),
    legend.position = "bottom",
    legend.key.size = grid::unit(0.18, "cm"),
    legend.text     = ggplot2::element_text(size = TEXT_SIZE),
    legend.margin   = ggplot2::margin(t = -3, b = 0),
    plot.margin     = ggplot2::margin(2, 4, 2, 2)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

or_keep <- c(
  "cds_frac"     = "CDS vs 3'UTR fraction",
  "log10_tpm"    = "Target abundance (log10 TPM)",
  "chronos_mean" = "DepMap Chronos",
  "gc_g2"        = "GC content (gRNA2)",
  "gc_g1"        = "GC content (gRNA1)",
  "t_g1"         = "T content (gRNA1)"
)

reg_summ <- df_res %>% dplyr::filter(section == "genomic_location_3utr_vs_cds")

df_b_or <- df_res %>%
  dplyr::filter(section == "multivariable_odds_ratios", stratum_or_feature %in% names(or_keep)) %>%
  dplyr::mutate(
    label = unname(or_keep[stratum_or_feature]),
    sig_class = dplyr::if_else(
      stratum_or_feature %in% c("cds_frac", "log10_tpm", "chronos_mean"),
      "Biological feature",
      "Sequence or folding"
    ),
    p_str = dplyr::case_when(
      p_value < 0.001 ~ paste0("P = ", formatC(p_value, format = "e", digits = 1)),
      TRUE ~ paste0("P = ", formatC(p_value, format = "f", digits = 3))
    ),
    or_lbl = sprintf("%.2f (%s)", effect_or_rho_or_or, p_str),
    label = factor(label, levels = rev(unname(or_keep)))
  )

stripe_b23 <- tibble::tibble(y_num = c(1, 3, 5))

p_b2 <- ggplot2::ggplot(df_b_or, ggplot2::aes(x = effect_or_rho_or_or, y = label, color = sig_class)) +
  ggplot2::geom_rect(
    data = stripe_b23,
    ggplot2::aes(xmin = 0, xmax = Inf, ymin = y_num - 0.5, ymax = y_num + 0.5),
    inherit.aes = FALSE, fill = col_stripe
  ) +
  ggplot2::geom_vline(xintercept = 1.0, linetype = "dashed", color = "black", linewidth = LW_025) +
  ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lo, xmax = ci_hi), width = 0.22, linewidth = LW_025, orientation = "y") +
  ggplot2::geom_point(shape = 23, size = 1.85, stroke = 0.30, fill = unname(pred_colors[df_b_or$sig_class]), color = "black") +
  ggplot2::geom_text(
    ggplot2::aes(x = ci_hi * 1.08, label = or_lbl),
    hjust = 0, vjust = 0.5, size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_color_manual(values = pred_colors, guide = "none") +
  ggplot2::scale_x_log10(
    limits = c(0.54, 24.0),
    breaks = c(0.6, 1.0, 1.5, 2.5),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::labs(
    x = "Multivariable odds ratio (95% CI, n = 894)",
    y = NULL
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line.y   = ggplot2::element_blank(),
    axis.ticks.y  = ggplot2::element_blank(),
    axis.text.y   = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.text.x   = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.x  = ggplot2::element_text(size = TEXT_SIZE),
    plot.margin   = ggplot2::margin(2, 4, 2, 2)
  )

r2_keep <- c(
  "8_Combined_Plus_CellLine" = "Combined + cell line",
  "1_Target_Expression"      = "Target expression",
  "7_CellLine_Only"          = "Cell line identity",
  "2_DepMap_Chronos"         = "DepMap Chronos",
  "4_Sequence_Design"        = "Sequence / design",
  "5_RNA_Folding"            = "RNA folding"
)

df_b_r2 <- df_res %>%
  dplyr::filter(section == "multivariable_groupkfold_r2", stratum_or_feature %in% names(r2_keep)) %>%
  dplyr::mutate(
    label = unname(r2_keep[stratum_or_feature]),
    sig_class = dplyr::if_else(
      stratum_or_feature %in% c("8_Combined_Plus_CellLine", "1_Target_Expression", "7_CellLine_Only", "2_DepMap_Chronos"),
      "Biological feature",
      "Sequence or folding"
    ),
    r2_lbl = sprintf("%.1f%% (AUC %.2f)", recall_or_frac_pct, effect_or_rho_or_or),
    label_ord = forcats::fct_reorder(label, recall_or_frac_pct + (6 - match(stratum_or_feature, names(r2_keep))) * 1e-3)
  )

p_b3 <- ggplot2::ggplot(df_b_r2, ggplot2::aes(x = recall_or_frac_pct, y = label_ord, fill = sig_class)) +
  ggplot2::geom_rect(
    data = stripe_b23,
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = y_num - 0.5, ymax = y_num + 0.5),
    inherit.aes = FALSE, fill = col_stripe
  ) +
  ggplot2::geom_col(width = 0.64, color = NA) +
  ggplot2::geom_text(
    ggplot2::aes(x = recall_or_frac_pct + 0.7, label = r2_lbl),
    hjust = 0, vjust = 0.5, size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_fill_manual(values = pred_colors, guide = "none") +
  ggplot2::scale_x_continuous(
    limits = c(0, 54),
    breaks = c(0, 10, 20),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::labs(
    x = "5-fold CV R^2 (%, n = 4,518)",
    y = NULL
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.line.y   = ggplot2::element_blank(),
    axis.ticks.y  = ggplot2::element_blank(),
    axis.text.y   = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.text.x   = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.x  = ggplot2::element_text(size = TEXT_SIZE),
    plot.margin   = ggplot2::margin(2, 6, 2, 2)
  )

hdr_b <- make_header_strip("b", "Biological versus computational predictors of residual inertness among defect-free arrays")
p_b_body <- (p_b1 | p_b2 | p_b3) + patchwork::plot_layout(widths = c(1.20, 1.24, 1.02))
p_b_ind  <- (hdr_b / p_b_body) + patchwork::plot_layout(heights = c(0.16, 2.50))

save_panel_pair(p_b_ind, file.path(path_fig4, "fig4b_residual_ae_predictors.pdf"), width = 7.20, height = 2.65)

# ------------------------------------------------------------------------------
# Panel c: Cold vs Seeded silent-hit audit rate & per-line zero-read rates
# ------------------------------------------------------------------------------
df_c_runs <- df_rubric %>%
  dplyr::filter(in_primary_N36 == TRUE) %>%
  dplyr::mutate(
    has_unexpr = stringr::str_detect(dplyr::coalesce(non_chain_discoveries, ""), "Called_lncRNA_hits_unexpressed"),
    arm_group  = dplyr::if_else(stringr::str_detect(condition, "^cold"), "Cold", "Seeded"),
    lit_group  = dplyr::if_else(stringr::str_detect(condition, "_lit$"), "With literature", "Base")
  )

calc_arm_stats <- function(sub_df, cohort_label) {
  tab <- table(factor(sub_df$arm_group, levels = c("Cold", "Seeded")), sub_df$has_unexpr)
  ft  <- stats::fisher.test(tab)
  sub_df %>%
    dplyr::group_by(arm_group) %>%
    dplyr::summarise(k = sum(has_unexpr), n = dplyr::n(), .groups = "drop") %>%
    { dplyr::bind_cols(dplyr::select(., arm_group), wilson_ci(.$k, .$n)) } %>%
    dplyr::mutate(
      cohort = cohort_label,
      pct = 100 * prop,
      ci_lo_pct = 100 * conf_low,
      ci_hi_pct = 100 * conf_high,
      fisher_p = ft$p.value,
      bar_lbl = sprintf("%d/%d\n(%.0f%%)", k, n, pct)
    )
}

df_c1 <- dplyr::bind_rows(
  calc_arm_stats(df_c_runs, "All runs\n(N = 36)"),
  calc_arm_stats(df_c_runs %>% dplyr::filter(lit_group == "Base"), "Base arm\n(N = 24)")
) %>%
  dplyr::mutate(
    cohort = factor(cohort, levels = c("All runs\n(N = 36)", "Base arm\n(N = 24)")),
    arm_group = factor(arm_group, levels = c("Cold", "Seeded"))
  )

br_unexpr <- df_beyond %>% dplyr::filter(discovery_key == "Called_lncRNA_hits_unexpressed")
stopifnot(
  sum(df_c_runs$has_unexpr & df_c_runs$arm_group == "Cold") == br_unexpr$n_cold_18,
  sum(df_c_runs$has_unexpr & df_c_runs$arm_group == "Seeded") == br_unexpr$n_seeded_18
)

p_c1_ann <- df_c1 %>%
  dplyr::distinct(cohort, fisher_p) %>%
  dplyr::mutate(
    x_pos = as.numeric(cohort),
    p_lbl = format_pval(fisher_p)
  )

p_c1 <- ggplot2::ggplot(df_c1, ggplot2::aes(x = cohort, y = pct, fill = arm_group)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.62, color = NA) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = ci_lo_pct, ymax = ci_hi_pct),
    position = ggplot2::position_dodge(width = 0.72),
    width = 0.18, linewidth = LW_025, color = "black"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(y = pmin(ci_hi_pct + 4, 102), label = bar_lbl),
    position = ggplot2::position_dodge(width = 0.72),
    vjust = 0, size = GG_TEXT_SIZE, family = "Helvetica", color = "black", lineheight = 0.90
  ) +
  ggplot2::geom_segment(
    data = p_c1_ann,
    ggplot2::aes(x = x_pos - 0.18, xend = x_pos + 0.18, y = 117, yend = 117),
    inherit.aes = FALSE, color = "black", linewidth = LW_025
  ) +
  ggplot2::geom_text(
    data = p_c1_ann,
    ggplot2::aes(x = x_pos, y = 123, label = p_lbl),
    inherit.aes = FALSE, size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_fill_manual(values = c("Cold" = pal_blue, "Seeded" = gray_bar_med), name = NULL) +
  ggplot2::scale_y_continuous(limits = c(0, 134), breaks = c(0, 25, 50, 75, 100), expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::labs(
    x = NULL,
    y = "Runs auditing unexpressed hits (%)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x     = ggplot2::element_text(size = TEXT_SIZE, color = "black", lineheight = 0.90),
    axis.text.y     = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.y    = ggplot2::element_text(size = TEXT_SIZE),
    legend.position = "bottom",
    legend.key.size = grid::unit(0.18, "cm"),
    legend.text     = ggplot2::element_text(size = TEXT_SIZE),
    legend.margin   = ggplot2::margin(t = -3, b = 0),
    plot.margin     = ggplot2::margin(2, 4, 2, 2)
  )

line_display_short <- c(
  "LN18"     = "LN-18",
  "LN229"    = "LN-229",
  "A549"     = "A549",
  "NCIH460"  = "NCI-\nH460",
  "KP4"      = "KP-4",
  "MIAPACA2" = "MIA\nPaCa-2"
)

line_display_c2 <- c(
  "LN18"     = "LN-18",
  "LN229"    = "LN-229",
  "A549"     = "A549",
  "NCIH460"  = "NCI-H460",
  "KP4"      = "KP-4",
  "MIAPACA2" = "MIA PaCa-2"
)

df_c2_raw <- df_lnc %>%
  dplyr::filter(section == "lncrna_206_hit_expression_audit") %>%
  dplyr::mutate(
    line_lbl = sprintf("%s (%d)", unname(line_display_c2[cell_line_or_bin]), n_items),
    line_ord = factor(line_lbl, levels = sprintf("%s (%d)", unname(line_display_c2), n_items[match(names(line_display_c2), cell_line_or_bin)]))
  )

df_c2 <- df_c2_raw %>%
  dplyr::select(line_ord, pct_t7_reads_eq_0, pct_tpm_lt_001, pct_t7_reads_lt_10) %>%
  tidyr::pivot_longer(
    cols = c(pct_t7_reads_eq_0, pct_tpm_lt_001, pct_t7_reads_lt_10),
    names_to = "metric",
    values_to = "pct"
  ) %>%
  dplyr::mutate(
    metric_lbl = dplyr::recode(
      metric,
      "pct_t7_reads_eq_0"  = "Zero reads",
      "pct_tpm_lt_001"     = "TPM < 0.01",
      "pct_t7_reads_lt_10" = "<10 reads"
    ),
    metric_lbl = factor(metric_lbl, levels = c("Zero reads", "TPM < 0.01", "<10 reads"))
  )

silent_colors <- c(
  "Zero reads" = pal_coral,
  "TPM < 0.01" = gray_bar_med,
  "<10 reads"  = gray_bar_lt
)

tot_calls <- sum(df_c2_raw$n_items)
tot_zero  <- sum(df_c2_raw$n_t7_reads_eq_0)
tot_lt001 <- sum(df_c2_raw$n_tpm_lt_001)
tot_lt10  <- sum(df_c2_raw$n_t7_reads_lt_10)

p_c2 <- ggplot2::ggplot(df_c2, ggplot2::aes(x = line_ord, y = pct, fill = metric_lbl)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.88), width = 0.62, color = NA) +
  ggplot2::geom_text(
    ggplot2::aes(y = pct + 1.5, label = sprintf("%.0f", pct)),
    position = ggplot2::position_dodge(width = 0.88),
    vjust = 0, size = 4.2 / ggplot2::.pt, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_fill_manual(values = silent_colors, name = NULL) +
  ggplot2::scale_y_continuous(limits = c(0, 68), breaks = c(0, 20, 40, 60), expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::labs(
    x = NULL,
    y = "Called lncRNA hits (%)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x          = ggplot2::element_text(size = TEXT_SIZE, color = "black", angle = 25, hjust = 1),
    axis.text.y          = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.y         = ggplot2::element_text(size = TEXT_SIZE),
    legend.position      = "bottom",
    legend.key.size      = grid::unit(0.18, "cm"),
    legend.key.spacing.x = grid::unit(0.12, "cm"),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE),
    legend.margin        = ggplot2::margin(t = -3, b = 0),
    plot.margin          = ggplot2::margin(2, 4, 2, 2)
  )

hdr_c   <- make_header_strip("c", "Cold review uncovers unexpressed called lncRNA hits")
p_c_ind <- (hdr_c / ((p_c1 | p_c2) + patchwork::plot_layout(widths = c(0.35, 0.65)))) +
  patchwork::plot_layout(heights = c(0.16, 2.50))
save_panel_pair(p_c_ind, file.path(path_fig4, "fig4c_cold_vs_seeded_silent_hits.pdf"), width = 3.55, height = 2.65)

# ------------------------------------------------------------------------------
# Panel d: Empirical revision of FP3 — Silent non-essential controls & collateral depletion
# ------------------------------------------------------------------------------
line_display_d1 <- c(
  "LN18"     = "LN-18",
  "LN229"    = "LN-229",
  "A549"     = "A549",
  "NCIH460"  = "NCI-H460",
  "KP4"      = "KP-4",
  "MIAPACA2" = "MIA PaCa-2"
)

df_d1 <- df_lnc %>%
  dplyr::filter(section == "albarossa_vs_controls_ne_expression") %>%
  dplyr::mutate(
    pct_expressed_ge1 = 100 - pct_tpm_lt_1,
    lib_lbl = dplyr::recode(
      library,
      "ALBAROSSA_library" = "Albarossa (n = 299)",
      "controls_library"  = "Control (n = 300)"
    ),
    lib_lbl  = factor(lib_lbl, levels = c("Albarossa (n = 299)", "Control (n = 300)")),
    line_ord = factor(unname(line_display_d1[cell_line_or_bin]), levels = unname(line_display_d1))
  )

ne_colors <- c(
  "Albarossa (n = 299)" = gray_bar_med,
  "Control (n = 300)"   = pal_teal
)

df_ctrl_ne   <- df_lnc %>% dplyr::filter(section == "controls_library_ne_collateral")
ctrl_rho_min <- min(abs(df_ctrl_ne$spearman_rho_vs_expr))
ctrl_rho_max <- max(abs(df_ctrl_ne$spearman_rho_vs_expr))
ctrl_p_max   <- max(df_ctrl_ne$p_value)

p_d1 <- ggplot2::ggplot(df_d1, ggplot2::aes(x = line_ord, y = pct_expressed_ge1, fill = lib_lbl)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.74), width = 0.66, color = NA) +
  ggplot2::geom_text(
    ggplot2::aes(y = pct_expressed_ge1 + 1.8, label = sprintf("%.0f%%", pct_expressed_ge1)),
    position = ggplot2::position_dodge(width = 0.74),
    vjust = 0, size = 4.9 / ggplot2::.pt, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_fill_manual(values = ne_colors, name = NULL) +
  ggplot2::scale_y_continuous(limits = c(0, 108), breaks = c(0, 25, 50, 75, 100), expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::labs(
    x = NULL,
    y = "Non-essential controls >=1 TPM (%)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x          = ggplot2::element_text(size = TEXT_SIZE, color = "black", angle = 25, hjust = 1),
    axis.text.y          = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.y         = ggplot2::element_text(size = TEXT_SIZE),
    legend.position      = "bottom",
    legend.key.size      = grid::unit(0.18, "cm"),
    legend.key.spacing.x = grid::unit(0.10, "cm"),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE),
    legend.margin        = ggplot2::margin(t = -3, b = 0),
    plot.margin          = ggplot2::margin(2, 4, 2, 2)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

bin_short <- c(
  "CPM == 0"       = "0",
  "0 < CPM < 1"    = "0-1",
  "1 <= CPM < 10"  = "1-10",
  "10 <= CPM < 50" = "10-50",
  "CPM >= 50"      = ">=50",
  "CPM >= 100"     = ">=100"
)

fmt_n_compact <- function(n) {
  dplyr::if_else(n >= 1000, sprintf("%.1fk", n / 1000), scales::comma(n))
}

df_d2_raw <- df_lnc %>%
  dplyr::filter(section == "nonhit_lncrna_casrx_vs_wocasrx_cpm_bins") %>%
  dplyr::mutate(
    casrx3_med = as.numeric(stringr::str_match(notes, "casrx3_med=([-0-9.]+)")[, 2]),
    bin_lbl    = factor(unname(bin_short[cell_line_or_bin]), levels = unname(bin_short)),
    bin_n_lbl  = sprintf("%s\n%s", unname(bin_short[cell_line_or_bin]), fmt_n_compact(n_items))
  )

df_d2_raw$bin_n_lbl <- factor(df_d2_raw$bin_n_lbl, levels = df_d2_raw$bin_n_lbl[match(unname(bin_short), df_d2_raw$bin_lbl)])

df_d2 <- df_d2_raw %>%
  dplyr::select(bin_n_lbl, casrx3_med, median_casrx_lfc, median_wocasrx_lfc) %>%
  tidyr::pivot_longer(
    cols = c(casrx3_med, median_casrx_lfc, median_wocasrx_lfc),
    names_to = "series",
    values_to = "med_lfc"
  ) %>%
  dplyr::mutate(
    series_lbl = dplyr::recode(
      series,
      "casrx3_med"         = "CasRx (3 lines)",
      "median_casrx_lfc"   = "CasRx (6 lines)",
      "median_wocasrx_lfc" = "No-nuclease"
    ),
    series_lbl = factor(
      series_lbl,
      levels = c("CasRx (3 lines)", "CasRx (6 lines)", "No-nuclease")
    )
  )

lfc_curve_colors <- c(
  "CasRx (3 lines)" = pal_coral,
  "CasRx (6 lines)" = pal_teal,
  "No-nuclease"     = gray_line
)

p_casrx_10  <- df_lnc$p_value[df_lnc$cell_line_or_bin == "lncRNA_NONHIT_CPM>=10_vs_<1_CasRx6"]
p_wo_10     <- df_lnc$p_value[df_lnc$cell_line_or_bin == "lncRNA_NONHIT_CPM>=10_vs_<1_woCasRx3"]
p_casrx_100 <- df_lnc$p_value[df_lnc$cell_line_or_bin == "lncRNA_NONHIT_CPM>=100_vs_<1_CasRx6"]

p_d2 <- ggplot2::ggplot(
  df_d2,
  ggplot2::aes(x = bin_n_lbl, y = med_lfc, group = series_lbl, color = series_lbl, linetype = series_lbl)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = LW_025) +
  ggplot2::geom_line(linewidth = LW_050) +
  ggplot2::geom_point(size = 1.35) +
  ggplot2::annotate(
    "text", x = 0.62, y = -0.55,
    label = sprintf(
      "Non-hit >=10 vs <1 CPM:\nCasRx P = %s\nNo-nuclease P = %.2f\n>=100 CPM CasRx P = %s",
      formatC(p_casrx_10, format = "e", digits = 1),
      p_wo_10,
      formatC(p_casrx_100, format = "e", digits = 1)
    ),
    hjust = 0, vjust = 0.5, size = GG_TEXT_SIZE, family = "Helvetica", color = "black", lineheight = 0.92
  ) +
  ggplot2::scale_color_manual(values = lfc_curve_colors, name = NULL) +
  ggplot2::scale_linetype_manual(
    values = c("CasRx (3 lines)" = "solid", "CasRx (6 lines)" = "dashed", "No-nuclease" = "solid"),
    name = NULL
  ) +
  ggplot2::scale_y_continuous(limits = c(-0.74, 0.05), breaks = seq(-0.6, 0, by = 0.2)) +
  ggplot2::labs(
    x = "Target lncRNA expression bin (CPM, n genes)",
    y = "Median gene log2 fold change"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x          = ggplot2::element_text(size = TEXT_SIZE, color = "black", lineheight = 0.88),
    axis.text.y          = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    axis.title.x         = ggplot2::element_text(size = TEXT_SIZE),
    axis.title.y         = ggplot2::element_text(size = TEXT_SIZE),
    legend.position      = "bottom",
    legend.key.size      = grid::unit(0.18, "cm"),
    legend.key.spacing.x = grid::unit(0.10, "cm"),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE),
    legend.margin        = ggplot2::margin(t = -3, b = 0),
    plot.margin          = ggplot2::margin(2, 4, 2, 2)
  ) +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 2), linetype = ggplot2::guide_legend(nrow = 2))

hdr_d   <- make_header_strip("d", "Silent non-essential controls mask CasRx collateral RNA depletion")
p_d_ind <- (hdr_d / ((p_d1 | p_d2) + patchwork::plot_layout(widths = c(0.47, 0.53)))) +
  patchwork::plot_layout(heights = c(0.16, 2.50))
save_panel_pair(p_d_ind, file.path(path_fig4, "fig4d_silent_ne_and_collateral.pdf"), width = 3.65, height = 2.65)

# ------------------------------------------------------------------------------
# Composite Figure 4 (3 stacked rows at 7.20 in Nature width: a / b / [c | d])
# ------------------------------------------------------------------------------
w_a <- patchwork::wrap_elements(full = p_a_ind)
w_b <- patchwork::wrap_elements(full = patchwork::patchworkGrob(p_b_ind))
w_c <- patchwork::wrap_elements(full = patchwork::patchworkGrob(p_c_ind))
w_d <- patchwork::wrap_elements(full = patchwork::patchworkGrob(p_d_ind))

row_bot_4 <- (w_c | w_d) + patchwork::plot_layout(widths = c(1.00, 1.06))

fig4_comp <- (w_a / w_b / row_bot_4) +
  patchwork::plot_layout(heights = c(1.16, 0.98, 1.00)) +
  patchwork::plot_annotation(
    caption = "*Assigned Claude Opus 5.5; all six runs executed via automatic bio-safety fallback to Claude Opus 5.0.",
    theme = ggplot2::theme(
      plot.caption = ggplot2::element_text(
        family = "Helvetica", face = "plain", size = TEXT_SIZE,
        color = gray_pt_dark, hjust = 0, margin = ggplot2::margin(t = 2, l = 4)
      )
    )
  )

save_panel_pair(fig4_comp, file.path(path_fig4, "Figure_4_composite.pdf"), width = 7.20, height = 8.65, dpi = 300L)

# ------------------------------------------------------------------------------
# Summary log
# ------------------------------------------------------------------------------
cat("fig04.R complete.\n")
cat("Outputs saved to:", path_fig4, "\n")
cat(sprintf("Cold unexpressed audit: %d/%d (%.1f%%); Seeded: %d/%d (%.1f%%); Fisher P = %.4g\n",
            df_c1$k[df_c1$cohort == "All runs\n(N = 36)" & df_c1$arm_group == "Cold"],
            df_c1$n[df_c1$cohort == "All runs\n(N = 36)" & df_c1$arm_group == "Cold"],
            df_c1$pct[df_c1$cohort == "All runs\n(N = 36)" & df_c1$arm_group == "Cold"],
            df_c1$k[df_c1$cohort == "All runs\n(N = 36)" & df_c1$arm_group == "Seeded"],
            df_c1$n[df_c1$cohort == "All runs\n(N = 36)" & df_c1$arm_group == "Seeded"],
            df_c1$pct[df_c1$cohort == "All runs\n(N = 36)" & df_c1$arm_group == "Seeded"],
            df_c1$fisher_p[df_c1$cohort == "All runs\n(N = 36)"][1]))
cat(sprintf("Pooled 274 lncRNA hit calls: %d zero reads (%.1f%%), %d <0.01 TPM (%.1f%%), %d <10 reads (%.1f%%)\n",
            tot_zero, 100 * tot_zero / tot_calls,
            tot_lt001, 100 * tot_lt001 / tot_calls,
            tot_lt10, 100 * tot_lt10 / tot_calls))
