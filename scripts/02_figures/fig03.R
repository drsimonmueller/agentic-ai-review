#!/usr/bin/env Rscript
# Figure 3: Primary-data (E1-E3) proof of library defects and the in silico rescue paradox
# Standardized to Nature 7.20 in width, Helvetica Bold 9 pt tags, Helvetica Plain 6 pt text,
# 0.25 pt hairline strokes, and high-level peer-review-ready labels.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

repo_root_init <- Sys.getenv("AGENTIC_REPO_ROOT", unset = ".")
source(file.path(repo_root_init, "config", "paths.R"))
source(file.path(repo_root, "R", "colors.R"))
source(file.path(repo_root, "R", "theme.R"))
source(file.path(repo_root, "R", "stats_utils.R"))
source(file.path(repo_root, "R", "plot_utils.R"))

set.seed(GLOBAL_SEED)

fig3_dir <- ensure_dir(file.path(path_figures, "Figure_3"))
ensure_dir(path_tables)

# Nature typography & shared structural constants (R/colors.R)
TAG_SIZE     <- 9.0
TEXT_SIZE    <- 6.0
GG_TEXT_SIZE <- TEXT_SIZE / ggplot2::.pt

gray_bar_med <- col_qc_gray
gray_bar_lt  <- col_bar_gray
gray_pt_dark <- col_dark
gray_line    <- col_divider

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

format_p_short <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 1e-300 ~ "P < 1e-300",
    p < 0.001 ~ paste0("P = ", formatC(p, format = "e", digits = 1)),
    p >= 0.05 ~ paste0("P = ", formatC(p, format = "f", digits = 2), " (NS)"),
    TRUE ~ paste0("P = ", formatC(p, format = "f", digits = 3))
  )
}

cell_order_code <- c("LN-18", "KP-4", "MIA PaCa-2", "NCI-H460", "A549", "LN-229")
cell_order_disp <- c("LN-18", "KP-4", "MIA\nPaCa-2", "NCI-\nH460", "A549", "LN-229")
cell_disp_map   <- stats::setNames(cell_order_disp, cell_order_code)

cell_code_to_disp <- c(
  "LN18"     = "LN-18",
  "KP4"      = "KP-4",
  "MIAPACA2" = "MIA PaCa-2",
  "NCIH460"  = "NCI-H460",
  "A549"     = "A549",
  "LN229"    = "LN-229"
)

# ==============================================================================
# Load primary source datasets
# ==============================================================================
ae_depmap_path <- require_file(file.path(path_sd_forensics, "ae_recall_vs_depmap_6lines.csv"))
arrays_qc_path <- require_file(file.path(path_sd_forensics, "albarossa_arrays_qc.tsv.gz"))
plasmid_path   <- require_file(file.path(path_sd_forensics, "plasmid_chimerism_and_bsmbi.csv"))
mageck_path    <- require_file(file.path(path_sd_forensics, "mageck_rescue_and_rra_cliff.csv"))

ae_depmap <- readr::read_csv(ae_depmap_path, show_col_types = FALSE)
require_columns(ae_depmap, c(
  "cell_line", "display_name", "depmap_cas9_recall_lt_neg05_pct",
  "casrx_ae_recall_fdr25_pct", "casrx_ne_hits_fdr25", "casrx_ne_tested",
  "lncrna_hits_fdr25"
), "ae_recall_vs_depmap_6lines.csv")

arrays_qc <- readr::read_tsv(arrays_qc_path, show_col_types = FALSE)
require_columns(arrays_qc, c(
  "array_id", "category", "has_T4_core23", "has_T4_ext7", "has_T4_any",
  "casrx6_mean_lfc", "wo3_mean_lfc"
), "albarossa_arrays_qc.tsv.gz")

plasmid_df <- readr::read_csv(plasmid_path, show_col_types = FALSE)
require_columns(plasmid_df, c(
  "section", "sample", "ena_accession", "total_pairs", "concordant_pairs",
  "concordant_pct", "discordant_pairs", "discordant_pct",
  "bsmbi_class", "n_arrays", "concordant_median",
  "chim_spacer1_median", "chim_spacer2_median", "ref_concordant_median",
  "mwu_p_vs_intact"
), "plasmid_chimerism_and_bsmbi.csv")

mageck_df <- readr::read_csv(mageck_path, show_col_types = FALSE)
require_columns(mageck_df, c(
  "section", "step_or_mode", "cell_line", "is_wocasrx", "control_sgrna_ne",
  "k_arrays", "ae_recall_fdr25_pct", "ae_k3_recall_pct", "ae_k2_recall_pct"
), "mageck_rescue_and_rra_cliff.csv")

# ==============================================================================
# Panel a: Essential-gene recall gap versus Cas9 and sparse lncRNA hits
# ==============================================================================
ae_6 <- ae_depmap %>%
  dplyr::filter(cell_line != "UNION_6LINES") %>%
  dplyr::mutate(
    line_2line = factor(unname(cell_disp_map[display_name]), levels = cell_order_disp)
  )

ae_union <- ae_depmap %>% dplyr::filter(cell_line == "UNION_6LINES")

ae_long <- ae_6 %>%
  dplyr::select(
    line_2line,
    depmap = depmap_cas9_recall_lt_neg05_pct,
    casrx  = casrx_ae_recall_fdr25_pct
  ) %>%
  tidyr::pivot_longer(
    cols = c(depmap, casrx),
    names_to = "platform",
    values_to = "recall_pct"
  ) %>%
  dplyr::mutate(
    platform = dplyr::recode(
      platform,
      depmap = "DepMap Cas9 essentiality",
      casrx  = "Albarossa CasRx screen"
    ),
    platform = factor(platform, levels = c(
      "DepMap Cas9 essentiality",
      "Albarossa CasRx screen"
    )),
    lbl = sprintf("%.1f%%", recall_pct)
  )

rho_ae_lnc <- stats::cor.test(
  ae_6$casrx_ae_recall_fdr25_pct,
  ae_6$lncrna_hits_fdr25,
  method = "spearman",
  exact = FALSE
)

p3a_top <- ggplot2::ggplot(ae_long, ggplot2::aes(x = line_2line, y = recall_pct, fill = platform)) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.74),
    width = 0.66,
    color = NA
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = lbl),
    position = ggplot2::position_dodge(width = 0.74),
    vjust = -0.35,
    size = GG_TEXT_SIZE,
    family = "Helvetica",
    color = "black"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "DepMap Cas9 essentiality" = gray_bar_med,
      "Albarossa CasRx screen"   = pal_yellow
    ),
    name = NULL
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 114),
    breaks = c(0, 25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Essential-gene recall (%)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    legend.position      = "top",
    legend.direction     = "horizontal",
    legend.key.size      = grid::unit(0.20, "cm"),
    legend.key.spacing.x = grid::unit(0.25, "cm"),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE, margin = ggplot2::margin(l = 2, r = 4)),
    legend.margin        = ggplot2::margin(0, 0, -4, 0),
    axis.text.x          = ggplot2::element_blank(),
    axis.ticks.x         = ggplot2::element_blank(),
    plot.margin          = ggplot2::margin(1, 4, 1, 4)
  )

p3a_bot <- ggplot2::ggplot(ae_6, ggplot2::aes(x = line_2line, y = lncrna_hits_fdr25)) +
  ggplot2::geom_col(width = 0.50, fill = pal_teal, color = NA) +
  ggplot2::geom_text(
    ggplot2::aes(label = lncrna_hits_fdr25),
    vjust = -0.35,
    size = GG_TEXT_SIZE,
    family = "Helvetica",
    color = "black"
  ) +
  ggplot2::annotate(
    "text",
    x = 6.38, y = 104,
    label = sprintf("6-line union: %d hits (Spearman r = %.2f)", ae_union$lncrna_hits_fdr25, unname(rho_ae_lnc$estimate)),
    hjust = 1, vjust = 1,
    size = GG_TEXT_SIZE, family = "Helvetica", color = gray_pt_dark
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 112), breaks = c(0, 40, 80), expand = c(0, 0)) +
  ggplot2::labs(x = NULL, y = "lncRNA\nhits") +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x  = ggplot2::element_text(size = TEXT_SIZE, color = "black", lineheight = 0.90),
    axis.title.y = ggplot2::element_text(size = TEXT_SIZE, angle = 90, vjust = 0.5),
    plot.margin  = ggplot2::margin(1, 4, 2, 4)
  )

hdr_a <- make_header_strip("a", "Positive-control recall gap versus Cas9 and sparse lncRNA hits")
fig3a_panel <- (hdr_a / p3a_top / p3a_bot) + patchwork::plot_layout(heights = c(0.20, 2.35, 1.05))

save_panel_pair(
  fig3a_panel,
  file.path(fig3_dir, "fig3a_ae_recall_vs_depmap.pdf"),
  width = 3.70, height = 2.65
)

# ==============================================================================
# Panel b: Pol III poly-T positional terminator effect in essential arrays (n = 900)
# ==============================================================================
ae_arrays <- arrays_qc %>% dplyr::filter(category == "AE")

lbl_no_t4   <- "No poly-T"
lbl_ext7    <- "3' extension\n(nt 24-30)"
lbl_core    <- "Core spacer\n(nt 1-23)"
lbl_wocasrx <- "No-nuclease\ncontrol"

t4_casrx <- ae_arrays %>%
  dplyr::mutate(
    t4_stratum = dplyr::case_when(
      has_T4_core23 == 1L ~ lbl_core,
      has_T4_ext7 == 1L   ~ lbl_ext7,
      TRUE                ~ lbl_no_t4
    ),
    lfc = casrx6_mean_lfc
  ) %>%
  dplyr::select(array_id, t4_stratum, lfc, has_T4_core23, has_T4_any)

t4_wo <- ae_arrays %>%
  dplyr::transmute(
    array_id = array_id,
    t4_stratum = lbl_wocasrx,
    lfc = wo3_mean_lfc,
    has_T4_core23 = has_T4_core23,
    has_T4_any = has_T4_any
  )

b_levels <- c(lbl_no_t4, lbl_ext7, lbl_core, lbl_wocasrx)

t4_plot_df <- dplyr::bind_rows(t4_casrx, t4_wo) %>%
  dplyr::mutate(t4_stratum = factor(t4_stratum, levels = b_levels))

b_summary <- t4_plot_df %>%
  dplyr::group_by(t4_stratum) %>%
  dplyr::summarise(
    n = dplyr::n(),
    median_lfc = stats::median(lfc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    x_lbl   = paste0(as.character(t4_stratum), "\n(n = ", n, ")"),
    med_lbl = sprintf("%.2f", median_lfc)
  )

mwu_core_vs_no <- run_wilcox_pair(
  t4_casrx$lfc[t4_casrx$t4_stratum == lbl_core],
  t4_casrx$lfc[t4_casrx$t4_stratum == lbl_no_t4]
)
mwu_ext7_vs_no <- run_wilcox_pair(
  t4_casrx$lfc[t4_casrx$t4_stratum == lbl_ext7],
  t4_casrx$lfc[t4_casrx$t4_stratum == lbl_no_t4]
)
mwu_any_vs_no <- run_wilcox_pair(
  t4_casrx$lfc[t4_casrx$has_T4_any == 1L],
  t4_casrx$lfc[t4_casrx$has_T4_any == 0L]
)
mwu_wo_core_no <- run_wilcox_pair(
  t4_wo$lfc[t4_wo$has_T4_core23 == 1L],
  t4_wo$lfc[t4_wo$has_T4_core23 == 0L]
)

b_palette <- stats::setNames(
  c(gray_bar_med, gray_bar_lt, pal_yellow, gray_bar_lt),
  b_levels
)

n_lib_t4     <- sum(arrays_qc$has_T4_any == 1L)
pct_lib_t4   <- 100 * n_lib_t4 / nrow(arrays_qc)
shift_any_t4 <- mwu_any_vs_no$median_x - mwu_any_vs_no$median_y

p3b_core <- ggplot2::ggplot(t4_plot_df, ggplot2::aes(x = t4_stratum, y = lfc, fill = t4_stratum)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = pal_med_gray, linewidth = LW_025) +
  ggplot2::geom_hline(yintercept = -0.5, linetype = "dotted", color = pal_med_gray, linewidth = LW_025) +
  ggplot2::geom_violin(trim = TRUE, width = 0.64, alpha = 0.72, color = "black", linewidth = LW_025) +
  ggplot2::geom_boxplot(
    width = 0.14, outlier.shape = NA, fill = "white", color = "black",
    linewidth = LW_025, median.linewidth = LW_050
  ) +
  ggplot2::geom_point(
    data = b_summary,
    ggplot2::aes(x = t4_stratum, y = median_lfc),
    inherit.aes = FALSE,
    shape = 23, size = 1.65, fill = "white", color = "black", stroke = 0.25
  ) +
  ggplot2::geom_text(
    data = b_summary,
    ggplot2::aes(x = as.numeric(t4_stratum) + 0.22, y = median_lfc, label = med_lbl),
    inherit.aes = FALSE,
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black", hjust = 0
  ) +
  # Bracket: No poly-T vs 3' extension
  ggplot2::annotate("segment", x = 1, xend = 2, y = 1.48, yend = 1.48, linewidth = LW_025, color = "black") +
  ggplot2::annotate("segment", x = 1, xend = 1, y = 1.35, yend = 1.48, linewidth = LW_025, color = "black") +
  ggplot2::annotate("segment", x = 2, xend = 2, y = 1.35, yend = 1.48, linewidth = LW_025, color = "black") +
  ggplot2::annotate(
    "text", x = 1.5, y = 1.72,
    label = format_p_short(mwu_ext7_vs_no$p_value),
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  # Bracket: No poly-T vs Core spacer
  ggplot2::annotate("segment", x = 1, xend = 3, y = 2.15, yend = 2.15, linewidth = LW_025, color = "black") +
  ggplot2::annotate("segment", x = 1, xend = 1, y = 2.02, yend = 2.15, linewidth = LW_025, color = "black") +
  ggplot2::annotate("segment", x = 3, xend = 3, y = 2.02, yend = 2.15, linewidth = LW_025, color = "black") +
  ggplot2::annotate(
    "text", x = 2.0, y = 2.45,
    label = sprintf("Core vs none: %s", format_p_short(mwu_core_vs_no$p_value)),
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::annotate(
    "text", x = 4.00, y = 2.35,
    label = sprintf("No-nuclease\n%s", format_p_short(mwu_wo_core_no$p_value)),
    size = GG_TEXT_SIZE, family = "Helvetica", color = gray_pt_dark, lineheight = 0.92
  ) +
  ggplot2::scale_fill_manual(values = b_palette, guide = "none") +
  ggplot2::scale_x_discrete(labels = b_summary$x_lbl, expand = ggplot2::expansion(add = c(0.55, 0.72))) +
  ggplot2::scale_y_continuous(limits = c(-4.75, 3.05), breaks = c(-4, -3, -2, -1, 0, 1, 2)) +
  ggplot2::labs(
    x = NULL,
    y = "6-line mean depletion (log2 fold change)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    axis.text.x   = ggplot2::element_text(size = TEXT_SIZE, color = "black", lineheight = 0.90),
    plot.margin   = ggplot2::margin(1, 4, 2, 4)
  )

hdr_b <- make_header_strip("b", "Positional RNA Pol III poly-T termination in 23-nt core vs 3' extension")
fig3b_panel <- (hdr_b / p3b_core) + patchwork::plot_layout(heights = c(0.20, 3.40))

save_panel_pair(
  fig3b_panel,
  file.path(fig3_dir, "fig3b_pol3_t4_positional.pdf"),
  width = 3.50, height = 2.65
)

# ==============================================================================
# Panel c: Raw-read plasmid chimerism & effective cellular coverage loss
# ==============================================================================
ena_df <- plasmid_df %>%
  dplyr::filter(section == "ena_alignment_summary") %>%
  dplyr::mutate(
    other_pct = pmax(0, 100 - concordant_pct - discordant_pct),
    doubly_mapped_disc_pct = 100 * discordant_pairs / (concordant_pairs + discordant_pairs),
    sample_label = dplyr::case_when(
      sample == "ControlLibrary_Library"        ~ "Control library (ERR12330450)",
      sample == "ALBAROSSA_Library"             ~ "Albarossa plasmid (ERR12330401)",
      sample == "LN18_CasRx_ALBAROSSA_Rep1"     ~ "LN-18 gDNA (ERR12330410)",
      sample == "A549_CasRx_ALBAROSSA_Rep1"     ~ "A549 gDNA (ERR12330402)",
      sample == "MIAPACA2_CasRx_ALBAROSSA_Rep1" ~ "MIA PaCa-2 gDNA (ERR12330419)",
      sample == "KP4_CasRx_ALBAROSSA_Rep1"      ~ "KP-4 gDNA (ERR12330406)",
      sample == "LN229_CasRx_ALBAROSSA_Rep1"    ~ "LN-229 gDNA (ERR12330414)",
      sample == "NCIH460wo_ALBAROSSA_Rep1"      ~ "NCI-H460 no-nuclease (ERR12330421)",
      TRUE ~ sample
    )
  )

c_order <- rev(c(
  "Control library (ERR12330450)",
  "Albarossa plasmid (ERR12330401)",
  "LN-18 gDNA (ERR12330410)",
  "A549 gDNA (ERR12330402)",
  "MIA PaCa-2 gDNA (ERR12330419)",
  "KP-4 gDNA (ERR12330406)",
  "LN-229 gDNA (ERR12330414)",
  "NCI-H460 no-nuclease (ERR12330421)"
))

ena_long <- ena_df %>%
  dplyr::mutate(sample_label = factor(sample_label, levels = c_order)) %>%
  dplyr::select(
    sample_label,
    Concordant = concordant_pct,
    Discordant = discordant_pct,
    Other      = other_pct
  ) %>%
  tidyr::pivot_longer(
    cols = -sample_label,
    names_to = "pair_class",
    values_to = "pct"
  ) %>%
  dplyr::mutate(
    pair_class = dplyr::recode(pair_class, Other = "Unmapped"),
    pair_class = factor(pair_class, levels = c(
      "Unmapped",
      "Discordant",
      "Concordant"
    ))
  )

ena_ann <- ena_df %>%
  dplyr::mutate(
    sample_label = factor(sample_label, levels = c_order),
    conc_lbl = sprintf("%.1f%%", concordant_pct)
  )

p3c_left <- ggplot2::ggplot(ena_long, ggplot2::aes(x = pct, y = sample_label, fill = pair_class)) +
  ggplot2::geom_rect(
    data = tibble::tibble(y_num = c(1, 3, 5, 7)),
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = y_num - 0.5, ymax = y_num + 0.5),
    inherit.aes = FALSE, fill = col_stripe
  ) +
  ggplot2::geom_col(width = 0.64, color = NA) +
  ggplot2::geom_text(
    data = ena_ann,
    ggplot2::aes(x = concordant_pct / 2, y = sample_label, label = conc_lbl),
    inherit.aes = FALSE,
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::geom_text(
    data = ena_ann %>% dplyr::filter(discordant_pct > 10),
    ggplot2::aes(x = concordant_pct + discordant_pct / 2, y = sample_label, label = sprintf("%.1f%%", discordant_pct)),
    inherit.aes = FALSE,
    size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::geom_text(
    data = ena_ann,
    ggplot2::aes(x = 102.0, y = sample_label, label = sprintf("%.1f%%*", doubly_mapped_disc_pct)),
    inherit.aes = FALSE,
    hjust = 0, size = GG_TEXT_SIZE, family = "Helvetica", color = pal_yellow_dark
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Concordant" = gray_bar_med,
      "Discordant" = pal_yellow,
      "Unmapped"   = gray_bar_lt
    ),
    breaks = c("Concordant", "Discordant", "Unmapped"),
    name = NULL
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0, 128),
    breaks = c(0, 25, 50, 75, 100),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    x = "Raw paired-end reads (%)",
    y = NULL,
    caption = "*Discordant % of doubly-mapped pairs"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    legend.position      = "top",
    legend.key.size      = grid::unit(0.18, "cm"),
    legend.key.spacing.x = grid::unit(0.10, "cm"),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE, margin = ggplot2::margin(l = 2, r = 2)),
    legend.margin        = ggplot2::margin(0, 0, -4, 0),
    axis.line.y          = ggplot2::element_blank(),
    axis.ticks.y         = ggplot2::element_blank(),
    axis.text.y          = ggplot2::element_text(size = TEXT_SIZE, color = "black"),
    plot.caption         = ggplot2::element_text(size = TEXT_SIZE, hjust = 0, color = gray_pt_dark, margin = ggplot2::margin(t = 2)),
    plot.margin          = ggplot2::margin(1, 2, 2, 2)
  )

alb_plasmid         <- ena_df %>% dplyr::filter(ena_accession == "ERR12330401")
intact_doubly_frac  <- alb_plasmid$concordant_pairs / (alb_plasmid$concordant_pairs + alb_plasmid$discordant_pairs)
concordant_all_frac <- alb_plasmid$concordant_pct / 100

cov_df <- tibble::tibble(
  stage = factor(
    c("Transduction (23.5M)", "Transduction (23.5M)",
      "Passaging (20M)", "Passaging (20M)"),
    levels = c("Transduction (23.5M)", "Passaging (20M)")
  ),
  status = factor(
    c("Nominal", "Intact", "Nominal", "Intact"),
    levels = c("Nominal", "Intact")
  ),
  cov_x = c(
    23.5e6 / MONTERO_N_ARRAYS,
    (23.5e6 / MONTERO_N_ARRAYS) * intact_doubly_frac,
    20.0e6 / MONTERO_N_ARRAYS,
    (20.0e6 / MONTERO_N_ARRAYS) * concordant_all_frac
  )
) %>%
  dplyr::mutate(lbl = sprintf("%.0f\u00d7", cov_x))

p3c_right <- ggplot2::ggplot(cov_df, ggplot2::aes(x = stage, y = cov_x, fill = status)) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.72),
    width = 0.62, color = NA
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = lbl),
    position = ggplot2::position_dodge(width = 0.72),
    vjust = -0.35, size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_fill_manual(
    values = c("Nominal" = gray_bar_med, "Intact" = pal_teal),
    name = NULL
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 355), breaks = c(0, 100, 200, 300), expand = c(0, 0)) +
  ggplot2::labs(x = NULL, y = "Coverage (cells/array)") +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    legend.position      = "top",
    legend.key.size      = grid::unit(0.20, "cm"),
    legend.key.spacing.x = grid::unit(0.20, "cm"),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE, margin = ggplot2::margin(l = 2, r = 3)),
    legend.margin        = ggplot2::margin(0, 0, -4, 0),
    axis.text.x          = ggplot2::element_text(size = TEXT_SIZE, color = "black", lineheight = 0.88, angle = 22, hjust = 1),
    plot.margin          = ggplot2::margin(1, 4, 2, 4)
  )

hdr_c <- make_header_strip("c", "Direct-repeat recombination halves effective cellular coverage")
p3c_body <- (p3c_left + p3c_right) + patchwork::plot_layout(widths = c(1.56, 1.00))
fig3c_panel <- (hdr_c / p3c_body) + patchwork::plot_layout(heights = c(0.20, 3.15))

save_panel_pair(
  fig3c_panel,
  file.path(fig3_dir, "fig3c_plasmid_chimerism_coverage.pdf"),
  width = 4.25, height = 2.55
)

# ==============================================================================
# Panel d: BsmBI molecular chronometer proof of pre-cloning recombination
# ==============================================================================
bsmbi_sum <- plasmid_df %>%
  dplyr::filter(section == "bsmbi_chronometer", bsmbi_class != "site_in_both")

bsmbi_det <- plasmid_df %>%
  dplyr::filter(section == "bsmbi_chronometer_detailed", ena_accession == "ERR12330401")

bsmbi_bysite <- plasmid_df %>%
  dplyr::filter(section == "bsmbi_chronometer_by_site")

d_class_map <- c(
  "no_site"              = "No BsmBI\n(n = 77,632)",
  "site_in_spacer1_only" = "Spacer 1\n(n = 208)",
  "site_in_spacer2_only" = "Spacer 2\n(n = 201)",
  "junction_only"        = "Junction\n(n = 83)"
)

bsmbi_long <- bsmbi_sum %>%
  dplyr::mutate(class_lbl = factor(unname(d_class_map[bsmbi_class]), levels = unname(d_class_map))) %>%
  dplyr::select(
    bsmbi_class, class_lbl, n_arrays,
    concordant = concordant_median,
    chim_sp1   = chim_spacer1_median,
    chim_sp2   = chim_spacer2_median
  ) %>%
  tidyr::pivot_longer(
    cols = c(concordant, chim_sp1, chim_sp2),
    names_to = "read_type",
    values_to = "median_reads"
  ) %>%
  dplyr::mutate(
    read_type = factor(read_type, levels = c("concordant", "chim_sp1", "chim_sp2")),
    cut_state = dplyr::case_when(
      bsmbi_class == "no_site" ~ "No BsmBI site",
      bsmbi_class == "site_in_spacer1_only" & read_type == "chim_sp2" ~ "Intact partner",
      bsmbi_class == "site_in_spacer2_only" & read_type == "chim_sp1" ~ "Intact partner",
      bsmbi_class == "junction_only" & read_type == "chim_sp1"        ~ "Intact partner",
      TRUE ~ "Cleaved"
    ),
    cut_state = factor(cut_state, levels = c("No BsmBI site", "Cleaved", "Intact partner")),
    bar_lbl = ifelse(median_reads == floor(median_reads),
                     sprintf("%.0f", median_reads),
                     sprintf("%.1f", median_reads))
  )

p_sp1_conc    <- bsmbi_det$mwu_p_vs_intact[bsmbi_det$bsmbi_class == "site_in_spacer1_only__intact"]
p_sp1_chim2   <- bsmbi_det$mwu_p_vs_intact[bsmbi_det$bsmbi_class == "site_in_spacer1_only__chim_spacer2"]
p_sp2_chim1   <- bsmbi_det$mwu_p_vs_intact[bsmbi_det$bsmbi_class == "site_in_spacer2_only__chim_spacer1"]
none_ref_med  <- bsmbi_bysite$concordant_median[bsmbi_bysite$bsmbi_class == "none"]
n_total_bsmbi <- sum(bsmbi_bysite$n_arrays[bsmbi_bysite$bsmbi_class != "none"])

p3d_core <- ggplot2::ggplot(bsmbi_long, ggplot2::aes(x = class_lbl, y = median_reads, fill = cut_state, group = read_type)) +
  ggplot2::geom_hline(yintercept = 258, linetype = "dashed", color = pal_med_gray, linewidth = LW_025) +
  ggplot2::geom_hline(yintercept = 197, linetype = "dotted", color = pal_teal_dark, linewidth = LW_025) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.76),
    width = 0.66, color = NA
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = bar_lbl),
    position = ggplot2::position_dodge(width = 0.76),
    vjust = -0.40, size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::annotate(
    "text", x = 2.00, y = 228,
    label = format_p_short(p_sp1_chim2),
    size = GG_TEXT_SIZE, family = "Helvetica", color = pal_teal_dark
  ) +
  ggplot2::annotate(
    "text", x = 3.25, y = 228,
    label = format_p_short(p_sp2_chim1),
    size = GG_TEXT_SIZE, family = "Helvetica", color = pal_teal_dark
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "No BsmBI site"  = gray_bar_med,
      "Cleaved"        = pal_yellow,
      "Intact partner" = pal_teal
    ),
    name = NULL
  ) +
  ggplot2::scale_y_continuous(limits = c(0, 300), breaks = c(0, 100, 200, 300), expand = c(0, 0)) +
  ggplot2::labs(
    x = NULL,
    y = "Median plasmid reads (ERR12330401)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    legend.position      = "top",
    legend.direction     = "horizontal",
    legend.key.size      = grid::unit(0.18, "cm"),
    legend.key.spacing.x = grid::unit(0.14, "cm"),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE, margin = ggplot2::margin(l = 2, r = 3)),
    legend.margin        = ggplot2::margin(0, 0, -4, 0),
    axis.text.x          = ggplot2::element_text(size = TEXT_SIZE, color = "black", lineheight = 0.90),
    plot.caption         = ggplot2::element_text(size = TEXT_SIZE, hjust = 0, color = gray_pt_dark, margin = ggplot2::margin(t = 2)),
    plot.margin          = ggplot2::margin(1, 4, 2, 4)
  )

hdr_d <- make_header_strip("d", "BsmBI molecular chronometer proves pre-cloning recombination")
fig3d_panel <- (hdr_d / p3d_core) + patchwork::plot_layout(heights = c(0.20, 3.15))

save_panel_pair(
  fig3d_panel,
  file.path(fig3_dir, "fig3d_bsmbi_chronometer.pdf"),
  width = 3.20, height = 2.55
)

# ==============================================================================
# Panel e: MAGeCK RRA array-count cliff and the in silico rescue paradox
# ==============================================================================
rra_sim <- mageck_df %>%
  dplyr::filter(section == "rra_power_simulation_120") %>%
  dplyr::mutate(display_name = factor(unname(cell_code_to_disp[cell_line]),
                                      levels = c("LN-18", "KP-4", "A549", "LN-229")))

rra_sum <- rra_sim %>%
  dplyr::group_by(display_name, k_arrays) %>%
  dplyr::summarise(
    mean_recall = mean(ae_recall_fdr25_pct),
    sd_recall   = stats::sd(ae_recall_fdr25_pct),
    n_seeds     = dplyr::n(),
    .groups     = "drop"
  ) %>%
  dplyr::mutate(is_focal = display_name %in% c("LN-18", "LN-229"))

line_cols_sim <- c(
  "LN-18"  = pal_blue,
  "KP-4"   = gray_line,
  "A549"   = gray_bar_med,
  "LN-229" = pal_teal
)

p3e_left <- ggplot2::ggplot(
  rra_sum,
  ggplot2::aes(x = k_arrays, y = mean_recall, color = display_name, fill = display_name)
) +
  ggplot2::annotate(
    "rect", xmin = 1.85, xmax = 3.15, ymin = 0, ymax = 105,
    fill = pal_yellow_pale, alpha = 0.80
  ) +
  ggplot2::geom_vline(xintercept = 3, linetype = "dashed", color = pal_yellow_dark, linewidth = LW_025) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = pmax(0, mean_recall - sd_recall), ymax = pmin(100, mean_recall + sd_recall)),
    alpha = 0.16, color = NA
  ) +
  ggplot2::geom_line(ggplot2::aes(linewidth = is_focal)) +
  ggplot2::scale_linewidth_manual(values = c("TRUE" = LW_050, "FALSE" = pt_to_lwd(0.35)), guide = "none") +
  ggplot2::geom_point(size = 1.25) +
  ggplot2::annotate(
    "text", x = 2.00, y = 101,
    label = "Power cliff (k = 2-3)\n2.98 to 2.53 arrays/gene",
    hjust = 0, vjust = 1, size = GG_TEXT_SIZE, family = "Helvetica",
    color = pal_yellow_dark, lineheight = 0.92
  ) +
  ggplot2::annotate(
    "text", x = 7.1, y = 4,
    label = "120 simulations\n(4 lines \u00d7 6 k \u00d7 5 seeds)",
    hjust = 1, vjust = 0, size = GG_TEXT_SIZE, family = "Helvetica",
    color = gray_pt_dark, lineheight = 0.92
  ) +
  ggplot2::scale_color_manual(values = line_cols_sim, name = NULL) +
  ggplot2::scale_fill_manual(values = line_cols_sim, name = NULL) +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
    fill  = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  ) +
  ggplot2::scale_x_continuous(breaks = 2:7, limits = c(1.8, 7.2)) +
  ggplot2::scale_y_continuous(limits = c(0, 106), breaks = c(0, 25, 50, 75, 100), expand = c(0, 0)) +
  ggplot2::labs(
    x = "Arrays per gene (k)",
    y = "Simulated essential-gene recall (%)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    legend.position      = "top",
    legend.key.size      = grid::unit(0.20, "cm"),
    legend.key.spacing.x = grid::unit(0.22, "cm"),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE, margin = ggplot2::margin(l = 2, r = 4)),
    legend.margin        = ggplot2::margin(0, 0, -4, 0),
    plot.margin          = ggplot2::margin(1, 4, 2, 4)
  )

s0_base <- mageck_df %>%
  dplyr::filter(section == "baseline_vs_filter_and_rescue",
                step_or_mode == "Step0_published_T12", !is_wocasrx) %>%
  dplyr::transmute(
    cell_line,
    mode = "Published baseline (n = 300)",
    recall_pct = ae_recall_fdr25_pct
  )

t3_naive <- mageck_df %>%
  dplyr::filter(section == "neon_naive_filter_crosscheck",
                step_or_mode == "Tier3_NoBsmBI_NoAnyTTTT") %>%
  dplyr::transmute(
    cell_line,
    mode = "Naive defect filter (n = 300)",
    recall_pct = ae_recall_fdr25_pct
  )

s1c_k3 <- mageck_df %>%
  dplyr::filter(section == "baseline_vs_filter_and_rescue",
                step_or_mode == "Step1c_filter_TTTT30_kge2", !is_wocasrx) %>%
  dplyr::transmute(
    cell_line,
    mode = "Power-matched clean (k = 3, n = 184)",
    recall_pct = ae_k3_recall_pct
  )

s3b_ne <- mageck_df %>%
  dplyr::filter(section == "baseline_vs_filter_and_rescue",
                step_or_mode == "Step3b_rescue_TTTT23_ctrlNE_kge2", !is_wocasrx) %>%
  dplyr::transmute(
    cell_line,
    mode = "Non-essential calibrated rescue (n = 285)",
    recall_pct = ae_recall_fdr25_pct
  )

mode_levels <- c(
  "Published baseline (n = 300)",
  "Naive defect filter (n = 300)",
  "Power-matched clean (k = 3, n = 184)",
  "Non-essential calibrated rescue (n = 285)"
)

rescue_df <- dplyr::bind_rows(s0_base, t3_naive, s1c_k3, s3b_ne) %>%
  dplyr::mutate(
    line_name  = unname(cell_code_to_disp[cell_line]),
    line_2line = factor(unname(cell_disp_map[line_name]), levels = cell_order_disp),
    mode       = factor(mode, levels = mode_levels),
    lbl        = sprintf("%.0f", recall_pct)
  )

rescue_cols <- c(
  "Published baseline (n = 300)"              = gray_bar_med,
  "Naive defect filter (n = 300)"             = pal_yellow,
  "Power-matched clean (k = 3, n = 184)"      = pal_teal,
  "Non-essential calibrated rescue (n = 285)" = pal_blue
)

p3e_right <- ggplot2::ggplot(rescue_df, ggplot2::aes(x = line_2line, y = recall_pct, fill = mode)) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.78),
    width = 0.70, color = NA
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = lbl),
    position = ggplot2::position_dodge(width = 0.78),
    vjust = -0.35, size = GG_TEXT_SIZE, family = "Helvetica", color = "black"
  ) +
  ggplot2::scale_fill_manual(values = rescue_cols, name = NULL) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
  ggplot2::scale_y_continuous(limits = c(0, 92), breaks = c(0, 25, 50, 75), expand = c(0, 0)) +
  ggplot2::labs(
    x = NULL,
    y = "Essential-gene recall (FDR < 0.25, %)"
  ) +
  theme_paper(base_size = TEXT_SIZE) +
  ggplot2::theme(
    legend.position      = "top",
    legend.key.size      = grid::unit(0.20, "cm"),
    legend.key.spacing.x = grid::unit(0.22, "cm"),
    legend.text          = ggplot2::element_text(size = TEXT_SIZE, margin = ggplot2::margin(l = 2, r = 4)),
    legend.margin        = ggplot2::margin(0, 0, -4, 0),
    axis.text.x          = ggplot2::element_text(size = TEXT_SIZE, color = "black", lineheight = 0.90),
    plot.margin          = ggplot2::margin(1, 4, 2, 4)
  )

hdr_e <- make_header_strip("e", "In silico rescue paradox: RRA array-count cliff versus calibrated rescue")
p3e_body <- (p3e_left + p3e_right) + patchwork::plot_layout(widths = c(0.95, 1.55))
fig3e_panel <- (hdr_e / p3e_body) + patchwork::plot_layout(heights = c(0.20, 2.75))

save_panel_pair(
  fig3e_panel,
  file.path(fig3_dir, "fig3e_mageck_rra_cliff_rescue.pdf"),
  width = 7.20, height = 2.50
)

# ==============================================================================
# Composite Figure 3 (3 rows: a+b / c+d / e at 7.20 in Nature width)
# ==============================================================================
row1 <- patchwork::wrap_elements(full = patchwork::patchworkGrob(fig3a_panel)) +
  patchwork::wrap_elements(full = patchwork::patchworkGrob(fig3b_panel)) +
  patchwork::plot_layout(widths = c(1.06, 1.00))

row2 <- patchwork::wrap_elements(full = patchwork::patchworkGrob(fig3c_panel)) +
  patchwork::wrap_elements(full = patchwork::patchworkGrob(fig3d_panel)) +
  patchwork::plot_layout(widths = c(1.38, 1.00))

row3 <- patchwork::wrap_elements(full = patchwork::patchworkGrob(fig3e_panel))

fig3_composite <- (row1 / row2 / row3) +
  patchwork::plot_layout(heights = c(1.06, 1.00, 0.96))

composite_pdf <- file.path(fig3_dir, "Figure_3_composite.pdf")
save_panel_pair(fig3_composite, composite_pdf, width = 7.20, height = 7.95, dpi = 300L)

# ==============================================================================
# Write summary statistics table for verification
# ==============================================================================
summary_stats <- tibble::tibble(
  metric = c(
    "ae_recall_cas9_min_pct", "ae_recall_cas9_max_pct",
    "ae_recall_casrx_min_pct", "ae_recall_casrx_max_pct",
    "ae_lnc_hits_min", "ae_lnc_hits_max",
    "lib_t4_count", "lib_t4_pct",
    "ae_core23_t4_n", "ae_core23_t4_median",
    "ae_ext7_only_t4_n", "ae_ext7_only_t4_median",
    "ae_no_t4_n", "ae_no_t4_median",
    "ae_any_t4_shift_lfc", "ae_any_t4_mwu_p", "ae_core23_vs_no_mwu_p",
    "ae_ext7_vs_no_mwu_p", "wo_core23_vs_no_mwu_p",
    "ctrl_lib_disc_pct", "ctrl_lib_disc_doubly_pct",
    "alb_plasmid_disc_pct", "alb_plasmid_disc_doubly_pct",
    "cov_transduction_nom_x", "cov_transduction_eff_x",
    "cov_passaging_nom_x", "cov_passaging_eff_x",
    "bsmbi_total_albarossa_n", "bsmbi_sp1_conc_median", "bsmbi_sp1_chim2_median", "bsmbi_sp1_chim2_p"
  ),
  value = c(
    min(ae_6$depmap_cas9_recall_lt_neg05_pct), max(ae_6$depmap_cas9_recall_lt_neg05_pct),
    min(ae_6$casrx_ae_recall_fdr25_pct), max(ae_6$casrx_ae_recall_fdr25_pct),
    min(ae_6$lncrna_hits_fdr25), max(ae_6$lncrna_hits_fdr25),
    n_lib_t4, pct_lib_t4,
    b_summary$n[b_summary$t4_stratum == lbl_core],
    b_summary$median_lfc[b_summary$t4_stratum == lbl_core],
    b_summary$n[b_summary$t4_stratum == lbl_ext7],
    b_summary$median_lfc[b_summary$t4_stratum == lbl_ext7],
    b_summary$n[b_summary$t4_stratum == lbl_no_t4],
    b_summary$median_lfc[b_summary$t4_stratum == lbl_no_t4],
    shift_any_t4, mwu_any_vs_no$p_value, mwu_core_vs_no$p_value,
    mwu_ext7_vs_no$p_value, mwu_wo_core_no$p_value,
    ena_df$discordant_pct[ena_df$ena_accession == "ERR12330450"],
    100 * ena_df$discordant_pairs[ena_df$ena_accession == "ERR12330450"] /
      (ena_df$concordant_pairs[ena_df$ena_accession == "ERR12330450"] +
         ena_df$discordant_pairs[ena_df$ena_accession == "ERR12330450"]),
    alb_plasmid$discordant_pct,
    100 * (1 - intact_doubly_frac),
    cov_df$cov_x[1], cov_df$cov_x[2],
    cov_df$cov_x[3], cov_df$cov_x[4],
    n_total_bsmbi,
    bsmbi_sum$concordant_median[bsmbi_sum$bsmbi_class == "site_in_spacer1_only"],
    bsmbi_sum$chim_spacer2_median[bsmbi_sum$bsmbi_class == "site_in_spacer1_only"],
    p_sp1_chim2
  )
)

readr::write_csv(summary_stats, file.path(path_tables, "fig03_summary_stats.csv"))
message("Figure 3 generation complete: ", composite_pdf)
