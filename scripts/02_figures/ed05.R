#!/usr/bin/env Rscript
# Extended Data Figure 5: ComplexHeatmap beyond-rubric discovery matrix (19 x 37)
# Standardized to 7.20 in width, Helvetica Bold 9 pt tags, Helvetica Plain 6 pt body,
# 0.25 pt lines, Figure 4a labels, and canonical 4-model top strip from R/colors.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(grid)
  library(ComplexHeatmap)
})

source(file.path("config", "paths.R"))
source(file.path("R", "colors.R"))
source(file.path("R", "theme.R"))

set.seed(GLOBAL_SEED)

out_dir <- ensure_dir(file.path(path_figures, "Extended_Data_5"))

TAG_SIZE  <- 9.0
TEXT_SIZE <- 6.0

disc_meta <- require_file(file.path(path_sd_benchmark, "beyond_rubric_discoveries_N36.csv")) %>%
  readr::read_csv(show_col_types = FALSE, progress = FALSE)
sc37 <- require_file(file.path(path_sd_benchmark, "scored_rubric_all37.csv")) %>%
  readr::read_csv(show_col_types = FALSE, progress = FALSE)

require_columns(
  disc_meta,
  c("discovery_key", "discovery_label", "category", "n_runs_N36", "pct_of_N36"),
  "beyond_rubric_discoveries_N36.csv"
)
require_columns(
  sc37,
  c("actual_ceo_model", "condition", "replicate", "run_path", "in_primary_N36",
    "causal_node_score_sum", "n_fp_triggered", "non_chain_discoveries"),
  "scored_rubric_all37.csv"
)

# High-level Nature labels synced verbatim with Figure 4a
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

domain_map <- c(
  "BsmBI_strand_and_junction_bug"            = "Design",
  "Code_repo_and_ENA_defects"                = "Design",
  "Table13_redundancy_and_Fig4f_205_vs_206"  = "Design",
  "DR_A7T_substitution_and_predictR_mismatch"= "Design",
  "In_silico_rescue_tradeoff"                = "Drivers",
  "RNAfold_efficacy_null"                    = "Drivers",
  "Target_expression_AE_gradient"            = "Drivers",
  "Target_region_3UTR_vs_CDS"                = "Drivers",
  "Subcellular_nuclear_localization_NLS"     = "Drivers",
  "NT_baseline_drift_and_unscored_RRA"       = "Controls",
  "Called_lncRNA_hits_unexpressed"           = "Controls",
  "Albarossa_NE_unexpressed"                 = "Controls",
  "Background_lncRNA_library_unexpressed"    = "Controls",
  "CRISPRi_benchmark_reversal"               = "Controls",
  "Target_RNA_collateral_masked_by_silent_NE"= "Controls",
  "RRA_2of3_cliff_and_power"                 = "Statistics",
  "BH_pool_contamination_206_to_51"          = "Statistics",
  "BsmBI_chronometer_precloning_proof"       = "Forensics",
  "In_cell_RT_deletion_and_chimera_killing"  = "Forensics"
)

domain_levels <- c("Design", "Drivers", "Controls", "Statistics", "Forensics")

disc_ord <- disc_meta %>%
  dplyr::mutate(
    short_label = unname(short_labels[discovery_key]),
    domain      = factor(unname(domain_map[discovery_key]), levels = domain_levels)
  ) %>%
  dplyr::arrange(domain, dplyr::desc(n_runs_N36), discovery_key)

# Canonical model slice order matching Figure 1, 2, 4 & ED2-ED4
model_levels <- c(
  "Claude Opus 5.0*",
  "Claude Sonnet 5.5",
  "Gemini 4 Hydrogen",
  "Gemini 4 Neon",
  "*"
)
cond_col_levels <- c("cold", "cold_lit", "seeded", "seeded_lit")

runs_ord <- sc37 %>%
  dplyr::mutate(
    in_n36 = tolower(as.character(in_primary_N36)) == "true",
    model_slice = dplyr::case_when(
      !in_n36 ~ "*",
      actual_ceo_model == "Claude Opus 5.0" ~ "Claude Opus 5.0*",
      TRUE ~ actual_ceo_model
    ),
    model_slice = factor(model_slice, levels = model_levels),
    cond_ord    = factor(condition, levels = cond_col_levels),
    framing     = factor(dplyr::if_else(condition %in% c("cold", "cold_lit"), "Cold", "Seeded"),
                         levels = c("Cold", "Seeded")),
    lit_arm     = factor(dplyr::if_else(condition %in% c("cold_lit", "seeded_lit"), "With literature", "Without literature"),
                         levels = c("Without literature", "With literature")),
    rep_idx     = sub("run_0", "", replicate),
    col_lbl     = dplyr::if_else(
      in_n36,
      paste0(ifelse(framing == "Cold", "Cold ", "Seeded "), ifelse(lit_arm == "With literature", "lit ", ""), rep_idx),
      "Neon lit 1*"
    ),
    disc_vec = strsplit(tidyr::replace_na(non_chain_discoveries, ""), ";")
  ) %>%
  dplyr::arrange(model_slice, cond_ord, replicate)

# Build 19 x 37 matrix
mat <- matrix("Not reported", nrow = nrow(disc_ord), ncol = nrow(runs_ord))
rownames(mat) <- disc_ord$short_label
colnames(mat) <- runs_ord$col_lbl

for (j in seq_len(nrow(runs_ord))) {
  found_keys <- trimws(runs_ord$disc_vec[[j]])
  is_extra <- !runs_ord$in_n36[j]
  for (i in seq_len(nrow(disc_ord))) {
    k <- disc_ord$discovery_key[i]
    if (k %in% found_keys) {
      if (is_extra || k == "Target_RNA_collateral_masked_by_silent_NE") {
        mat[i, j] <- "Collateral revision / Extra*"
      } else {
        mat[i, j] <- "Found"
      }
    }
  }
}

cell_cols <- c(
  "Not reported"                 = col_stripe,
  "Found"                        = pal_blue,
  "Collateral revision / Extra*" = pal_teal
)

model_top_cols <- c(
  "Claude Opus 5.0*"  = unname(model_colors["Claude Opus 5.0*"]),
  "Claude Sonnet 5.5" = unname(model_colors["Claude Sonnet 5.5"]),
  "Gemini 4 Hydrogen" = unname(model_colors["Gemini 4 Hydrogen"]),
  "Gemini 4 Neon"     = unname(model_colors["Gemini 4 Neon"]),
  "*"                 = pal_med_gray
)

top_ha <- ComplexHeatmap::HeatmapAnnotation(
  Model      = runs_ord$model_slice,
  Framing    = runs_ord$framing,
  Literature = runs_ord$lit_arm,
  col = list(
    Model      = model_top_cols,
    Framing    = c("Cold" = col_dark, "Seeded" = col_bar_gray),
    Literature = c("Without literature" = col_stripe, "With literature" = pal_teal_light)
  ),
  gp = grid::gpar(col = "white", lwd = 0.5),
  simple_anno_size = grid::unit(2.3, "mm"),
  gap = grid::unit(0.7, "mm"),
  annotation_name_side = "left",
  annotation_name_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  show_legend = c(Model = FALSE, Framing = TRUE, Literature = TRUE),
  annotation_legend_param = list(
    Framing = list(
      title = "Framing",
      title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TEXT_SIZE),
      labels_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
      grid_height = grid::unit(2.1, "mm"),
      grid_width  = grid::unit(2.1, "mm"),
      nrow = 1
    ),
    Literature = list(
      title = "Literature",
      title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TEXT_SIZE),
      labels_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
      grid_height = grid::unit(2.1, "mm"),
      grid_width  = grid::unit(2.1, "mm"),
      nrow = 1
    )
  )
)

pct_vec  <- as.numeric(disc_ord$pct_of_N36)
bar_cols <- dplyr::if_else(
  disc_ord$discovery_key == "Target_RNA_collateral_masked_by_silent_NE",
  pal_teal,
  pal_blue
)
n_lbl <- sprintf("%d/36 (%.0f%%)", disc_ord$n_runs_N36, pct_vec)

right_ha <- ComplexHeatmap::rowAnnotation(
  `Runs (%)` = ComplexHeatmap::anno_barplot(
    pct_vec,
    ylim = c(0, 108),
    bar_width = 0.68,
    border = FALSE,
    gp = grid::gpar(fill = bar_cols, col = NA),
    axis_param = list(
      side = "top",
      at = c(0, 50, 100),
      labels = c("0", "50", "100"),
      labels_rot = 0,
      gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE, lwd = GRID_LWD_025)
    ),
    width = grid::unit(14, "mm")
  ),
  ` ` = ComplexHeatmap::anno_text(
    n_lbl,
    gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE, col = "black"),
    just = "left",
    location = 0.05,
    width = grid::unit(12, "mm")
  ),
  annotation_name_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  annotation_name_side = "top",
  gap = grid::unit(1.0, "mm")
)

ht_disc <- ComplexHeatmap::Heatmap(
  mat,
  name = "Discovery status",
  col = cell_cols,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_split = disc_ord$domain,
  row_title_rot = 0,
  row_title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  row_gap = grid::unit(1.8, "mm"),
  row_names_side = "left",
  row_names_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  row_names_max_width = grid::unit(68, "mm"),
  column_split = runs_ord$model_slice,
  column_title = c("Claude\nOpus 5.0*", "Claude\nSonnet 5.5", "Gemini 4\nHydrogen", "Gemini 4\nNeon", "*"),
  column_title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  column_gap = grid::unit(1.8, "mm"),
  column_names_side = "bottom",
  column_names_rot = 90,
  column_names_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = 5.2),
  top_annotation = top_ha,
  right_annotation = right_ha,
  rect_gp = grid::gpar(col = "white", lwd = 0.6),
  width = grid::unit(70, "mm"),
  heatmap_legend_param = list(
    title = "Discovery",
    at = c("Found", "Collateral revision / Extra*", "Not reported"),
    labels = c("Found (N = 36)", "Collateral / Extra*", "Not reported"),
    title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TEXT_SIZE),
    labels_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
    grid_height = grid::unit(2.1, "mm"),
    grid_width  = grid::unit(2.1, "mm"),
    nrow = 1
  )
)

draw_ed5 <- function(pdf_path) {
  grDevices::pdf(pdf_path, width = 7.20, height = 5.95, family = "Helvetica", encoding = "WinAnsi.enc", useDingbats = FALSE)
  ComplexHeatmap::draw(
    ht_disc,
    heatmap_legend_side = "bottom",
    annotation_legend_side = "bottom",
    merge_legends = TRUE,
    padding = grid::unit(c(7, 2.5, 8, 2.5), "mm")
  )
  grid::grid.text(
    "a",
    x = grid::unit(2.5, "mm"), y = grid::unit(1, "npc") - grid::unit(2.0, "mm"),
    just = c("left", "top"),
    gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TAG_SIZE)
  )
  grid::grid.text(
    "*Assigned Claude Opus 5.5 (fallback to Claude Opus 5.0). Extra* denotes sensitivity run Gemini 4 Neon Seeded, literature (1).",
    x = grid::unit(2.5, "mm"), y = grid::unit(2.0, "mm"),
    just = c("left", "bottom"),
    gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE, col = col_dark)
  )
  grDevices::dev.off()
  save_panel_png(pdf_path, dpi = 300L)
}

draw_ed5(file.path(out_dir, "ed5a_discovery_model_matrix.pdf"))
draw_ed5(file.path(out_dir, "Extended_Data_5_composite.pdf"))

message("Extended Data Fig. 5 (ComplexHeatmap) complete: ", out_dir)
