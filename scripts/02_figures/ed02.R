#!/usr/bin/env Rscript
# Extended Data Figure 2: ComplexHeatmap N = 36 (+1 extra sensitivity run) master rubric scorecard
# Standardized to 7.20 in width, Helvetica Bold 9 pt tags, Helvetica Plain 6 pt body,
# 0.25 pt lines, and canonical 4-model colors from R/colors.R.

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

out_dir <- ensure_dir(file.path(path_figures, "Extended_Data_2"))

TAG_SIZE  <- 9.0
TEXT_SIZE <- 6.0
CELL_SIZE <- 5.2

sc37 <- require_file(file.path(path_sd_benchmark, "scored_rubric_all37.csv")) %>%
  readr::read_csv(show_col_types = FALSE, progress = FALSE)

require_columns(
  sc37,
  c(
    "model_slug", "actual_ceo_model", "condition", "replicate", "run_path", "in_primary_N36",
    paste0("N", 1:7, "_score"), paste0("N", 1:7, "_tier"),
    paste0("FP", 1:7, "_status"), "causal_node_score_sum", "n_fp_triggered"
  ),
  "scored_rubric_all37.csv"
)

# Canonical model order: Opus -> Sonnet -> Hydrogen -> Neon -> Extra*
model_levels <- c(
  "Claude Opus 5.0*",
  "Claude Sonnet 5.5",
  "Gemini 4 Hydrogen",
  "Gemini 4 Neon",
  "Extra*"
)
cond_levels <- c("seeded", "cold", "seeded_lit", "cold_lit")
cond_pretty <- c(
  "seeded"     = "Seeded",
  "cold"       = "Cold",
  "seeded_lit" = "Seeded, literature",
  "cold_lit"   = "Cold, literature"
)

runs_ord <- sc37 %>%
  dplyr::mutate(
    in_n36 = tolower(as.character(in_primary_N36)) == "true",
    model_slice = dplyr::case_when(
      !in_n36 ~ "Extra*",
      actual_ceo_model == "Claude Opus 5.0" ~ "Claude Opus 5.0*",
      TRUE ~ actual_ceo_model
    ),
    model_slice = factor(model_slice, levels = model_levels),
    cond_ord    = factor(condition, levels = cond_levels),
    arm_label   = factor(unname(cond_pretty[condition]), levels = unname(cond_pretty)),
    rep_idx     = sub("run_0", "", replicate),
    row_lbl     = dplyr::if_else(
      in_n36,
      paste0(arm_label, " (", rep_idx, ")"),
      paste0("Gemini 4 Neon (", rep_idx, "*)")
    )
  ) %>%
  dplyr::arrange(model_slice, cond_ord, replicate)

# Node matrix (37 x 7)
node_mat <- matrix("Not recovered", nrow = nrow(runs_ord), ncol = 7L)
colnames(node_mat) <- paste0("N", 1:7)
rownames(node_mat) <- runs_ord$row_lbl

for (j in 1:7) {
  sc_col <- runs_ord[[paste0("N", j, "_score")]]
  tr_col <- runs_ord[[paste0("N", j, "_tier")]]
  node_mat[, j] <- dplyr::case_when(
    sc_col == 1 & tr_col == "E1" ~ "E1",
    sc_col == 1 & tr_col == "E2" ~ "E2",
    TRUE ~ "Not recovered"
  )
}

# Control-trap matrix (37 x 7)
fp_mat <- matrix("Avoided", nrow = nrow(runs_ord), ncol = 7L)
colnames(fp_mat) <- paste0("FP", 1:7)
rownames(fp_mat) <- runs_ord$row_lbl

for (j in 1:7) {
  st_col <- runs_ord[[paste0("FP", j, "_status")]]
  fp_name <- paste0("FP", j)
  fp_mat[, j] <- dplyr::case_when(
    st_col == "Clean"                 ~ "Avoided",
    st_col == "Caught_in_Discard_Log" ~ "Refuted in QC",
    st_col == "Refined_by_Data"       ~ "Revised (FP3)",
    st_col == "Triggered_in_Final" & fp_name == "FP7" ~ "Flagged (FP7)",
    TRUE ~ "Avoided"
  )
}

# Unified palettes matching Figure 2
node_cols <- c("E1" = pal_blue, "E2" = pal_blue_light, "Not recovered" = col_stripe)
fp_cols   <- c(
  "Avoided"       = col_stripe,
  "Refuted in QC" = col_qc_gray,
  "Revised (FP3)" = pal_teal,
  "Flagged (FP7)" = pal_coral
)
arm_cols <- c(
  "Seeded"             = col_bar_gray,
  "Cold"               = col_dark,
  "Seeded, literature" = pal_blue_pale,
  "Cold, literature"   = pal_blue_light
)
model_strip_cols <- c(
  "Claude Opus 5.0*"  = unname(model_colors["Claude Opus 5.0*"]),
  "Claude Sonnet 5.5" = unname(model_colors["Claude Sonnet 5.5"]),
  "Gemini 4 Hydrogen" = unname(model_colors["Gemini 4 Hydrogen"]),
  "Gemini 4 Neon"     = unname(model_colors["Gemini 4 Neon"]),
  "Extra*"            = pal_med_gray
)

left_ha <- ComplexHeatmap::rowAnnotation(
  Model = runs_ord$model_slice,
  Run = ComplexHeatmap::anno_text(
    runs_ord$row_lbl,
    gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE, col = "black"),
    just = "right",
    location = 0.96,
    width = grid::unit(23.5, "mm")
  ),
  Framing = runs_ord$arm_label,
  col = list(Model = model_strip_cols, Framing = arm_cols),
  width = grid::unit(28.5, "mm"),
  simple_anno_size = grid::unit(2.4, "mm"),
  gap = grid::unit(1.0, "mm"),
  annotation_name_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  annotation_name_side = "top",
  annotation_name_rot = 0,
  show_legend = c(Model = FALSE, Framing = TRUE),
  annotation_legend_param = list(
    Framing = list(
      title = "Experimental arm",
      title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TEXT_SIZE),
      labels_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
      grid_height = grid::unit(2.2, "mm"),
      grid_width  = grid::unit(2.2, "mm"),
      nrow = 1
    )
  )
)

score_vec <- as.numeric(runs_ord$causal_node_score_sum)
score_txt <- sprintf("%d/7", as.integer(score_vec))
fp16_txt  <- dplyr::if_else(runs_ord$in_n36, "0/6", "0/6*")

right_ha_nodes <- ComplexHeatmap::rowAnnotation(
  Recall = ComplexHeatmap::anno_barplot(
    score_vec,
    ylim = c(0, 7.6),
    bar_width = 0.70,
    border = FALSE,
    gp = grid::gpar(
      fill = dplyr::if_else(score_vec == 7, pal_blue, col_qc_gray),
      col = NA
    ),
    axis_param = list(
      side = "top",
      at = c(0, 7),
      labels = c("0", "7"),
      gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE, lwd = GRID_LWD_025)
    ),
    width = grid::unit(10.5, "mm")
  ),
  ` ` = ComplexHeatmap::anno_text(
    score_txt,
    gp = grid::gpar(
      fontfamily = "Helvetica",
      fontface = "plain",
      fontsize = TEXT_SIZE,
      col = dplyr::if_else(score_vec == 7, pal_blue_dark, "black")
    ),
    just = "left",
    location = 0.08,
    width = grid::unit(5.8, "mm")
  ),
  annotation_name_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  annotation_name_side = "top",
  gap = grid::unit(1.0, "mm")
)

right_ha_fp <- ComplexHeatmap::rowAnnotation(
  `FP1-6` = ComplexHeatmap::anno_text(
    fp16_txt,
    gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE, col = pal_teal_dark),
    just = "center",
    location = 0.5,
    width = grid::unit(7.5, "mm")
  ),
  annotation_name_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  annotation_name_side = "top"
)

ht_nodes <- ComplexHeatmap::Heatmap(
  node_mat,
  name = "Evidence tier",
  col = node_cols,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_split = runs_ord$model_slice,
  row_title_rot = 0,
  row_title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  row_gap = grid::unit(2.0, "mm"),
  show_row_names = FALSE,
  column_names_side = "top",
  column_names_rot = 0,
  column_names_centered = TRUE,
  column_names_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  column_title = "Causal-chain nodes (N1-N7)",
  column_title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  rect_gp = grid::gpar(col = "white", lwd = 0.7),
  width = grid::unit(34, "mm"),
  cell_fun = function(j, i, x, y, w, h, fill) {
    v <- node_mat[i, j]
    lbl <- if (v == "Not recovered") "-" else v
    col_t <- if (v == "E1") "white" else if (v == "E2") "black" else "#878787"
    grid::grid.text(lbl, x, y, gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = CELL_SIZE, col = col_t))
  },
  heatmap_legend_param = list(
    title = "Evidence tier",
    at = c("E1", "E2", "Not recovered"),
    labels = c("Primary raw data (E1)", "Supplementary tables (E2)", "Not recovered"),
    title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TEXT_SIZE),
    labels_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
    grid_height = grid::unit(2.2, "mm"),
    grid_width  = grid::unit(2.2, "mm"),
    nrow = 1
  )
)

ht_fp <- ComplexHeatmap::Heatmap(
  fp_mat,
  name = "Trap outcome",
  col = fp_cols,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_split = runs_ord$model_slice,
  row_gap = grid::unit(2.0, "mm"),
  show_row_names = FALSE,
  column_names_side = "top",
  column_names_rot = 0,
  column_names_centered = TRUE,
  column_names_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  column_title = "Control traps (0/36 FP1-FP6 triggers)",
  column_title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
  rect_gp = grid::gpar(col = "white", lwd = 0.7),
  width = grid::unit(34, "mm"),
  cell_fun = function(j, i, x, y, w, h, fill) {
    v <- fp_mat[i, j]
    lbl <- dplyr::case_when(
      v == "Refuted in QC" ~ "QC",
      v == "Revised (FP3)" ~ "Rev",
      v == "Flagged (FP7)" ~ "Flag",
      TRUE ~ ""
    )
    if (nzchar(lbl)) {
      col_t <- if (v == "Flagged (FP7)") "white" else "black"
      grid::grid.text(lbl, x, y, gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = CELL_SIZE, col = col_t))
    }
  },
  heatmap_legend_param = list(
    title = "Control trap",
    at = c("Avoided", "Refuted in QC", "Revised (FP3)", "Flagged (FP7)"),
    labels = c("Avoided", "Refuted in QC", "Revised (FP3)", "Flagged (FP7)"),
    title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TEXT_SIZE),
    labels_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
    grid_height = grid::unit(2.2, "mm"),
    grid_width  = grid::unit(2.2, "mm"),
    nrow = 1
  )
)

# Standalone Panel a
pdf_a <- file.path(out_dir, "ed2a_per_run_nodes_36.pdf")
grDevices::pdf(pdf_a, width = 5.40, height = 7.40, family = "Helvetica", encoding = "WinAnsi.enc", useDingbats = FALSE)
ComplexHeatmap::draw(
  left_ha + ht_nodes + right_ha_nodes,
  heatmap_legend_side = "bottom",
  annotation_legend_side = "bottom",
  merge_legends = TRUE,
  padding = grid::unit(c(5, 3, 7, 3), "mm")
)
grid::grid.text("a", x = grid::unit(2.5, "mm"), y = grid::unit(1, "npc") - grid::unit(2.0, "mm"),
                just = c("left", "top"), gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TAG_SIZE))
grDevices::dev.off()
save_panel_png(pdf_a, dpi = 300L)

# Standalone Panel b
pdf_b <- file.path(out_dir, "ed2b_per_run_fp_traps_36.pdf")
grDevices::pdf(pdf_b, width = 5.20, height = 7.40, family = "Helvetica", encoding = "WinAnsi.enc", useDingbats = FALSE)
ComplexHeatmap::draw(
  left_ha + ComplexHeatmap::Heatmap(
    fp_mat,
    name = "Trap outcome",
    col = fp_cols,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    row_split = runs_ord$model_slice,
    row_title_rot = 0,
    row_title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
    row_gap = grid::unit(2.0, "mm"),
    show_row_names = FALSE,
    column_names_side = "top",
    column_names_rot = 0,
    column_names_centered = TRUE,
    column_names_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
    column_title = "Control traps (0/36 FP1-FP6 triggers)",
    column_title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
    rect_gp = grid::gpar(col = "white", lwd = 0.7),
    width = grid::unit(34, "mm"),
    cell_fun = function(j, i, x, y, w, h, fill) {
      v <- fp_mat[i, j]
      lbl <- dplyr::case_when(
        v == "Refuted in QC" ~ "QC",
        v == "Revised (FP3)" ~ "Rev",
        v == "Flagged (FP7)" ~ "Flag",
        TRUE ~ ""
      )
      if (nzchar(lbl)) {
        col_t <- if (v == "Flagged (FP7)") "white" else "black"
        grid::grid.text(lbl, x, y, gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = CELL_SIZE, col = col_t))
      }
    },
    heatmap_legend_param = list(
      title = "Control trap",
      at = c("Avoided", "Refuted in QC", "Revised (FP3)", "Flagged (FP7)"),
      labels = c("Avoided", "Refuted in QC", "Revised (FP3)", "Flagged (FP7)"),
      title_gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TEXT_SIZE),
      labels_gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE),
      grid_height = grid::unit(2.2, "mm"),
      grid_width  = grid::unit(2.2, "mm"),
      nrow = 1
    )
  ) + right_ha_fp,
  heatmap_legend_side = "bottom",
  annotation_legend_side = "bottom",
  merge_legends = TRUE,
  padding = grid::unit(c(5, 3, 7, 3), "mm")
)
grid::grid.text("b", x = grid::unit(2.5, "mm"), y = grid::unit(1, "npc") - grid::unit(2.0, "mm"),
                just = c("left", "top"), gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TAG_SIZE))
grDevices::dev.off()
save_panel_png(pdf_b, dpi = 300L)

# Composite Extended Data Fig. 2 (7.20 in Nature width)
pdf_comp <- file.path(out_dir, "Extended_Data_2_composite.pdf")
grDevices::pdf(pdf_comp, width = 7.20, height = 7.50, family = "Helvetica", encoding = "WinAnsi.enc", useDingbats = FALSE)
ht_list_comp <- left_ha + ht_nodes + right_ha_nodes + ht_fp + right_ha_fp
ComplexHeatmap::draw(
  ht_list_comp,
  ht_gap = grid::unit(c(1.4, 1.4, 4.5, 1.4), "mm"),
  heatmap_legend_side = "bottom",
  annotation_legend_side = "bottom",
  merge_legends = TRUE,
  padding = grid::unit(c(7, 3, 7, 3), "mm")
)
grid::grid.text("a", x = grid::unit(2.5, "mm"), y = grid::unit(1, "npc") - grid::unit(2.0, "mm"),
                just = c("left", "top"), gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TAG_SIZE))
grid::grid.text("b", x = grid::unit(0.64, "npc"), y = grid::unit(1, "npc") - grid::unit(2.0, "mm"),
                just = c("left", "top"), gp = grid::gpar(fontfamily = "Helvetica", fontface = "bold", fontsize = TAG_SIZE))
grid::grid.text(
  "*Claude Opus 5.0 via bio-safety fallback (n = 6). Extra* denotes sensitivity run Gemini 4 Neon Seeded, literature (1) excluded from N = 36.",
  x = grid::unit(2.5, "mm"), y = grid::unit(2.0, "mm"),
  just = c("left", "bottom"), gp = grid::gpar(fontfamily = "Helvetica", fontface = "plain", fontsize = TEXT_SIZE, col = col_dark)
)
grDevices::dev.off()
save_panel_png(pdf_comp, dpi = 300L)

message("Extended Data Fig. 2 (ComplexHeatmap) complete: ", out_dir)
