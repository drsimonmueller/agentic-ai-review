#!/usr/bin/env Rscript
# Build publication-grade Table 1 purely in ggplot2 (vector PDF + 300 DPI PNG)
# using Helvetica, crisp Nature rules, and only the 9-color palette + lighter tints.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tibble)
})

repo_root <- Sys.getenv("AGENTIC_REPO_ROOT", unset = ".")
source(file.path(repo_root, "config", "paths.R"))
source(file.path(repo_root, "R", "colors.R"))
source(file.path(repo_root, "R", "theme.R"))
source(file.path(repo_root, "R", "stats_utils.R"))
source(file.path(repo_root, "R", "plot_utils.R"))

ensure_dir(path_tables)
set.seed(GLOBAL_SEED)

# ---------------------------------------------------------------------------
# 1. Load live N = 36 data & summary tables
# ---------------------------------------------------------------------------
p_sc36  <- require_file(file.path(path_sd_benchmark, "scored_rubric_N36.csv"))
p_pn    <- require_file(file.path(path_tables, "per_node_and_fp_summary.csv"))
p_bsum  <- require_file(file.path(path_tables, "benchmark_summary_stats.csv"))

sc36 <- read_csv(p_sc36, show_col_types = FALSE)
pn   <- read_csv(p_pn, show_col_types = FALSE)
bsum <- read_csv(p_bsum, show_col_types = FALSE)

n_runs   <- nrow(sc36)
n_models <- dplyr::n_distinct(sc36$actual_ceo_model)

# Extract live summary strings directly from scored_rubric_N36 & per_node_and_fp_summary
get_node_rec <- function(id) {
  r <- pn %>% filter(item_id == id)
  sprintf("%d/%d (%.1f%%)", r$n_recovered_or_triggered, r$n_runs, r$pct_recovered_or_triggered)
}
get_node_tier <- function(id) {
  r <- pn %>% filter(item_id == id)
  r$tier_summary
}
get_fp_row <- function(id) {
  pn %>% filter(item_id == id)
}

fp1 <- get_fp_row("FP1")
fp2 <- get_fp_row("FP2")
fp3 <- get_fp_row("FP3")
fp4 <- get_fp_row("FP4")
fp5 <- get_fp_row("FP5")
fp6 <- get_fp_row("FP6")
fp7 <- get_fp_row("FP7")

fp4_6_qc_min <- min(fp4$Caught_in_Discard_Log_count, fp5$Caught_in_Discard_Log_count, fp6$Caught_in_Discard_Log_count)
fp4_6_qc_max <- max(fp4$Caught_in_Discard_Log_count, fp5$Caught_in_Discard_Log_count, fp6$Caught_in_Discard_Log_count)

disc_row <- bsum %>% filter(section == "tier4_falsification", metric == "discard_rate_pct_all36")
disc_range_str <- sprintf("%.0f\u2013%.0f%%", disc_row$min, disc_row$max)

# Neutral light gray (#F2F3F3) for gray-first Nature structural banners
pal_gray_banner <- "#F2F3F3"
pal_text_muted  <- grDevices::colorRampPalette(c("#000000", pal_med_gray))(5)[3]

# ---------------------------------------------------------------------------
# 2. Construct table rows (from top to bottom)
# ---------------------------------------------------------------------------
rows_spec <- tribble(
  ~row_type, ~group_bg, ~id, ~category, ~criterion, ~recovery, ~evidence, ~outcome, ~pill_fill, ~pill_text,
  "group", pal_gray_banner, "Layer 1 \u00b7 Observed phenotype", "", "", "", "", "", NA_character_, NA_character_,
  "data", NA_character_, "N1", "Sensitivity gap",
    "AE PCG recall 30.0\u201368.7% (FDR < 0.25) vs. 95.6\u201399.3% DepMap Cas9",
    get_node_rec("N1"), get_node_tier("N1"), "Universal entry-point check", NA_character_, NA_character_,
  "data", NA_character_, "N2", "Sparse lncRNA hits",
    "26\u201377 essential lncRNAs/line (206 union) linked to sensitivity",
    get_node_rec("N2"), get_node_tier("N2"), "Tied to AE recall gap", NA_character_, NA_character_,

  "group", pal_gray_banner, "Layer 2 \u00b7 Mechanistic causal chain (H1 & H2)", "", "", "", "", "", NA_character_, NA_character_,
  "data", NA_character_, "N3", "H1 \u00b7 Pol III motif",
    "Internal Pol III T>=4 terminators in 15,082 / 78,125 (19.3%) arrays",
    get_node_rec("N3"), paste0("E2+E3 (", pn$n_recovered_or_triggered[pn$item_id == "N3"], ")"), "core23 active; 3' ext7 null", NA_character_, NA_character_,
  "data", NA_character_, "N4", "H1 \u00b7 Depletion shift",
    "T>=4 AE depletion attenuation (+0.85 log2FC, P = 4.1e-7; null woCasRx)",
    get_node_rec("N4"), get_node_tier("N4"), "Confirmed in all 6 cell lines", NA_character_, NA_character_,
  "data", NA_character_, "N5", "H2 \u00b7 Recombination",
    "42.9% discordant gRNA1\u2013gRNA2 pairs across 36-nt DR (ERR12330401)",
    get_node_rec("N5"), get_node_tier("N5"), "11.4\u00d7 vs. ControlLibrary (3.7%)", NA_character_, NA_character_,
  "data", NA_character_, "N6", "H2 \u00b7 Coverage drop",
    "Effective intact-array coverage drop (301\u00d7 to ~172\u00d7 / 122\u2013134\u00d7)",
    get_node_rec("N6"), get_node_tier("N6"), "Primary quantitative bottleneck", pal_orange_pale, pal_orange_dark,
  "data", NA_character_, "N7", "H1+H2 \u00b7 RRA power",
    "Diluted coverage + inert T>=4 weaken RRA power (FN, not FP)",
    get_node_rec("N7"), get_node_tier("N7"), "120 live MAGeCK simulations", NA_character_, NA_character_,

  "group", pal_gray_banner, "Pre-registered false-positive traps (FP1\u2013FP7)", "", "", "", "", "", NA_character_, NA_character_,
  "data", NA_character_, "FP1\u2013FP2", "Counts / NT guides",
    "Counting pipeline bug (r = 0.996) or NT off-targets (0/600 align)",
    sprintf("%d/%d triggered", fp1$Triggered_in_Final_count + fp2$Triggered_in_Final_count, n_runs),
    sprintf("%d / %d QC", fp1$Caught_in_Discard_Log_count, fp2$Caught_in_Discard_Log_count),
    "Counts match (r = 0.996\u20131.000)", pal_green_pale, pal_green_dark,
  "data", NA_character_, "FP3", "Collateral (revised)",
    "CasRx collateral scaling with gRNA production (r = -0.091)",
    sprintf("%d/%d trig. \u00b7 %d/%d +E2", fp3$Triggered_in_Final_count, n_runs, fp3$Refined_by_Data_count, n_runs),
    sprintf("%d QC \u00b7 %d +E2", fp3$Caught_in_Discard_Log_count, fp3$Refined_by_Data_count),
    "Silent NE masked collateral", pal_teal_pale, pal_teal_dark,
  "data", NA_character_, "FP4\u2013FP6", "GC / Skew / Spec.",
    "GC bias, plasmid skew/T1\u2013T3, or NE specificity failure (0/300 NE)",
    sprintf("%d/%d triggered", fp4$Triggered_in_Final_count + fp5$Triggered_in_Final_count + fp6$Triggered_in_Final_count, n_runs),
    sprintf("%d\u2013%d QC", fp4_6_qc_min, fp4_6_qc_max),
    paste0("Tier 4 QC discarded ", disc_range_str), pal_green_pale, pal_green_dark,
  "data", NA_character_, "FP7", "Editorial calibration",
    "Dismissing all 206 lncRNA hits as artefacts despite 0/300 NE",
    sprintf("%d/%d flagged", fp7$Triggered_in_Final_count, n_runs),
    sprintf("%d QC-refuted", fp7$Caught_in_Discard_Log_count),
    "BH-pool & silent-hit E2 data", pal_coral_pale, pal_coral_dark
)

# Assign vertical geometry (y goes up, so top of table has higher y)
header_h <- 0.85
group_h  <- 0.62
data_h   <- 0.72

rows_spec <- rows_spec %>%
  mutate(
    row_h = if_else(row_type == "group", group_h, data_h)
  )

# Compute y_top and y_bottom for each row below header
total_body_h <- sum(rows_spec$row_h)
y_header_top <- total_body_h + header_h
y_header_bot <- total_body_h

y_curr <- y_header_bot
y_top_vec <- numeric(nrow(rows_spec))
y_bot_vec <- numeric(nrow(rows_spec))
for (i in seq_len(nrow(rows_spec))) {
  y_top_vec[i] <- y_curr
  y_bot_vec[i] <- y_curr - rows_spec$row_h[i]
  y_curr <- y_bot_vec[i]
}
rows_spec <- rows_spec %>%
  mutate(
    y_top = y_top_vec,
    y_bot = y_bot_vec,
    y_mid = (y_top + y_bot) / 2
  )

# Column layout (0 to 100 coordinate space)
col_bounds <- list(
  id        = c(0.0,  7.5),
  category  = c(7.5,  22.5),
  criterion = c(22.5, 57.2),
  recovery  = c(57.2, 72.8),
  evidence  = c(72.8, 83.0),
  outcome   = c(83.0, 100.0)
)

headers_df <- tibble(
  x = c(0.7, 8.2, 23.2, 65.0, 77.9, 83.6),
  hjust = c(0, 0, 0, 0.5, 0.5, 0),
  label = c(
    "ID",
    "Layer / Category",
    "Ground-truth criterion",
    sprintf("Recovery (N = %d)", n_runs),
    "Evidence / QC",
    "Key benchmark outcome"
  )
)

group_df <- rows_spec %>% filter(row_type == "group")
data_df  <- rows_spec %>% filter(row_type == "data")
pill_df  <- data_df %>% filter(!is.na(pill_fill))

# Thin horizontal lines below each data row except the very last row (which gets bottom 1.2pt rule)
hlines_df <- data_df %>%
  filter(row_number() < n()) %>%
  select(y = y_bot)

# Group divider borders (top and bottom of each group banner)
g_lines_df <- tibble(
  y = unique(c(group_df$y_top, group_df$y_bot))
)

# Footnote & title coordinates
y_title    <- y_header_top + 0.95
y_subtitle <- y_header_top + 0.42
y_foot1    <- -0.36
y_foot2    <- -0.72
y_foot3    <- -1.08

footnote_line1 <- paste0(
  "Abbreviations: AE, always-essential; BH, Benjamini\u2013Hochberg; DR, direct repeat; ",
  "E1\u2013E3, primary-data, supplementary-table, and repository-code evidence tiers;"
)
footnote_line2 <- paste0(
  "FDR, false discovery rate; FN, false negative; FP, false positive; GC, guanine\u2013cytosine; ",
  "lncRNA, long noncoding RNA; log2FC, log2 fold change; NE, non-essential;"
)
footnote_line3 <- paste0(
  "NT, non-targeting; PCG, protein-coding gene; Pol III, RNA polymerase III; QC, quality control; ",
  "RRA, Robust Rank Aggregation. +E2 denotes empirical refinement of FP3."
)

# ---------------------------------------------------------------------------
# 3. Render pure ggplot2 Table 1
# ---------------------------------------------------------------------------
p_tbl <- ggplot() +
  # Title: bold "Table 1 |" + plain description
  annotate(
    "text", x = 0.0, y = y_title, label = "Table 1 |",
    family = "Helvetica", fontface = "bold", size = 3.6, hjust = 0, vjust = 0.5, color = "black"
  ) +
  annotate(
    "text", x = 6.5, y = y_title,
    label = "Ground-truth causal rubric, false-positive traps, and cross-provider agent recovery",
    family = "Helvetica", fontface = "plain", size = 3.6, hjust = 0, vjust = 0.5, color = "black"
  ) +
  annotate(
    "text", x = 0.0, y = y_subtitle,
    label = sprintf(
      "Strict binary scoring across N = %d finalized clean-room runs (%d executed models; 24 Base + 12 +Literature)",
      n_runs, n_models
    ),
    family = "Helvetica", fontface = "plain", size = 2.85, hjust = 0, vjust = 0.5, color = pal_text_muted
  ) +

  # Header background
  annotate(
    "rect",
    xmin = 0.0, xmax = 100.0, ymin = y_header_bot, ymax = y_header_top,
    fill = pal_light_gray, color = NA
  ) +
  # Group divider banners
  geom_rect(
    data = group_df,
    aes(xmin = 0.0, xmax = 100.0, ymin = y_bot, ymax = y_top, fill = group_bg),
    color = NA
  ) +
  scale_fill_identity() +

  # Semantic pill highlights in Recovery column
  geom_rect(
    data = pill_df,
    aes(
      xmin = col_bounds$recovery[1] + 0.4,
      xmax = col_bounds$recovery[2] - 0.4,
      ymin = y_bot + 0.08,
      ymax = y_top - 0.08,
      fill = pill_fill
    ),
    color = NA
  ) +

  # Horizontal rules: subtle row separators, group borders, crisp black header/bottom rules
  geom_segment(
    data = hlines_df,
    aes(x = 0.0, xend = 100.0, y = y, yend = y),
    color = pal_light_gray, linewidth = 0.25
  ) +
  geom_segment(
    data = g_lines_df,
    aes(x = 0.0, xend = 100.0, y = y, yend = y),
    color = pal_med_gray, linewidth = 0.28
  ) +
  # Top rule (1.2 pt)
  annotate(
    "segment", x = 0.0, xend = 100.0, y = y_header_top, yend = y_header_top,
    color = "black", linewidth = 0.56
  ) +
  # Header midrule (0.6 pt)
  annotate(
    "segment", x = 0.0, xend = 100.0, y = y_header_bot, yend = y_header_bot,
    color = "black", linewidth = 0.28
  ) +
  # Bottom rule (1.2 pt)
  annotate(
    "segment", x = 0.0, xend = 100.0, y = 0.0, yend = 0.0,
    color = "black", linewidth = 0.56
  ) +

  # Header labels (bold)
  geom_text(
    data = headers_df,
    aes(x = x, y = (y_header_top + y_header_bot) / 2, label = label, hjust = hjust),
    family = "Helvetica", fontface = "bold", size = 2.85, color = "black", vjust = 0.5
  ) +

  # Group banner labels (bold)
  geom_text(
    data = group_df,
    aes(x = 0.7, y = y_mid, label = id),
    family = "Helvetica", fontface = "bold", size = 2.75, color = "black", hjust = 0, vjust = 0.5
  ) +

  # Data columns (plain weight everywhere)
  geom_text(
    data = data_df,
    aes(x = 0.7, y = y_mid, label = id),
    family = "Helvetica", fontface = "plain", size = 2.65, color = "black", hjust = 0, vjust = 0.5
  ) +
  geom_text(
    data = data_df,
    aes(x = 8.2, y = y_mid, label = category),
    family = "Helvetica", fontface = "plain", size = 2.65, color = "black", hjust = 0, vjust = 0.5
  ) +
  geom_text(
    data = data_df,
    aes(x = 23.2, y = y_mid, label = criterion),
    family = "Helvetica", fontface = "plain", size = 2.60, color = "black", hjust = 0, vjust = 0.5
  ) +
  # Recovery column: default black text for unhighlighted rows, colored text for pill rows
  geom_text(
    data = data_df %>% filter(is.na(pill_fill)),
    aes(x = 65.0, y = y_mid, label = recovery),
    family = "Helvetica", fontface = "plain", size = 2.65, color = "black", hjust = 0.5, vjust = 0.5
  ) +
  geom_text(
    data = pill_df,
    aes(x = 65.0, y = y_mid, label = recovery, color = pill_text),
    family = "Helvetica", fontface = "plain", size = 2.65, hjust = 0.5, vjust = 0.5
  ) +
  scale_color_identity() +
  geom_text(
    data = data_df,
    aes(x = 77.9, y = y_mid, label = evidence),
    family = "Helvetica", fontface = "plain", size = 2.60, color = "black", hjust = 0.5, vjust = 0.5
  ) +
  geom_text(
    data = data_df,
    aes(x = 83.6, y = y_mid, label = outcome),
    family = "Helvetica", fontface = "plain", size = 2.60, color = "black", hjust = 0, vjust = 0.5
  ) +

  # Footnote
  annotate(
    "text", x = 0.0, y = y_foot1, label = footnote_line1,
    family = "Helvetica", fontface = "plain", size = 2.35, color = pal_text_muted, hjust = 0, vjust = 0.5
  ) +
  annotate(
    "text", x = 0.0, y = y_foot2, label = footnote_line2,
    family = "Helvetica", fontface = "plain", size = 2.35, color = pal_text_muted, hjust = 0, vjust = 0.5
  ) +
  annotate(
    "text", x = 0.0, y = y_foot3, label = footnote_line3,
    family = "Helvetica", fontface = "plain", size = 2.35, color = pal_text_muted, hjust = 0, vjust = 0.5
  ) +

  coord_cartesian(
    xlim = c(-0.5, 100.5),
    ylim = c(-1.45, y_header_top + 1.35),
    expand = FALSE,
    clip = "off"
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 6, r = 8, b = 6, l = 8, unit = "pt")
  )

out_pdf <- file.path(path_tables, "Table1_rubric_and_recovery.pdf")
save_panel_pair(p_tbl, out_pdf, width = 11.4, height = 5.6, dpi = 300L)

cat("[table01.R] Saved Table 1 PDF and 300 DPI PNG cleanly:\n")
cat("  ", out_pdf, "\n")
cat("  ", sub("\\.pdf$", ".png", out_pdf), "\n")
