#!/usr/bin/env Rscript
# Build clean Nature-style booktabs Table 1 in ggplot2 (7.20 in width,
# Helvetica 6 pt body, 0.25 pt hairlines, zero boxed pills, plain prose).

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
p_sc36 <- require_file(file.path(path_sd_benchmark, "scored_rubric_N36.csv"))
p_pn   <- require_file(file.path(path_tables, "per_node_and_fp_summary.csv"))

sc36 <- read_csv(p_sc36, show_col_types = FALSE)
pn   <- read_csv(p_pn, show_col_types = FALSE)

n_runs <- nrow(sc36)

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

# Shared typography (exact match to Figures 1-4)
TEXT_SIZE    <- 6.0
HEAD_SIZE    <- 6.2
FOOT_SIZE    <- 5.5
GG_TEXT      <- TEXT_SIZE / ggplot2::.pt
GG_HEAD      <- HEAD_SIZE / ggplot2::.pt
GG_FOOT      <- FOOT_SIZE / ggplot2::.pt

# ---------------------------------------------------------------------------
# 2. Construct clean 5-column Nature booktabs rows
# ---------------------------------------------------------------------------
rows_spec <- tribble(
  ~row_type, ~id, ~category, ~criterion, ~outcome, ~evidence, ~outcome_col, ~stripe,
  "section", "Observed deficits (Layer 1)", "", "", "", "", "black", FALSE,
  "data", "N1", "Essential-gene recall",
    "CasRx recovers 30.0-68.7% of 300 essential genes (FDR < 0.25) vs. 95.6-99.3% Cas9",
    get_node_rec("N1"), get_node_tier("N1"), "black", FALSE,
  "data", "N2", "Sparse lncRNA hits",
    "26-77 essential lncRNA hits per cell line (206 union) linked to low sensitivity",
    get_node_rec("N2"), get_node_tier("N2"), "black", TRUE,

  "section", "Mechanistic causal chain (Layer 2)", "", "", "", "", "black", FALSE,
  "data", "N3", "Pol III poly-T motif",
    "Omitted Pol III terminator filter leaves T>=4 in 15,082/78,125 (19.3%) arrays",
    get_node_rec("N3"), sprintf("E2+E3 (%d)", pn$n_recovered_or_triggered[pn$item_id == "N3"]), "black", FALSE,
  "data", "N4", "Poly-T depletion loss",
    "Core spacer T>=4 blunts essential-gene depletion (+0.85 log2FC, P = 4.1e-7)",
    get_node_rec("N4"), get_node_tier("N4"), "black", TRUE,
  "data", "N5", "Repeat recombination",
    "42.9% doubly mapped plasmid pairs recombined across 36-nt direct repeats (11.4x)",
    get_node_rec("N5"), get_node_tier("N5"), "black", FALSE,
  "data", "N6", "Coverage dilution",
    "Recombination halves intact coverage (301x to 172x; passaging 256x to 134x)",
    get_node_rec("N6"), get_node_tier("N6"), col_dark, TRUE,
  "data", "N7", "Statistical power loss",
    "Inert T>=4 arrays and coverage loss weaken RRA power (false negatives only)",
    get_node_rec("N7"), get_node_tier("N7"), "black", FALSE,

  "section", "Pre-registered false-positive control traps (FP1-FP7)", "", "", "", "", "black", FALSE,
  "data", "FP1-FP2", "Counts & off-targets",
    "Read-counting bug (counts match, r = 0.996-1.000) or non-targeting off-targets (0/600)",
    sprintf("%d/%d triggered", fp1$Triggered_in_Final_count + fp2$Triggered_in_Final_count, n_runs),
    sprintf("%d, %d QC", fp1$Caught_in_Discard_Log_count, fp2$Caught_in_Discard_Log_count),
    pal_blue_dark, FALSE,
  "data", "FP3", "Collateral cleavage",
    "CasRx collateral toxicity (0/300 silent controls; revised via expressed controls)",
    sprintf("%d/%d (%d revised)", fp3$Triggered_in_Final_count, n_runs, fp3$Refined_by_Data_count),
    sprintf("%d QC, %d rev.", fp3$Caught_in_Discard_Log_count, fp3$Refined_by_Data_count),
    pal_teal_dark, TRUE,
  "data", "FP4-FP6", "GC, skew & specificity",
    "Guide GC bias, plasmid skew/T1-T3 confound, or non-essential specificity (0/300)",
    sprintf("%d/%d triggered", fp4$Triggered_in_Final_count + fp5$Triggered_in_Final_count + fp6$Triggered_in_Final_count, n_runs),
    sprintf("%d-%d QC", fp4_6_qc_min, fp4_6_qc_max),
    pal_blue_dark, FALSE,
  "data", "FP7", "Editorial calibration",
    "Dismissing all 206 lncRNA hits (flagged via multiple-testing & unexpressed hits)",
    sprintf("%d/%d flagged", fp7$Triggered_in_Final_count, n_runs),
    sprintf("%d QC", fp7$Caught_in_Discard_Log_count),
    pal_coral_dark, TRUE
)

# Vertical geometry (y runs bottom -> top)
header_h  <- 0.95
section_h <- 0.78
data_h    <- 0.80

rows_spec <- rows_spec %>%
  mutate(row_h = if_else(row_type == "section", section_h, data_h))

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

sec_df    <- rows_spec %>% filter(row_type == "section")
data_df   <- rows_spec %>% filter(row_type == "data")
stripe_df <- data_df %>% filter(stripe)

# Subtle separator rules above sections 2 and 3 (classic booktabs breathing space)
sec_rules <- sec_df %>%
  slice(-1) %>%
  select(y = y_top)

# 5-column coordinates across 0..100 (7.20 in width)
col_x <- c(id = 0.4, cat = 7.4, crit = 22.0, out = 78.6, ev = 92.6)

headers_df <- tibble(
  x     = unname(col_x),
  hjust = c(0, 0, 0, 0.5, 0.5),
  label = c("ID", "Category", "Benchmark criterion", sprintf("Outcome (N = %d)", n_runs), "Evidence / QC")
)

footnote_line1 <- "E1, primary raw reads or external atlases; E2, supplementary tables; E3, published code; FDR, false discovery rate;"
footnote_line2 <- "Pol III, RNA polymerase III; QC, refuted in Tier 4 blinded quality control; rev., empirically revised; RRA, Robust Rank Aggregation."

# ---------------------------------------------------------------------------
# 3. Render clean Nature booktabs Table 1 (7.20 in width)
# ---------------------------------------------------------------------------
p_tbl <- ggplot() +
  # Alternating subtle #F6F6F6 row stripes on data rows (matching Fig. 2b-c / Fig. 4a)
  geom_rect(
    data = stripe_df,
    aes(xmin = 0.0, xmax = 100.0, ymin = y_bot, ymax = y_top),
    fill = col_stripe, color = NA
  ) +
  # Crisp booktabs horizontal rules (0.50 pt top/bottom, 0.25 pt mid/section rules)
  annotate("segment", x = 0.0, xend = 100.0, y = y_header_top, yend = y_header_top,
           color = "black", linewidth = LW_050) +
  annotate("segment", x = 0.0, xend = 100.0, y = y_header_bot, yend = y_header_bot,
           color = "black", linewidth = LW_025) +
  geom_segment(
    data = sec_rules,
    aes(x = 0.0, xend = 100.0, y = y, yend = y),
    color = col_divider, linewidth = LW_025
  ) +
  annotate("segment", x = 0.0, xend = 100.0, y = 0.0, yend = 0.0,
           color = "black", linewidth = LW_050) +

  # Column headers (bold 6.2 pt)
  geom_text(
    data = headers_df,
    aes(x = x, y = (y_header_top + y_header_bot) / 2, label = label, hjust = hjust),
    family = "Helvetica", fontface = "bold", size = GG_HEAD, color = "black", vjust = 0.5
  ) +

  # Section subheaders (bold 6 pt, clean left-aligned without heavy box)
  geom_text(
    data = sec_df,
    aes(x = 0.4, y = y_mid - 0.06, label = id),
    family = "Helvetica", fontface = "bold", size = GG_TEXT, color = col_dark, hjust = 0, vjust = 0.5
  ) +

  # Data columns (plain 6 pt Helvetica)
  geom_text(
    data = data_df,
    aes(x = col_x["id"], y = y_mid, label = id),
    family = "Helvetica", fontface = "plain", size = GG_TEXT, color = "black", hjust = 0, vjust = 0.5
  ) +
  geom_text(
    data = data_df,
    aes(x = col_x["cat"], y = y_mid, label = category),
    family = "Helvetica", fontface = "plain", size = GG_TEXT, color = "black", hjust = 0, vjust = 0.5
  ) +
  geom_text(
    data = data_df,
    aes(x = col_x["crit"], y = y_mid, label = criterion),
    family = "Helvetica", fontface = "plain", size = GG_TEXT, color = "black", hjust = 0, vjust = 0.5
  ) +
  geom_text(
    data = data_df,
    aes(x = col_x["out"], y = y_mid, label = outcome, color = outcome_col),
    family = "Helvetica", fontface = "plain", size = GG_TEXT, hjust = 0.5, vjust = 0.5
  ) +
  scale_color_identity() +
  geom_text(
    data = data_df,
    aes(x = col_x["ev"], y = y_mid, label = evidence),
    family = "Helvetica", fontface = "plain", size = GG_TEXT, color = col_dark, hjust = 0.5, vjust = 0.5
  ) +

  # Clean two-line footnote
  annotate(
    "text", x = col_x["id"], y = -0.48, label = footnote_line1,
    family = "Helvetica", fontface = "plain", size = GG_FOOT, color = col_dark, hjust = 0, vjust = 0.5
  ) +
  annotate(
    "text", x = col_x["id"], y = -1.00, label = footnote_line2,
    family = "Helvetica", fontface = "plain", size = GG_FOOT, color = col_dark, hjust = 0, vjust = 0.5
  ) +

  coord_cartesian(
    xlim = c(0.0, 100.0),
    ylim = c(-1.45, y_header_top + 0.25),
    expand = FALSE,
    clip = "off"
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin      = margin(t = 4, r = 6, b = 4, l = 6, unit = "pt")
  )

out_pdf <- file.path(path_tables, "Table1_rubric_and_recovery.pdf")
save_panel_pair(p_tbl, out_pdf, width = 7.20, height = 3.70, dpi = 300L)

cat("[table01.R] Saved clean Nature Table 1 PDF and 300 DPI PNG:\n")
cat("  ", out_pdf, "\n")
cat("  ", sub("\\.pdf$", ".png", out_pdf), "\n")
