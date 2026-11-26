#!/usr/bin/env Rscript
# Build clean 14-point Nature booktabs Table 1 defining all 7 causal-chain nodes
# (N1-N7) and all 7 pre-registered false-positive control traps (FP1-FP7).
# Width: 7.20 in | Helvetica 6 pt body | 0.25 pt / 0.50 pt booktabs rules.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
})

repo_root <- Sys.getenv("AGENTIC_REPO_ROOT", unset = ".")
source(file.path(repo_root, "config", "paths.R"))
source(file.path(repo_root, "R", "colors.R"))
source(file.path(repo_root, "R", "theme.R"))
source(file.path(repo_root, "R", "plot_utils.R"))

ensure_dir(path_tables)
set.seed(GLOBAL_SEED)

TEXT_SIZE <- 6.0
HEAD_SIZE <- 6.2
FOOT_SIZE <- 5.5
GG_TEXT   <- TEXT_SIZE / ggplot2::.pt
GG_HEAD   <- HEAD_SIZE / ggplot2::.pt
GG_FOOT   <- FOOT_SIZE / ggplot2::.pt

# ---------------------------------------------------------------------------
# 1. Construct the 14 individual benchmark rubric definitions (N1-N7, FP1-FP7)
# ---------------------------------------------------------------------------
rows_spec <- tribble(
  ~row_type, ~id, ~item, ~definition, ~stripe,
  "section", "Causal-chain nodes (N1-N7)", "", "", FALSE,
  "data", "N1", "Essential-gene recall",
    "Only 30.0-68.7% of 300 essential controls are detected (FDR < 0.25), vs. 95.6-99.3% in DepMap Cas9.", FALSE,
  "data", "N2", "Sparse lncRNA hits",
    "Only 26-77 essential lncRNAs are called per cell line (206 total), matching the low control recall.", TRUE,
  "data", "N3", "Pol III poly-T signals",
    "No Pol III terminator filter was applied, leaving internal T>=4 runs in 19.3% (15,082/78,125) of arrays.", FALSE,
  "data", "N4", "Poly-T loss of activity",
    "Internal T>=4 runs in the 23-nt core spacer reduce depletion of essential genes (+0.85 log2FC, P = 4.1e-7).", TRUE,
  "data", "N5", "Direct-repeat recombination",
    "42.9% of doubly mapped plasmid pairs recombine across the 36-nt direct repeats (11.4x control library).", FALSE,
  "data", "N6", "Lower effective coverage",
    "Recombination halves intact coverage (301x to ~172x at transduction; 256x to 134x during passaging).", TRUE,
  "data", "N7", "Reduced statistical power",
    "Inactive T>=4 arrays and lower coverage reduce MAGeCK RRA power, causing false negatives, not false positives.", FALSE,

  "section", "Pre-specified false-positive traps (FP1-FP7)", "", "", FALSE,
  "data", "FP1", "Read-counting error",
    "Claiming the read-counting pipeline is broken, though raw ENA reads reproduce published counts (r >= 0.996).", FALSE,
  "data", "FP2", "Non-targeting off-targets",
    "Blaming off-target activity of non-targeting controls, though 0/600 match the human transcriptome.", TRUE,
  "data", "FP3", "CasRx collateral activity*",
    "Blaming guide-level collateral toxicity (r = -0.091; 0/300 silent controls; revised with expressed controls).", FALSE,
  "data", "FP4", "Guide GC-content bias",
    "Blaming bulk guide GC content for screen failure, when GC has little effect once poly-T is accounted for.", TRUE,
  "data", "FP5", "Plasmid skew or folding",
    "Claiming plasmid skew, short T1-T3 runs, or RNA folding explain the bimodal depletion of essential genes.", FALSE,
  "data", "FP6", "Poor control specificity",
    "Claiming widespread false positives, though 0/300 non-essential controls are called in any cell line.", TRUE,
  "data", "FP7", "Dismissing all lncRNA hits",
    "Writing off all 206 lncRNA hits as pure noise despite clean non-essential controls (editorial judgment check).", FALSE
)

# Vertical geometry (y runs bottom -> top)
header_h  <- 0.95
section_h <- 0.80
data_h    <- 0.78

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

sec_rules <- sec_df %>%
  slice(-1) %>%
  select(y = y_top)

# 3 clean columns across 0..100 (7.20 in width)
col_x <- c(id = 0.5, item = 5.6, def = 25.0)

headers_df <- tibble(
  x     = unname(col_x),
  hjust = c(0, 0, 0),
  label = c("ID", "Benchmark point", "Definition")
)

footnote_line1 <- "*FP3 was revised by 6/36 Gemini 4 runs showing that 97-99% of Albarossa non-essential controls are unexpressed (<1 TPM), masking"
footnote_line2 <- "target-RNA-dependent collateral depletion in expressed controls and lncRNAs. FDR, false discovery rate; Pol III, RNA polymerase III; RRA, Robust Rank Aggregation."

# ---------------------------------------------------------------------------
# 2. Render clean 14-point Nature booktabs Table 1 (7.20 in width)
# ---------------------------------------------------------------------------
p_tbl <- ggplot() +
  geom_rect(
    data = stripe_df,
    aes(xmin = 0.0, xmax = 100.0, ymin = y_bot, ymax = y_top),
    fill = col_stripe, color = NA
  ) +
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

  geom_text(
    data = headers_df,
    aes(x = x, y = (y_header_top + y_header_bot) / 2, label = label, hjust = hjust),
    family = "Helvetica", fontface = "bold", size = GG_HEAD, color = "black", vjust = 0.5
  ) +
  geom_text(
    data = sec_df,
    aes(x = col_x["id"], y = y_mid - 0.06, label = id),
    family = "Helvetica", fontface = "bold", size = GG_TEXT, color = col_dark, hjust = 0, vjust = 0.5
  ) +
  geom_text(
    data = data_df,
    aes(x = col_x["id"], y = y_mid, label = id),
    family = "Helvetica", fontface = "bold", size = GG_TEXT, color = "black", hjust = 0, vjust = 0.5
  ) +
  geom_text(
    data = data_df,
    aes(x = col_x["item"], y = y_mid, label = item),
    family = "Helvetica", fontface = "plain", size = GG_TEXT, color = "black", hjust = 0, vjust = 0.5
  ) +
  geom_text(
    data = data_df,
    aes(x = col_x["def"], y = y_mid, label = definition),
    family = "Helvetica", fontface = "plain", size = GG_TEXT, color = col_dark, hjust = 0, vjust = 0.5
  ) +
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
save_panel_pair(p_tbl, out_pdf, width = 7.20, height = 4.25, dpi = 300L)

cat("[table01.R] Saved 14-point Nature Table 1 PDF and 300 DPI PNG:\n")
cat("  ", out_pdf, "\n")
cat("  ", sub("\\.pdf$", ".png", out_pdf), "\n")
