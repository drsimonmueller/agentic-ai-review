#!/usr/bin/env Rscript
# Recompute primary Montero et al. forensic statistics from albarossa_arrays_qc.tsv.gz
# and companion source_data/montero_forensics/ datasets, verifying exact integrity
# against albarossa_arrays_qc_readme_stats.json and writing montero_forensics_stats.csv.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(jsonlite)
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
# 1. Load and validate all source_data/montero_forensics inputs
# ---------------------------------------------------------------------------
p_qc     <- require_file(file.path(path_sd_forensics, "albarossa_arrays_qc.tsv.gz"))
p_depmap <- require_file(file.path(path_sd_forensics, "ae_recall_vs_depmap_6lines.csv"))
p_plas   <- require_file(file.path(path_sd_forensics, "plasmid_chimerism_and_bsmbi.csv"))
p_rra    <- require_file(file.path(path_sd_forensics, "mageck_rescue_and_rra_cliff.csv"))
p_res    <- require_file(file.path(path_sd_forensics, "residual_ae_predictors.csv"))
p_lnc    <- require_file(file.path(path_sd_forensics, "lncrna_expression_and_collateral.csv"))
p_json   <- require_file(file.path(path_sd_forensics, "albarossa_arrays_qc_readme_stats.json"))

qc     <- read_tsv(p_qc, show_col_types = FALSE)
depmap <- read_csv(p_depmap, show_col_types = FALSE)
plas   <- read_csv(p_plas, show_col_types = FALSE)
rra    <- read_csv(p_rra, show_col_types = FALSE)
res    <- read_csv(p_res, show_col_types = FALSE)
lnc    <- read_csv(p_lnc, show_col_types = FALSE)
ref_json <- fromJSON(p_json)

require_columns(
  qc,
  c(
    "array_id", "gene_id", "category", "has_T4_core23", "has_T4_ext7", "has_T4_any",
    "has_bsmbi_fwd", "has_bsmbi_array", "has_bsmbi_rev_or_junc", "plasmid_count",
    "is_plasmid_dropout_lt30", "is_offtarget", "is_noisy_nan_pval", "is_clean_array",
    "LN18_lfc", "LN229_lfc", "A549_lfc", "NCIH460_lfc", "KP4_lfc", "MIAPACA2_lfc",
    "LN18wo_lfc", "NCIH460wo_lfc", "MIAPACA2wo_lfc", "casrx6_mean_lfc", "wo3_mean_lfc"
  ),
  "albarossa_arrays_qc"
)

# Integrity assertions against albarossa_arrays_qc_readme_stats.json
stopifnot(
  nrow(qc) == ref_json$total_albarossa_arrays,
  sum(qc$has_T4_core23) == ref_json$t4_counts$has_T4_core23,
  sum(qc$has_T4_ext7) == ref_json$t4_counts$has_T4_ext7_created,
  sum(qc$has_T4_any) == ref_json$t4_counts$has_T4_any,
  sum(qc$has_bsmbi_fwd) == ref_json$bsmbi_counts$has_bsmbi_fwd_spacer,
  sum(qc$has_bsmbi_array) == ref_json$bsmbi_counts$has_bsmbi_array_internal,
  sum(qc$has_bsmbi_rev_or_junc) == ref_json$bsmbi_counts$has_bsmbi_oligo_extra,
  sum(qc$is_plasmid_dropout_lt30) == ref_json$plasmid_dropout_lt30_total,
  sum(qc$is_clean_array) == ref_json$clean_arrays_total
)

add_row <- function(section, metric, stratum, n = NA_integer_, k = NA_integer_,
                    value = NA_real_, secondary_value = NA_real_,
                    p_value = NA_real_, notes = "") {
  tibble(
    section = section,
    metric = metric,
    stratum = stratum,
    n = as.integer(n),
    k = as.integer(k),
    value = as.numeric(value),
    secondary_value = as.numeric(secondary_value),
    p_value = as.numeric(p_value),
    notes = as.character(notes)
  )
}

rows <- list()

# ---------------------------------------------------------------------------
# 2. 6-line AE recall vs DepMap 22Q2
# ---------------------------------------------------------------------------
dm_lines <- depmap %>% filter(cell_line != "UNION_6LINES")
dm_union <- depmap %>% filter(cell_line == "UNION_6LINES")

for (i in seq_len(nrow(dm_lines))) {
  r <- dm_lines[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "ae_recall_vs_depmap", "casrx_ae_recall_fdr25_pct", r$display_name,
    n = r$casrx_ae_tested, k = r$casrx_ae_hits_fdr25,
    value = r$casrx_ae_recall_fdr25_pct, secondary_value = r$depmap_cas9_recall_lt_neg05_pct,
    p_value = r$spearman_casrx_lfc_vs_chronos_p,
    notes = sprintf("FDR<0.05=%d (%.1f%%); NE hits=%d/%d; lncRNA hits=%d",
                    r$casrx_ae_hits_fdr05, r$casrx_ae_recall_fdr05_pct,
                    r$casrx_ne_hits_fdr25, r$casrx_ne_tested, r$lncrna_hits_fdr25)
  )
}

rows[[length(rows) + 1]] <- add_row(
  "ae_recall_vs_depmap", "casrx_ae_recall_6line_mean_fdr25_pct", "6-Line Mean",
  n = 6L, k = NA_integer_,
  value = mean(dm_lines$casrx_ae_recall_fdr25_pct),
  secondary_value = mean(dm_lines$depmap_cas9_recall_lt_neg05_pct),
  notes = sprintf("CasRx range %.1f%%-%.1f%% vs DepMap Cas9 range %.1f%%-%.1f%%",
                  min(dm_lines$casrx_ae_recall_fdr25_pct), max(dm_lines$casrx_ae_recall_fdr25_pct),
                  min(dm_lines$depmap_cas9_recall_lt_neg05_pct), max(dm_lines$depmap_cas9_recall_lt_neg05_pct))
)
rows[[length(rows) + 1]] <- add_row(
  "ae_recall_vs_depmap", "casrx_ae_recall_6line_union_fdr25_pct", "6-Line Union",
  n = dm_union$casrx_ae_tested, k = dm_union$casrx_ae_hits_fdr25,
  value = dm_union$casrx_ae_recall_fdr25_pct,
  secondary_value = dm_union$casrx_ae_recall_fdr05_pct,
  notes = sprintf("FDR<0.05 union=%d/300 (%.1f%%); NE hits=%d/299; lncRNA union=%d",
                  dm_union$casrx_ae_hits_fdr05, dm_union$casrx_ae_recall_fdr05_pct,
                  dm_union$casrx_ne_hits_fdr25, dm_union$lncrna_hits_fdr25)
)

# ---------------------------------------------------------------------------
# 3. T>=4 counts and AE LFC shifts (casrx6_mean_lfc, 6 lines, wo3_mean_lfc, core23 vs ext7)
# ---------------------------------------------------------------------------
n_tot <- nrow(qc)
n_t4_any <- sum(qc$has_T4_any)
n_t4_c23 <- sum(qc$has_T4_core23)
n_t4_ext <- sum(qc$has_T4_ext7)
rows[[length(rows) + 1]] <- add_row(
  "t4_prevalence", "has_T4_any", "Albarossa_all_78125",
  n = n_tot, k = n_t4_any, value = 100 * n_t4_any / n_tot,
  notes = sprintf("core23=%d (%.2f%%), ext7=%d (%.2f%%)",
                  n_t4_c23, 100 * n_t4_c23 / n_tot, n_t4_ext, 100 * n_t4_ext / n_tot)
)

for (cat_name in c("AE", "NE", "NT", "lncRNA")) {
  sub_cat <- qc %>% filter(category == cat_name)
  k_t4 <- sum(sub_cat$has_T4_any)
  rows[[length(rows) + 1]] <- add_row(
    "t4_prevalence", "has_T4_any_by_category", cat_name,
    n = nrow(sub_cat), k = k_t4, value = 100 * k_t4 / nrow(sub_cat),
    notes = sprintf("core23=%d, ext7=%d", sum(sub_cat$has_T4_core23), sum(sub_cat$has_T4_ext7))
  )
}

ae <- qc %>% filter(category == "AE")
ae_nd <- ae %>% filter(has_bsmbi_rev_or_junc == 0, is_plasmid_dropout_lt30 == 0)

# Overall casrx6_mean_lfc & wo3_mean_lfc (all 900 and nondropout 894)
add_mwu_shift <- function(df_sub, col_name, stratum_label, group_col = "has_T4_any", pos_val = 1L, neg_val = 0L) {
  x_pos <- df_sub[[col_name]][df_sub[[group_col]] == pos_val]
  x_neg <- df_sub[[col_name]][df_sub[[group_col]] == neg_val]
  x_pos <- x_pos[is.finite(x_pos)]
  x_neg <- x_neg[is.finite(x_neg)]
  wt <- stats::wilcox.test(x_pos, x_neg)
  med_pos <- stats::median(x_pos)
  med_neg <- stats::median(x_neg)
  add_row(
    "t4_ae_lfc_shift", col_name, stratum_label,
    n = length(x_pos) + length(x_neg), k = length(x_pos),
    value = med_pos - med_neg, secondary_value = med_pos,
    p_value = wt$p.value,
    notes = sprintf("med_pos=%.5f (n=%d) vs med_neg=%.5f (n=%d)",
                    med_pos, length(x_pos), med_neg, length(x_neg))
  )
}

rows[[length(rows) + 1]] <- add_mwu_shift(ae, "casrx6_mean_lfc", "AE_all900_T4any_vs_noT4")
rows[[length(rows) + 1]] <- add_mwu_shift(ae_nd, "casrx6_mean_lfc", "AE_nondropout894_T4any_vs_noT4")
rows[[length(rows) + 1]] <- add_mwu_shift(ae, "wo3_mean_lfc", "AE_all900_woCasRx_T4any_vs_noT4")
rows[[length(rows) + 1]] <- add_mwu_shift(ae_nd, "wo3_mean_lfc", "AE_nondropout894_woCasRx_T4any_vs_noT4")

# Per-line LFC shifts (CasRx 6 lines + woCasRx 3 lines)
for (lcol in c("LN18_lfc", "LN229_lfc", "A549_lfc", "NCIH460_lfc", "KP4_lfc", "MIAPACA2_lfc",
               "LN18wo_lfc", "NCIH460wo_lfc", "MIAPACA2wo_lfc")) {
  rows[[length(rows) + 1]] <- add_mwu_shift(ae, lcol, paste0("AE_all900_", lcol))
}

# core23 vs ext7_only vs noT4
ae_core23_vs_no <- ae %>% filter(has_T4_core23 == 1 | has_T4_any == 0)
ae_ext7only_vs_no <- ae %>% filter((has_T4_ext7 == 1 & has_T4_core23 == 0) | has_T4_any == 0) %>%
  mutate(is_ext7_only = as.integer(has_T4_ext7 == 1 & has_T4_core23 == 0))
ae_core23_vs_ext7only <- ae %>% filter(has_T4_core23 == 1 | (has_T4_ext7 == 1 & has_T4_core23 == 0)) %>%
  mutate(is_core23 = as.integer(has_T4_core23 == 1))

rows[[length(rows) + 1]] <- add_mwu_shift(ae_core23_vs_no, "casrx6_mean_lfc", "AE_core23(97)_vs_noT4(765)", group_col = "has_T4_core23")
rows[[length(rows) + 1]] <- add_mwu_shift(ae_ext7only_vs_no, "casrx6_mean_lfc", "AE_ext7_only(38)_vs_noT4(765)", group_col = "is_ext7_only")
rows[[length(rows) + 1]] <- add_mwu_shift(ae_core23_vs_ext7only, "casrx6_mean_lfc", "AE_core23(97)_vs_ext7_only(38)", group_col = "is_core23")
rows[[length(rows) + 1]] <- add_mwu_shift(ae_core23_vs_no, "wo3_mean_lfc", "AE_woCasRx_core23(97)_vs_noT4(765)", group_col = "has_T4_core23")

# ---------------------------------------------------------------------------
# 4. BsmBI counts & plasmid dropout overlap
# ---------------------------------------------------------------------------
n_bsmbi_arr   <- sum(qc$has_bsmbi_array)
n_bsmbi_oligo <- sum(qc$has_bsmbi_rev_or_junc)
n_drop_tot    <- sum(qc$is_plasmid_dropout_lt30)
n_bsmbi_drop  <- sum(qc$has_bsmbi_rev_or_junc == 1 & qc$is_plasmid_dropout_lt30 == 1)
n_bsmbi_zero  <- sum(qc$has_bsmbi_rev_or_junc == 1 & qc$plasmid_count == 0)
med_bsmbi_cnt <- stats::median(qc$plasmid_count[qc$has_bsmbi_rev_or_junc == 1])
med_intact_cnt <- stats::median(qc$plasmid_count[qc$has_bsmbi_rev_or_junc == 0])
wt_bsmbi_cnt  <- stats::wilcox.test(
  qc$plasmid_count[qc$has_bsmbi_rev_or_junc == 1],
  qc$plasmid_count[qc$has_bsmbi_rev_or_junc == 0]
)

rows[[length(rows) + 1]] <- add_row(
  "bsmbi_defects", "bsmbi_internal_array_count", "Albarossa_all_78125",
  n = n_tot, k = n_bsmbi_arr, value = 100 * n_bsmbi_arr / n_tot,
  notes = sprintf("fwd_spacer=%d", sum(qc$has_bsmbi_fwd))
)
rows[[length(rows) + 1]] <- add_row(
  "bsmbi_defects", "bsmbi_oligo_extra_count", "Albarossa_all_78125",
  n = n_tot, k = n_bsmbi_oligo, value = 100 * n_bsmbi_oligo / n_tot,
  secondary_value = 100 * n_bsmbi_drop / n_drop_tot,
  p_value = wt_bsmbi_cnt$p.value,
  notes = sprintf("bsmbi_dropout_lt30=%d/%d (%.1f%% of all dropouts); zero_count=%d; med_cnt=%g vs intact=%g",
                  n_bsmbi_drop, n_drop_tot, 100 * n_bsmbi_drop / n_drop_tot,
                  n_bsmbi_zero, med_bsmbi_cnt, med_intact_cnt)
)
rows[[length(rows) + 1]] <- add_row(
  "bsmbi_defects", "clean_arrays_total", "Albarossa_all_78125",
  n = n_tot, k = sum(qc$is_clean_array), value = 100 * mean(qc$is_clean_array),
  secondary_value = sum(ae$is_clean_array),
  notes = sprintf("clean AE=%d/%d (non-null no-offtarget=%d)",
                  sum(ae$is_clean_array), nrow(ae),
                  sum(ae$is_clean_array == 1 & ae$is_offtarget == 0 & ae$is_noisy_nan_pval == 0))
)

# ---------------------------------------------------------------------------
# 5. ERR12330401 vs ERR12330450 chimerism & effective coverage + BsmBI chronometer
# ---------------------------------------------------------------------------
ena <- plas %>% filter(section == "ena_alignment_summary") %>%
  mutate(both_mapped_discordant_pct = 100 * discordant_pairs / (concordant_pairs + discordant_pairs))

alb_pl <- ena %>% filter(ena_accession == "ERR12330401")
ctl_pl <- ena %>% filter(ena_accession == "ERR12330450")

fisher_chim <- run_fisher_2x2(
  round(alb_pl$discordant_pairs), round(alb_pl$concordant_pairs),
  round(ctl_pl$discordant_pairs), round(ctl_pl$concordant_pairs)
)

rows[[length(rows) + 1]] <- add_row(
  "plasmid_chimerism", "err12330401_vs_err12330450_discordant_pct", "Plasmid_Pools",
  n = as.integer(alb_pl$total_pairs), k = as.integer(alb_pl$discordant_pairs),
  value = alb_pl$both_mapped_discordant_pct,
  secondary_value = ctl_pl$both_mapped_discordant_pct,
  p_value = fisher_chim$p_value,
  notes = sprintf("ERR12330401 total_disc=%.3f%% (both_mapped=%.3f%%, cov %.2fx->%.2fx) vs ERR12330450 total_disc=%.3f%% (both_mapped=%.3f%%); ratio=%.2fx",
                  alb_pl$discordant_pct, alb_pl$both_mapped_discordant_pct,
                  alb_pl$nominal_raw_coverage_x, alb_pl$effective_concordant_coverage_x,
                  ctl_pl$discordant_pct, ctl_pl$both_mapped_discordant_pct,
                  alb_pl$both_mapped_discordant_pct / ctl_pl$both_mapped_discordant_pct)
)

# Effective cellular coverage scaling from 300.8x (23.5M / 78,125) and screen gDNA samples
eff_300x <- 300.8 * (100 - alb_pl$both_mapped_discordant_pct) / 100
rows[[length(rows) + 1]] <- add_row(
  "plasmid_chimerism", "transduction_effective_coverage_x", "Albarossa_23.5M_cells",
  n = MONTERO_N_ARRAYS, k = NA_integer_,
  value = eff_300x, secondary_value = 300.8,
  notes = sprintf("300.8x * (1 - %.4f) = %.2fx; screen gDNA concordant_pct range %.1f%%-%.1f%%",
                  alb_pl$both_mapped_discordant_pct / 100, eff_300x,
                  min(ena$concordant_pct[ena$library == "ALBAROSSA_library" & ena$ena_accession != "ERR12330401"]),
                  max(ena$concordant_pct[ena$library == "ALBAROSSA_library" & ena$ena_accession != "ERR12330401"]))
)

# BsmBI chronometer summary rows
chron <- plas %>% filter(section == "bsmbi_chronometer")
for (i in seq_len(nrow(chron))) {
  r <- chron[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "bsmbi_chronometer", r$bsmbi_class, r$ena_accession,
    n = as.integer(r$n_arrays), k = NA_integer_,
    value = r$concordant_median, secondary_value = r$ref_concordant_median,
    p_value = r$mwu_p_vs_intact,
    notes = sprintf("chim_spacer1_med=%g, chim_spacer2_med=%g", r$chim_spacer1_median, r$chim_spacer2_median)
  )
}

# ---------------------------------------------------------------------------
# 6. MAGeCK RRA cliff, naive filter drop, 120 live simulations, & calibration
# ---------------------------------------------------------------------------
base_6 <- rra %>% filter(section == "baseline_vs_filter_and_rescue", !is_wocasrx)
step0  <- base_6 %>% filter(step_or_mode == "Step0_published_T12")
step1c <- base_6 %>% filter(step_or_mode == "Step1c_filter_TTTT30_kge2")
step2a <- base_6 %>% filter(step_or_mode == "Step2a_ctrl_NE_median")

rows[[length(rows) + 1]] <- add_row(
  "rra_cliff", "naive_filter_ae_recall_change_pct", "Step1c_TTTT30_kge2_vs_Step0",
  n = 6L, k = sum(step1c$ae_hits_fdr25 < step0$ae_hits_fdr25),
  value = mean(step1c$ae_recall_fdr25_pct - step0$ae_recall_fdr25_pct),
  secondary_value = mean(step1c$ae_hits_fdr25 - step0$ae_hits_fdr25),
  notes = sprintf("Step0 mean AE hits=%.1f (%.2f%%) vs Step1c mean AE hits=%.1f (%.2f%%); dropped in %d/6 lines",
                  mean(step0$ae_hits_fdr25), mean(step0$ae_recall_fdr25_pct),
                  mean(step1c$ae_hits_fdr25), mean(step1c$ae_recall_fdr25_pct),
                  sum(step1c$ae_hits_fdr25 < step0$ae_hits_fdr25))
)
rows[[length(rows) + 1]] <- add_row(
  "rra_cliff", "ctrl_sgrna_ne_calibration_gain", "Step2a_ctrl_NE_vs_Step0",
  n = 6L, k = sum(step2a$ae_hits_fdr25 > step0$ae_hits_fdr25),
  value = mean(step2a$ae_recall_fdr25_pct - step0$ae_recall_fdr25_pct),
  secondary_value = mean(step2a$lncrna_hits_fdr25 / step0$lncrna_hits_fdr25),
  notes = sprintf("AE recall +%.2f pp; lncRNA hits ratio %.2fx-%.2fx (0 woCasRx AE hits)",
                  mean(step2a$ae_recall_fdr25_pct - step0$ae_recall_fdr25_pct),
                  min(step2a$lncrna_hits_fdr25 / step0$lncrna_hits_fdr25),
                  max(step2a$lncrna_hits_fdr25 / step0$lncrna_hits_fdr25))
)

sim120 <- rra %>% filter(section == "rra_power_simulation_120") %>%
  group_by(k_arrays) %>%
  summarise(
    n_sims = n(),
    mean_recall_pct = mean(ae_recall_fdr25_pct),
    sd_recall_pct = sd(ae_recall_fdr25_pct),
    median_recall_pct = median(ae_recall_fdr25_pct),
    .groups = "drop"
  )
for (i in seq_len(nrow(sim120))) {
  r <- sim120[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "rra_cliff", "rra_sim120_recall_by_k", paste0("k=", r$k_arrays),
    n = r$n_sims, k = NA_integer_,
    value = r$mean_recall_pct, secondary_value = r$median_recall_pct,
    notes = sprintf("SD=%.2f%% across 4 lines x 5 seeds", r$sd_recall_pct)
  )
}

emp_k <- rra %>% filter(section == "empirical_active_arrays_k0_to_3", step_or_mode == "lfc_lt_neg05")
# Note empirical_active_arrays_k0_to_3 contains both CasRx and any extra rows; check distinct cell lines
emp_casrx <- emp_k %>% filter(cell_line %in% c("LN18", "LN229", "A549", "NCIH460", "KP4", "MIAPACA2"))
emp_sum <- emp_casrx %>%
  group_by(k_arrays) %>%
  summarise(mean_pct = mean(ae_recall_fdr25_pct), .groups = "drop")
# Note: LN18 had 2 rows (1 CasRx 88.7%, 1 woCasRx 13.3% where cell_line=="LN18" in lines 224-227 vs 296-299); use first 6 lines (pct_ge_2_of_3 > 50)
emp_casrx_clean <- emp_k %>% filter(extra_metric_value > 50) %>%
  group_by(k_arrays) %>%
  summarise(n_lines = n(), mean_pct = mean(ae_recall_fdr25_pct), .groups = "drop")
for (i in seq_len(nrow(emp_casrx_clean))) {
  r <- emp_casrx_clean[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "rra_cliff", "empirical_active_arrays_lfc_lt_neg05_pct", paste0("active_k=", r$k_arrays),
    n = r$n_lines, k = NA_integer_, value = r$mean_pct,
    notes = "Mean % of 300 AE genes across 6 CasRx lines"
  )
}

# ---------------------------------------------------------------------------
# 7. Residual AE predictor stats
# ---------------------------------------------------------------------------
res_utr <- res %>% filter(section == "genomic_location_3utr_vs_cds")
for (i in seq_len(nrow(res_utr))) {
  r <- res_utr[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "residual_ae_predictors", "utr_vs_cds_depleted_pct", r$stratum_or_feature,
    n = r$n, k = as.integer(r$n_hits_or_depleted), value = r$recall_or_frac_pct,
    notes = sprintf("95%% CI [%.2f%%, %.2f%%]", r$ci_lo, r$ci_hi)
  )
}

res_or <- res %>% filter(section == "multivariable_odds_ratios", stratum_or_feature %in% c("cds_frac", "log10_tpm", "chronos_mean", "tttt_in_g1", "tttt_in_g2"))
for (i in seq_len(nrow(res_or))) {
  r <- res_or[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "residual_ae_predictors", "multivariable_or", r$stratum_or_feature,
    n = r$n, k = NA_integer_, value = r$effect_or_rho_or_or,
    p_value = r$p_value,
    notes = sprintf("95%% CI [%.3f, %.3f]", r$ci_lo, r$ci_hi)
  )
}

res_loc <- res %>% filter(section == "subcellular_localization")
for (i in seq_len(nrow(res_loc))) {
  r <- res_loc[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "residual_ae_predictors", "subcellular_localization_median_lfc", r$stratum_or_feature,
    n = r$n, k = NA_integer_, value = r$median_lfc, secondary_value = r$mean_lfc
  )
}

res_null <- res %>% filter(section == "null_rnafold_and_cas13design", stratum_or_feature %in% c("pre_mfe", "wessels_rf_mean", "both_dr1_intact"))
for (i in seq_len(nrow(res_null))) {
  r <- res_null[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "residual_ae_predictors", "null_predictor_spearman_rho", r$stratum_or_feature,
    n = r$n, k = NA_integer_, value = r$effect_or_rho_or_or, p_value = r$p_value,
    notes = r$notes
  )
}

res_r2 <- res %>% filter(section == "multivariable_groupkfold_r2")
for (i in seq_len(nrow(res_r2))) {
  r <- res_r2[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "residual_ae_predictors", "groupkfold_r2_pct", r$stratum_or_feature,
    n = r$n, k = NA_integer_, value = r$recall_or_frac_pct, secondary_value = r$effect_or_rho_or_or,
    notes = r$notes
  )
}

# ---------------------------------------------------------------------------
# 8. Silent-hit & FP3 collateral stats
# ---------------------------------------------------------------------------
hits_audit <- lnc %>% filter(section == "lncrna_206_hit_expression_audit")
tot_hit_calls <- sum(hits_audit$n_items)
tot_tpm0      <- sum(hits_audit$n_tpm_eq_0)
tot_t7_0      <- sum(hits_audit$n_t7_reads_eq_0)
tot_t7_lt10   <- sum(hits_audit$n_t7_reads_lt_10)

rows[[length(rows) + 1]] <- add_row(
  "silent_hits_and_fp3", "called_lncrna_hits_tpm_eq_0_pooled", "6lines_274_hit_calls",
  n = tot_hit_calls, k = tot_tpm0, value = 100 * tot_tpm0 / tot_hit_calls,
  notes = sprintf("Per-line TPM=0 range %.1f%%-%.1f%%", min(hits_audit$pct_tpm_eq_0), max(hits_audit$pct_tpm_eq_0))
)
rows[[length(rows) + 1]] <- add_row(
  "silent_hits_and_fp3", "called_lncrna_hits_t7_reads_eq_0_pooled", "6lines_274_hit_calls",
  n = tot_hit_calls, k = tot_t7_0, value = 100 * tot_t7_0 / tot_hit_calls,
  secondary_value = 100 * tot_t7_lt10 / tot_hit_calls,
  notes = sprintf("Per-line T7=0 range %.1f%%-%.1f%%; T7<10 pooled=%d/%d (%.1f%%, range %.1f%%-%.1f%%)",
                  min(hits_audit$pct_t7_reads_eq_0), max(hits_audit$pct_t7_reads_eq_0),
                  tot_t7_lt10, tot_hit_calls, 100 * tot_t7_lt10 / tot_hit_calls,
                  min(hits_audit$pct_t7_reads_lt_10), max(hits_audit$pct_t7_reads_lt_10))
)

ne_alb <- lnc %>% filter(section == "albarossa_vs_controls_ne_expression", library == "ALBAROSSA_library")
ne_ctl <- lnc %>% filter(section == "albarossa_vs_controls_ne_expression", library == "controls_library")

rows[[length(rows) + 1]] <- add_row(
  "silent_hits_and_fp3", "albarossa_ne_unexpressed_pct", "Albarossa_NE_299",
  n = 299L, k = NA_integer_,
  value = mean(ne_alb$pct_tpm_lt_1), secondary_value = mean(ne_alb$pct_tpm_eq_0),
  notes = sprintf("TPM<1 range %.2f%%-%.2f%%; TPM=0 range %.2f%%-%.2f%%; median TPM=0.0",
                  min(ne_alb$pct_tpm_lt_1), max(ne_alb$pct_tpm_lt_1),
                  min(ne_alb$pct_tpm_eq_0), max(ne_alb$pct_tpm_eq_0))
)
rows[[length(rows) + 1]] <- add_row(
  "silent_hits_and_fp3", "controls_library_ne_expressed_pct", "controls_library_NE_300",
  n = 300L, k = NA_integer_,
  value = 100 - mean(ne_ctl$pct_tpm_eq_0), secondary_value = mean(ne_ctl$median_tpm),
  notes = sprintf("TPM>0 range %.2f%%-%.2f%%; median TPM range %.2f-%.2f",
                  100 - max(ne_ctl$pct_tpm_eq_0), 100 - min(ne_ctl$pct_tpm_eq_0),
                  min(ne_ctl$median_tpm), max(ne_ctl$median_tpm))
)

neon_collat <- lnc %>% filter(section == "neon_collateral_tests_crosscheck")
for (i in seq_len(nrow(neon_collat))) {
  r <- neon_collat[i, ]
  rows[[length(rows) + 1]] <- add_row(
    "silent_hits_and_fp3", paste0("collateral_", r$category), r$cell_line_or_bin,
    n = r$n_items, k = NA_integer_,
    value = r$spearman_rho_vs_expr, secondary_value = r$median_casrx_lfc,
    p_value = r$p_value,
    notes = r$notes
  )
}

montero_forensics_stats <- bind_rows(rows)
write_csv(montero_forensics_stats, file.path(path_tables, "montero_forensics_stats.csv"))

cat("[02_montero_forensics_stats.R] Completed cleanly.\n")
cat(sprintf("  Wrote %d forensic summary rows to %s\n",
            nrow(montero_forensics_stats),
            file.path(path_tables, "montero_forensics_stats.csv")))
