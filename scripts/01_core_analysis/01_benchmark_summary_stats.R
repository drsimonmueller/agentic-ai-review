#!/usr/bin/env Rscript
# Compute verified N = 36 benchmark statistics, matched arm comparisons,
# per-node / per-FP breakdowns, telemetry summaries, and key_manuscript_numbers.json.

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
# 1. Load and validate source_data/benchmark inputs
# ---------------------------------------------------------------------------
p_sc36  <- require_file(file.path(path_sd_benchmark, "scored_rubric_N36.csv"))
p_sc37  <- require_file(file.path(path_sd_benchmark, "scored_rubric_all37.csv"))
p_tel36 <- require_file(file.path(path_sd_benchmark, "run_telemetry_N36.csv"))
p_inv36 <- require_file(file.path(path_sd_benchmark, "run_inventory_N36.csv"))
p_disc  <- require_file(file.path(path_sd_benchmark, "beyond_rubric_discoveries_N36.csv"))

sc36 <- read_csv(p_sc36, show_col_types = FALSE)
sc37 <- read_csv(p_sc37, show_col_types = FALSE)
tel36 <- read_csv(p_tel36, show_col_types = FALSE)
inv36 <- read_csv(p_inv36, show_col_types = FALSE)
disc36 <- read_csv(p_disc, show_col_types = FALSE)

require_columns(
  sc36,
  c(
    "provider", "model_slug", "model_label", "actual_ceo_model",
    "ceo_biosafety_fallback", "condition", "replicate", "run_path",
    paste0(BENCHMARK_CAUSAL_NODES, "_score"),
    paste0(BENCHMARK_CAUSAL_NODES, "_tier"),
    paste0(BENCHMARK_FP_TRAPS, "_status"),
    "causal_node_score_sum", "causal_node_recall_pct",
    "n_fp_triggered", "n_fp_caught_in_discard", "n_fp_refined_by_data", "n_fp_clean",
    "n_non_chain_discoveries", "non_chain_discoveries"
  ),
  "scored_rubric_N36"
)
require_columns(
  tel36,
  c(
    "model_slug", "actual_ceo_model", "ceo_biosafety_fallback", "condition",
    "replicate", "run_path", "status", "wall_clock_min", "slurm_job_union_min",
    "slurm_queue_union_min", "active_agent_min", "n_slurm_jobs",
    "tokens_input_uncached", "tokens_cache_read", "tokens_cache_write",
    "tokens_output", "tokens_reasoning", "tokens_total_noncache",
    "estimated_usd_cost", "n_scripts_executed", "n_sbatch_scripts",
    "n_unique_figures", "n_confirmed_findings", "n_discarded_hypotheses",
    "ceo_safeguard_events", "subagent_safety_interrupts", "biosafety_refusals"
  ),
  "run_telemetry_N36"
)
require_columns(
  inv36,
  c("run_path", "status", "has_manifest", "has_audit_plan", "has_findings_json",
    "has_referee_report", "has_discard_log", "has_slurm_jobs_csv", "has_model_routing_csv"),
  "run_inventory_N36"
)

stopifnot(nrow(sc36) == 36L, nrow(tel36) == 36L, nrow(inv36) == 36L, nrow(sc37) == 37L)

# Merge rubric + telemetry + inventory at run_path level
runs <- sc36 %>%
  left_join(
    tel36 %>% select(-provider, -model_slug, -model_label, -actual_ceo_model,
                     -ceo_biosafety_fallback, -condition, -replicate, -in_primary_N36),
    by = "run_path"
  ) %>%
  left_join(
    inv36 %>% select(run_path, has_manifest, has_audit_plan, has_findings_json,
                     has_referee_report, has_discard_log, has_slurm_jobs_csv, has_model_routing_csv),
    by = "run_path"
  ) %>%
  mutate(
    framing = if_else(condition %in% c("cold", "cold_lit"), "Cold", "Seeded"),
    lit_arm = if_else(condition %in% c("seeded_lit", "cold_lit"), "+Lit", "Base"),
    slurm_pct_wall = 100 * slurm_job_union_min / wall_clock_min,
    total_hypotheses = n_confirmed_findings + n_discarded_hypotheses,
    discard_rate_pct = if_else(total_hypotheses > 0, 100 * n_discarded_hypotheses / total_hypotheses, NA_real_),
    has_called_unexpr = purrr::map_lgl(
      non_chain_discoveries,
      ~ "Called_lncRNA_hits_unexpressed" %in% trimws(strsplit(ifelse(is.na(.x), "", .x), ";", fixed = TRUE)[[1]])
    ),
    model_display = if_else(
      actual_ceo_model == "Claude Opus 5.0",
      "Claude Opus 5.0 (Opus 5.5->5.0 fallback)",
      actual_ceo_model
    )
  )

# Helper for continuous summary
summarize_vec <- function(x) {
  xf <- x[is.finite(x)]
  q <- stats::quantile(xf, probs = c(0.25, 0.5, 0.75), names = FALSE)
  list(
    n = length(xf),
    mean = mean(xf),
    sd = stats::sd(xf),
    median = q[2],
    q25 = q[1],
    q75 = q[3],
    iqr = q[3] - q[1],
    min = min(xf),
    max = max(xf),
    sum = sum(xf)
  )
}

# ---------------------------------------------------------------------------
# 2. Overall causal node recall (N = 36 and all 37)
# ---------------------------------------------------------------------------
overall_nodes <- summarize_vec(runs$causal_node_score_sum)
overall_pct   <- summarize_vec(runs$causal_node_recall_pct)
n_7of7 <- sum(runs$causal_node_score_sum == 7)
ci_7of7 <- wilson_ci(n_7of7, nrow(runs))

all37_nodes <- summarize_vec(sc37$causal_node_score_sum)
all37_pct   <- summarize_vec(sc37$causal_node_recall_pct)
n_7of7_37   <- sum(sc37$causal_node_score_sum == 7)

# ---------------------------------------------------------------------------
# 3. Recall by model and model x condition
# ---------------------------------------------------------------------------
summarize_arm_df <- function(df) {
  df %>%
    summarise(
      n_runs = n(),
      mean_nodes = mean(causal_node_score_sum),
      sd_nodes = sd(causal_node_score_sum),
      median_nodes = median(causal_node_score_sum),
      min_nodes = min(causal_node_score_sum),
      max_nodes = max(causal_node_score_sum),
      mean_recall_pct = mean(causal_node_recall_pct),
      sd_recall_pct = sd(causal_node_recall_pct),
      median_recall_pct = median(causal_node_recall_pct),
      min_recall_pct = min(causal_node_recall_pct),
      max_recall_pct = max(causal_node_recall_pct),
      n_7of7 = sum(causal_node_score_sum == 7),
      n_n6_recovered = sum(N6_score == 1),
      n_fp1_6_triggered = sum(n_fp_triggered - as.integer(FP7_status == "Triggered_in_Final")),
      n_fp3_refined = sum(FP3_status == "Refined_by_Data"),
      n_fp7_triggered = sum(FP7_status == "Triggered_in_Final"),
      median_wall_min = median(wall_clock_min),
      min_wall_min = min(wall_clock_min),
      max_wall_min = max(wall_clock_min),
      mean_discoveries = mean(n_non_chain_discoveries),
      .groups = "drop"
    )
}

by_model <- runs %>%
  group_by(provider, model_slug, model_label, actual_ceo_model, model_display) %>%
  summarize_arm_df() %>%
  mutate(condition = "ALL", summary_level = "by_model") %>%
  arrange(match(actual_ceo_model, c("Claude Opus 5.0", "Claude Sonnet 5.5", "Gemini 4 Neon", "Gemini 4 Hydrogen")))

by_model_cond <- runs %>%
  group_by(provider, model_slug, model_label, actual_ceo_model, model_display, condition) %>%
  summarize_arm_df() %>%
  mutate(summary_level = "by_model_condition") %>%
  arrange(
    match(actual_ceo_model, c("Claude Opus 5.0", "Claude Sonnet 5.5", "Gemini 4 Neon", "Gemini 4 Hydrogen")),
    match(condition, BENCHMARK_ARMS)
  )

by_condition <- runs %>%
  group_by(condition) %>%
  summarize_arm_df() %>%
  mutate(
    provider = "ALL", model_slug = "all", model_label = "All Models",
    actual_ceo_model = "All Models", model_display = "All Models",
    summary_level = "by_condition"
  ) %>%
  arrange(match(condition, BENCHMARK_ARMS))

model_arm_summary <- bind_rows(by_model, by_model_cond, by_condition) %>%
  select(summary_level, provider, model_slug, model_label, actual_ceo_model, model_display, condition, everything())

write_csv(model_arm_summary, file.path(path_tables, "model_arm_summary.csv"))

# ---------------------------------------------------------------------------
# 4. Matched comparisons
# ---------------------------------------------------------------------------
# 4a. Cold (18) vs Seeded (18) causal recall & Called_lncRNA_hits_unexpressed
cold_runs   <- runs %>% filter(framing == "Cold")
seeded_runs <- runs %>% filter(framing == "Seeded")

wilcox_cold_seeded_recall <- run_wilcox_pair(cold_runs$causal_node_score_sum, seeded_runs$causal_node_score_sum)
cold_unexpr_k   <- sum(cold_runs$has_called_unexpr)
seeded_unexpr_k <- sum(seeded_runs$has_called_unexpr)
fisher_unexpr_n36 <- run_fisher_2x2(
  cold_unexpr_k, nrow(cold_runs) - cold_unexpr_k,
  seeded_unexpr_k, nrow(seeded_runs) - seeded_unexpr_k
)

# Base N = 24 Cold (12) vs Seeded (12)
base_cold   <- runs %>% filter(lit_arm == "Base", framing == "Cold")
base_seeded <- runs %>% filter(lit_arm == "Base", framing == "Seeded")
wilcox_base_cold_seeded_recall <- run_wilcox_pair(base_cold$causal_node_score_sum, base_seeded$causal_node_score_sum)
base_cold_unexpr_k   <- sum(base_cold$has_called_unexpr)
base_seeded_unexpr_k <- sum(base_seeded$has_called_unexpr)
fisher_unexpr_base24 <- run_fisher_2x2(
  base_cold_unexpr_k, nrow(base_cold) - base_cold_unexpr_k,
  base_seeded_unexpr_k, nrow(base_seeded) - base_seeded_unexpr_k
)

# 4b. +Lit (12) vs matched Base (12 on Sonnet 5.5 + Gemini 4 Hydrogen)
matched_lit_models <- c("claude_sonnet_5_5", "google_gemini_4_hydrogen")
lit_runs          <- runs %>% filter(model_slug %in% matched_lit_models, lit_arm == "+Lit")
matched_base_runs <- runs %>% filter(model_slug %in% matched_lit_models, lit_arm == "Base")

stopifnot(nrow(lit_runs) == 12L, nrow(matched_base_runs) == 12L)

wilcox_lit_vs_base_recall  <- run_wilcox_pair(lit_runs$causal_node_score_sum, matched_base_runs$causal_node_score_sum)
wilcox_lit_vs_base_runtime <- run_wilcox_pair(lit_runs$wall_clock_min, matched_base_runs$wall_clock_min)

# Also N6 recovery in +Lit vs matched Base
lit_n6_k  <- sum(lit_runs$N6_score == 1)
base_n6_k <- sum(matched_base_runs$N6_score == 1)
fisher_lit_vs_base_n6 <- run_fisher_2x2(
  lit_n6_k, nrow(lit_runs) - lit_n6_k,
  base_n6_k, nrow(matched_base_runs) - base_n6_k
)

# ---------------------------------------------------------------------------
# 5. Per-node (N1-N7) and Per-FP trap (FP1-FP7) summaries
# ---------------------------------------------------------------------------
node_meta <- tribble(
  ~item_id, ~group, ~label,
  "N1", "Layer 1: Observed phenotype", "O1: Always-essential (AE) protein-coding recall gap vs DepMap Cas9",
  "N2", "Layer 1: Observed phenotype", "O2: Low essential lncRNA hit rate tied to control sensitivity",
  "N3", "Layer 2: Mechanistic causal chain", "H1: Internal Pol III T>=4 terminators in 15,082 / 78,125 (19.3%) arrays",
  "N4", "Layer 2: Mechanistic causal chain", "H1: T>=4 AE depletion attenuation (+0.85 log2FC, null in woCasRx)",
  "N5", "Layer 2: Mechanistic causal chain", "H2: 42.9% plasmid chimerism across duplicated 36-nt DR (ERR12330401)",
  "N6", "Layer 2: Mechanistic causal chain", "H2: Effective intact-array coverage drop (301x to ~172x / 122-134x)",
  "N7", "Layer 2: Mechanistic causal chain", "H1+H2: Diluted coverage & inert T>=4 reduce MAGeCK RRA power (FN, not FP)"
)

node_rows <- map_dfr(seq_len(nrow(node_meta)), function(i) {
  id <- node_meta$item_id[i]
  s_col <- paste0(id, "_score")
  t_col <- paste0(id, "_tier")
  k <- sum(runs[[s_col]] == 1)
  n <- nrow(runs)
  ci <- wilson_ci(k, n)
  tiers <- runs[[t_col]]
  tiers <- tiers[!is.na(tiers) & nzchar(tiers)]
  e1_n <- sum(tiers == "E1")
  e2_n <- sum(tiers == "E2")
  e3_n <- sum(tiers == "E3")
  miss_n <- n - k
  tier_parts <- c(
    if (e1_n > 0) paste0("E1 (", e1_n, ")"),
    if (e2_n > 0) paste0("E2 (", e2_n, ")"),
    if (e3_n > 0) paste0("E3 (", e3_n, ")")
  )
  tibble(
    item_type = "Causal_Node",
    item_id = id,
    group = node_meta$group[i],
    label = node_meta$label[i],
    n_runs = n,
    n_recovered_or_triggered = k,
    pct_recovered_or_triggered = 100 * k / n,
    ci_low_pct = 100 * ci$conf_low,
    ci_high_pct = 100 * ci$conf_high,
    E1_count = e1_n,
    E2_count = e2_n,
    E3_count = e3_n,
    Miss_count = miss_n,
    tier_summary = if (length(tier_parts) > 0) paste(tier_parts, collapse = ", ") else "—",
    Clean_count = NA_integer_,
    Caught_in_Discard_Log_count = NA_integer_,
    Triggered_in_Final_count = NA_integer_,
    Refined_by_Data_count = NA_integer_
  )
})

fp_meta <- tribble(
  ~item_id, ~group, ~label,
  "FP1", "False-positive traps", "Broken counting pipeline (counts reproduce at r = 0.996-1.000)",
  "FP2", "False-positive traps", "Off-target activity of 600 non-targeting (NT) gRNAs (0/600 align)",
  "FP3", "False-positive traps", "Collateral CasRx toxicity scaling with gRNA production (revised +E2)",
  "FP4", "False-positive traps", "Bulk GC content drives cross-screen correlation",
  "FP5", "False-positive traps", "Plasmid skew, replicate noise, or T1-T3 tracts explain bimodality",
  "FP6", "False-positive traps", "Gene-level specificity failure (0/300 NE controls called)",
  "FP7", "False-positive traps", "Dismissing all 206 reported lncRNA hits as artefacts (calibration)"
)

fp_rows <- map_dfr(seq_len(nrow(fp_meta)), function(i) {
  id <- fp_meta$item_id[i]
  col <- paste0(id, "_status")
  vals <- runs[[col]]
  n <- nrow(runs)
  clean_n   <- sum(vals == "Clean")
  caught_n  <- sum(vals == "Caught_in_Discard_Log")
  trig_n    <- sum(vals == "Triggered_in_Final")
  refined_n <- sum(vals == "Refined_by_Data")
  ci <- wilson_ci(trig_n, n)
  tibble(
    item_type = "FP_Trap",
    item_id = id,
    group = fp_meta$group[i],
    label = fp_meta$label[i],
    n_runs = n,
    n_recovered_or_triggered = trig_n,
    pct_recovered_or_triggered = 100 * trig_n / n,
    ci_low_pct = 100 * ci$conf_low,
    ci_high_pct = 100 * ci$conf_high,
    E1_count = NA_integer_,
    E2_count = NA_integer_,
    E3_count = NA_integer_,
    Miss_count = NA_integer_,
    tier_summary = NA_character_,
    Clean_count = clean_n,
    Caught_in_Discard_Log_count = caught_n,
    Triggered_in_Final_count = trig_n,
    Refined_by_Data_count = refined_n
  )
})

per_node_and_fp_summary <- bind_rows(node_rows, fp_rows)
write_csv(per_node_and_fp_summary, file.path(path_tables, "per_node_and_fp_summary.csv"))

# ---------------------------------------------------------------------------
# 6. Tier 4 discard rates, telemetry & bio-safety metrics
# ---------------------------------------------------------------------------
stopifnot(all(runs$n_discarded_hypotheses > 0))
discard_all36   <- summarize_vec(runs$n_discarded_hypotheses)
discard_pct_36  <- summarize_vec(runs$discard_rate_pct)
confirmed_all36 <- summarize_vec(runs$n_confirmed_findings)

wall_stats      <- summarize_vec(runs$wall_clock_min)
slurm_min_stats <- summarize_vec(runs$slurm_job_union_min)
slurm_pct_stats <- summarize_vec(runs$slurm_pct_wall)
queue_stats     <- summarize_vec(runs$slurm_queue_union_min)
active_stats    <- summarize_vec(runs$active_agent_min)
jobs_stats      <- summarize_vec(runs$n_slurm_jobs)
scripts_stats   <- summarize_vec(runs$n_scripts_executed)
sbatch_stats    <- summarize_vec(runs$n_sbatch_scripts)
figs_stats      <- summarize_vec(runs$n_unique_figures)

# Anthropic tokens/cost (18 Anthropic runs)
anth <- runs %>% filter(provider == "Anthropic")
tok_uncached <- summarize_vec(anth$tokens_input_uncached)
tok_cache_rd <- summarize_vec(anth$tokens_cache_read)
tok_cache_wr <- summarize_vec(anth$tokens_cache_write)
tok_output   <- summarize_vec(anth$tokens_output)
tok_reason   <- summarize_vec(anth$tokens_reasoning)
tok_noncache <- summarize_vec(anth$tokens_total_noncache)
n_cost_nonna <- sum(!is.na(runs$estimated_usd_cost))

# Bio-safety telemetry
opus_runs <- runs %>% filter(model_slug == "claude_opus_5_5")
opus_fallback_k <- sum(opus_runs$ceo_biosafety_fallback)
opus_fallback_n <- nrow(opus_runs)

sonnet_base <- runs %>% filter(model_slug == "claude_sonnet_5_5", lit_arm == "Base")
sonnet_lit  <- runs %>% filter(model_slug == "claude_sonnet_5_5", lit_arm == "+Lit")
sonnet_base_int_k <- sum(sonnet_base$subagent_safety_interrupts > 0)
sonnet_lit_int_k  <- sum(sonnet_lit$subagent_safety_interrupts > 0)
fisher_sonnet_interrupts <- run_fisher_2x2(
  sonnet_base_int_k, nrow(sonnet_base) - sonnet_base_int_k,
  sonnet_lit_int_k, nrow(sonnet_lit) - sonnet_lit_int_k
)

google_runs <- runs %>% filter(provider == "Google")
google_refusals_k <- sum(
  google_runs$ceo_safeguard_events > 0 |
    google_runs$subagent_safety_interrupts > 0 |
    google_runs$biosafety_refusals > 0
)

# ---------------------------------------------------------------------------
# 7. Assemble benchmark_summary_stats.csv
# ---------------------------------------------------------------------------
make_stat_row <- function(section, metric, group, s, ci_low = NA_real_, ci_high = NA_real_, p_val = NA_real_, notes = "") {
  tibble(
    section = section,
    metric = metric,
    group = group,
    n = as.integer(s$n),
    sum_or_k = s$sum,
    mean = s$mean,
    sd = s$sd,
    median = s$median,
    q25 = s$q25,
    q75 = s$q75,
    iqr = s$iqr,
    min = s$min,
    max = s$max,
    ci_low = ci_low,
    ci_high = ci_high,
    p_value = p_val,
    notes = notes
  )
}

summary_rows <- bind_rows(
  make_stat_row("overall_recall", "causal_nodes_out_of_7", "N36_primary", overall_nodes),
  make_stat_row("overall_recall", "causal_node_recall_pct", "N36_primary", overall_pct),
  tibble(
    section = "overall_recall", metric = "perfect_7of7_runs", group = "N36_primary",
    n = nrow(runs), sum_or_k = n_7of7, mean = 100 * ci_7of7$prop,
    sd = NA_real_, median = NA_real_, q25 = NA_real_, q75 = NA_real_, iqr = NA_real_,
    min = NA_real_, max = NA_real_, ci_low = 100 * ci_7of7$conf_low, ci_high = 100 * ci_7of7$conf_high,
    p_value = NA_real_, notes = "Wilson 95% CI in pct"
  ),
  make_stat_row("overall_recall", "causal_nodes_out_of_7", "all37_completed", all37_nodes),
  make_stat_row("overall_recall", "causal_node_recall_pct", "all37_completed", all37_pct),
  # Matched comparisons
  tibble(
    section = "matched_comparisons", metric = "cold_vs_seeded_causal_recall_nodes", group = "N36_Cold(18)_vs_Seeded(18)",
    n = 36L, sum_or_k = NA_real_,
    mean = mean(cold_runs$causal_node_score_sum) - mean(seeded_runs$causal_node_score_sum),
    sd = NA_real_, median = wilcox_cold_seeded_recall$median_x - wilcox_cold_seeded_recall$median_y,
    q25 = NA_real_, q75 = NA_real_, iqr = NA_real_, min = NA_real_, max = NA_real_,
    ci_low = NA_real_, ci_high = NA_real_, p_value = wilcox_cold_seeded_recall$p_value,
    notes = sprintf("Cold mean=%.3f (med=%.1f) vs Seeded mean=%.3f (med=%.1f)",
                    mean(cold_runs$causal_node_score_sum), wilcox_cold_seeded_recall$median_x,
                    mean(seeded_runs$causal_node_score_sum), wilcox_cold_seeded_recall$median_y)
  ),
  tibble(
    section = "matched_comparisons", metric = "cold_vs_seeded_unexpressed_hits_rate", group = "N36_Cold(18)_vs_Seeded(18)",
    n = 36L, sum_or_k = cold_unexpr_k + seeded_unexpr_k,
    mean = 100 * cold_unexpr_k / nrow(cold_runs), sd = NA_real_, median = 100 * seeded_unexpr_k / nrow(seeded_runs),
    q25 = NA_real_, q75 = NA_real_, iqr = NA_real_, min = NA_real_, max = NA_real_,
    ci_low = fisher_unexpr_n36$conf_low, ci_high = fisher_unexpr_n36$conf_high, p_value = fisher_unexpr_n36$p_value,
    notes = sprintf("Cold %d/18 vs Seeded %d/18 (two-sided Fisher OR=%.2f)", cold_unexpr_k, seeded_unexpr_k, fisher_unexpr_n36$odds_ratio)
  ),
  tibble(
    section = "matched_comparisons", metric = "cold_vs_seeded_unexpressed_hits_rate_base24", group = "Base24_Cold(12)_vs_Seeded(12)",
    n = 24L, sum_or_k = base_cold_unexpr_k + base_seeded_unexpr_k,
    mean = 100 * base_cold_unexpr_k / nrow(base_cold), sd = NA_real_, median = 100 * base_seeded_unexpr_k / nrow(base_seeded),
    q25 = NA_real_, q75 = NA_real_, iqr = NA_real_, min = NA_real_, max = NA_real_,
    ci_low = fisher_unexpr_base24$conf_low, ci_high = fisher_unexpr_base24$conf_high, p_value = fisher_unexpr_base24$p_value,
    notes = sprintf("Base Cold %d/12 vs Base Seeded %d/12 (two-sided Fisher OR=%.2f)", base_cold_unexpr_k, base_seeded_unexpr_k, fisher_unexpr_base24$odds_ratio)
  ),
  tibble(
    section = "matched_comparisons", metric = "lit_vs_matched_base_causal_recall_nodes", group = "Sonnet_Hydrogen_+Lit(12)_vs_Base(12)",
    n = 24L, sum_or_k = NA_real_,
    mean = mean(lit_runs$causal_node_score_sum) - mean(matched_base_runs$causal_node_score_sum),
    sd = NA_real_, median = wilcox_lit_vs_base_recall$median_x - wilcox_lit_vs_base_recall$median_y,
    q25 = NA_real_, q75 = NA_real_, iqr = NA_real_, min = NA_real_, max = NA_real_,
    ci_low = NA_real_, ci_high = NA_real_, p_value = wilcox_lit_vs_base_recall$p_value,
    notes = sprintf("+Lit mean=%.3f vs matched Base mean=%.3f; N6 %d/12 vs %d/12 (Fisher P=%.4f)",
                    mean(lit_runs$causal_node_score_sum), mean(matched_base_runs$causal_node_score_sum),
                    lit_n6_k, base_n6_k, fisher_lit_vs_base_n6$p_value)
  ),
  tibble(
    section = "matched_comparisons", metric = "lit_vs_matched_base_wall_clock_min", group = "Sonnet_Hydrogen_+Lit(12)_vs_Base(12)",
    n = 24L, sum_or_k = NA_real_,
    mean = mean(lit_runs$wall_clock_min) - mean(matched_base_runs$wall_clock_min),
    sd = NA_real_, median = wilcox_lit_vs_base_runtime$median_x - wilcox_lit_vs_base_runtime$median_y,
    q25 = NA_real_, q75 = NA_real_, iqr = NA_real_, min = NA_real_, max = NA_real_,
    ci_low = NA_real_, ci_high = NA_real_, p_value = wilcox_lit_vs_base_runtime$p_value,
    notes = sprintf("+Lit med=%.2f (mean=%.2f) min vs Base med=%.2f (mean=%.2f) min",
                    median(lit_runs$wall_clock_min), mean(lit_runs$wall_clock_min),
                    median(matched_base_runs$wall_clock_min), mean(matched_base_runs$wall_clock_min))
  ),
  # Tier 4 discards
  make_stat_row("tier4_falsification", "n_confirmed_findings", "N36_primary", confirmed_all36),
  make_stat_row("tier4_falsification", "n_discarded_hypotheses_all36", "N36_primary", discard_all36),
  make_stat_row("tier4_falsification", "discard_rate_pct_all36", "N36_primary", discard_pct_36),
  # Telemetry
  make_stat_row("telemetry", "wall_clock_min", "N36_primary", wall_stats),
  make_stat_row("telemetry", "slurm_job_union_min", "N36_primary", slurm_min_stats),
  make_stat_row("telemetry", "slurm_pct_of_wall_clock", "N36_primary", slurm_pct_stats),
  make_stat_row("telemetry", "slurm_queue_union_min", "N36_primary", queue_stats),
  make_stat_row("telemetry", "active_agent_min", "N36_primary", active_stats),
  make_stat_row("telemetry", "n_slurm_jobs", "N36_primary", jobs_stats),
  make_stat_row("telemetry", "n_scripts_executed", "N36_primary", scripts_stats),
  make_stat_row("telemetry", "n_sbatch_scripts", "N36_primary", sbatch_stats),
  make_stat_row("telemetry", "n_unique_figures", "N36_primary", figs_stats),
  # Anthropic tokens
  make_stat_row("anthropic_tokens", "tokens_input_uncached", "Anthropic_N18", tok_uncached),
  make_stat_row("anthropic_tokens", "tokens_cache_read", "Anthropic_N18", tok_cache_rd),
  make_stat_row("anthropic_tokens", "tokens_cache_write", "Anthropic_N18", tok_cache_wr),
  make_stat_row("anthropic_tokens", "tokens_output", "Anthropic_N18", tok_output),
  make_stat_row("anthropic_tokens", "tokens_reasoning", "Anthropic_N18", tok_reason),
  make_stat_row("anthropic_tokens", "tokens_total_noncache", "Anthropic_N18", tok_noncache),
  # Bio-safety
  tibble(
    section = "biosafety", metric = "opus_5_5_ceo_fallback_rate", group = "Opus_5_5_N6",
    n = opus_fallback_n, sum_or_k = opus_fallback_k, mean = 100 * opus_fallback_k / opus_fallback_n,
    sd = NA_real_, median = NA_real_, q25 = NA_real_, q75 = NA_real_, iqr = NA_real_,
    min = NA_real_, max = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_value = NA_real_,
    notes = sprintf("%d/%d Opus 5.5 runs fell back to Claude Opus 5.0", opus_fallback_k, opus_fallback_n)
  ),
  tibble(
    section = "biosafety", metric = "sonnet_base_vs_lit_subagent_interrupts", group = "Sonnet_Base(6)_vs_+Lit(6)",
    n = 12L, sum_or_k = sonnet_base_int_k + sonnet_lit_int_k,
    mean = 100 * sonnet_base_int_k / nrow(sonnet_base), sd = NA_real_,
    median = 100 * sonnet_lit_int_k / nrow(sonnet_lit),
    q25 = NA_real_, q75 = NA_real_, iqr = NA_real_, min = NA_real_, max = NA_real_,
    ci_low = fisher_sonnet_interrupts$conf_low, ci_high = fisher_sonnet_interrupts$conf_high,
    p_value = fisher_sonnet_interrupts$p_value,
    notes = sprintf("Base %d/6 vs +Lit %d/6 runs with >=1 subagent interrupt (Fisher P=%.5f)",
                    sonnet_base_int_k, sonnet_lit_int_k, fisher_sonnet_interrupts$p_value)
  ),
  tibble(
    section = "biosafety", metric = "google_biosafety_refusals", group = "Google_N18",
    n = nrow(google_runs), sum_or_k = google_refusals_k, mean = 100 * google_refusals_k / nrow(google_runs),
    sd = NA_real_, median = NA_real_, q25 = NA_real_, q75 = NA_real_, iqr = NA_real_,
    min = NA_real_, max = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_value = NA_real_,
    notes = sprintf("%d/%d Google Gemini 4 runs had any CEO/subagent safety refusal", google_refusals_k, nrow(google_runs))
  )
)

write_csv(summary_rows, file.path(path_tables, "benchmark_summary_stats.csv"))

# ---------------------------------------------------------------------------
# 8. Write key_manuscript_numbers.json
# ---------------------------------------------------------------------------
key_json <- list(
  cohort = list(
    primary_N = nrow(runs),
    all_completed_N = nrow(sc37),
    n_base = sum(runs$lit_arm == "Base"),
    n_lit = sum(runs$lit_arm == "+Lit"),
    n_cold = nrow(cold_runs),
    n_seeded = nrow(seeded_runs),
    n_models_executed = dplyr::n_distinct(runs$actual_ceo_model),
    n_providers = dplyr::n_distinct(runs$provider),
    all_completed_status = all(runs$status == "Completed")
  ),
  overall_causal_recall_N36 = list(
    mean_nodes = round(overall_nodes$mean, 4),
    sd_nodes = round(overall_nodes$sd, 4),
    median_nodes = overall_nodes$median,
    min_nodes = overall_nodes$min,
    max_nodes = overall_nodes$max,
    mean_pct = round(overall_pct$mean, 2),
    sd_pct = round(overall_pct$sd, 2),
    median_pct = overall_pct$median,
    min_pct = overall_pct$min,
    max_pct = overall_pct$max,
    n_7of7_runs = n_7of7,
    pct_7of7_runs = round(100 * ci_7of7$prop, 2),
    ci95_7of7_pct = c(round(100 * ci_7of7$conf_low, 2), round(100 * ci_7of7$conf_high, 2))
  ),
  overall_causal_recall_all37 = list(
    mean_nodes = round(all37_nodes$mean, 4),
    mean_pct = round(all37_pct$mean, 2),
    n_7of7_runs = n_7of7_37
  ),
  recall_by_model = purrr::pmap(
    by_model,
    function(...) {
      r <- list(...)
      list(
        model = r$actual_ceo_model,
        model_label = r$model_label,
        n_runs = r$n_runs,
        mean_nodes = round(r$mean_nodes, 3),
        sd_nodes = round(r$sd_nodes, 3),
        median_nodes = r$median_nodes,
        min_nodes = r$min_nodes,
        max_nodes = r$max_nodes,
        mean_recall_pct = round(r$mean_recall_pct, 2),
        n_7of7 = r$n_7of7,
        n_n6_recovered = r$n_n6_recovered,
        n_fp1_6_triggered = r$n_fp1_6_triggered,
        n_fp3_refined = r$n_fp3_refined,
        n_fp7_triggered = r$n_fp7_triggered,
        median_wall_min = round(r$median_wall_min, 2)
      )
    }
  ),
  matched_comparisons = list(
    cold_vs_seeded_N36 = list(
      cold_n = nrow(cold_runs),
      seeded_n = nrow(seeded_runs),
      cold_mean_nodes = round(mean(cold_runs$causal_node_score_sum), 4),
      cold_sd_nodes = round(sd(cold_runs$causal_node_score_sum), 4),
      cold_median_nodes = median(cold_runs$causal_node_score_sum),
      seeded_mean_nodes = round(mean(seeded_runs$causal_node_score_sum), 4),
      seeded_sd_nodes = round(sd(seeded_runs$causal_node_score_sum), 4),
      seeded_median_nodes = median(seeded_runs$causal_node_score_sum),
      recall_wilcoxon_p = wilcox_cold_seeded_recall$p_value,
      cold_unexpressed_hits_k = cold_unexpr_k,
      cold_unexpressed_hits_pct = round(100 * cold_unexpr_k / nrow(cold_runs), 2),
      seeded_unexpressed_hits_k = seeded_unexpr_k,
      seeded_unexpressed_hits_pct = round(100 * seeded_unexpr_k / nrow(seeded_runs), 2),
      unexpressed_fisher_p = fisher_unexpr_n36$p_value
    ),
    cold_vs_seeded_base_N24 = list(
      base_cold_n = nrow(base_cold),
      base_seeded_n = nrow(base_seeded),
      base_cold_mean_nodes = round(mean(base_cold$causal_node_score_sum), 4),
      base_seeded_mean_nodes = round(mean(base_seeded$causal_node_score_sum), 4),
      recall_wilcoxon_p = wilcox_base_cold_seeded_recall$p_value,
      base_cold_unexpressed_k = base_cold_unexpr_k,
      base_seeded_unexpressed_k = base_seeded_unexpr_k,
      unexpressed_fisher_p = fisher_unexpr_base24$p_value
    ),
    lit_vs_matched_base_N24 = list(
      models = c("Claude Sonnet 5.5", "Gemini 4 Hydrogen"),
      lit_n = nrow(lit_runs),
      base_n = nrow(matched_base_runs),
      lit_mean_nodes = round(mean(lit_runs$causal_node_score_sum), 4),
      base_mean_nodes = round(mean(matched_base_runs$causal_node_score_sum), 4),
      recall_wilcoxon_p = wilcox_lit_vs_base_recall$p_value,
      lit_n6_k = lit_n6_k,
      base_n6_k = base_n6_k,
      n6_fisher_p = fisher_lit_vs_base_n6$p_value,
      lit_median_wall_min = round(median(lit_runs$wall_clock_min), 2),
      lit_mean_wall_min = round(mean(lit_runs$wall_clock_min), 2),
      base_median_wall_min = round(median(matched_base_runs$wall_clock_min), 2),
      base_mean_wall_min = round(mean(matched_base_runs$wall_clock_min), 2),
      runtime_wilcoxon_p = wilcox_lit_vs_base_runtime$p_value
    )
  ),
  per_node_recovery_N36 = setNames(
    purrr::pmap(node_rows, function(...) {
      r <- list(...)
      list(
        n_recovered = r$n_recovered_or_triggered,
        n_total = r$n_runs,
        pct = round(r$pct_recovered_or_triggered, 2),
        E1 = r$E1_count,
        E2 = r$E2_count,
        E3 = r$E3_count,
        miss = r$Miss_count,
        tier_summary = r$tier_summary
      )
    }),
    node_rows$item_id
  ),
  per_fp_trap_N36 = setNames(
    purrr::pmap(fp_rows, function(...) {
      r <- list(...)
      list(
        clean = r$Clean_count,
        caught_in_discard_log = r$Caught_in_Discard_Log_count,
        triggered_in_final = r$Triggered_in_Final_count,
        refined_by_data = r$Refined_by_Data_count
      )
    }),
    fp_rows$item_id
  ),
  tier4_discard_stats = list(
    n_runs_all = nrow(runs),
    n_runs_zero_discard_log = sum(runs$n_discarded_hypotheses == 0),
    n_discarded_min_all36 = discard_all36$min,
    n_discarded_median_all36 = discard_all36$median,
    n_discarded_max_all36 = discard_all36$max,
    discard_rate_min_pct_all36 = round(discard_pct_36$min, 2),
    discard_rate_median_pct_all36 = round(discard_pct_36$median, 2),
    discard_rate_max_pct_all36 = round(discard_pct_36$max, 2)
  ),
  telemetry_N36 = list(
    wall_clock_min = list(
      min = round(wall_stats$min, 2),
      q25 = round(wall_stats$q25, 2),
      median = round(wall_stats$median, 2),
      q75 = round(wall_stats$q75, 2),
      iqr = round(wall_stats$iqr, 2),
      max = round(wall_stats$max, 2),
      mean = round(wall_stats$mean, 2),
      sd = round(wall_stats$sd, 2)
    ),
    slurm_jobs = list(
      total = jobs_stats$sum,
      min = jobs_stats$min,
      median = jobs_stats$median,
      max = jobs_stats$max
    ),
    slurm_pct_of_wall = list(
      min = round(slurm_pct_stats$min, 2),
      median = round(slurm_pct_stats$median, 2),
      max = round(slurm_pct_stats$max, 2),
      mean = round(slurm_pct_stats$mean, 2)
    ),
    scripts_executed = list(
      total = scripts_stats$sum,
      min = scripts_stats$min,
      median = scripts_stats$median,
      max = scripts_stats$max
    ),
    anthropic_tokens_N18 = list(
      n_runs = tok_noncache$n,
      total_cache_read = tok_cache_rd$sum,
      median_cache_read = tok_cache_rd$median,
      total_cache_write = tok_cache_wr$sum,
      total_output = tok_output$sum,
      median_output = tok_output$median,
      total_noncache = tok_noncache$sum,
      median_noncache = tok_noncache$median,
      min_noncache = tok_noncache$min,
      max_noncache = tok_noncache$max,
      estimated_usd_cost_nonmissing_runs = n_cost_nonna
    ),
    biosafety = list(
      opus_5_5_fallback_k = opus_fallback_k,
      opus_5_5_total_n = opus_fallback_n,
      sonnet_base_interrupt_k = sonnet_base_int_k,
      sonnet_base_n = nrow(sonnet_base),
      sonnet_lit_interrupt_k = sonnet_lit_int_k,
      sonnet_lit_n = nrow(sonnet_lit),
      sonnet_interrupts_fisher_p = fisher_sonnet_interrupts$p_value,
      google_refusals_k = google_refusals_k,
      google_total_n = nrow(google_runs)
    )
  ),
  beyond_rubric_discoveries = list(
    n_recurring = nrow(disc36),
    universal_100pct = disc36 %>% filter(n_runs_N36 == 36) %>% pull(discovery_key)
  )
)

write_json(
  key_json,
  file.path(path_tables, "key_manuscript_numbers.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = 8
)

cat("[01_benchmark_summary_stats.R] Completed cleanly.\n")
cat(sprintf("  N=36 mean recall: %.3f/7 (%.1f%%), 7/7 runs: %d/36\n",
            overall_nodes$mean, overall_pct$mean, n_7of7))
cat(sprintf("  Cold vs Seeded unexpressed hits: %d/18 vs %d/18 (Fisher P=%.5f)\n",
            cold_unexpr_k, seeded_unexpr_k, fisher_unexpr_n36$p_value))
cat(sprintf("  Base N=24 Cold vs Seeded unexpressed hits: %d/12 vs %d/12 (Fisher P=%.5f)\n",
            base_cold_unexpr_k, base_seeded_unexpr_k, fisher_unexpr_base24$p_value))
cat(sprintf("  Sonnet Base vs +Lit interrupts: %d/6 vs %d/6 (Fisher P=%.5f)\n",
            sonnet_base_int_k, sonnet_lit_int_k, fisher_sonnet_interrupts$p_value))
