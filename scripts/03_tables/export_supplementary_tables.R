#!/usr/bin/env Rscript
# Export clean, documented Supplementary Tables S1-S4, data dictionary, and
# verified manifest into outputs/supplementary_tables/.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(purrr)
})

repo_root <- Sys.getenv("AGENTIC_REPO_ROOT", unset = ".")
source(file.path(repo_root, "config", "paths.R"))
source(file.path(repo_root, "R", "colors.R"))
source(file.path(repo_root, "R", "theme.R"))
source(file.path(repo_root, "R", "stats_utils.R"))
source(file.path(repo_root, "R", "plot_utils.R"))

ensure_dir(path_supp_tables)
set.seed(GLOBAL_SEED)

# ---------------------------------------------------------------------------
# 1. Load source_data inputs
# ---------------------------------------------------------------------------
p_inv36  <- require_file(file.path(path_sd_benchmark, "run_inventory_N36.csv"))
p_tel36  <- require_file(file.path(path_sd_benchmark, "run_telemetry_N36.csv"))
p_sc36   <- require_file(file.path(path_sd_benchmark, "scored_rubric_N36.csv"))
p_sc37   <- require_file(file.path(path_sd_benchmark, "scored_rubric_all37.csv"))
p_disc36 <- require_file(file.path(path_sd_benchmark, "beyond_rubric_discoveries_N36.csv"))
p_alb    <- require_file(file.path(path_sd_forensics, "albarossa_arrays_qc.tsv.gz"))

inv36  <- read_csv(p_inv36, show_col_types = FALSE)
tel36  <- read_csv(p_tel36, show_col_types = FALSE)
sc36   <- read_csv(p_sc36, show_col_types = FALSE)
sc37   <- read_csv(p_sc37, show_col_types = FALSE)
disc36 <- read_csv(p_disc36, show_col_types = FALSE)
alb    <- read_tsv(p_alb, show_col_types = FALSE)

stopifnot(nrow(inv36) == 36L, nrow(tel36) == 36L, nrow(sc36) == 36L,
          nrow(sc37) == 37L, nrow(disc36) == 19L, nrow(alb) == MONTERO_N_ARRAYS)

model_order <- c("claude_opus_5_5", "claude_sonnet_5_5", "google_gemini_4_neon", "google_gemini_4_hydrogen")

# ---------------------------------------------------------------------------
# 2. Supplementary Table S1: Run inventory and telemetry (N = 36)
# ---------------------------------------------------------------------------
supp_s1 <- inv36 %>%
  select(
    provider, model_slug, model_label, actual_ceo_model, ceo_biosafety_fallback,
    condition, replicate, run_path, status,
    has_manifest, has_audit_plan, has_findings_json, has_referee_report,
    has_discard_log, has_slurm_jobs_csv, has_model_routing_csv,
    n_tables, status_note, in_primary_N36
  ) %>%
  left_join(
    tel36 %>% select(
      run_path, runtime_source, wall_clock_min, slurm_job_union_min,
      slurm_queue_union_min, active_agent_min, n_slurm_jobs,
      tokens_input_uncached, tokens_cache_read, tokens_cache_write,
      tokens_output, tokens_reasoning, tokens_total_noncache, estimated_usd_cost,
      n_scripts_executed, n_sbatch_scripts, n_unique_figures, n_figure_files,
      n_confirmed_findings, n_discarded_hypotheses,
      ceo_safeguard_events, subagent_safety_interrupts, biosafety_refusals, model_fallbacks
    ),
    by = "run_path"
  ) %>%
  mutate(
    prompt_framing = if_else(condition %in% c("cold", "cold_lit"), "Cold", "Seeded"),
    literature_arm = if_else(condition %in% c("seeded_lit", "cold_lit"), "+Literature", "Base"),
    slurm_pct_of_wall = round(100 * slurm_job_union_min / wall_clock_min, 2),
    total_hypotheses = n_confirmed_findings + n_discarded_hypotheses,
    discard_rate_pct = if_else(total_hypotheses > 0, round(100 * n_discarded_hypotheses / total_hypotheses, 2), NA_real_)
  ) %>%
  arrange(
    match(model_slug, model_order),
    match(condition, BENCHMARK_ARMS),
    replicate
  )

out_s1 <- file.path(path_supp_tables, "supplementary_table_01_run_inventory_and_telemetry.csv")
write_csv(supp_s1, out_s1)

# ---------------------------------------------------------------------------
# 3. Supplementary Table S2: Rubric scores (N = 36 + all37 companion)
# ---------------------------------------------------------------------------
supp_s2 <- sc36 %>%
  arrange(
    match(model_slug, model_order),
    match(condition, BENCHMARK_ARMS),
    replicate
  )

supp_s2b <- sc37 %>%
  arrange(
    desc(in_primary_N36),
    match(model_slug, model_order),
    match(condition, BENCHMARK_ARMS),
    replicate
  )

out_s2  <- file.path(path_supp_tables, "supplementary_table_02_rubric_scores_N36.csv")
out_s2b <- file.path(path_supp_tables, "supplementary_table_02b_rubric_scores_all37.csv")
write_csv(supp_s2, out_s2)
write_csv(supp_s2b, out_s2b)

# ---------------------------------------------------------------------------
# 4. Supplementary Table S3: Beyond-rubric discoveries (19 discoveries, N = 36)
# ---------------------------------------------------------------------------
supp_s3 <- disc36 %>%
  arrange(desc(n_runs_N36), desc(n_models), discovery_key)

out_s3 <- file.path(path_supp_tables, "supplementary_table_03_beyond_rubric_discoveries.csv")
write_csv(supp_s3, out_s3)

# ---------------------------------------------------------------------------
# 5. Supplementary Table S4: Curated QC-annotated Albarossa 78,125 arrays
# ---------------------------------------------------------------------------
supp_s4 <- alb %>%
  mutate(
    defect_flags = case_when(
      is_clean_array == 1L ~ "clean",
      has_T4_any == 1L & has_bsmbi_rev_or_junc == 1L ~ "T>=4;BsmBI_site",
      has_T4_any == 1L & is_plasmid_dropout_lt30 == 1L ~ "T>=4;plasmid_dropout_lt30",
      has_T4_any == 1L ~ "T>=4",
      has_bsmbi_rev_or_junc == 1L ~ "BsmBI_site",
      is_plasmid_dropout_lt30 == 1L ~ "plasmid_dropout_lt30",
      TRUE ~ "other_qc_flag"
    ),
    qc_recommendation = if_else(
      is_clean_array == 1L,
      "retain_clean_array",
      "flag_defective_array_do_not_drop_posthoc_without_power_check"
    )
  ) %>%
  select(
    array_id, gene_id, gene_symbol, category,
    qc_recommendation, defect_flags,
    has_T4_core23, has_T4_ext7, has_T4_any,
    has_bsmbi_fwd, has_bsmbi_array, has_bsmbi_rev_or_junc,
    plasmid_count, is_plasmid_dropout_lt30, is_offtarget, is_noisy_nan_pval,
    is_clean_array, is_table12_tested,
    everything()
  )

out_s4 <- file.path(path_supp_tables, "supplementary_table_04_albarossa_78125_qc_annotated.csv.gz")
write_csv(supp_s4, out_s4)

# ---------------------------------------------------------------------------
# 6. Build Data Dictionary
# ---------------------------------------------------------------------------
describe_df <- function(df, table_id, file_name, col_notes = list()) {
  tibble(
    table_id = table_id,
    file_name = file_name,
    column_name = colnames(df),
    r_class = purrr::map_chr(df, ~ paste(class(.x), collapse = "/")),
    n_rows = nrow(df),
    n_missing = purrr::map_int(df, ~ sum(is.na(.x))),
    description = purrr::map_chr(colnames(df), function(cn) {
      if (!is.null(col_notes[[cn]])) col_notes[[cn]] else paste0("Field ", cn, " from source dataset")
    })
  )
}

notes_s1 <- list(
  provider = "Model provider (Anthropic or Google)",
  model_slug = "Canonical directory slug for provisioned model",
  model_label = "Provisioned Tier 1 CEO model label",
  actual_ceo_model = "Dominant executed Tier 1 CEO model verified from CLI session logs",
  ceo_biosafety_fallback = "TRUE if Tier 1 CEO fell back via API bio-safety refusal (Opus 5.5 -> Opus 5.0)",
  condition = "2x2 factorial experimental arm (seeded, cold, seeded_lit, cold_lit)",
  replicate = "Independent replicate identifier (run_01..run_03)",
  run_path = "Relative clean-room run path",
  wall_clock_min = "Union of active agent CLI session wall-clock intervals (minutes)",
  slurm_job_union_min = "Union of Slurm job execution intervals clipped to session window (minutes)",
  slurm_queue_union_min = "Union of Slurm queue wait intervals (minutes)",
  active_agent_min = "Non-Slurm active agent reasoning/IO time (wall_clock_min - slurm_job_union_min)",
  slurm_pct_of_wall = "Percentage of wall-clock time spent inside Slurm compute jobs",
  tokens_total_noncache = "Sum of uncached input, cache write, output, and reasoning tokens (Anthropic)",
  estimated_usd_cost = "API-equivalent cost in USD (NA when model_prices.csv is unpopulated)",
  discard_rate_pct = "100 * n_discarded_hypotheses / (n_confirmed_findings + n_discarded_hypotheses)",
  in_primary_N36 = "TRUE for the balanced N = 36 primary first-submission cohort"
)

notes_s2 <- list(
  audit_status = "Scoring provenance (auto_prelim)",
  N1_score = "Binary recovery of causal node N1 (1 = Found with E1-E3, 0 = Missed)",
  N6_score = "Binary recovery of causal node N6 (recombination-adjusted effective coverage drop)",
  causal_node_score_sum = "Sum of binary scores across N1-N7 (0-7)",
  causal_node_recall_pct = "Percentage of 7 causal nodes recovered",
  FP3_status = "FP3 status (Clean, Caught_in_Discard_Log, Triggered_in_Final, or Refined_by_Data [+E2])",
  FP7_status = "FP7 editorial calibration status (Clean, Caught_in_Discard_Log, or Triggered_in_Final)",
  in_primary_N36 = "TRUE for N = 36 primary cohort; FALSE for extra 37th run (Neon seeded_lit_run_01 in S2b)"
)

notes_s3 <- list(
  discovery_key = "Canonical snake_case identifier for recurring beyond-rubric discovery",
  discovery_label = "Human-readable discovery description",
  category = "Analytical domain and evidence tier",
  n_runs_N36 = "Number of primary N = 36 runs independently reporting discovery",
  pct_of_N36 = "Percentage of N = 36 runs reporting discovery",
  n_base_24 = "Count among Base runs (n = 24)",
  n_lit_12 = "Count among +Literature runs (n = 12)",
  n_cold_18 = "Count among unseeded Cold runs (n = 18)",
  n_seeded_18 = "Count among Seeded runs (n = 18)"
)

notes_s4 <- list(
  array_id = "Albarossa dual-gRNA array identifier (78,125 total arrays; Addgene #212966)",
  gene_id = "Target gene or lncRNA locus identifier",
  gene_symbol = "Annotated gene symbol or unified lncRNA ID",
  category = "Library feature class (lncRNA, AE, NE, NT)",
  qc_recommendation = "Practical Addgene #212966 user guidance (retain clean array vs flag defective)",
  defect_flags = "Combined primary sequence/cloning/plasmid defect classification",
  has_T4_core23 = "1 if internal T>=4 Pol III terminator occurs in 23-nt CasRx core (pos 1-23)",
  has_T4_ext7 = "1 if T>=4 is created by 3' 7-nt extension (pos 24-30)",
  has_T4_any = "1 if any spacer carries internal T>=4 (15,082 / 78,125 arrays)",
  has_bsmbi_array = "1 if internal array carries CGTCTC or GAGACG BsmBI site (493 arrays)",
  has_bsmbi_rev_or_junc = "1 if full synthesized oligo carries >1 CGTCTC/GAGACG site incl. junctions (522 arrays)",
  plasmid_count = "Montero et al. Table 11 Library_rep1 plasmid read count",
  is_plasmid_dropout_lt30 = "1 if plasmid_count < 30 (642 arrays; 522 BsmBI-cleaved)",
  is_clean_array = "1 if free of T>=4, BsmBI sites, and plasmid dropout (<30 reads) (62,523 arrays)",
  casrx6_mean_lfc = "Mean log2 fold change across all 6 CasRx screens",
  wo3_mean_lfc = "Mean log2 fold change across 3 nuclease-free woCasRx control screens"
)

dict_df <- bind_rows(
  describe_df(supp_s1,  "Supplementary Table S1",  basename(out_s1),  notes_s1),
  describe_df(supp_s2,  "Supplementary Table S2",  basename(out_s2),  notes_s2),
  describe_df(supp_s2b, "Supplementary Table S2b", basename(out_s2b), notes_s2),
  describe_df(supp_s3,  "Supplementary Table S3",  basename(out_s3),  notes_s3),
  describe_df(supp_s4,  "Supplementary Table S4",  basename(out_s4),  notes_s4)
)

out_dict <- file.path(path_supp_tables, "supplementary_tables_dictionary.csv")
write_csv(dict_df, out_dict)

# ---------------------------------------------------------------------------
# 7. Build Verified Manifest with non-empty assertions
# ---------------------------------------------------------------------------
exported_specs <- list(
  list(id = "Supplementary Table S1",  path = out_s1,   df = supp_s1,  desc = "N = 36 run inventory, Slurm decomposition, tokens, and bio-safety provenance"),
  list(id = "Supplementary Table S2",  path = out_s2,   df = supp_s2,  desc = "N = 36 per-run binary causal-node (N1-N7) and FP-trap (FP1-FP7) scorecard"),
  list(id = "Supplementary Table S2b", path = out_s2b,  df = supp_s2b, desc = "All 37 completed clean-room runs scorecard (includes extra Neon seeded_lit_run_01)"),
  list(id = "Supplementary Table S3",  path = out_s3,   df = supp_s3,  desc = "19 recurring beyond-rubric discoveries across N = 36 with model and arm breakdowns"),
  list(id = "Supplementary Table S4",  path = out_s4,   df = supp_s4,  desc = "Curated QC-annotated Albarossa 78,125-array table for Addgene #212966 users"),
  list(id = "Dictionary",              path = out_dict, df = dict_df,  desc = "Column-level metadata dictionary for Supplementary Tables S1-S4")
)

manifest_df <- map_dfr(exported_specs, function(spec) {
  stopifnot(file.exists(spec$path))
  fsize <- as.numeric(file.info(spec$path)$size)
  stopifnot(is.finite(fsize), fsize > 0, nrow(spec$df) > 0, ncol(spec$df) > 0)
  tibble(
    table_id = spec$id,
    file_name = basename(spec$path),
    n_rows = nrow(spec$df),
    n_cols = ncol(spec$df),
    file_bytes = fsize,
    md5 = unname(tools::md5sum(spec$path)),
    non_empty_verified = TRUE,
    description = spec$desc
  )
})

out_manifest <- file.path(path_supp_tables, "supplementary_tables_manifest.csv")
write_csv(manifest_df, out_manifest)

cat("[export_supplementary_tables.R] Completed cleanly.\n")
print(manifest_df %>% select(table_id, file_name, n_rows, n_cols, file_bytes, non_empty_verified))
