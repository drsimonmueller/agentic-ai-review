# paths and analysis parameters

repo_root <- Sys.getenv("AGENTIC_REPO_ROOT", unset = ".")

path_source_data    <- file.path(repo_root, "source_data")
path_sd_benchmark   <- file.path(path_source_data, "benchmark")
path_sd_forensics   <- file.path(path_source_data, "montero_forensics")

path_static_assets  <- file.path(repo_root, "static_assets")
path_schematics     <- file.path(path_static_assets, "schematics")

path_harness        <- file.path(repo_root, "harness")

path_outputs        <- file.path(repo_root, "outputs")
path_tables         <- file.path(path_outputs, "tables")
path_figures        <- file.path(path_outputs, "figures")
path_supp_tables    <- file.path(path_outputs, "supplementary_tables")
path_logs           <- file.path(path_outputs, "logs")

# benchmark design parameters
BENCHMARK_MODELS      <- c(
  "claude_opus_5_5",
  "claude_sonnet_5_5",
  "google_gemini_4_neon",
  "google_gemini_4_hydrogen"
)
BENCHMARK_ARMS        <- c("seeded", "cold", "seeded_lit", "cold_lit")
BENCHMARK_REPLICATES  <- c("run_01", "run_02", "run_03")
BENCHMARK_CAUSAL_NODES <- paste0("N", 1:7)
BENCHMARK_FP_TRAPS     <- paste0("FP", 1:7)

# montero et al. primary forensics parameters
MONTERO_N_ARRAYS        <- 78125L
MONTERO_T_THRESHOLD     <- 4L
MONTERO_FDR_CUTOFF      <- 0.25
MONTERO_CELL_LINES      <- c("LN-18", "LN-229", "A549", "NCI-H460", "KP-4", "MIA PaCa-2")
MONTERO_SPACER_CORE_BP  <- 23L
MONTERO_SPACER_TOTAL_BP <- 30L

GLOBAL_SEED <- 42L

require_file <- function(path) {
  if (!file.exists(path)) {
    stop("required input file not found: ", path, call. = FALSE)
  }
  path
}

require_columns <- function(df, cols, label = "data frame") {
  missing_cols <- setdiff(cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(label, " is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(df)
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}
