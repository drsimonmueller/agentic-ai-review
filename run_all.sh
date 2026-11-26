#!/usr/bin/env bash
# Reproduce all core statistical tables, Main Figures 1-4, Main Table 1 (ggplot2),
# Extended Data Figures 1-5, and Supplementary Tables S1-S4 from source_data/.
# Usage:
#   ./run_all.sh
#   ./run_all.sh --skip-core
#   ./run_all.sh --only-fig fig03

set -euo pipefail

if locale -a 2>/dev/null | grep -qi "^en_US\.utf8$"; then
  export LC_ALL=en_US.utf8 LANG=en_US.utf8
elif locale -a 2>/dev/null | grep -qi "^C\.utf8$"; then
  export LC_ALL=C.utf8 LANG=C.utf8
fi

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${REPO_ROOT}"

SKIP_CORE=0
ONLY_FIG=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --skip-core)
      SKIP_CORE=1
      shift
      ;;
    --only-fig)
      ONLY_FIG="${2:-}"
      if [[ -z "${ONLY_FIG}" ]]; then
        echo "error: --only-fig requires a figure stem (e.g. fig02, ed05, table01)" >&2
        exit 1
      fi
      shift 2
      ;;
    -h|--help)
      sed -n '2,6p' "$0"
      exit 0
      ;;
    *)
      echo "error: unknown argument: $1" >&2
      exit 1
      ;;
  esac
done

# Resolve isolated Rscript inside agentic-ai-review env (with chr20-paper fallback)
env_exists() {
  local tool="$1" env_name="$2"
  "${tool}" env list 2>/dev/null | awk '{print $1}' | grep -qx "${env_name}"
}

if [[ "${CONDA_DEFAULT_ENV:-}" == "agentic-ai-review" || "${CONDA_DEFAULT_ENV:-}" == "chr20-paper" ]] && command -v Rscript >/dev/null 2>&1; then
  RUN_R=(env R_LIBS="" R_LIBS_USER="" R_ENVIRON_USER="/dev/null" AGENTIC_REPO_ROOT="${REPO_ROOT}" Rscript --vanilla)
elif command -v mamba >/dev/null 2>&1 && env_exists mamba agentic-ai-review; then
  RUN_R=(env R_LIBS="" R_LIBS_USER="" R_ENVIRON_USER="/dev/null" AGENTIC_REPO_ROOT="${REPO_ROOT}" mamba run -n agentic-ai-review Rscript --vanilla)
elif command -v mamba >/dev/null 2>&1 && env_exists mamba chr20-paper; then
  RUN_R=(env R_LIBS="" R_LIBS_USER="" R_ENVIRON_USER="/dev/null" AGENTIC_REPO_ROOT="${REPO_ROOT}" mamba run -n chr20-paper Rscript --vanilla)
elif command -v conda >/dev/null 2>&1 && env_exists conda agentic-ai-review; then
  RUN_R=(env R_LIBS="" R_LIBS_USER="" R_ENVIRON_USER="/dev/null" AGENTIC_REPO_ROOT="${REPO_ROOT}" conda run -n agentic-ai-review Rscript --vanilla)
elif command -v conda >/dev/null 2>&1 && env_exists conda chr20-paper; then
  RUN_R=(env R_LIBS="" R_LIBS_USER="" R_ENVIRON_USER="/dev/null" AGENTIC_REPO_ROOT="${REPO_ROOT}" conda run -n chr20-paper Rscript --vanilla)
else
  echo "error: agentic-ai-review env not active/found; run bash env/setup_env.sh" >&2
  exit 1
fi

mkdir -p outputs/tables outputs/figures outputs/supplementary_tables outputs/logs
RUN_LOG="outputs/run_summary.txt"
: > "${RUN_LOG}"

log() {
  local msg="[$(date '+%Y-%m-%d %H:%M:%S')] $*"
  echo "${msg}"
  echo "${msg}" >> "${RUN_LOG}"
}

run_r_step() {
  local script="$1"
  local t0 t1 dt
  t0="$(date +%s)"
  log "start  ${script}"
  "${RUN_R[@]}" "${script}"
  t1="$(date +%s)"
  dt=$((t1 - t0))
  log "ok     ${script} (${dt}s)"
}

T_START="$(date +%s)"
log "reproducing agentic-ai-review from ${REPO_ROOT}"

# Verify source data checksums
if [[ ! -f "source_data/CHECKSUMS.sha256" ]]; then
  echo "error: source_data/CHECKSUMS.sha256 not found; extract source_data/ first" >&2
  exit 1
fi
(cd source_data && sha256sum -c CHECKSUMS.sha256 --quiet)
log "ok     source_data checksums verified"

# Single-figure mode
if [[ -n "${ONLY_FIG}" ]]; then
  stem="${ONLY_FIG%.R}"
  if [[ -f "scripts/02_figures/${stem}.R" ]]; then
    run_r_step "scripts/02_figures/${stem}.R"
  elif [[ -f "scripts/03_tables/${stem}.R" ]]; then
    run_r_step "scripts/03_tables/${stem}.R"
  else
    echo "error: script not found for --only-fig ${ONLY_FIG}" >&2
    exit 1
  fi
  exit 0
fi

# Core statistical analyses
if [[ "${SKIP_CORE}" -eq 0 ]]; then
  CORE_SCRIPTS=(
    "scripts/01_core_analysis/01_benchmark_summary_stats.R"
    "scripts/01_core_analysis/02_montero_forensics_stats.R"
  )
  for s in "${CORE_SCRIPTS[@]}"; do
    run_r_step "${s}"
  done
else
  log "skip   core analysis (--skip-core)"
fi

# Main Figures (1-4) and Extended Data Figures (1-5)
FIGURE_SCRIPTS=(
  "scripts/02_figures/fig01.R"
  "scripts/02_figures/fig02.R"
  "scripts/02_figures/fig03.R"
  "scripts/02_figures/fig04.R"
  "scripts/02_figures/ed01.R"
  "scripts/02_figures/ed02.R"
  "scripts/02_figures/ed03.R"
  "scripts/02_figures/ed04.R"
  "scripts/02_figures/ed05.R"
)
for s in "${FIGURE_SCRIPTS[@]}"; do
  run_r_step "${s}"
done

# Main Text Table 1 (ggplot2) & Supplementary Tables S1-S4
run_r_step "scripts/03_tables/table01.R"
run_r_step "scripts/03_tables/export_supplementary_tables.R"

# Verify manifest outputs
python3 - <<'PY' | tee -a "${RUN_LOG}"
import csv, os, sys

missing, empty, verified = [], [], 0

with open("config/figure_manifest.csv", newline="", encoding="utf-8") as fh:
    for row in csv.DictReader(fh):
        if row["asset_type"].strip() != "scripted_headless":
            continue
        for out_path in [x.strip() for x in row["output_file"].split("|") if x.strip()]:
            if not os.path.isfile(out_path):
                missing.append((row["figure"], row["panel"], out_path))
            elif os.path.getsize(out_path) == 0:
                empty.append((row["figure"], row["panel"], out_path))
            else:
                verified += 1

supp_expected = [
    "outputs/supplementary_tables/supplementary_table_01_run_inventory_and_telemetry.csv",
    "outputs/supplementary_tables/supplementary_table_02_rubric_scores_N36.csv",
    "outputs/supplementary_tables/supplementary_table_02b_rubric_scores_all37.csv",
    "outputs/supplementary_tables/supplementary_table_03_beyond_rubric_discoveries.csv",
    "outputs/supplementary_tables/supplementary_table_04_albarossa_78125_qc_annotated.csv.gz",
    "outputs/supplementary_tables/supplementary_tables_dictionary.csv",
    "outputs/supplementary_tables/supplementary_tables_manifest.csv",
]
missing_supp = [p for p in supp_expected if not (os.path.isfile(p) and os.path.getsize(p) > 0)]

core_expected = [
    "outputs/tables/benchmark_summary_stats.csv",
    "outputs/tables/model_arm_summary.csv",
    "outputs/tables/per_node_and_fp_summary.csv",
    "outputs/tables/montero_forensics_stats.csv",
    "outputs/tables/key_manuscript_numbers.json",
    "outputs/tables/Table1_rubric_and_recovery.pdf",
]
missing_core = [p for p in core_expected if not (os.path.isfile(p) and os.path.getsize(p) > 0)]

print(
    f"verified {verified} figure/table pdfs, "
    f"{len(supp_expected) - len(missing_supp)}/{len(supp_expected)} supplementary table files, and "
    f"{len(core_expected) - len(missing_core)}/{len(core_expected)} core summary outputs"
)

if missing or empty or missing_supp or missing_core:
    for item in missing:
        print("missing figure/table:", item)
    for item in empty:
        print("empty figure/table:", item)
    for item in missing_supp:
        print("missing supp table:", item)
    for item in missing_core:
        print("missing core output:", item)
    sys.exit(1)
PY

T_END="$(date +%s)"
log "done in $((T_END - T_START))s"
