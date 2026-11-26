# Autonomous Multi-Agent Peer Review of Functional Genomics Screens

**Interactive 36-Report Archive (GitHub Pages):** https://drsimonmueller.github.io/agentic-ai-review/  
**Code & Source Data Repository:** https://github.com/drsimonmueller/agentic-ai-review

Reproducible analysis, benchmark evaluation, and publication plotting codebase for auditing pooled CRISPR-Cas13d (`CasRx`) screens directly from raw ENA sequencing reads (`PRJEB60776`), deposited supplementary tables, and analysis code across `N = 36` primary clean-room multi-agent peer reviews (`24` standard audits across `Claude Opus 5.5` [routing via bio-safety fallback to `Claude Opus 5.0`], `Claude Sonnet 5.5`, `Gemini 4 Neon`, and `Gemini 4 Hydrogen`, plus `12` matched literature-augmented ablation audits on `Claude Sonnet 5.5` and `Gemini 4 Hydrogen`, alongside `1` additional sensitivity run `Gemini 4 Neon seeded_lit_run_01`).

## Structure

- `run_all.sh` — single headless command reproducing all statistical summaries (`outputs/tables/key_manuscript_numbers.json`), **Main Table 1** (`ggplot2`), **35** vector PDFs across **Figures 1–4** and **Extended Data Figures 1–5** (individual panels + composites), and **Supplementary Tables S1–S4** from `source_data/`.
- `source_data/` — compact, checksum-verified inputs (`15` files, `3.9 MB` total, verified in `source_data/CHECKSUMS.sha256` and documented in `source_data/README.md`, tracked directly in this repository):
  - `source_data/benchmark/`: `N = 36` and `all-37` rubric scorecards, CLI/Slurm telemetry, `19` beyond-rubric discovery frequencies, and `frozen_rubric.yaml`.
  - `source_data/montero_forensics/`: QC annotation across all `78,125` Albarossa arrays, 6-line CasRx vs. DepMap `22Q2` recall, raw `ERR12330401`/`ERR12330450` chimerism + `BsmBI` molecular chronometer, `120` live MAGeCK `RRA` simulations, Round 2 residual predictors, and target-RNA collateral/expression tables.
- `harness/` — reusable autonomous multi-agent peer-review protocol: `AGENTS_hpc_slurm.md`, `AGENTS_local_workstation.md`, 2×2 benchmark prompts (`prompt_seeded.md`, `prompt_cold.md`, `prompt_seeded_lit.md`, `prompt_cold_lit.md`), `frozen_rubric.yaml`, and blinded adjudication script (`make_blinding_key.py`).
- `config/` — repository-relative paths (`paths.R`) and complete panel-to-script manifest (`figure_manifest.csv`).
- `R/` — shared gray-first 9-color palette (`colors.R`), Helvetica publication theme and Ghostscript PNG export (`theme.R`), statistical helpers (`stats_utils.R`), and plotting helpers (`plot_utils.R`).
- `scripts/01_core_analysis/` — statistical recomputation across all `N = 36` clean-room runs (`01_benchmark_summary_stats.R`) and primary Montero et al. data (`02_montero_forensics_stats.R`).
- `scripts/02_figures/` — one script per figure (`fig01.R`–`fig04.R`, `ed01.R`–`ed05.R`), including Bioconductor `ComplexHeatmap` matrices for Extended Data Figs. 2 and 5.
- `scripts/03_tables/` — pure `ggplot2` **Table 1** (`table01.R`) and verified Supplementary Tables S1–S4 export (`export_supplementary_tables.R`, including `supplementary_table_04_albarossa_78125_qc_annotated.csv.gz` for Addgene `#212966` users).
- `scripts/04_website/` — static site builder (`build_reports_website.py`) packaging all `36` verbatim autonomous referee reports, prompts, `186` agent-generated diagnostic figures (WebP), and Tier 4 blinded QC discard logs into `docs/`.
- `docs/` — self-contained **GitHub Pages** interactive supplementary archive (`docs/index.html`) for browsing, filtering, and searching all `36` primary clean-room peer reviews with full-resolution figure zoom (also works offline via `file://`).
- `env/` — self-contained Conda/Mamba environment specification (`environment.yml`, `setup_env.sh`).
- `static_assets/` — Figure 1 vector schematic (`schematics/Figure1_workflow_architecture.{svg,pdf,png}`).

## Setup and Usage

### 1. Create the Mamba environment

```bash
bash env/setup_env.sh
```

This builds the isolated `agentic-ai-review` environment (R 4.4.1, tidyverse, `patchwork`, `ComplexHeatmap`, `circlize`, `ggrepel`, `ggbeeswarm`, `openxlsx`, `yaml`, `jsonlite`, Python 3.11).

### 2. Verify `source_data/` checksums

All **15** source-data files (`3.9 MB` total) are tracked directly in `source_data/`. Verify checksums before running:

```bash
(cd source_data && sha256sum -c CHECKSUMS.sha256)
```

### 3. Run the full reproduction

```bash
./run_all.sh
```

Optional modes:

```bash
./run_all.sh --skip-core         # re-run figure and table scripts only
./run_all.sh --only-fig fig03    # re-run a single figure or table script (e.g. fig03, ed02, table01)
```

## Outputs

- **Main Table 1 (`outputs/tables/Table1_rubric_and_recovery.{pdf,png}`):** Pure `ggplot2` summary of `N1–N7`, `FP1–FP7`, and `N = 36` recovery.
- **Main Figures 1–4 (`outputs/figures/Figure_1/`–`Figure_4/`):** Individual panel PDFs/PNGs plus multi-panel composites (`Figure_2_composite.pdf`–`Figure_4_composite.pdf`).
- **Extended Data Figures 1–5 (`outputs/figures/Extended_Data_1/`–`Extended_Data_5/`):** Individual panel PDFs/PNGs plus composites (`Extended_Data_1_composite.pdf`–`Extended_Data_5_composite.pdf`).
- **Supplementary Tables S1–S4 (`outputs/supplementary_tables/`):** Run inventory & telemetry (`S1`), per-run rubric scorecards (`S2` for `N = 36`, `S2b` for all `37`), beyond-rubric discovery catalog (`S3`), curated QC-annotated list of all `78,125` Albarossa arrays for Addgene `#212966` users (`S4`), dictionary, and manifest.
- **Interactive 36-Report Archive (`docs/index.html`):** Deploy directly via **GitHub Pages** (**Settings → Pages → Deploy from branch `main` / `/docs`**) or open locally in any browser to inspect every run's executed model, verbatim prompt, diagnostic figures, full referee report, and blinded QC discard log.
