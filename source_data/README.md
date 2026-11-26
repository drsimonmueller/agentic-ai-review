# Source Data (`agentic-ai-review`)

This directory contains the compact, self-contained, SHA-256 checksum-verified processed source datasets required to reproduce every analysis table, main figure (**Figures 1–4**), main table (**Table 1**), Extended Data figure (**Extended Data Figures 1–5**), and Supplementary Table (**Supplementary Tables S1–S4**) via `./run_all.sh`.

Raw sequencing reads (`ENA PRJEB60776`), full Montero et al. supplementary tables (`Nat. Methods` 2024; DOI: `10.1038/s41592-024-02190-0`), and all 36 autonomous peer-review reports + figures (browsable in `docs/` via GitHub Pages at https://drsimonmueller.github.io/agentic-ai-review/) underlie these compact source-data tables, which are tracked directly in this GitHub repository.

---

## Directory Contents

### 1. `source_data/benchmark/`
- `run_inventory_N36.csv`: Run completion status, artifact presence flags, safety event counts, and metadata for the finalized `N = 36` balanced first-submission benchmark cohort (`24` Base runs across 4 models + `12` matched `+Literature` ablation runs on `Claude Sonnet 5.5` and `Gemini 4 Hydrogen`).
- `run_inventory_48grid.csv`: Full 48-slot provision matrix across the 4 executed Anthropic and Google models (`4 models × 4 arms × k = 3`), marking `in_primary_N36` and completion state.
- `run_telemetry_N36.csv`: Independent CLI session JSONL and Slurm `sacct` runtime, queue wait, token/cost (Anthropic), script/job footprint, and bio-safety fallback/interrupt metrics for `N = 36`.
- `run_telemetry_all37.csv`: Same telemetry table across all `37` completed clean-room runs (including extra `google_gemini_4_neon/seeded_lit_run_01`).
- `scored_rubric_N36.csv`: Strict binary causal-node (`N1`–`N7`, evidence tiers `E1`–`E3`) and false-positive trap (`FP1`–`FP7`, including `+E2` `Refined_by_Data` on `FP3`) scorecard for the finalized `N = 36` cohort.
- `scored_rubric_all37.csv`: Complete `37`-run scorecard including `in_primary_N36` flag.
- `beyond_rubric_discoveries_N36.csv`: Aggregate run counts, model counts, and arm breakdowns (`Base` vs `+Lit`, `Cold` vs `Seeded`) for all `19` recurring beyond-rubric discoveries across `N = 36`.
- `frozen_rubric.yaml`: Pre-registered reference causal-node and false-positive trap definitions.

### 2. `source_data/montero_forensics/`
- `albarossa_arrays_qc.tsv.gz`: All `78,125` Albarossa dual-gRNA arrays annotated with `T≥4` Pol III termination flags (`core23`, `ext7`, `any`), unidirectional/bidirectional/junctional `BsmBI` sites, plasmid read counts (`<30` dropouts), `is_clean_array`, and 6-line `CasRx` / `woCasRx` `log2` fold changes (basis for **Supplementary Table S4** for Addgene `#212966` users).
- `ae_recall_vs_depmap_6lines.csv`: Per-cell-line (`LN-18`, `LN-229`, `A549`, `NCI-H460`, `KP-4`, `MIA PaCa-2`) and union-of-6 CasRx always-essential (`AE`) recall (`FDR < 0.25` and `< 0.05`) versus DepMap `22Q2` Cas9, `NE` false positives (`0/299`), and called essential lncRNAs.
- `plasmid_chimerism_and_bsmbi.csv`: Raw ENA `PRJEB60776` paired-end alignment reconciliation (`ERR12330401` Albarossa plasmid vs `ERR12330450` `ControlLibrary` and screen gDNA samples), recombination-adjusted effective cellular coverage, and `BsmBI` molecular chronometer counts.
- `mageck_rescue_and_rra_cliff.csv`: Baseline vs naive-filtered vs calibrated `--control-sgrna NE` MAGeCK RRA recall, `120` live synthetic `RRA v0.5.9` power simulations (`k = 2–7` arrays/gene), empirical `k = 0–3` active-array recall curve, and power-matched comparisons.
- `residual_ae_predictors.csv`: Clean (`T≥4`-free, `BsmBI`-free) `AE` array depletion across target expression quartiles, `3′UTR` vs `CDS` region, `NLS` nuclear localization, null `RNAfold`/`Cas13design` predictors, and `GroupKFold` `R²`.
- `lncrna_expression_and_collateral.csv`: Per-line expression audit of the `206` called essential lncRNAs, silent `Albarossa` vs expressed `controls_library` `NE` distributions, and target-RNA-dependent `CasRx` vs `woCasRx` collateral depletion slopes across CPM bins.
- `albarossa_arrays_qc_readme_stats.json`: Machine-readable integrity summary of key reference-audit counts and effect sizes.

---

## Checksum Verification

```bash
(cd source_data && sha256sum -c CHECKSUMS.sha256)
```
