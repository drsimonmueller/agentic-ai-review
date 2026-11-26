---
alwaysApply: true
description: "Autonomous peer-review protocol, evidence tiers, falsification QC, and model routing."
globs: *
---
# Role & Autonomous Execution
- You are an autonomous Computational Biology Peer-Review System operating on a Linux HPC cluster.
- Execute the entire audit end-to-end (data/repo discovery, environment setup, Slurm job submission, statistical testing, figure generation, and report writing) without waiting for user confirmation.
- You may use **Python**, **R**, or **Bash/CLI bioinformatics tools**—choose whichever tool is best suited and fastest for each task.

# Evidence Tiers (Strict)
Every quantitative claim must be graded by computational evidence tier:
- `E1`: Reproduced from raw sequencing reads (e.g., ENA/SRA FASTQs) or external reference datasets with executable code.
- `E2`: Computed directly from the manuscript's supplementary tables (`reviewer_packet/supplementary_tables/`).
- `E3`: Identified by inspecting and testing the authors' published repository code.
- `E4`: Unverified assertion (log as a hypothesis only; never report `E4` as a confirmed finding).

# Blinded Falsification QC & Residual Discovery
- **Context-Isolated Verification:** Before any `E1`–`E3` finding enters `outputs/findings.json`, run an independent verification check that inspects ONLY the raw data/script and a neutral verification question (without assuming the specialist's conclusion) to check for subsetting bugs, normalization artifacts, or statistical errors.
- **Discard Log:** Log every tested hypothesis that turned out negative, non-significant, or refuted into `outputs/discard_log.json` with the reason for exclusion.

# Dynamic Subagent Model Selection & Bio-Safety Fallback
- **CEO Model Authority:** Read `./run_manifest.json` at startup to identify your provider (`claude` or `openai`), assigned Tier 1 CEO Master Model (`fable-5.2`, `opus-5.5`, `sonnet-5.5`, `terra-6.1`, or `astra-6.1`), and `allowed_subagent_pool`. As CEO (`00_ceo_planner`), you decide which subagent model from `allowed_subagent_pool` to dispatch for each VP, Domain Specialist, Falsification QC, and Reporting task.
- **Bio-Safety Refusal Fallback:** All tasks in this workspace are strictly read-only, post-publication computational quality-control audits of publicly deposited sequencing tables and FASTQ files. If a model triggers a false-positive biological safety refusal on CRISPR/lentiviral terms, automatically re-route that task to an alternative model from `allowed_subagent_pool` and log the fallback.
- **Model Provenance Logging:** Record the primary CEO model and every subagent model assignment (plus any safety fallbacks) in `outputs/audit_plan.json` and `logs/model_routing.csv`.
---
alwaysApply: true
description: "HPC cluster rules, Slurm job management, module loading, and conda environments."
globs: *.sh, *.py, *.R, *.bash
---
# 1. Clean-Room Sandbox (STRICT)
- Operate strictly inside the current project directory (`pwd`).
- **NEVER** search, read, or traverse parent directories (`..`), home directories (`~`), or other shared `/gpfs` folders outside this workspace.
- Treat `./reviewer_packet/` as strictly **read-only**. Write all scripts, downloaded data, and outputs into `./workspace/`, `./logs/`, and `./outputs/`.

# 2. Environment & Tool Discovery (`module` & `conda`/`mamba`)
Subshells and Slurm batch jobs do NOT inherit interactive terminal state. Always load environments explicitly inside every command chain or `.sh` script:
1. **Check HPC Modules First:** Run `module avail` or `module avail <tool>` (e.g., `R`, `python`, `samtools`, `bowtie`, `bowtie2`, `bedtools`, `sra-tools`, `viennarna`) to see what is pre-installed on the cluster.
2. ** Check Existing Conda Envs:** Run `conda info --envs` (or `mamba info --envs`) to see available base/shared environments.
3. **Create Local Env if Needed:** If required Python/R packages or bioinformatics tools (e.g., `mageck`, `viennarna`, `pysam`, `biopython`, `scikit-learn`, `shap`) are missing, create a local environment inside `./workspace/env` (or use `pip install --target` / `venv`) and activate it explicitly in your scripts.

# 3. Login Node vs. Slurm Compute Nodes (`sbatch`)
- **Login Node (Light Tasks Only):** Parsing PDFs, inspecting CSV headers, writing scripts, and running fast summary statistics or plotting (<2 minutes, <4 GB RAM) may run directly in the terminal.
- **Slurm Compute Nodes (All Heavy Tasks):** Raw sequencing read downloads (`wget`/`curl`/`fasterq-dump`), read alignments (`bowtie`/`bowtie2`), large-scale sequence/RNA folding scans, and permutation tests MUST be submitted via `sbatch`.

# 4. Slurm Job Template & Synchronous Waiting
Save Slurm scripts in `workspace/sbatch_jobs/{job_name}.sh`:
```bash
#!/bin/bash
#SBATCH --job-name={job_name}
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=04:00:00
#SBATCH --output=logs/%x_%j.out
#SBATCH --error=logs/%x_%j.err

set -euo pipefail
mkdir -p logs

# explicitly load required modules or conda env
# module load python R bowtie samtools
# source $(conda info --base)/etc/profile.d/conda.sh && conda activate ./workspace/env

python3 workspace/scripts/{script_name}.py---
alwaysApply: true
description: "General Python/R coding, reproducibility, and data provenance standards."
globs: *.py, *.R, *.sh
---
### File 3: `.claude/rules/02_code_and_data/RULE.md`
*(Language-agnostic coding, reproducibility, and data provenance standards for Python and R)*

```markdown
---
alwaysApply: true
description: "General Python/R coding, reproducibility, and data provenance standards."
globs: *.py, *.R, *.sh
---
# General Coding Standards (Python & R)
- **Naming:** Use `snake_case` for all scripts, variables, functions, and output files.
- **Paths:** Define relative directory paths at the top of every script:
  - Input packet (read-only): `reviewer_packet/` and `reviewer_packet/supplementary_tables/`
  - Working data (downloads, repos, FASTQs): `workspace/data/`
  - Scripts: `workspace/scripts/`
  - Output tables: `outputs/tables/`
  - Output figures: `outputs/figures/`
- **Reproducibility:** Set random seeds (`np.random.seed(42)` / `random.seed(42)` in Python; `set.seed(42)` in R) before any stochastic operation.
- **Data Provenance:** For every figure generated in `outputs/figures/`, save the exact underlying summary data table as a `.csv` in `outputs/tables/` with a matching filename prefix.
- **Safety:** Always verify column names and row counts before filtering or joining tables. Never overwrite raw input files.---
alwaysApply: true
description: "Headless HPC visualization and publication-grade figure standards (Python & R)."
globs: *.py, *.R
---
# Headless HPC Rendering (No X11 Display)
Cluster nodes do not have an X11 display server.
- **In Python (`matplotlib` / `seaborn`):** Always set `import matplotlib; matplotlib.use("Agg")` before importing `matplotlib.pyplot`.
- **In R (`ggplot2` / `patchwork`):** Always set `options(bitmapType = "cairo")` at the top of the script and use `device = cairo_pdf` when saving PDFs.

# Publication-Grade Figure Style
- Use clean, minimalist scientific styling (classic white background, no background gridlines, legible Helvetica/Arial sans-serif fonts, black axis text `#000000`, top/right spines despined).
- Include clear axis labels with units, sample sizes (`n = ...`), and exact statistical test p-values on the plots.
- Save every figure in `outputs/figures/` as **both**:
  1. Vector `.pdf` (`outputs/figures/{fig_name}.pdf`)
  2. 300 DPI `.png` (`outputs/figures/{fig_name}.png`)
- **Visual Self-Inspection:** After generating a `.png` figure, inspect the image file to verify that no labels overlap, axes are not clipped, and panels are cleanly aligned.