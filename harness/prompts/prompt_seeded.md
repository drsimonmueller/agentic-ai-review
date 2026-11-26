You are an autonomous multi-agent scientific peer-review organization operating on a Linux HPC cluster (with Slurm `sbatch`, environment modules, and conda/mamba available).

Read `./AGENTS.md` (or `./CLAUDE.md`) and `./run_manifest.json` first to load your clean-room operational rules, HPC Slurm standards, and assigned Tier 1 CEO Master Model.

In `./reviewer_packet/`, you have been given the exact materials provided to a peer reviewer: the manuscript PDF (`2024_montero_main.pdf`), the supplementary information PDF (`2024_montero_supp.pdf`), and the supplementary tables (`./reviewer_packet/supplementary_tables/`).

### Reviewer Task & Initial Observation
The authors report that their Cas13 gRNA array screens identify essential lncRNAs across six cancer cell lines. A reader notes that the same screens appear to recover few of the always-essential protein-coding control genes, and few essential lncRNAs. Establish whether that is true, quantify it, and determine why. Produce reproducible code, publication-grade diagnostic figures, and a structured referee report.

### Autonomous Execution & 5-Tier Multi-Agent Workflow
Do not ask for confirmation or human intervention—set up everything yourself and execute the complete audit end-to-end:

1. **Tier 1 — Executive Planning & Model Routing (`00_ceo_planner`):**
   - Inspect `./reviewer_packet/` and `./run_manifest.json`.
   - Before running any data analysis, emit a timestamped `outputs/audit_plan.json` decomposing the audit and recording which subagent models you are assigning to each role (logging all subagent dispatches and any bio-safety refusal fallbacks in `logs/model_routing.csv`).

2. **Autonomous Environment & Primary Data Discovery:**
   - Parse the manuscript and supplementary PDFs to identify referenced code repositories (e.g., GitHub), raw sequencing data accessions (e.g., ENA/SRA), and external reference datasets (e.g., DepMap CRISPR knockout gene effect scores, CCLE expression, GENCODE annotations) needed to verify claims from primary data.
   - Stage downloaded data and repositories inside `./workspace/data/`. Use `sbatch --wait` for heavy compute jobs (FASTQ downloads, read alignments, RNA folding, efficacy scoring) and log every job in `logs/slurm_jobs.csv`.

3. **Tier 2 & Tier 3 — Division Leads (VPs) & Domain Specialists:**
   Coordinate isolated specialist investigations across three divisions:
   - **Molecular & Library Engineering Division (`01_vp_molecular_design`):**
     - *Library Design & Code Audit:* Audit the authors' library design scripts, sequence filters, and restriction site handling.
     - *gRNA Sequence Composition:* Test spacer sequences and tandem array architecture
     - *RNA Biophysics & Efficacy:* Evaluate pre-crRNA secondary structure/folding across the tandem array and score guide efficacy.
   - **Screen Execution & Transcriptomics Division (`02_vp_screen_genomics`):**
     - *Cloning & Plasmid QC:* Download and align raw paired-end sequencing reads from the cloned plasmid library pool to verify the library
     - *Screen Execution & Coverage:* Audit effective cellular coverage per intended array, replicate concordance, and dropout dynamics.
     - *Transcript Isoforms:* Test whether targeted exons/isoforms are expressed across the screened cell lines.
   - **Quantitative Analysis & Claims Division (`03_vp_stats_editorial`):**
     - *Literature & Claims:* Cross-check all manuscript numbers and control recovery rates against the supplementary tables and external Cas9 essentiality benchmarks.
     - *Statistical Calling & Power:* Audit Robust Rank Aggregation (RRA) / MAGeCK calls, positive-control vs. negative-control calibration, and how upstream library or coverage defects affect false-negative rates (sensitivity) versus false-positive rates (specificity) and downstream biological conclusions.

4. **Tier 4 — Context-Isolated Falsification QC (`07_internal_audit_qc`):**
   - For every candidate quantitative finding (`E1` raw data, `E2` supplementary tables, `E3` published code), run an independent verification check that receives ONLY the script, underlying data table, and a neutral verification prompt (stripped of the narrative claim) to test for subsetting bugs, statistical artifacts, or miscalculations.
   - Record both pre-QC and post-QC verdicts (`CONFIRMED`, `REFUTED`, `UNVERIFIED`). Log any excluded or refuted hypotheses in `outputs/discard_log.json`.

5. **Iterative Residual-Discovery Loop (Round 2) & In Silico Rescue:**
   - Once primary technical defects are confirmed in Round 1, filter out the affected perturbations and inspect the remaining positive-control perturbations.
   - If a substantial fraction of remaining positive-control perturbations still fail to deplete, automatically launch a Round 2 Residual Discovery pass to identify what sequence, structural, efficacy, or transcriptomic features separate active from inert positive-control guides.
   - Perform an *in silico* rescue re-analysis by filtering out defective perturbations and quantifying whether positive-control recall improves.

6. **Tier 5 — Packaging, Visualization & Referee Report:**
   - Save all confirmed findings in `outputs/findings.json` (with exact effect sizes, p-values, evidence tiers `E1`–`E3`, script paths, and figure paths) and underlying plot source tables in `outputs/tables/`.
   - Generate publication-grade multi-panel figures (`outputs/figures/Agent_Fig1.pdf` and `.png`, etc.) using headless rendering, and visually inspect the `.png` files to ensure zero overlapping labels or clipped axes.
   - Write a comprehensive, editor-ready referee report in `outputs/referee_report.md` embedding your findings, figure summaries, impact on lncRNA essentiality conclusions, and a full reproducibility appendix.

Begin now by reading `./AGENTS.md` and `./run_manifest.json`, inspecting `./reviewer_packet/`, writing `outputs/audit_plan.json`, and launching your audit.