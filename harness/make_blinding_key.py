"""Assign blinded run IDs and write the empty adjudication sheet.

Writes two files next to this script:
  blinding_key.csv          run_id -> provider, model, condition, replicate, run path
                            (keep away from adjudicators)
  adjudication_blinded.csv  three rows per run_id: adjudicator_1, adjudicator_2, consensus

Refuses to overwrite existing files, so IDs stay stable once assigned.
"""

import csv
import random
import re
import sys
from pathlib import Path

seed = 20261119
path_eval = Path(__file__).resolve().parent
path_runs = path_eval.parents[1] / "runs"
path_key = path_eval / "blinding_key.csv"
path_sheet = path_eval / "adjudication_blinded.csv"

run_pattern = re.compile(r"^(?P<condition>seeded|cold|seeded_lit|cold_lit)_run_(?P<replicate>\d{2})$")
readers = ["adjudicator_1", "adjudicator_2", "consensus"]
sheet_columns = (
    ["run_id", "reader", "reader_initials"]
    + [f"N{i}" for i in range(1, 8)]
    + [f"N{i}_tier" for i in range(1, 8)]
    + [f"FP{i}" for i in range(1, 8)]
    + [
        "QC_caught_FP",
        "nonchain_confirmed",
        "nonchain_refuted",
        "nonchain_not_testable",
        "nonchain_prior_analysis",
        "novel_E1_E3_count",
        "notes",
    ]
)


def list_runs(runs_root):
    runs = []
    for model_dir in sorted(p for p in runs_root.iterdir() if p.is_dir()):
        provider = model_dir.name.split("_", 1)[0]
        for run_dir in sorted(p for p in model_dir.iterdir() if p.is_dir()):
            match = run_pattern.match(run_dir.name)
            if match is None:
                print(f"skipping unrecognized run folder: {run_dir}", file=sys.stderr)
                continue
            runs.append(
                {
                    "provider": provider,
                    "model": model_dir.name,
                    "condition": match["condition"],
                    "replicate": match["replicate"],
                    "run_path": str(run_dir.relative_to(runs_root.parent)),
                }
            )
    return runs


if __name__ == "__main__":
    for existing in (path_key, path_sheet):
        if existing.exists():
            sys.exit(f"{existing} already exists; refusing to reassign blinded IDs")

    runs = list_runs(path_runs)
    random.Random(seed).shuffle(runs)
    width = max(3, len(str(len(runs))))
    for i, run in enumerate(runs, start=1):
        run["run_id"] = f"RUN_{i:0{width}d}"

    with path_key.open("w", newline="") as handle:
        writer = csv.DictWriter(
            handle, fieldnames=["run_id", "provider", "model", "condition", "replicate", "run_path"]
        )
        writer.writeheader()
        writer.writerows(sorted(runs, key=lambda r: r["run_id"]))

    with path_sheet.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=sheet_columns)
        writer.writeheader()
        for run in sorted(runs, key=lambda r: r["run_id"]):
            for reader in readers:
                writer.writerow({"run_id": run["run_id"], "reader": reader})

    print(f"assigned {len(runs)} blinded IDs (seed {seed})")
