#!/usr/bin/env python3
"""Build a self-contained static website archiving all 36 autonomous peer-review
reports, verbatim prompts, agent-generated figures, and blinded QC discard logs.

Outputs to `<repo>/docs/` (GitHub Pages and local `file://` ready).
"""

from __future__ import annotations

import csv
import html
import json
import os
import re
import shutil
from pathlib import Path
from typing import Any

import mistune
from PIL import Image

REPO_ROOT = Path("/gpfs/commons/groups/sanjana_lab/smuller/papers/agentic-ai-review")
PROJ_ROOT = Path("/gpfs/commons/groups/sanjana_lab/smuller/projects/agentic_peer-review")
DOCS_DIR = REPO_ROOT / "docs"
ASSETS_FIG_DIR = DOCS_DIR / "assets" / "figures"
DATA_DIR = DOCS_DIR / "data"

PROMPT_DIR = REPO_ROOT / "harness" / "prompts"
RUBRIC_CSV = REPO_ROOT / "source_data" / "benchmark" / "scored_rubric_N36.csv"
TELEMETRY_CSV = REPO_ROOT / "source_data" / "benchmark" / "run_telemetry_N36.csv"

MODEL_META = {
    "Claude Opus 5.5": {
        "display": "Claude Opus 5.0*",
        "short": "Opus 5.0*",
        "provider": "Anthropic",
        "color": "#FBB381",
        "color_dark": "#B8662E",
        "order": 0,
    },
    "Claude Sonnet 5.5": {
        "display": "Claude Sonnet 5.5",
        "short": "Sonnet 5.5",
        "provider": "Anthropic",
        "color": "#58BBBC",
        "color_dark": "#267B7C",
        "order": 1,
    },
    "Gemini 4 Hydrogen": {
        "display": "Gemini 4 Hydrogen",
        "short": "Hydrogen",
        "provider": "Google",
        "color": "#3489BF",
        "color_dark": "#1D5E89",
        "order": 2,
    },
    "Gemini 4 Neon": {
        "display": "Gemini 4 Neon",
        "short": "Neon",
        "provider": "Google",
        "color": "#F05F70",
        "color_dark": "#B43142",
        "order": 3,
    },
}

COND_META = {
    "seeded": {
        "label": "Seeded",
        "short": "Seeded",
        "framing": "Seeded",
        "literature": False,
        "prompt_file": "prompt_seeded.md",
        "order": 0,
    },
    "cold": {
        "label": "Cold",
        "short": "Cold",
        "framing": "Cold",
        "literature": False,
        "prompt_file": "prompt_cold.md",
        "order": 1,
    },
    "seeded_lit": {
        "label": "Seeded + literature",
        "short": "Seeded + lit.",
        "framing": "Seeded",
        "literature": True,
        "prompt_file": "prompt_seeded_lit.md",
        "order": 2,
    },
    "cold_lit": {
        "label": "Cold + literature",
        "short": "Cold + lit.",
        "framing": "Cold",
        "literature": True,
        "prompt_file": "prompt_cold_lit.md",
        "order": 3,
    },
}

NODE_NAMES = {
    "N1": "Essential-gene recall deficit",
    "N2": "Sparse lncRNA hit calls",
    "N3": "Pol III poly-T termination",
    "N4": "Poly-T functional depletion loss",
    "N5": "Direct-repeat recombination",
    "N6": "Effective cellular coverage loss",
    "N7": "Statistical power collapse",
}

FP_NAMES = {
    "FP1": "Read-count mapping error",
    "FP2": "Non-targeting off-target effect",
    "FP3": "CasRx collateral RNA cleavage",
    "FP4": "Guide GC-content bias",
    "FP5": "Plasmid skew or folding confound",
    "FP6": "Non-essential control specificity",
    "FP7": "Dismissal of all 206 lncRNA hits",
}


def make_markdown_renderer():
    return mistune.create_markdown(
        escape=False,
        plugins=["table", "strikethrough", "url", "task_lists"],
    )


MD = make_markdown_renderer()


def parse_prompt(prompt_path: Path) -> dict[str, str]:
    raw = prompt_path.read_text(encoding="utf-8").strip()
    lines = raw.splitlines()
    task_heading = ""
    task_body = []
    in_task = False
    for line in lines:
        if line.startswith("### Reviewer Task"):
            task_heading = line.replace("###", "").strip()
            in_task = True
            continue
        if in_task:
            if line.startswith("### "):
                break
            if line.strip():
                task_body.append(line.strip())
    # Also note reference literature sentence if present
    lit_note = ""
    for line in lines:
        if "reference_literature" in line:
            lit_note = line.strip()
            break
    return {
        "task_heading": task_heading or "Reviewer Task",
        "task_summary": " ".join(task_body),
        "lit_note": lit_note,
        "full_html": MD(raw),
        "full_raw": raw,
    }


def convert_figure(src_png: Path, dst_webp: Path, max_dim: int = 1900) -> dict[str, Any]:
    dst_webp.parent.mkdir(parents=True, exist_ok=True)
    with Image.open(src_png) as img:
        w, h = img.size
        if img.mode not in ("RGB", "RGBA"):
            img = img.convert("RGBA")
        # Composite transparent PNGs onto crisp white background
        if img.mode == "RGBA":
            bg = Image.new("RGB", img.size, (255, 255, 255))
            bg.paste(img, mask=img.split()[3])
            img = bg
        scale = min(1.0, max_dim / max(w, h))
        if scale < 1.0:
            nw, nh = int(round(w * scale)), int(round(h * scale))
            img = img.resize((nw, nh), Image.Resampling.LANCZOS)
        else:
            nw, nh = w, h
        img.save(dst_webp, format="WEBP", quality=86, method=4)
    return {
        "width": nw,
        "height": nh,
        "bytes": dst_webp.stat().st_size,
    }


def prettify_fig_title(filename: str) -> str:
    stem = Path(filename).stem
    stem = re.sub(r"^Agent_Fig(\d+)", r"Agent Figure \1", stem)
    stem = re.sub(r"^Fig(\d+)", r"Figure \1", stem)
    stem = stem.replace("_", " ").strip()
    return stem


def extract_discard_entries(discard_path: Path) -> list[dict[str, str]]:
    if not discard_path.exists():
        return []
    try:
        data = json.loads(discard_path.read_text(encoding="utf-8"))
    except Exception:
        return []
    raw_list: list[Any] = []
    if isinstance(data, list):
        raw_list = data
    elif isinstance(data, dict):
        for key in (
            "entries",
            "discarded_hypotheses",
            "discards",
            "hypotheses",
            "items",
            "discarded",
            "records",
            "refuted_hypotheses",
        ):
            if isinstance(data.get(key), list):
                raw_list = data[key]
                break
        if not raw_list:
            for v in data.values():
                if isinstance(v, list) and v and isinstance(v[0], dict):
                    raw_list = v
                    break

    cleaned = []
    for i, item in enumerate(raw_list, 1):
        if not isinstance(item, dict):
            cleaned.append(
                {"id": f"D{i}", "hypothesis": str(item), "verdict": "Refuted / Excluded", "reason": ""}
            )
            continue
        hyp = (
            item.get("hypothesis")
            or item.get("claim")
            or item.get("title")
            or item.get("description")
            or item.get("candidate_claim")
            or item.get("finding")
            or item.get("name")
            or item.get("test")
            or ""
        )
        verdict = (
            item.get("post_qc_verdict")
            or item.get("verdict")
            or item.get("status")
            or item.get("outcome")
            or item.get("disposition")
            or "Refuted"
        )
        reason = (
            item.get("reason")
            or item.get("rationale")
            or item.get("exclusion_reason")
            or item.get("exclusion_rationale")
            or item.get("qc_notes")
            or item.get("summary")
            or item.get("result")
            or item.get("evidence")
            or ""
        )
        if isinstance(reason, (dict, list)):
            reason = json.dumps(reason, ensure_ascii=False)[:320]
        if isinstance(hyp, (dict, list)):
            hyp = json.dumps(hyp, ensure_ascii=False)[:240]
        item_id = str(item.get("id") or item.get("discard_id") or item.get("finding_id") or f"D{i}")
        cleaned.append(
            {
                "id": item_id,
                "hypothesis": str(hyp).strip()[:320],
                "verdict": str(verdict).strip()[:60],
                "reason": str(reason).strip()[:420],
            }
        )
    return cleaned


def render_report_html(raw_md: str, fig_map: dict[str, str], run_id: str) -> tuple[str, list[dict[str, str]]]:
    # Rewrite relative image references if any match our converted figures
    def _img_repl(m: re.Match) -> str:
        alt, src = m.group(1), m.group(2).strip()
        fname = Path(src).name
        if fname in fig_map:
            return f"![{alt}]({fig_map[fname]})"
        # Try matching stem (e.g. .pdf -> .png)
        stem = Path(src).stem
        for orig_name, rel_webp in fig_map.items():
            if Path(orig_name).stem == stem:
                return f"![{alt}]({rel_webp})"
        return m.group(0)

    rewritten = re.sub(r"!\[([^\]]*)\]\(([^)]+)\)", _img_repl, raw_md)
    rendered = MD(rewritten)

    # Wrap <table> in scrollable div
    rendered = rendered.replace("<table>", '<div class="table-wrap"><table>').replace(
        "</table>", "</table></div>"
    )

    # Add click-to-zoom class on inline images
    rendered = re.sub(
        r'<img src="([^"]+)" alt="([^"]*)"',
        r'<img class="report-inline-img" loading="lazy" data-zoom-src="\1" src="\1" alt="\2"',
        rendered,
    )

    # Inject deterministic IDs into h2/h3 headings for section navigation
    toc: list[dict[str, str]] = []
    sec_counter = [0]

    def _head_repl(m: re.Match) -> str:
        tag = m.group(1)
        inner = m.group(2)
        plain = re.sub(r"<[^>]+>", "", inner).strip()
        sec_counter[0] += 1
        hid = f"sec-{run_id}-{sec_counter[0]}"
        if tag == "h2" and len(plain) > 1:
            toc.append({"id": hid, "text": html.unescape(plain)})
        return f'<{tag} id="{hid}">{inner}</{tag}>'

    rendered = re.sub(r"<(h[23])>(.*?)</\1>", _head_repl, rendered, flags=re.S)
    return rendered, toc


def clean_discovery_label(key: str) -> str:
    return key.replace("_", " ").strip()


def build_archive() -> None:
    if DOCS_DIR.exists():
        shutil.rmtree(DOCS_DIR)
    ASSETS_FIG_DIR.mkdir(parents=True, exist_ok=True)
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    # 1. Load prompts
    prompts = {}
    for cond_key, cmeta in COND_META.items():
        pfile = PROMPT_DIR / cmeta["prompt_file"]
        prompts[cond_key] = parse_prompt(pfile)

    # 2. Load scored rubric and telemetry
    rubric_rows = list(csv.DictReader(RUBRIC_CSV.open(encoding="utf-8")))
    tel_map = {
        r["run_path"]: r
        for r in csv.DictReader(TELEMETRY_CSV.open(encoding="utf-8"))
    }

    runs_out: list[dict[str, Any]] = []
    total_figs = 0
    total_webp_bytes = 0

    for row in rubric_rows:
        if row.get("in_primary_N36", "True") != "True":
            continue
        run_path_rel = row["run_path"]
        tel = tel_map[run_path_rel]
        run_dir = PROJ_ROOT / run_path_rel
        out_dir = run_dir / "outputs"

        # Locate verbatim report markdown
        report_candidates = [
            out_dir / "referee_report.md",
            out_dir / "peer_review_report.md",
        ]
        report_file = next((p for p in report_candidates if p.exists()), None)
        if report_file is None:
            mds = sorted(out_dir.glob("*report*.md"))
            report_file = mds[0] if mds else None
        if report_file is None:
            raise FileNotFoundError(f"No referee report markdown found in {out_dir}")

        raw_md = report_file.read_text(encoding="utf-8")

        model_assigned = row["model_label"]
        mmeta = MODEL_META[model_assigned]
        cond_key = row["condition"]
        cmeta = COND_META[cond_key]
        rep_str = row["replicate"]
        rep_num = int(re.search(r"(\d+)", rep_str).group(1))
        run_id = f"{row['model_slug']}__{cond_key}__{rep_str}"

        # Convert PNG figures
        figs_src_dir = out_dir / "figures"
        pngs = sorted(figs_src_dir.glob("*.png")) if figs_src_dir.exists() else sorted(out_dir.glob("*.png"))
        fig_entries = []
        fig_map: dict[str, str] = {}
        for png in pngs:
            webp_name = f"{png.stem}.webp"
            dst_webp = ASSETS_FIG_DIR / run_id / webp_name
            rel_webp = f"assets/figures/{run_id}/{webp_name}"
            meta_img = convert_figure(png, dst_webp)
            total_figs += 1
            total_webp_bytes += meta_img["bytes"]
            fig_map[png.name] = rel_webp
            fig_entries.append(
                {
                    "orig_name": png.name,
                    "title": prettify_fig_title(png.name),
                    "src": rel_webp,
                    "width": meta_img["width"],
                    "height": meta_img["height"],
                }
            )

        # Render markdown to HTML
        report_html, toc = render_report_html(raw_md, fig_map, run_id)

        # Nodes N1-N7
        nodes = []
        for i in range(1, 8):
            nk = f"N{i}"
            sc_val = int(float(row[f"{nk}_score"]))
            tier = row[f"{nk}_tier"].strip()
            nodes.append(
                {
                    "id": nk,
                    "name": NODE_NAMES[nk],
                    "found": sc_val == 1,
                    "tier": tier if sc_val == 1 else "",
                }
            )

        # Control traps FP1-FP7
        traps = []
        for i in range(1, 8):
            fk = f"FP{i}"
            st = row[f"{fk}_status"].strip()
            traps.append({"id": fk, "name": FP_NAMES[fk], "status": st})

        # Discoveries
        disc_raw = (row.get("non_chain_discoveries") or "").strip()
        disc_list = [
            clean_discovery_label(x)
            for x in re.split(r"[;|,]", disc_raw)
            if x.strip()
        ]

        # Telemetry metrics
        wall_min = round(float(tel["wall_clock_min"]), 1)
        slurm_jobs = int(float(tel["n_slurm_jobs"]))
        n_scripts = int(float(tel["n_scripts_executed"]))
        n_conf = int(float(tel["n_confirmed_findings"]))
        n_disc = int(float(tel["n_discarded_hypotheses"]))
        disc_pct = round(100.0 * n_disc / (n_conf + n_disc), 1) if (n_conf + n_disc) > 0 else 0.0
        fallback = row["ceo_biosafety_fallback"].strip().lower() == "true"
        sub_halts = int(float(tel.get("subagent_safety_interrupts") or 0))

        # Discard log entries
        discard_entries = extract_discard_entries(out_dir / "discard_log.json")

        # Searchable plain text snippet (condensed for fast search)
        plain_text = re.sub(r"\s+", " ", re.sub(r"[#*`|\[\]()>]", " ", raw_md)).strip()

        runs_out.append(
            {
                "id": run_id,
                "order_key": (cmeta["order"], mmeta["order"], rep_num),
                "model_assigned": model_assigned,
                "model_executed": row["actual_ceo_model"],
                "model_display": mmeta["display"],
                "model_short": mmeta["short"],
                "provider": mmeta["provider"],
                "color": mmeta["color"],
                "color_dark": mmeta["color_dark"],
                "fallback": fallback,
                "subagent_halts": sub_halts,
                "condition": cond_key,
                "condition_label": cmeta["label"],
                "condition_short": cmeta["short"],
                "framing": cmeta["framing"],
                "literature": cmeta["literature"],
                "replicate": rep_num,
                "replicate_label": f"Replicate {rep_num}",
                "recall": int(float(row["causal_node_score_sum"])),
                "recall_pct": round(float(row["causal_node_recall_pct"]), 1),
                "nodes": nodes,
                "traps": traps,
                "fp_triggered": int(float(row["n_fp_triggered"])),
                "wall_min": wall_min,
                "slurm_jobs": slurm_jobs,
                "n_scripts": n_scripts,
                "n_confirmed": n_conf,
                "n_discarded": n_disc,
                "discard_pct": disc_pct,
                "n_discoveries": int(float(row["n_non_chain_discoveries"])),
                "discoveries": disc_list,
                "report_filename": report_file.name,
                "report_words": len(raw_md.split()),
                "report_html": report_html,
                "report_raw": raw_md,
                "search_text": plain_text[:20000].lower(),
                "toc": toc,
                "figures": fig_entries,
                "discard_log": discard_entries,
            }
        )

    runs_out.sort(key=lambda r: r["order_key"])
    for r in runs_out:
        del r["order_key"]

    assert len(runs_out) == 36, f"Expected 36 runs, got {len(runs_out)}"

    # Summary metadata
    summary = {
        "n_runs": len(runs_out),
        "n_figures": total_figs,
        "mean_recall": round(sum(r["recall"] for r in runs_out) / len(runs_out), 2),
        "mean_recall_pct": round(100.0 * sum(r["recall"] for r in runs_out) / (7 * len(runs_out)), 1),
        "n_perfect": sum(1 for r in runs_out if r["recall"] == 7),
        "median_runtime_min": sorted(r["wall_min"] for r in runs_out)[len(runs_out) // 2],
        "median_discard_pct": round(
            0.5
            * (
                sorted(r["discard_pct"] for r in runs_out)[17]
                + sorted(r["discard_pct"] for r in runs_out)[18]
            ),
            1,
        ),
        "models": [
            {
                "display": m["display"],
                "short": m["short"],
                "provider": m["provider"],
                "color": m["color"],
                "color_dark": m["color_dark"],
            }
            for _, m in sorted(MODEL_META.items(), key=lambda kv: kv[1]["order"])
        ],
        "conditions": [
            {"key": k, "label": v["label"], "short": v["short"]}
            for k, v in sorted(COND_META.items(), key=lambda kv: kv[1]["order"])
        ],
    }

    payload = {
        "summary": summary,
        "prompts": prompts,
        "runs": runs_out,
    }

    # Write JS data bundle (works identically over file:// and https://)
    js_path = DATA_DIR / "reports_data.js"
    js_content = "window.AUDIT_ARCHIVE = " + json.dumps(payload, ensure_ascii=False) + ";\n"
    js_path.write_text(js_content, encoding="utf-8")

    # Write clean index.html + styles.css + app.js
    write_frontend(DOCS_DIR)

    # Touch .nojekyll for clean GitHub Pages serving
    (DOCS_DIR / ".nojekyll").write_text("", encoding="utf-8")

    print(f"Built website cleanly in {DOCS_DIR}")
    print(f"  Runs: {len(runs_out)}/36")
    print(f"  Figures converted: {total_figs} WebP files ({total_webp_bytes / 1e6:.1f} MB total)")
    print(f"  Data bundle: {js_path.stat().st_size / 1e6:.2f} MB")


def write_frontend(docs_dir: Path) -> None:
    css = r"""
:root {
  --bg: #ffffff;
  --bg-subtle: #f7f8fa;
  --bg-stripe: #f4f5f7;
  --border: #e4e6eb;
  --border-strong: #cfd4dc;
  --ink: #14171a;
  --ink-2: #4a515c;
  --ink-3: #767f8c;
  --blue: #3489bf;
  --cyan: #58bbbc;
  --coral: #f05f70;
  --peach: #fbb381;
  --yellow: #f9c274;
  --yellow-pale: #fef6e4;
  --radius: 8px;
  --radius-sm: 5px;
  --sidebar-w: 300px;
  --topbar-h: 54px;
  --font-sans: -apple-system, BlinkMacSystemFont, "Helvetica Neue", Helvetica, Arial, sans-serif;
  --font-serif: Georgia, "Times New Roman", serif;
  --font-mono: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
}

* { box-sizing: border-box; }
html, body {
  margin: 0; padding: 0; height: 100%;
  background: var(--bg); color: var(--ink);
  font-family: var(--font-sans); font-size: 14px; line-height: 1.55;
  -webkit-font-smoothing: antialiased;
}

/* Top header bar */
.topbar {
  position: fixed; top: 0; left: 0; right: 0; height: var(--topbar-h);
  background: rgba(255, 255, 255, 0.96);
  backdrop-filter: blur(8px);
  border-bottom: 1px solid var(--border);
  display: flex; align-items: center; justify-content: space-between;
  padding: 0 20px; z-index: 40; gap: 14px;
}
.brand {
  display: flex; align-items: center; gap: 11px; min-width: 0; cursor: pointer;
}
.brand-badge {
  background: var(--ink); color: #fff; font-size: 10px; font-weight: 700;
  letter-spacing: 0.06em; text-transform: uppercase;
  padding: 3px 7px; border-radius: 4px; flex-shrink: 0;
}
.brand-title {
  font-size: 14px; font-weight: 650; color: var(--ink);
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
.brand-sub {
  font-size: 12px; color: var(--ink-3); font-weight: 400; margin-left: 6px;
}
.topbar-actions { display: flex; align-items: center; gap: 8px; flex-shrink: 0; }
.btn {
  display: inline-flex; align-items: center; gap: 6px;
  border: 1px solid var(--border-strong); background: #fff; color: var(--ink);
  font-size: 12.5px; font-weight: 500; padding: 6px 11px; border-radius: 6px;
  cursor: pointer; transition: all 0.14s ease; text-decoration: none;
}
.btn:hover { background: var(--bg-subtle); border-color: #9ea6b2; }
.btn:disabled { opacity: 0.4; cursor: not-allowed; }
.btn-primary { background: var(--ink); color: #fff; border-color: var(--ink); }
.btn-primary:hover { background: #2b3038; border-color: #2b3038; }
.btn-ghost { border-color: transparent; background: transparent; color: var(--ink-2); }
.btn-ghost:hover { background: var(--bg-subtle); color: var(--ink); }
.mobile-menu-btn { display: none; }

/* Main layout */
.layout {
  display: flex; padding-top: var(--topbar-h); min-height: 100vh;
}
.sidebar {
  position: fixed; top: var(--topbar-h); bottom: 0; left: 0;
  width: var(--sidebar-w); border-right: 1px solid var(--border);
  background: var(--bg-subtle); display: flex; flex-direction: column;
  z-index: 30;
}
.sidebar-controls {
  padding: 12px 14px 10px; border-bottom: 1px solid var(--border);
  background: #fff;
}
.search-input {
  width: 100%; padding: 7px 10px; font-size: 12.5px;
  border: 1px solid var(--border-strong); border-radius: 6px;
  background: var(--bg-subtle); color: var(--ink); outline: none;
}
.search-input:focus { background: #fff; border-color: var(--blue); }
.filter-label {
  font-size: 10.5px; font-weight: 650; text-transform: uppercase;
  letter-spacing: 0.05em; color: var(--ink-3); margin: 9px 0 5px;
}
.pill-row { display: flex; flex-wrap: wrap; gap: 4px; }
.fpill {
  display: inline-flex; align-items: center; gap: 5px;
  font-size: 11px; font-weight: 500; padding: 3px 8px;
  border-radius: 99px; border: 1px solid var(--border);
  background: #fff; color: var(--ink-2); cursor: pointer;
  transition: all 0.12s ease;
}
.fpill:hover { border-color: #adb5c0; color: var(--ink); }
.fpill.active { background: var(--ink); color: #fff; border-color: var(--ink); }
.dot { width: 8px; height: 8px; border-radius: 50%; flex-shrink: 0; display: inline-block; }

.run-list {
  flex: 1; overflow-y: auto; padding: 8px;
}
.run-list-count {
  font-size: 11px; color: var(--ink-3); padding: 4px 6px 6px;
  display: flex; justify-content: space-between; align-items: center;
}
.run-item {
  width: 100%; text-align: left; border: 1px solid transparent;
  background: transparent; border-radius: 6px; padding: 8px 9px;
  cursor: pointer; display: flex; align-items: center; gap: 9px;
  margin-bottom: 2px; transition: all 0.12s ease;
}
.run-item:hover { background: #fff; border-color: var(--border); }
.run-item.active {
  background: #fff; border-color: var(--blue);
  box-shadow: 0 1px 3px rgba(20,23,26,0.06);
}
.run-item-main { flex: 1; min-width: 0; }
.run-item-model {
  font-size: 12.5px; font-weight: 600; color: var(--ink);
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
.run-item-meta { font-size: 11px; color: var(--ink-3); }
.score-pill {
  font-size: 11px; font-weight: 650; padding: 2px 6px; border-radius: 5px;
  background: var(--bg-stripe); color: var(--ink-2); flex-shrink: 0;
}
.score-pill.perfect { background: #e3f2fb; color: #165e8c; }

/* Main content pane */
.main {
  margin-left: var(--sidebar-w); flex: 1; min-width: 0;
  padding: 28px 38px 72px; max-width: 1220px;
}

/* Overview page */
.hero {
  border-bottom: 1px solid var(--border); padding-bottom: 22px; margin-bottom: 24px;
}
.hero-eyebrow {
  font-size: 11.5px; font-weight: 650; letter-spacing: 0.06em;
  text-transform: uppercase; color: var(--blue); margin-bottom: 6px;
}
.hero h1 {
  font-size: 26px; line-height: 1.22; margin: 0 0 10px;
  font-weight: 700; letter-spacing: -0.015em;
}
.hero p { margin: 0; color: var(--ink-2); max-width: 78ch; font-size: 14.5px; }

.kpi-grid {
  display: grid; grid-template-columns: repeat(5, minmax(0, 1fr));
  gap: 12px; margin: 20px 0 28px;
}
.kpi-card {
  background: var(--bg-subtle); border: 1px solid var(--border);
  border-radius: var(--radius); padding: 13px 15px;
}
.kpi-val { font-size: 21px; font-weight: 700; color: var(--ink); line-height: 1.15; }
.kpi-lbl { font-size: 11.5px; color: var(--ink-3); margin-top: 3px; }

.cond-section { margin-bottom: 28px; }
.cond-header {
  display: flex; align-items: baseline; justify-content: space-between;
  border-bottom: 1px solid var(--border); padding-bottom: 7px; margin-bottom: 13px;
}
.cond-title { font-size: 15.5px; font-weight: 700; color: var(--ink); margin: 0; }
.cond-sub { font-size: 12px; color: var(--ink-3); }

.cards-grid {
  display: grid; grid-template-columns: repeat(auto-fill, minmax(255px, 1fr));
  gap: 12px;
}
.run-card {
  border: 1px solid var(--border); border-radius: var(--radius);
  background: #fff; padding: 13px 14px; cursor: pointer;
  transition: transform 0.12s ease, box-shadow 0.12s ease, border-color 0.12s ease;
  display: flex; flex-direction: column; gap: 9px;
}
.run-card:hover {
  border-color: var(--blue); transform: translateY(-1px);
  box-shadow: 0 4px 14px rgba(20, 23, 26, 0.06);
}
.run-card-top { display: flex; justify-content: space-between; align-items: flex-start; gap: 8px; }
.model-tag {
  display: inline-flex; align-items: center; gap: 6px;
  font-size: 12.5px; font-weight: 650; color: var(--ink);
}
.rep-tag { font-size: 11.5px; color: var(--ink-3); font-weight: 500; }
.mini-nodes { display: grid; grid-template-columns: repeat(7, 1fr); gap: 3px; }
.mini-node {
  font-size: 9.5px; font-weight: 650; text-align: center;
  padding: 2px 0; border-radius: 3px; background: var(--bg-stripe); color: #a1a9b4;
}
.mini-node.e1 { background: #3489bf; color: #fff; }
.mini-node.e2 { background: #a8d2ec; color: #123e5c; }
.run-card-foot {
  display: flex; justify-content: space-between; font-size: 11.5px; color: var(--ink-3);
  border-top: 1px solid var(--bg-stripe); padding-top: 7px;
}

/* Single Run Detail View */
.run-header {
  display: flex; flex-wrap: wrap; align-items: flex-start;
  justify-content: space-between; gap: 14px;
  padding-bottom: 18px; border-bottom: 1px solid var(--border); margin-bottom: 20px;
}
.run-header-left { min-width: 0; }
.run-badges { display: flex; flex-wrap: wrap; align-items: center; gap: 7px; margin-bottom: 8px; }
.badge {
  display: inline-flex; align-items: center; gap: 6px;
  font-size: 12px; font-weight: 600; padding: 3px 9px;
  border-radius: 99px; background: var(--bg-stripe); color: var(--ink);
  border: 1px solid var(--border);
}
.badge-warn {
  background: var(--yellow-pale); border-color: #efd19a; color: #7e500a;
}
.run-title { font-size: 22px; font-weight: 700; margin: 0 0 4px; letter-spacing: -0.01em; }
.run-sub { font-size: 13px; color: var(--ink-2); margin: 0; }
.run-nav-btns { display: flex; align-items: center; gap: 6px; flex-wrap: wrap; }

/* Scorecard strip */
.scorecard {
  display: grid; grid-template-columns: 1.55fr 1.35fr 1fr; gap: 12px;
  margin-bottom: 22px;
}
.sc-box {
  border: 1px solid var(--border); border-radius: var(--radius);
  padding: 12px 14px; background: var(--bg-subtle);
}
.sc-head {
  font-size: 10.5px; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.05em; color: var(--ink-3); margin-bottom: 8px;
  display: flex; justify-content: space-between; align-items: center;
}
.sc-pills { display: flex; flex-wrap: wrap; gap: 5px; }
.npill {
  font-size: 11px; font-weight: 600; padding: 3px 7px; border-radius: 4px;
  background: #eceef2; color: #8c95a3; border: 1px solid transparent;
}
.npill.e1 { background: #3489bf; color: #fff; }
.npill.e2 { background: #cde4f3; color: #164b70; }
.fpill-trap {
  font-size: 11px; font-weight: 600; padding: 3px 7px; border-radius: 4px;
  background: #fff; color: var(--ink-3); border: 1px solid var(--border);
}
.fpill-trap.qc { background: #e9ecef; color: #2b3038; border-color: #d0d5dd; }
.fpill-trap.rev { background: #d6f1f1; color: #145b5c; border-color: #9edada; }
.fpill-trap.flag { background: #fde5e8; color: #9c2030; border-color: #f6b6be; }

.sc-metrics { display: grid; grid-template-columns: 1fr 1fr; gap: 6px 10px; font-size: 12px; }
.sc-m-lbl { color: var(--ink-3); font-size: 11px; }
.sc-m-val { font-weight: 650; color: var(--ink); }

/* Section blocks inside run view */
.section-card {
  border: 1px solid var(--border); border-radius: var(--radius);
  background: #fff; margin-bottom: 22px; overflow: hidden;
}
.section-bar {
  background: var(--bg-subtle); border-bottom: 1px solid var(--border);
  padding: 10px 16px; display: flex; align-items: center;
  justify-content: space-between; gap: 10px;
}
.section-bar h2 {
  margin: 0; font-size: 13px; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.045em; color: var(--ink);
  display: flex; align-items: center; gap: 8px;
}
.step-num {
  display: inline-flex; align-items: center; justify-content: center;
  width: 20px; height: 20px; border-radius: 50%;
  background: var(--ink); color: #fff; font-size: 11px; font-weight: 700;
}
.section-body { padding: 18px 20px; }

/* Prompt box */
.prompt-task-box {
  background: #f5f9fc; border-left: 3px solid var(--blue);
  padding: 12px 15px; border-radius: 0 6px 6px 0;
  font-size: 13.5px; color: var(--ink); margin-bottom: 10px;
}
.prompt-task-title { font-weight: 700; margin-bottom: 4px; font-size: 13px; color: #19567e; }
.prompt-lit-note {
  font-size: 12.5px; color: var(--ink-2); background: var(--yellow-pale);
  border: 1px solid #f2dca8; border-radius: 6px; padding: 8px 12px; margin-bottom: 10px;
}
.prompt-full-wrap {
  margin-top: 10px; padding: 14px 16px; background: var(--bg-subtle);
  border: 1px solid var(--border); border-radius: 6px;
  font-size: 12.5px; color: var(--ink-2); max-height: 460px; overflow-y: auto;
}
.prompt-full-wrap h3 { font-size: 13px; margin: 12px 0 4px; color: var(--ink); }
.prompt-full-wrap code { font-family: var(--font-mono); font-size: 11.5px; background: #e9edf2; padding: 1px 4px; border-radius: 3px; }

/* Figures gallery */
.fig-grid {
  display: grid; grid-template-columns: repeat(auto-fill, minmax(290px, 1fr));
  gap: 14px;
}
.fig-card {
  border: 1px solid var(--border); border-radius: var(--radius-sm);
  overflow: hidden; background: #fff; cursor: zoom-in;
  transition: border-color 0.14s ease, box-shadow 0.14s ease;
  display: flex; flex-direction: column;
}
.fig-card:hover { border-color: var(--blue); box-shadow: 0 4px 14px rgba(20,23,26,0.08); }
.fig-thumb-wrap {
  background: #fff; padding: 8px; display: flex; align-items: center;
  justify-content: center; aspect-ratio: 4 / 3; overflow: hidden;
  border-bottom: 1px solid var(--bg-stripe);
}
.fig-thumb-wrap img {
  max-width: 100%; max-height: 100%; object-fit: contain; display: block;
}
.fig-cap {
  padding: 8px 11px; font-size: 12px; font-weight: 600; color: var(--ink);
  display: flex; justify-content: space-between; align-items: center; gap: 8px;
  background: var(--bg-subtle);
}
.fig-cap span.fig-sub { font-size: 10.5px; color: var(--ink-3); font-weight: 400; font-family: var(--font-mono); }

/* Rendered Referee Report prose */
.report-toc {
  display: flex; flex-wrap: wrap; gap: 6px; margin-bottom: 18px;
  padding-bottom: 14px; border-bottom: 1px solid var(--border);
}
.toc-chip {
  font-size: 11.5px; color: var(--ink-2); background: var(--bg-subtle);
  border: 1px solid var(--border); border-radius: 5px;
  padding: 3px 8px; text-decoration: none; cursor: pointer;
}
.toc-chip:hover { border-color: var(--blue); color: var(--blue); }

.report-prose {
  max-width: 84ch; font-size: 14px; line-height: 1.65; color: var(--ink);
}
.report-prose h1 { font-size: 20px; margin: 0 0 12px; border-bottom: 1px solid var(--border); padding-bottom: 8px; }
.report-prose h2 { font-size: 16.5px; margin: 26px 0 10px; color: var(--ink); border-bottom: 1px solid var(--bg-stripe); padding-bottom: 5px; }
.report-prose h3 { font-size: 14.5px; margin: 18px 0 6px; color: #1e242b; }
.report-prose h4 { font-size: 13.5px; margin: 14px 0 5px; color: var(--ink-2); }
.report-prose p { margin: 0 0 11px; }
.report-prose ul, .report-prose ol { margin: 0 0 12px; padding-left: 22px; }
.report-prose li { margin-bottom: 4px; }
.report-prose blockquote {
  margin: 12px 0; padding: 8px 14px; border-left: 3px solid var(--border-strong);
  background: var(--bg-subtle); color: var(--ink-2); border-radius: 0 5px 5px 0;
}
.report-prose code {
  font-family: var(--font-mono); font-size: 12px; background: var(--bg-stripe);
  padding: 1px 5px; border-radius: 4px; color: #1b2838;
}
.report-prose pre {
  background: #f6f8fa; border: 1px solid var(--border); border-radius: 6px;
  padding: 11px 14px; overflow-x: auto; font-size: 12px; line-height: 1.45;
}
.report-prose pre code { background: transparent; padding: 0; }
.table-wrap { overflow-x: auto; margin: 14px 0; border: 1px solid var(--border); border-radius: 6px; }
.report-prose table { border-collapse: collapse; width: 100%; font-size: 12.5px; }
.report-prose th {
  background: var(--bg-subtle); text-align: left; font-weight: 650;
  padding: 7px 10px; border-bottom: 1px solid var(--border-strong); white-space: nowrap;
}
.report-prose td {
  padding: 6px 10px; border-bottom: 1px solid var(--bg-stripe); vertical-align: top;
}
.report-prose tr:last-child td { border-bottom: none; }
.report-inline-img {
  max-width: 100%; height: auto; border: 1px solid var(--border);
  border-radius: 6px; margin: 12px 0; cursor: zoom-in; display: block;
}

/* Discovery chips & discard table */
.disc-chips { display: flex; flex-wrap: wrap; gap: 6px; margin-bottom: 12px; }
.disc-chip {
  font-size: 11.5px; background: var(--bg-subtle); border: 1px solid var(--border);
  border-radius: 5px; padding: 3px 8px; color: var(--ink-2);
}
.discard-table { width: 100%; border-collapse: collapse; font-size: 12.5px; }
.discard-table th {
  text-align: left; background: var(--bg-subtle); padding: 7px 10px;
  border-bottom: 1px solid var(--border-strong); font-weight: 650;
}
.discard-table td {
  padding: 7px 10px; border-bottom: 1px solid var(--bg-stripe); vertical-align: top;
}
.verdict-badge {
  display: inline-block; font-size: 10.5px; font-weight: 650; padding: 2px 6px;
  border-radius: 4px; background: #eef0f4; color: #3d4450; white-space: nowrap;
}

/* Lightbox modal */
.lightbox {
  position: fixed; inset: 0; background: rgba(16, 20, 26, 0.88);
  z-index: 100; display: none; flex-direction: column;
  align-items: center; justify-content: center; padding: 20px;
}
.lightbox.open { display: flex; }
.lightbox-bar {
  width: 100%; max-width: 1320px; display: flex; justify-content: space-between;
  align-items: center; color: #fff; margin-bottom: 10px; gap: 12px;
}
.lightbox-title { font-size: 13.5px; font-weight: 600; }
.lightbox-stage {
  position: relative; max-width: 94vw; max-height: 84vh;
  background: #fff; border-radius: 8px; padding: 10px;
  display: flex; align-items: center; justify-content: center;
  overflow: auto;
}
.lightbox-stage img {
  max-width: 90vw; max-height: 80vh; object-fit: contain; display: block;
}
.lightbox-stage.zoomed img {
  max-width: none; max-height: none; width: auto; height: auto; cursor: zoom-out;
}
.lb-btn {
  background: rgba(255,255,255,0.14); color: #fff; border: 1px solid rgba(255,255,255,0.28);
  border-radius: 6px; padding: 5px 11px; font-size: 12.5px; cursor: pointer;
}
.lb-btn:hover { background: rgba(255,255,255,0.26); }

.hidden { display: none !important; }

@media (max-width: 980px) {
  .mobile-menu-btn { display: inline-flex; }
  .sidebar { transform: translateX(-100%); transition: transform 0.2s ease; }
  .sidebar.open { transform: translateX(0); box-shadow: 0 8px 28px rgba(0,0,0,0.18); }
  .main { margin-left: 0; padding: 20px 16px 56px; }
  .kpi-grid { grid-template-columns: repeat(2, 1fr); }
  .scorecard { grid-template-columns: 1fr; }
  .brand-sub { display: none; }
}
"""

    js = r"""
(function () {
  const ARCHIVE = window.AUDIT_ARCHIVE;
  const RUNS = ARCHIVE.runs;
  const PROMPTS = ARCHIVE.prompts;
  const SUMMARY = ARCHIVE.summary;

  const state = {
    view: "overview",
    runId: null,
    modelFilter: "ALL",
    condFilter: "ALL",
    recallFilter: "ALL",
    query: "",
    lightboxFigs: [],
    lightboxIdx: 0,
  };

  const els = {
    sidebar: document.getElementById("sidebar"),
    search: document.getElementById("searchInput"),
    modelPills: document.getElementById("modelPills"),
    condPills: document.getElementById("condPills"),
    recallPills: document.getElementById("recallPills"),
    runList: document.getElementById("runList"),
    runListCount: document.getElementById("runListCount"),
    main: document.getElementById("mainContent"),
    overviewBtn: document.getElementById("overviewBtn"),
    brandHome: document.getElementById("brandHome"),
    mobileMenuBtn: document.getElementById("mobileMenuBtn"),
    lightbox: document.getElementById("lightbox"),
    lbTitle: document.getElementById("lbTitle"),
    lbImg: document.getElementById("lbImg"),
    lbStage: document.getElementById("lbStage"),
    lbPrev: document.getElementById("lbPrev"),
    lbNext: document.getElementById("lbNext"),
    lbZoom: document.getElementById("lbZoom"),
    lbClose: document.getElementById("lbClose"),
  };

  function esc(s) {
    return String(s ?? "")
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function filteredRuns() {
    const q = state.query.trim().toLowerCase();
    return RUNS.filter((r) => {
      if (state.modelFilter !== "ALL" && r.model_display !== state.modelFilter) return false;
      if (state.condFilter !== "ALL" && r.condition !== state.condFilter) return false;
      if (state.recallFilter === "7" && r.recall !== 7) return false;
      if (state.recallFilter === "6" && r.recall !== 6) return false;
      if (state.recallFilter === "le5" && r.recall > 5) return false;
      if (q) {
        const hay = `${r.model_display} ${r.condition_label} replicate ${r.replicate} ${r.discoveries.join(" ")} ${r.search_text}`.toLowerCase();
        if (!hay.includes(q)) return false;
      }
      return true;
    });
  }

  function buildFilterPills() {
    // Models
    const mHtml = [
      `<button class="fpill ${state.modelFilter === "ALL" ? "active" : ""}" data-model="ALL">All (4)</button>`,
      ...SUMMARY.models.map(
        (m) =>
          `<button class="fpill ${state.modelFilter === m.display ? "active" : ""}" data-model="${esc(m.display)}"><span class="dot" style="background:${m.color}"></span>${esc(m.short)}</button>`
      ),
    ].join("");
    els.modelPills.innerHTML = mHtml;

    // Conditions
    const cHtml = [
      `<button class="fpill ${state.condFilter === "ALL" ? "active" : ""}" data-cond="ALL">All</button>`,
      ...SUMMARY.conditions.map(
        (c) =>
          `<button class="fpill ${state.condFilter === c.key ? "active" : ""}" data-cond="${esc(c.key)}">${esc(c.short)}</button>`
      ),
    ].join("");
    els.condPills.innerHTML = cHtml;

    // Recall
    const rOpts = [
      ["ALL", "All"],
      ["7", "7/7"],
      ["6", "6/7"],
      ["le5", "≤5/7"],
    ];
    els.recallPills.innerHTML = rOpts
      .map(
        ([k, lbl]) =>
          `<button class="fpill ${state.recallFilter === k ? "active" : ""}" data-recall="${k}">${lbl}</button>`
      )
      .join("");
  }

  function renderSidebarList() {
    const list = filteredRuns();
    els.runListCount.innerHTML = `<span>Showing ${list.length} of ${RUNS.length} audits</span>${
      state.modelFilter !== "ALL" || state.condFilter !== "ALL" || state.recallFilter !== "ALL" || state.query
        ? `<button class="btn-ghost" id="resetFiltersBtn" style="padding:1px 5px;font-size:11px;cursor:pointer;border:none;color:var(--blue)">Reset</button>`
        : ""
    }`;

    els.runList.innerHTML = list
      .map(
        (r) => `
      <button class="run-item ${state.view === "run" && state.runId === r.id ? "active" : ""}" data-run-id="${esc(r.id)}">
        <span class="dot" style="background:${r.color};width:9px;height:9px"></span>
        <div class="run-item-main">
          <div class="run-item-model">${esc(r.model_display)}</div>
          <div class="run-item-meta">${esc(r.condition_short)} · Rep ${r.replicate} · ${r.figures.length} figs</div>
        </div>
        <span class="score-pill ${r.recall === 7 ? "perfect" : ""}">${r.recall}/7</span>
      </button>`
      )
      .join("");

    const resetBtn = document.getElementById("resetFiltersBtn");
    if (resetBtn) {
      resetBtn.addEventListener("click", () => {
        state.modelFilter = "ALL";
        state.condFilter = "ALL";
        state.recallFilter = "ALL";
        state.query = "";
        els.search.value = "";
        buildFilterPills();
        renderSidebarList();
        if (state.view === "overview") renderOverview();
      });
    }
  }

  function renderOverview() {
    const list = filteredRuns();
    const byCond = {};
    SUMMARY.conditions.forEach((c) => (byCond[c.key] = []));
    list.forEach((r) => byCond[r.condition].push(r));

    const sectionsHtml = SUMMARY.conditions
      .filter((c) => byCond[c.key].length > 0)
      .map((c) => {
        const runs = byCond[c.key];
        const p = PROMPTS[c.key];
        const cards = runs
          .map((r) => {
            const nodeCells = r.nodes
              .map((n) => {
                const cls = n.found ? (n.tier === "E1" ? "e1" : "e2") : "";
                return `<div class="mini-node ${cls}" title="${esc(n.id)}: ${esc(n.name)} (${n.found ? n.tier : "Missed"})">${n.id}</div>`;
              })
              .join("");
            const flags = [];
            if (r.fallback) flags.push("Fallback");
            if (r.subagent_halts > 0) flags.push(`${r.subagent_halts} subagent halts`);
            const fp3 = r.traps.find((t) => t.id === "FP3");
            if (fp3 && fp3.status.toLowerCase().includes("refin")) flags.push("FP3 revised");
            return `
            <div class="run-card" data-open-run="${esc(r.id)}">
              <div class="run-card-top">
                <div>
                  <div class="model-tag"><span class="dot" style="background:${r.color};width:9px;height:9px"></span>${esc(r.model_display)}</div>
                  <div class="rep-tag">${esc(r.replicate_label)}${flags.length ? " · " + esc(flags.join(" · ")) : ""}</div>
                </div>
                <span class="score-pill ${r.recall === 7 ? "perfect" : ""}">${r.recall}/7</span>
              </div>
              <div class="mini-nodes">${nodeCells}</div>
              <div class="run-card-foot">
                <span>${r.figures.length} figures · ${r.report_words.toLocaleString()} words</span>
                <span>${r.wall_min} min · ${r.discard_pct}% QC</span>
              </div>
            </div>`;
          })
          .join("");

        return `
        <section class="cond-section">
          <div class="cond-header">
            <h2 class="cond-title">${esc(c.label)}</h2>
            <span class="cond-sub">${runs.length} audit${runs.length === 1 ? "" : "s"} · ${esc(p.task_heading)}</span>
          </div>
          <div class="cards-grid">${cards}</div>
        </section>`;
      })
      .join("");

    els.main.innerHTML = `
      <div class="hero">
        <div class="hero-eyebrow">Supplementary Interactive Archive</div>
        <h1>Autonomous Multi-Agent Peer Review of Functional Genomics Screens</h1>
        <p>Complete, unedited collection of all <strong>${SUMMARY.n_runs} primary clean-room referee reports</strong>, verbatim prompts, <strong>${SUMMARY.n_figures} agent-generated diagnostic figures</strong>, and Tier 4 blinded quality-control discard logs across four frontier models and four experimental arms auditing Montero et al. (<em>Nature Methods</em>, 2024). Select any audit below or from the sidebar to inspect its model provenance, exact prompt, diagnostic figures, and full report.</p>
      </div>
      <div class="kpi-grid">
        <div class="kpi-card"><div class="kpi-val">${SUMMARY.n_runs}</div><div class="kpi-lbl">Primary clean-room audits</div></div>
        <div class="kpi-card"><div class="kpi-val">${SUMMARY.mean_recall}/7</div><div class="kpi-lbl">Mean causal recall (${SUMMARY.mean_recall_pct}%)</div></div>
        <div class="kpi-card"><div class="kpi-val">0/36</div><div class="kpi-lbl">FP1–FP6 false-positive triggers</div></div>
        <div class="kpi-card"><div class="kpi-val">${SUMMARY.median_discard_pct}%</div><div class="kpi-lbl">Median hypotheses refuted in QC</div></div>
        <div class="kpi-card"><div class="kpi-val">${SUMMARY.n_figures}</div><div class="kpi-lbl">Agent-generated figures</div></div>
      </div>
      ${sectionsHtml || '<p style="color:var(--ink-3)">No audits match the current filters.</p>'}
    `;
  }

  function renderRun(runId) {
    const idx = RUNS.findIndex((r) => r.id === runId);
    if (idx < 0) return renderOverview();
    const r = RUNS[idx];
    const p = PROMPTS[r.condition];
    const prevRun = idx > 0 ? RUNS[idx - 1] : null;
    const nextRun = idx < RUNS.length - 1 ? RUNS[idx + 1] : null;

    const nodesHtml = r.nodes
      .map((n) => {
        const cls = n.found ? (n.tier === "E1" ? "e1" : "e2") : "";
        return `<span class="npill ${cls}" title="${esc(n.name)}">${n.id}${n.found ? " · " + n.tier : ""}</span>`;
      })
      .join("");

    const trapsHtml = r.traps
      .map((t) => {
        const sl = t.status.toLowerCase();
        let cls = "";
        let short = "Avoided";
        if (sl.includes("refin")) { cls = "rev"; short = "Revised"; }
        else if (sl.includes("flag") || sl.includes("trigger")) { cls = "flag"; short = "Flagged"; }
        else if (sl.includes("discard") || sl.includes("refut") || sl.includes("qc")) { cls = "qc"; short = "QC refuted"; }
        return `<span class="fpill-trap ${cls}" title="${esc(t.name)}: ${esc(t.status)}">${t.id} · ${short}</span>`;
      })
      .join("");

    const figCards = r.figures
      .map(
        (f, i) => `
      <div class="fig-card" data-lb-idx="${i}">
        <div class="fig-thumb-wrap">
          <img src="${esc(f.src)}" alt="${esc(f.title)}" loading="lazy" />
        </div>
        <div class="fig-cap">
          <span>${esc(f.title)}</span>
          <span class="fig-sub">${esc(f.orig_name)}</span>
        </div>
      </div>`
      )
      .join("");

    const tocHtml = r.toc.length
      ? `<div class="report-toc">${r.toc.map((t) => `<a class="toc-chip" href="#${esc(t.id)}" data-scroll-id="${esc(t.id)}">${esc(t.text)}</a>`).join("")}</div>`
      : "";

    const discChips = r.discoveries
      .map((d) => `<span class="disc-chip">${esc(d)}</span>`)
      .join("");

    const discardRows = r.discard_log.length
      ? `<div class="table-wrap"><table class="discard-table">
          <thead><tr><th style="width:70px">ID</th><th>Tested Hypothesis / Candidate Claim</th><th style="width:120px">QC Verdict</th><th>Falsification Rationale</th></tr></thead>
          <tbody>${r.discard_log
            .map(
              (d) => `<tr><td><code>${esc(d.id)}</code></td><td>${esc(d.hypothesis)}</td><td><span class="verdict-badge">${esc(d.verdict)}</span></td><td>${esc(d.reason)}</td></tr>`
            )
            .join("")}</tbody></table></div>`
      : `<p style="color:var(--ink-3);margin:0">Discard log summary unavailable in structured list format (${r.n_discarded} refuted hypotheses recorded in telemetry).</p>`;

    els.main.innerHTML = `
      <div class="run-header">
        <div class="run-header-left">
          <div class="run-badges">
            <span class="badge"><span class="dot" style="background:${r.color};width:9px;height:9px"></span>${esc(r.model_display)}</span>
            <span class="badge">${esc(r.condition_label)}</span>
            <span class="badge">${esc(r.replicate_label)}</span>
            ${r.fallback ? `<span class="badge badge-warn">Assigned ${esc(r.model_assigned)} → automatic bio-safety fallback to ${esc(r.model_executed)}</span>` : ""}
            ${r.subagent_halts > 0 ? `<span class="badge badge-warn">${r.subagent_halts} subagent bio-safety halts</span>` : ""}
          </div>
          <h1 class="run-title">${esc(r.model_display)} — ${esc(r.condition_label)} (${esc(r.replicate_label)})</h1>
          <p class="run-sub">Provider: ${esc(r.provider)} · Clean-room HPC audit of Montero et al. (<em>Nat. Methods</em>, 2024) · ${r.figures.length} diagnostic figures · ${r.report_words.toLocaleString()} words</p>
        </div>
        <div class="run-nav-btns">
          <button class="btn" id="prevRunBtn" ${prevRun ? "" : "disabled"}>← Prev</button>
          <button class="btn" id="nextRunBtn" ${nextRun ? "" : "disabled"}>Next →</button>
          <button class="btn btn-primary" id="downloadMdBtn">Download .md</button>
        </div>
      </div>

      <div class="scorecard">
        <div class="sc-box">
          <div class="sc-head"><span>Causal-chain recall (N1–N7)</span><strong>${r.recall}/7 (${r.recall_pct}%)</strong></div>
          <div class="sc-pills">${nodesHtml}</div>
        </div>
        <div class="sc-box">
          <div class="sc-head"><span>Control traps (FP1–FP7)</span><strong>${r.fp_triggered}/6 FP1–FP6 triggers</strong></div>
          <div class="sc-pills">${trapsHtml}</div>
        </div>
        <div class="sc-box">
          <div class="sc-head"><span>Execution &amp; Tier 4 QC</span></div>
          <div class="sc-metrics">
            <div><div class="sc-m-lbl">Wall-clock</div><div class="sc-m-val">${r.wall_min} min</div></div>
            <div><div class="sc-m-lbl">Blinded QC</div><div class="sc-m-val">${r.n_discarded}/${r.n_confirmed + r.n_discarded} (${r.discard_pct}%)</div></div>
            <div><div class="sc-m-lbl">Compute</div><div class="sc-m-val">${r.slurm_jobs} jobs · ${r.n_scripts} scripts</div></div>
            <div><div class="sc-m-lbl">Beyond rubric</div><div class="sc-m-val">${r.n_discoveries} discoveries</div></div>
          </div>
        </div>
      </div>

      <!-- 1. Prompt & Model Context -->
      <section class="section-card">
        <div class="section-bar">
          <h2><span class="step-num">1</span> Prompt &amp; Experimental Arm</h2>
          <button class="btn" id="toggleFullPromptBtn">Show full verbatim prompt</button>
        </div>
        <div class="section-body">
          <div class="prompt-task-box">
            <div class="prompt-task-title">${esc(p.task_heading)} (${esc(r.condition_label)})</div>
            <div>${esc(p.task_summary)}</div>
          </div>
          ${p.lit_note ? `<div class="prompt-lit-note"><strong>Reference literature provided:</strong> Four foundational CRISPR/Cas13 design papers (Doench 2018; Hanna &amp; Doench 2020; Wessels et al. 2020 Cas13design; Wessels et al. 2024 TIGER).</div>` : ""}
          <div class="prompt-full-wrap hidden" id="fullPromptWrap">${p.full_html}</div>
        </div>
      </section>

      <!-- 2. Agent-Generated Figures -->
      <section class="section-card">
        <div class="section-bar">
          <h2><span class="step-num">2</span> Agent-Generated Diagnostic Figures (${r.figures.length})</h2>
          <span style="font-size:11.5px;color:var(--ink-3)">Click any figure to enlarge</span>
        </div>
        <div class="section-body">
          <div class="fig-grid">${figCards}</div>
        </div>
      </section>

      <!-- 3. Verbatim Referee Report -->
      <section class="section-card">
        <div class="section-bar">
          <h2><span class="step-num">3</span> Autonomous Referee Report (Verbatim)</h2>
          <span style="font-size:11.5px;color:var(--ink-3);font-family:var(--font-mono)">${esc(r.report_filename)}</span>
        </div>
        <div class="section-body">
          ${tocHtml}
          <article class="report-prose" id="reportArticle">${r.report_html}</article>
        </div>
      </section>

      <!-- 4. Beyond-Rubric Discoveries & Tier 4 Blinded QC Discard Log -->
      <section class="section-card">
        <div class="section-bar">
          <h2><span class="step-num">4</span> Beyond-Rubric Discoveries &amp; Blinded QC Discard Log</h2>
          <button class="btn" id="toggleDiscardBtn">Show discard log (${r.discard_log.length || r.n_discarded})</button>
        </div>
        <div class="section-body">
          <div class="filter-label" style="margin-top:0">Recurring beyond-rubric discoveries matched (${r.discoveries.length})</div>
          <div class="disc-chips">${discChips || '<span style="color:var(--ink-3);font-size:12px">None tagged</span>'}</div>
          <div class="hidden" id="discardWrap" style="margin-top:14px">${discardRows}</div>
        </div>
      </section>
    `;

    // Wire run actions
    if (prevRun) document.getElementById("prevRunBtn").addEventListener("click", () => openRun(prevRun.id));
    if (nextRun) document.getElementById("nextRunBtn").addEventListener("click", () => openRun(nextRun.id));
    document.getElementById("downloadMdBtn").addEventListener("click", () => {
      const blob = new Blob([r.report_raw], { type: "text/markdown;charset=utf-8" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = `${r.id}_${r.report_filename}`;
      a.click();
      URL.revokeObjectURL(url);
    });

    const togglePrompt = document.getElementById("toggleFullPromptBtn");
    const fullPromptWrap = document.getElementById("fullPromptWrap");
    togglePrompt.addEventListener("click", () => {
      const open = fullPromptWrap.classList.toggle("hidden");
      togglePrompt.textContent = open ? "Show full verbatim prompt" : "Hide full verbatim prompt";
    });

    const toggleDiscard = document.getElementById("toggleDiscardBtn");
    const discardWrap = document.getElementById("discardWrap");
    toggleDiscard.addEventListener("click", () => {
      const hidden = discardWrap.classList.toggle("hidden");
      toggleDiscard.textContent = hidden
        ? `Show discard log (${r.discard_log.length || r.n_discarded})`
        : "Hide discard log";
    });

    // Gallery lightbox triggers
    state.lightboxFigs = r.figures;
    els.main.querySelectorAll("[data-lb-idx]").forEach((card) => {
      card.addEventListener("click", () => openLightbox(Number(card.dataset.lbIdx)));
    });
    // Inline report images lightbox
    els.main.querySelectorAll(".report-inline-img").forEach((img) => {
      img.addEventListener("click", () => {
        const src = img.getAttribute("data-zoom-src");
        const fi = r.figures.findIndex((f) => f.src === src);
        if (fi >= 0) openLightbox(fi);
        else {
          state.lightboxFigs = [{ title: img.alt || "Figure", src }];
          openLightbox(0);
        }
      });
    });

    // Smooth scroll TOC chips without overwriting hash route
    els.main.querySelectorAll("[data-scroll-id]").forEach((a) => {
      a.addEventListener("click", (e) => {
        e.preventDefault();
        const target = document.getElementById(a.dataset.scrollId);
        if (target) target.scrollIntoView({ behavior: "smooth", block: "start" });
      });
    });

    window.scrollTo({ top: 0, behavior: "instant" });
  }

  function openLightbox(idx) {
    if (!state.lightboxFigs.length) return;
    state.lightboxIdx = (idx + state.lightboxFigs.length) % state.lightboxFigs.length;
    const f = state.lightboxFigs[state.lightboxIdx];
    els.lbTitle.textContent = `${f.title} (${state.lightboxIdx + 1}/${state.lightboxFigs.length})`;
    els.lbImg.src = f.src;
    els.lbImg.alt = f.title;
    els.lbStage.classList.remove("zoomed");
    els.lightbox.classList.add("open");
  }

  function closeLightbox() {
    els.lightbox.classList.remove("open");
  }

  function openOverview(pushHash = true) {
    state.view = "overview";
    state.runId = null;
    if (pushHash) history.replaceState(null, "", "#overview");
    els.sidebar.classList.remove("open");
    renderSidebarList();
    renderOverview();
    window.scrollTo({ top: 0, behavior: "instant" });
  }

  function openRun(runId, pushHash = true) {
    state.view = "run";
    state.runId = runId;
    if (pushHash) history.replaceState(null, "", `#run=${encodeURIComponent(runId)}`);
    els.sidebar.classList.remove("open");
    renderSidebarList();
    renderRun(runId);
  }

  // Event bindings
  els.modelPills.addEventListener("click", (e) => {
    const btn = e.target.closest("[data-model]");
    if (!btn) return;
    state.modelFilter = btn.dataset.model;
    buildFilterPills();
    renderSidebarList();
    if (state.view === "overview") renderOverview();
  });

  els.condPills.addEventListener("click", (e) => {
    const btn = e.target.closest("[data-cond]");
    if (!btn) return;
    state.condFilter = btn.dataset.cond;
    buildFilterPills();
    renderSidebarList();
    if (state.view === "overview") renderOverview();
  });

  els.recallPills.addEventListener("click", (e) => {
    const btn = e.target.closest("[data-recall]");
    if (!btn) return;
    state.recallFilter = btn.dataset.recall;
    buildFilterPills();
    renderSidebarList();
    if (state.view === "overview") renderOverview();
  });

  els.search.addEventListener("input", () => {
    state.query = els.search.value;
    renderSidebarList();
    if (state.view === "overview") renderOverview();
  });

  els.runList.addEventListener("click", (e) => {
    const item = e.target.closest("[data-run-id]");
    if (item) openRun(item.dataset.runId);
  });

  els.main.addEventListener("click", (e) => {
    const card = e.target.closest("[data-open-run]");
    if (card) openRun(card.dataset.openRun);
  });

  els.overviewBtn.addEventListener("click", () => openOverview());
  els.brandHome.addEventListener("click", () => openOverview());
  els.mobileMenuBtn.addEventListener("click", () => els.sidebar.classList.toggle("open"));

  els.lbPrev.addEventListener("click", () => openLightbox(state.lightboxIdx - 1));
  els.lbNext.addEventListener("click", () => openLightbox(state.lightboxIdx + 1));
  els.lbZoom.addEventListener("click", () => els.lbStage.classList.toggle("zoomed"));
  els.lbClose.addEventListener("click", closeLightbox);
  els.lightbox.addEventListener("click", (e) => {
    if (e.target === els.lightbox) closeLightbox();
  });

  document.addEventListener("keydown", (e) => {
    if (!els.lightbox.classList.contains("open")) return;
    if (e.key === "Escape") closeLightbox();
    if (e.key === "ArrowLeft") openLightbox(state.lightboxIdx - 1);
    if (e.key === "ArrowRight") openLightbox(state.lightboxIdx + 1);
  });

  // Boot from URL hash
  buildFilterPills();
  const h = window.location.hash || "";
  const m = h.match(/^#run=(.+)$/);
  if (m && RUNS.some((r) => r.id === decodeURIComponent(m[1]))) {
    openRun(decodeURIComponent(m[1]), false);
  } else {
    openOverview(false);
  }
})();
"""

    index_html = """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Autonomous Multi-Agent Peer Review — 36 Benchmark Audit Reports</title>
  <link rel="stylesheet" href="assets/styles.css" />
</head>
<body>
  <header class="topbar">
    <div class="brand" id="brandHome" role="button" tabindex="0">
      <button class="btn mobile-menu-btn" id="mobileMenuBtn" type="button">☰ Runs</button>
      <span class="brand-badge">Supplementary Archive</span>
      <span class="brand-title">Autonomous Peer Review Benchmark<span class="brand-sub">36 Clean-Room Audits · 4 Frontier Models</span></span>
    </div>
    <div class="topbar-actions">
      <a class="btn btn-ghost" href="https://github.com/drsimonmueller/agentic-ai-review" target="_blank" rel="noopener">GitHub Repo ↗</a>
      <button class="btn" id="overviewBtn" type="button">All 36 Audits</button>
    </div>
  </header>

  <div class="layout">
    <aside class="sidebar" id="sidebar">
      <div class="sidebar-controls">
        <input class="search-input" id="searchInput" type="search" placeholder="Search reports, models, findings…" />
        <div class="filter-label">Model</div>
        <div class="pill-row" id="modelPills"></div>
        <div class="filter-label">Experimental arm</div>
        <div class="pill-row" id="condPills"></div>
        <div class="filter-label">Causal recall</div>
        <div class="pill-row" id="recallPills"></div>
      </div>
      <div class="run-list-count" id="runListCount"></div>
      <div class="run-list" id="runList"></div>
    </aside>

    <main class="main" id="mainContent"></main>
  </div>

  <div class="lightbox" id="lightbox" role="dialog" aria-modal="true">
    <div class="lightbox-bar">
      <span class="lightbox-title" id="lbTitle"></span>
      <div style="display:flex;gap:6px">
        <button class="lb-btn" id="lbPrev" type="button">← Prev</button>
        <button class="lb-btn" id="lbNext" type="button">Next →</button>
        <button class="lb-btn" id="lbZoom" type="button">Toggle 100%</button>
        <button class="lb-btn" id="lbClose" type="button">Close (Esc)</button>
      </div>
    </div>
    <div class="lightbox-stage" id="lbStage">
      <img id="lbImg" src="" alt="" />
    </div>
  </div>

  <script src="data/reports_data.js"></script>
  <script src="assets/app.js"></script>
</body>
</html>
"""

    assets_dir = docs_dir / "assets"
    assets_dir.mkdir(parents=True, exist_ok=True)
    (assets_dir / "styles.css").write_text(css.strip() + "\n", encoding="utf-8")
    (assets_dir / "app.js").write_text(js.strip() + "\n", encoding="utf-8")
    (docs_dir / "index.html").write_text(index_html.strip() + "\n", encoding="utf-8")


if __name__ == "__main__":
    build_archive()
