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
