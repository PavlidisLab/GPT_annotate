"""Generate a single-file portable curator web app from the long-format TSV.

Output:
  revisions/curator_app/index.html

Embeds the per-row data inline so the page works over file:// with no
network or build step. The curator opens the file in a browser, casts
correct/wrong/unsure verdicts per row, navigates with arrow keys or
buttons, and exports the verdicts as JSON when finished.

Rows already flagged ``auto_accept = TRUE`` are excluded from the
review queue (they can be re-included via a checkbox in the UI).
"""
import csv
import html
import json
import os
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent.parent
TSV  = REPO / "revisions" / "data" / "long_curation_sheet.tsv"
OUT  = REPO / "revisions" / "curator_app" / "index.html"

GEMMA_URL = "https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?shortName={gse}"
OBO_URL   = "http://purl.obolibrary.org/obo/{id_under}"  # for CLO, CL, BTO, UBERON
EFO_URL   = "http://www.ebi.ac.uk/efo/{id_under}"


def term_url(cid: str) -> str:
    """Return a clickable URL for an ontology id like 'EFO:0001185'."""
    if not cid or ":" not in cid:
        return ""
    prefix, local = cid.split(":", 1)
    id_under = f"{prefix}_{local}"
    if prefix.upper() in ("EFO", "efo"):
        return EFO_URL.format(id_under=id_under)
    return OBO_URL.format(id_under=id_under)


def build_rows():
    rows = []
    for r in csv.DictReader(open(TSV), delimiter="\t"):
        quotes = {
            "claude": [q for q in r.get("claude_quotes","").split("❗") if q],
            "opus":   [q for q in r.get("opus_quotes","").split("❗") if q],
            "geo":    [q for q in r.get("geo_evidence","").split("❗") if q],
        }
        sources = []
        for src, in_key, uri_key, color in (
            ("Gemma",  "in_gemma",  "gemma_uri",  "#10b981"),
            ("Claude Sonnet 4.6", "in_claude", "claude_uri", "#3b82f6"),
            ("Claude Opus 4.7",   "in_opus",   "opus_uri",   "#6366f1"),
            ("GPT-4o (published)","in_gpt4o",  "gpt4o_uri",  "#94a3b8"),
        ):
            sources.append({
                "name":  src,
                "color": color,
                "picked": r.get(in_key, "") == "✓",
                "uri":   r.get(uri_key, ""),
                "url":   term_url(r.get(uri_key, "")),
            })
        rows.append({
            "gse":              r["shortName"],
            "canonical_label":  r["canonical_label"],
            "canonical_uri":    r["canonical_uri"],
            "canonical_url":    term_url(r["canonical_uri"]),
            "sources":          sources,
            "quotes":           quotes,
            "picked_by":        r["picked_by"],
            "auto_accept":      r.get("auto_accept", "") == "TRUE",
        })
    return rows


HTML_TEMPLATE = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8" />
<title>Cell-line curator review</title>
<style>
  :root {
    --bg:        #f8fafc;
    --surface:   #ffffff;
    --border:    #e2e8f0;
    --text:      #0f172a;
    --muted:     #64748b;
    --accent:    #3b82f6;
    --green:     #10b981;
    --amber:     #f59e0b;
    --red:       #ef4444;
    --indigo:    #6366f1;
    --gray:      #94a3b8;
  }
  * { box-sizing: border-box; }
  html, body {
    margin: 0; padding: 0;
    font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
    background: var(--bg);
    color: var(--text);
    font-size: 14px;
    line-height: 1.5;
  }
  header {
    background: var(--surface);
    border-bottom: 1px solid var(--border);
    padding: 12px 24px;
    display: flex;
    justify-content: space-between;
    align-items: center;
    position: sticky; top: 0; z-index: 10;
  }
  header h1 {
    font-size: 14px; font-weight: 600; margin: 0;
  }
  .progress {
    display: flex; align-items: center; gap: 12px;
    color: var(--muted); font-size: 13px;
  }
  .progress-bar {
    width: 200px; height: 6px; background: var(--border); border-radius: 3px; overflow: hidden;
  }
  .progress-bar > div {
    height: 100%; background: var(--accent); transition: width .2s;
  }
  main {
    max-width: 880px; margin: 24px auto; padding: 0 24px;
  }
  .card {
    background: var(--surface);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 22px 26px;
    margin-bottom: 16px;
  }
  .card h2 {
    margin: 0 0 4px 0;
    font-size: 18px;
    font-weight: 600;
  }
  .card h2 a { color: inherit; text-decoration: none; border-bottom: 1px solid var(--border); }
  .card h2 a:hover { border-color: var(--accent); color: var(--accent); }
  .row-meta {
    color: var(--muted); font-size: 13px; margin-bottom: 12px;
  }
  .row-meta code {
    background: var(--bg);
    padding: 1px 6px; border-radius: 4px;
    font-size: 12px; font-family: "SF Mono", Menlo, monospace;
  }
  .canonical {
    margin-top: 6px; padding: 14px 16px;
    background: linear-gradient(180deg, #eff6ff 0%, #f8fafc 100%);
    border-left: 3px solid var(--accent);
    border-radius: 6px;
  }
  .canonical .label { font-size: 16px; font-weight: 600; }
  .canonical .uri  { color: var(--muted); font-size: 12px; font-family: "SF Mono", Menlo, monospace; margin-top: 2px; }
  .canonical a { color: inherit; text-decoration: none; border-bottom: 1px dotted var(--muted); }

  .sources { margin-top: 18px; }
  .source-row {
    display: grid; grid-template-columns: 24px 200px 1fr;
    gap: 12px;
    padding: 10px 12px;
    border-radius: 6px;
    align-items: baseline;
    margin-bottom: 4px;
  }
  .source-row.picked { background: var(--bg); }
  .source-row .pip {
    width: 18px; height: 18px; border-radius: 50%;
    display: inline-block;
    margin: 0 auto;
    border: 1.5px solid var(--border);
  }
  .source-row .pip.picked { background: var(--accent); border-color: var(--accent); }
  .source-row .name { font-weight: 500; }
  .source-row .uri {
    font-family: "SF Mono", Menlo, monospace; font-size: 12.5px; color: var(--muted);
  }
  .source-row .uri a { color: inherit; text-decoration: none; border-bottom: 1px dotted var(--muted); }
  .source-row .uri.empty { font-style: italic; color: #cbd5e1; }

  .evidence { margin-top: 18px; }
  .evidence h3 {
    font-size: 13px; font-weight: 600; color: var(--muted);
    text-transform: uppercase; letter-spacing: 0.06em;
    margin: 14px 0 6px 0;
  }
  .evidence ul {
    margin: 4px 0 12px 18px; padding: 0;
  }
  .evidence li {
    margin: 4px 0; color: var(--text); font-size: 13.5px;
  }
  .no-evidence {
    color: #cbd5e1; font-style: italic; font-size: 13px;
  }

  .verdict {
    margin-top: 22px; padding-top: 18px; border-top: 1px solid var(--border);
    display: flex; gap: 8px; align-items: center; flex-wrap: wrap;
  }
  .vote-btn {
    border: 1.5px solid var(--border);
    background: var(--surface);
    border-radius: 8px;
    padding: 10px 18px;
    font-size: 14px;
    font-weight: 500;
    cursor: pointer;
    color: var(--text);
    transition: border-color .15s, background .15s;
  }
  .vote-btn:hover { border-color: var(--accent); }
  .vote-btn.correct.active { background: var(--green); border-color: var(--green); color: #fff; }
  .vote-btn.wrong.active   { background: var(--red);   border-color: var(--red);   color: #fff; }
  .vote-btn.unsure.active  { background: var(--amber); border-color: var(--amber); color: #fff; }
  .note-input {
    flex: 1; min-width: 200px;
    padding: 9px 12px;
    border: 1px solid var(--border);
    border-radius: 6px;
    font-family: inherit; font-size: 13px;
  }
  .note-input:focus { outline: none; border-color: var(--accent); }

  .nav {
    display: flex; justify-content: space-between; gap: 8px; margin-top: 16px;
  }
  .nav-btn {
    flex: 1;
    border: 1px solid var(--border); background: var(--surface);
    border-radius: 6px; padding: 10px;
    font-size: 14px; cursor: pointer; color: var(--text);
  }
  .nav-btn:hover { border-color: var(--accent); color: var(--accent); }
  .nav-btn:disabled { opacity: .4; cursor: not-allowed; }

  .toolbar {
    background: var(--surface);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 12px 16px;
    display: flex; gap: 12px; align-items: center; flex-wrap: wrap;
    font-size: 13px;
  }
  .toolbar label { color: var(--muted); display: flex; gap: 6px; align-items: center; }
  .toolbar .spacer { flex: 1; }
  .toolbar button {
    border: 1px solid var(--border); background: var(--surface);
    border-radius: 6px; padding: 6px 12px;
    cursor: pointer; font-size: 13px; color: var(--text);
  }
  .toolbar button:hover { border-color: var(--accent); color: var(--accent); }
  .toolbar select {
    border: 1px solid var(--border); background: var(--surface);
    border-radius: 6px; padding: 5px 8px;
    font-size: 13px;
  }
  .kbd-hint {
    color: var(--muted); font-size: 12px; margin-top: 12px; text-align: center;
  }
  .kbd-hint kbd {
    border: 1px solid var(--border); border-bottom-width: 2px;
    border-radius: 4px; padding: 1px 6px;
    font-family: "SF Mono", Menlo, monospace; font-size: 11px;
    background: var(--surface);
  }
</style>
</head>
<body>
<header>
  <h1>Cell-line curator review</h1>
  <div class="progress">
    <span id="progress-text">0 / 0</span>
    <div class="progress-bar"><div id="progress-fill" style="width: 0%"></div></div>
  </div>
</header>

<main>
  <div class="toolbar">
    <label>Filter
      <select id="filter">
        <option value="all">All rows</option>
        <option value="unreviewed" selected>Unreviewed only</option>
        <option value="reviewed">Reviewed only</option>
      </select>
    </label>
    <label>
      <input type="checkbox" id="include-auto" />
      include auto-accepted rows
    </label>
    <div class="spacer"></div>
    <button id="export">Download verdicts JSON</button>
    <button id="import">Import…</button>
    <input type="file" id="import-file" hidden accept=".json" />
  </div>

  <div id="card" class="card">Loading…</div>

  <div class="nav">
    <button class="nav-btn" id="prev">← Previous</button>
    <button class="nav-btn" id="next">Next →</button>
  </div>
  <p class="kbd-hint">
    <kbd>←</kbd>/<kbd>→</kbd> navigate &middot;
    <kbd>1</kbd> correct &middot;
    <kbd>2</kbd> wrong &middot;
    <kbd>3</kbd> unsure &middot;
    <kbd>s</kbd> skip
  </p>
</main>

<script>
const DATA = __DATA_PLACEHOLDER__;
const STORAGE_KEY = "claude-cell-line-audit-verdicts-v1";
const GEMMA_URL = "https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?shortName={gse}";

let verdicts = JSON.parse(localStorage.getItem(STORAGE_KEY) || "{}");
let filter = "unreviewed";
let includeAuto = false;
let pos = 0;

function rowKey(r) { return r.gse + "::" + r.canonical_uri; }

function visibleRows() {
  return DATA.filter(r => {
    if (!includeAuto && r.auto_accept) return false;
    const v = verdicts[rowKey(r)];
    if (filter === "unreviewed") return !v || !v.verdict;
    if (filter === "reviewed")   return v && v.verdict;
    return true;
  });
}

function reviewedCount() {
  const candidates = DATA.filter(r => includeAuto || !r.auto_accept);
  return candidates.filter(r => verdicts[rowKey(r)] && verdicts[rowKey(r)].verdict).length;
}

function totalCount() {
  return DATA.filter(r => includeAuto || !r.auto_accept).length;
}

function escapeHtml(s) {
  return (s || "").replace(/[&<>"']/g, c =>
    ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
}

function renderRow(r) {
  if (!r) {
    return `<div class="card">All rows complete for this filter. Switch the filter above to keep reviewing, or export your verdicts.</div>`;
  }
  const v = verdicts[rowKey(r)] || {};
  const sources = r.sources.map(s => `
    <div class="source-row ${s.picked ? "picked" : ""}">
      <span class="pip ${s.picked ? "picked" : ""}" title="${s.picked ? "this source picked the URI" : "this source did not pick the URI"}"></span>
      <span class="name" style="${s.picked ? "color:" + s.color : "color:#94a3b8"}">${escapeHtml(s.name)}</span>
      <span class="uri ${s.uri ? "" : "empty"}">${s.uri ? `<a target="_blank" href="${s.url}">${escapeHtml(s.uri)}</a>` : "—"}</span>
    </div>`).join("");

  function evidenceBlock(label, items) {
    if (!items || !items.length) return "";
    return `<h3>${label}</h3><ul>${items.map(q => `<li>${escapeHtml(q)}</li>`).join("")}</ul>`;
  }
  let evidence = "";
  evidence += evidenceBlock("Sonnet 4.6 quotes",   r.quotes.claude);
  evidence += evidenceBlock("Opus 4.7 quotes",     r.quotes.opus);
  evidence += evidenceBlock("GEO characteristics", r.quotes.geo);
  if (!evidence) {
    evidence = `<p class="no-evidence">No supporting quotes captured. Open the Gemma link above to review the experiment metadata directly.</p>`;
  }

  return `
    <div class="card">
      <h2><a target="_blank" href="${GEMMA_URL.replace("{gse}", r.gse)}">${r.gse}</a></h2>
      <p class="row-meta">picked by <code>${escapeHtml(r.picked_by || "(none)")}</code>${r.auto_accept ? " &middot; auto-accepted" : ""}</p>

      <div class="canonical">
        <div class="label">${escapeHtml(r.canonical_label || "(no canonical label)")}</div>
        <div class="uri">${r.canonical_uri ? `<a target="_blank" href="${r.canonical_url}">${escapeHtml(r.canonical_uri)}</a>` : "(no URI)"}</div>
      </div>

      <div class="sources">${sources}</div>

      <div class="evidence">${evidence}</div>

      <div class="verdict">
        <button class="vote-btn correct ${v.verdict === "correct" ? "active" : ""}" data-vote="correct">✓ Correct</button>
        <button class="vote-btn wrong ${v.verdict === "wrong" ? "active" : ""}" data-vote="wrong">✗ Wrong</button>
        <button class="vote-btn unsure ${v.verdict === "unsure" ? "active" : ""}" data-vote="unsure">? Unsure</button>
        <input class="note-input" id="note" placeholder="Optional note…" value="${escapeHtml(v.note || "")}" />
      </div>
    </div>
  `;
}

function render() {
  const rows = visibleRows();
  if (pos >= rows.length) pos = Math.max(0, rows.length - 1);
  const r = rows[pos];
  document.getElementById("card").outerHTML = renderRow(r) || `<div id="card" class="card">Nothing to review.</div>`;
  // After replacing #card, re-bind event handlers
  document.querySelectorAll(".vote-btn").forEach(btn => {
    btn.addEventListener("click", () => {
      if (!r) return;
      const key = rowKey(r);
      const existing = verdicts[key] || {};
      verdicts[key] = {
        verdict: btn.dataset.vote,
        note:    existing.note || "",
        timestamp: new Date().toISOString(),
      };
      saveVerdicts();
      // Auto-advance after correct/wrong; stay on unsure
      if (btn.dataset.vote !== "unsure") {
        setTimeout(() => { advance(1); }, 80);
      } else {
        render();
      }
    });
  });
  const note = document.getElementById("note");
  if (note) {
    note.addEventListener("input", () => {
      if (!r) return;
      const key = rowKey(r);
      verdicts[key] = verdicts[key] || { verdict: "" };
      verdicts[key].note = note.value;
      saveVerdicts();
    });
  }
  // Progress
  const done = reviewedCount();
  const tot  = totalCount();
  document.getElementById("progress-text").textContent =
    `${done} / ${tot} reviewed${tot ? "" : ""} ${rows.length ? `· row ${pos + 1} / ${rows.length}` : ""}`;
  document.getElementById("progress-fill").style.width = tot ? `${100 * done / tot}%` : "0%";
  document.getElementById("prev").disabled = pos <= 0;
  document.getElementById("next").disabled = rows.length === 0 || pos >= rows.length - 1;
}

function saveVerdicts() {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(verdicts));
}

function advance(delta) {
  const rows = visibleRows();
  // When filter == 'unreviewed' and we just reviewed the current row, it
  // drops out of the list; stay at the same index (the next row slides in).
  if (filter === "unreviewed") {
    pos = Math.min(pos, Math.max(0, rows.length - 1));
  } else {
    pos = Math.max(0, Math.min(rows.length - 1, pos + delta));
  }
  render();
}

document.getElementById("prev").addEventListener("click", () => { pos = Math.max(0, pos - 1); render(); });
document.getElementById("next").addEventListener("click", () => { pos = pos + 1; render(); });

document.getElementById("filter").addEventListener("change", e => {
  filter = e.target.value; pos = 0; render();
});
document.getElementById("include-auto").addEventListener("change", e => {
  includeAuto = e.target.checked; pos = 0; render();
});

document.getElementById("export").addEventListener("click", () => {
  const blob = new Blob([JSON.stringify({
    version: "1.0",
    saved_at: new Date().toISOString(),
    total: totalCount(),
    completed: reviewedCount(),
    verdicts: verdicts,
  }, null, 2)], { type: "application/json" });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = `curator_verdicts_${new Date().toISOString().slice(0,10)}.json`;
  a.click();
});

document.getElementById("import").addEventListener("click", () => document.getElementById("import-file").click());
document.getElementById("import-file").addEventListener("change", e => {
  const f = e.target.files[0];
  if (!f) return;
  const reader = new FileReader();
  reader.onload = () => {
    try {
      const data = JSON.parse(reader.result);
      const merged = { ...verdicts, ...(data.verdicts || data) };
      verdicts = merged;
      saveVerdicts();
      render();
    } catch (err) { alert("Could not parse JSON: " + err.message); }
  };
  reader.readAsText(f);
});

document.addEventListener("keydown", e => {
  if (e.target && (e.target.tagName === "INPUT" || e.target.tagName === "TEXTAREA")) return;
  if (e.key === "ArrowLeft")  { document.getElementById("prev").click(); }
  if (e.key === "ArrowRight") { document.getElementById("next").click(); }
  if (e.key === "1") { document.querySelector('.vote-btn.correct')?.click(); }
  if (e.key === "2") { document.querySelector('.vote-btn.wrong')?.click(); }
  if (e.key === "3") { document.querySelector('.vote-btn.unsure')?.click(); }
  if (e.key === "s") { advance(1); }
});

render();
</script>
</body>
</html>
"""


def main():
    rows = build_rows()
    n_total = len(rows)
    n_review = sum(1 for r in rows if not r["auto_accept"])
    print(f"loaded {n_total} rows ({n_review} for review, {n_total - n_review} auto-accepted)",
          file=sys.stderr)
    page = HTML_TEMPLATE.replace("__DATA_PLACEHOLDER__", json.dumps(rows, ensure_ascii=False))
    OUT.write_text(page, encoding="utf-8")
    print(f"wrote {OUT}  ({OUT.stat().st_size // 1024} KB)", file=sys.stderr)


if __name__ == "__main__":
    main()
