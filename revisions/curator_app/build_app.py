"""Generate a single-file portable curator web app from the long-format TSV.

Output: revisions/curator_app/index.html

Embeds inline:
  - one card per row of long_curation_sheet.tsv;
  - a term-details map (name + synonyms + definition) for every URI
    mentioned in the audit, so curators can inspect an ontology term
    without clicking through to EBI / OBO;
  - a per-GSE Gemma metadata block (dataset description + relevant
    samples) fetched from the Gemma REST API.
"""
import csv
import html
import json
import os
import re
import sys
from pathlib import Path

REPO   = Path(__file__).resolve().parent.parent.parent
TSV    = REPO / "revisions" / "data" / "long_curation_sheet.tsv"
TERMS  = REPO / "revisions" / "data" / "cell_line_terms.json"
GEMMA  = REPO / "revisions" / "data" / "gemma_cache"
ISA    = REPO / "revisions" / "data" / "cell_line_isa.json"

# Sys-path shim so we can import the cross-walk index (cell_line_eval) from the
# parent revisions/ directory.
sys.path.insert(0, str(REPO / "revisions"))
OUT    = REPO / "revisions" / "curator_app" / "index.html"

OBO_URL = "http://purl.obolibrary.org/obo/{id_under}"
EFO_URL = "http://www.ebi.ac.uk/efo/{id_under}"


def canonical_id(s: str) -> str:
    if not s: return ""
    s = s.strip()
    if s.startswith("http"): s = s.rsplit("/", 1)[-1]
    local = s.split(":", 1)[1] if ":" in s else s
    if "_" in local:
        p, _, r = local.partition("_")
        return f"{p.upper()}:{r}"
    if ":" in s:
        p, _, r = s.partition(":")
        return f"{p.upper()}:{r}"
    return s


def term_url(cid: str) -> str:
    if not cid or ":" not in cid: return ""
    prefix, local = cid.split(":", 1)
    id_under = f"{prefix}_{local}"
    if prefix.upper() == "EFO": return EFO_URL.format(id_under=id_under)
    return OBO_URL.format(id_under=id_under)


def parse_tagged_quote(q: str) -> dict:
    """Split ``[source] body`` into structured form."""
    m = re.match(r"^\[([^\]]+)\]\s*(.*)$", q.strip(), re.DOTALL)
    if m:
        return {"source": m.group(1).strip(), "text": m.group(2).strip()}
    return {"source": "", "text": q.strip()}


_RELATED_CACHE = {}
_XREF = None
_ISA = None


def _load_isa_xref():
    global _XREF, _ISA
    if _ISA is None:
        if ISA.exists():
            _ISA = json.load(open(ISA))
        else:
            _ISA = {"parents": {}, "children": {}}
    if _XREF is None:
        from cell_line_eval import CellLineXrefIndex
        _XREF = CellLineXrefIndex()


def related_uris(cid: str) -> set[str]:
    """Return the set of ontology IDs considered 'closely related' to `cid`:
    cross-walk equivalents PLUS direct is_a parents PLUS direct is_a children.
    Cached. Used to recognise specificity disagreements (e.g. parent vs child
    in CLO) as 'Gemma-related' rather than as novel LLM extras."""
    if cid in _RELATED_CACHE:
        return _RELATED_CACHE[cid]
    _load_isa_xref()
    out = set()
    # cross-walk equivalence class
    out |= set(_XREF.expand(cid)) if cid else set()
    # is_a parents / children of `cid` and of every cross-walk equivalent
    for x in list(out) + [cid]:
        out |= set(_ISA["parents"].get(x, []))
        out |= set(_ISA["children"].get(x, []))
    _RELATED_CACHE[cid] = out
    return out


def build_rows():
    rows = []
    seen_uris = set()
    for r in csv.DictReader(open(TSV), delimiter="\t"):
        quotes = {
            "claude": [parse_tagged_quote(q) for q in r.get("claude_quotes","").split("❗") if q],
            "opus":   [parse_tagged_quote(q) for q in r.get("opus_quotes","").split("❗")   if q],
            "geo":    [parse_tagged_quote(q) for q in r.get("geo_evidence","").split("❗")  if q],
        }
        sources = []
        for src, in_key, uri_key, color in (
            ("Gemma",              "in_gemma",  "gemma_uri",  "#10b981"),
            ("Claude Sonnet 4.6",  "in_claude", "claude_uri", "#3b82f6"),
            ("Claude Opus 4.7",    "in_opus",   "opus_uri",   "#6366f1"),
            ("GPT-4o (published)", "in_gpt4o",  "gpt4o_uri",  "#94a3b8"),
        ):
            uri = r.get(uri_key, "")
            if uri: seen_uris.add(uri)
            sources.append({
                "name":  src,
                "color": color,
                "picked": r.get(in_key, "") == "✓",
                "uri":   uri,
                "url":   term_url(uri),
            })
        cu = r.get("canonical_uri", "")
        if cu: seen_uris.add(cu)
        rows.append({
            "gse":              r["shortName"],
            "canonical_label":  r["canonical_label"],
            "canonical_uri":    cu,
            "canonical_url":    term_url(cu),
            "sources":          sources,
            "quotes":           quotes,
            "picked_by":        r["picked_by"],
            "auto_accept":      r.get("auto_accept", "") == "TRUE",
        })

    # Second pass: for each row whose own Gemma source did NOT pick it, decide
    # whether the row is `gemma_related` — i.e. any Gemma URI on the same GSE
    # is in the row's cross-walk + direct is_a closure. Cases like Sonnet+Opus
    # picking CLO:0037308 ("human iPSC line") when Gemma picked the parent
    # CLO:0037307 ("iPSC line") are flagged as related so the audit can skip
    # them; they are specificity disagreements, not novel extras.
    print(f"  computing gemma_related flag …", file=sys.stderr)
    # Build per-GSE Gemma URI sets
    gse_gemma: dict[str, set[str]] = {}
    for row in rows:
        if row["sources"][0]["picked"] and row["canonical_uri"]:
            gse_gemma.setdefault(row["gse"], set()).add(row["canonical_uri"])
    n_flag = 0
    for row in rows:
        # Only flag rows where Gemma is silent on this canonical
        row["gemma_related"] = False
        if row["sources"][0]["picked"]:
            continue
        cu = row["canonical_uri"]
        if not cu:
            continue
        rel = related_uris(cu)
        if rel & gse_gemma.get(row["gse"], set()):
            row["gemma_related"] = True
            n_flag += 1
    print(f"  {n_flag} rows flagged as Gemma-related (specificity disagreements)",
          file=sys.stderr)

    return rows, seen_uris


def build_terms_map(seen_uris: set[str]) -> dict:
    """Subset cell_line_terms.json to just the URIs that appear in the audit."""
    if not TERMS.exists():
        return {}
    full = json.load(open(TERMS))
    out = {}
    for u in seen_uris:
        c = canonical_id(u)
        if ":" not in c:
            continue
        prefix, local = c.split(":", 1)
        # The terms JSON has historically mixed "efo:EFO_0001185" and "EFO:0001185"
        # style keys; try the obvious variants.
        candidates = [
            c,
            f"{prefix.lower()}:{prefix.upper()}_{local}",
            f"{prefix.upper()}_{local}",
            f"{prefix.lower()}:{local}",
        ]
        t = next((full[k] for k in candidates if k in full), None)
        if not t: continue
        names    = [n for n in (t.get("names") or []) if n][:1]
        synonyms = list({s for s in (t.get("synonyms") or []) if s})
        synonyms.sort()
        defn_raw = t.get("definition") or ""
        if isinstance(defn_raw, list): defn_raw = " ".join(str(x) for x in defn_raw)
        defn = str(defn_raw).strip()
        if len(defn) > 600: defn = defn[:600].rsplit(" ", 1)[0] + "…"
        out[c] = {
            "label":      names[0] if names else c,
            "synonyms":   synonyms[:12],
            "definition": defn,
        }
    return out


def build_gemma_map(gses: set[str], rows: list) -> dict:
    """Per-GSE Gemma metadata, trimmed to keep the embedded payload small.

    For each GSE we keep the dataset name + truncated description + the
    dataset-level characteristics, plus the subset of samples whose
    characteristics mention any name we audit on that experiment."""
    # Collect, per GSE, the set of cell-line names to search for in samples.
    names_per_gse: dict[str, set[str]] = {}
    for r in rows:
        names_per_gse.setdefault(r["gse"], set()).add(r["canonical_label"].lower())

    out: dict[str, dict] = {}
    for gse in gses:
        p = GEMMA / f"{gse}.json"
        if not p.exists():
            continue
        try:
            d = json.load(open(p))
        except Exception:
            continue
        if d.get("error"):
            out[gse] = {"error": d["error"], "name": "", "description": ""}
            continue
        desc = (d.get("description") or "").strip()
        if len(desc) > 800: desc = desc[:800].rsplit(" ", 1)[0] + "…"
        keep_samples = []
        needles = names_per_gse.get(gse, set())
        needles = {n for n in needles if n and len(n) >= 3}
        for s in (d.get("samples") or []):
            chars = s.get("characteristics") or []
            # Always keep the cell-line / cell-type characteristics
            keep_cats = ("cell line","cell type","cell","biosource","disease",
                         "treatment","genotype","strain")
            relevant = [c for c in chars
                        if (c.get("category") or "").lower() in keep_cats]
            mentions = False
            if needles:
                blob = " ".join((c.get("value","") or "") for c in chars).lower()
                if any(n in blob for n in needles):
                    mentions = True
            if mentions or relevant:
                keep_samples.append({
                    "name":  s.get("name",""),
                    "characteristics": [
                        {"category": c.get("category",""),
                         "value":    c.get("value",""),
                         "valueUri": c.get("valueUri","")}
                        for c in (relevant or chars[:6])
                    ],
                })
            if len(keep_samples) >= 30: break  # cap per-experiment payload
        out[gse] = {
            "name":            d.get("name") or "",
            "description":     desc,
            "curationNote":    d.get("curationNote") or "",
            "taxon":           d.get("taxon") or "",
            "bioAssayCount":   d.get("bioAssayCount") or 0,
            "characteristics": [
                {"category": c.get("category",""),
                 "value":    c.get("value",""),
                 "valueUri": c.get("valueUri","")}
                for c in (d.get("characteristics") or [])
            ],
            "samples":         keep_samples,
        }
    return out


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
  html, body { margin: 0; padding: 0; background: var(--bg); color: var(--text);
    font-family: "Helvetica Neue", Helvetica, Arial, sans-serif; font-size: 14px; line-height: 1.5; }
  header { background: var(--surface); border-bottom: 1px solid var(--border);
    padding: 12px 24px; display: flex; justify-content: space-between; align-items: center;
    position: sticky; top: 0; z-index: 10; }
  header h1 { font-size: 14px; font-weight: 600; margin: 0; }
  .progress { display: flex; align-items: center; gap: 12px; color: var(--muted); font-size: 13px; }
  .progress-bar { width: 200px; height: 6px; background: var(--border); border-radius: 3px; overflow: hidden; }
  .progress-bar > div { height: 100%; background: var(--accent); transition: width .2s; }
  main { max-width: 920px; margin: 24px auto; padding: 0 24px; }
  .card { background: var(--surface); border: 1px solid var(--border); border-radius: 8px;
    padding: 22px 26px; margin-bottom: 16px; }
  .card h2 { margin: 0 0 4px 0; font-size: 18px; font-weight: 600; }
  .card h2 a { color: inherit; text-decoration: none; border-bottom: 1px solid var(--border); }
  .card h2 a:hover { border-color: var(--accent); color: var(--accent); }
  .row-meta { color: var(--muted); font-size: 13px; margin-bottom: 12px; }
  .row-meta code { background: var(--bg); padding: 1px 6px; border-radius: 4px;
    font-size: 12px; font-family: "SF Mono", Menlo, monospace; }
  .canonical { margin-top: 6px; padding: 14px 16px;
    background: linear-gradient(180deg, #eff6ff 0%, #f8fafc 100%);
    border-left: 3px solid var(--accent); border-radius: 6px; }
  .canonical .label { font-size: 16px; font-weight: 600; cursor: pointer; }
  .canonical .label:hover { color: var(--accent); }
  .canonical .uri  { color: var(--muted); font-size: 12px;
    font-family: "SF Mono", Menlo, monospace; margin-top: 2px; }
  .canonical a { color: inherit; text-decoration: none; border-bottom: 1px dotted var(--muted); }

  .section { margin-top: 22px; }
  .section h3 { font-size: 12px; font-weight: 600; color: var(--muted);
    text-transform: uppercase; letter-spacing: 0.07em; margin: 0 0 8px 0; }

  .evidence-item { padding: 8px 12px; margin-bottom: 6px; border-radius: 6px;
    background: var(--bg); border-left: 3px solid #cbd5e1; }
  .evidence-item .src-tag { display: inline-block; font-size: 10.5px; font-weight: 600;
    text-transform: uppercase; letter-spacing: 0.06em; color: var(--muted);
    background: var(--surface); padding: 1px 6px; border-radius: 3px;
    border: 1px solid var(--border); margin-right: 8px; vertical-align: 1px; }
  .evidence-item .src-tag.characteristic { color: #0284c7; border-color: #bae6fd; }
  .evidence-item .src-tag.summary         { color: #7c3aed; border-color: #ddd6fe; }
  .evidence-item .src-tag.overall-design  { color: #7c3aed; border-color: #ddd6fe; }
  .evidence-item .src-tag.paper-abstract  { color: #9333ea; border-color: #e9d5ff; }
  .evidence-item .src-tag.paper-methods   { color: #9333ea; border-color: #e9d5ff; }
  .evidence-item .src-tag.sample-title    { color: #0284c7; border-color: #bae6fd; }
  .evidence-item .src-tag.unknown         { color: var(--muted); }
  .evidence-item .text { color: var(--text); font-size: 13.5px; }
  .evidence-from { color: var(--muted); font-size: 11.5px; margin: 6px 0 4px 0;
    text-transform: uppercase; letter-spacing: 0.05em; font-weight: 500; }
  .no-evidence { color: #cbd5e1; font-style: italic; font-size: 13px; }

  .source-row { display: grid; grid-template-columns: 24px 200px 1fr; gap: 12px;
    padding: 10px 12px; border-radius: 6px; align-items: baseline; margin-bottom: 4px; }
  .source-row.picked { background: var(--bg); }
  .source-row .pip { width: 18px; height: 18px; border-radius: 50%; display: inline-block;
    margin: 0 auto; border: 1.5px solid var(--border); }
  .source-row .pip.picked { background: var(--accent); border-color: var(--accent); }
  .source-row .name { font-weight: 500; }
  .source-row .uri { font-family: "SF Mono", Menlo, monospace; font-size: 12.5px; color: var(--muted); }
  .source-row .uri a { color: inherit; text-decoration: none; border-bottom: 1px dotted var(--muted); cursor: pointer; }
  .source-row .uri a:hover { color: var(--accent); border-color: var(--accent); }
  .source-row .uri.empty { font-style: italic; color: #cbd5e1; }

  .gemma details { margin-top: 4px; }
  .gemma summary { cursor: pointer; color: var(--muted); font-size: 13px; padding: 6px 0;
    list-style: none; }
  .gemma summary::marker, .gemma summary::-webkit-details-marker { display: none; }
  .gemma summary::before { content: "▸"; display: inline-block; width: 14px;
    transition: transform .15s; color: var(--gray); }
  .gemma details[open] summary::before { transform: rotate(90deg); }
  .gemma .gemma-body { padding: 12px 14px; background: var(--bg); border-radius: 6px;
    margin-top: 4px; }
  .gemma .gemma-name { font-weight: 600; }
  .gemma .gemma-desc { color: var(--muted); font-size: 12.5px; margin-top: 4px; }
  .gemma-chars { margin-top: 8px; }
  .gemma-chars .chip { display: inline-block; font-size: 11.5px;
    background: var(--surface); border: 1px solid var(--border); border-radius: 999px;
    padding: 2px 9px; margin: 2px 4px 2px 0; }
  .gemma-chars .chip .cat { color: var(--muted); margin-right: 4px; }
  .gemma-sample { margin-top: 8px; padding-left: 6px; border-left: 2px solid var(--border);
    color: var(--muted); font-size: 12.5px; }
  .gemma-sample .name { color: var(--text); font-weight: 500; }

  .other-rows { width: 100%; border-collapse: collapse; font-size: 13px; }
  .other-rows th { text-align: left; font-weight: 500; color: var(--muted); font-size: 11.5px;
    padding: 4px 8px; border-bottom: 1px solid var(--border); }
  .other-rows th:not(:first-child):not(:last-child) { text-align: center; width: 28px; }
  .other-rows td { padding: 4px 8px; border-bottom: 1px solid #f1f5f9; vertical-align: top; }
  .other-rows td:not(:first-child):not(:last-child) { text-align: center; }
  .other-rows tr.gemma-match td:first-child { border-left: 3px solid #10b981; padding-left: 6px; }
  .other-rows tr.llm-extra td:first-child   { border-left: 3px solid #f59e0b; padding-left: 6px; }
  .other-rows td.on  { color: #0f172a; font-weight: 600; }
  .other-rows td.off { color: #cbd5e1; }
  .other-rows td.muted { color: var(--muted); font-size: 12px; }
  .gemma-related-badge { color: #10b981; font-weight: 500; }

  .verdict { margin: 14px 0 18px 0; padding: 12px 14px;
    background: var(--surface); border: 1px solid var(--border); border-radius: 10px;
    position: sticky; top: 96px; z-index: 4;
    display: flex; gap: 8px; align-items: center; flex-wrap: wrap;
    box-shadow: 0 1px 3px rgba(15,23,42,0.04); }
  .vote-btn { border: 1.5px solid var(--border); background: var(--surface);
    border-radius: 8px; padding: 10px 18px; font-size: 14px; font-weight: 500;
    cursor: pointer; color: var(--text); transition: border-color .15s, background .15s; }
  .vote-btn:hover { border-color: var(--accent); }
  .vote-btn.correct.active { background: var(--green); border-color: var(--green); color: #fff; }
  .vote-btn.wrong.active   { background: var(--red);   border-color: var(--red);   color: #fff; }
  .vote-btn.unsure.active  { background: var(--amber); border-color: var(--amber); color: #fff; }
  .note-input { flex: 1; min-width: 200px; padding: 9px 12px; border: 1px solid var(--border);
    border-radius: 6px; font-family: inherit; font-size: 13px; }
  .note-input:focus { outline: none; border-color: var(--accent); }

  .nav { display: flex; justify-content: space-between; gap: 8px; margin: 12px 0; }
  .nav-top { position: sticky; top: 48px; z-index: 5;
    background: var(--bg); padding: 6px 0; margin-top: 0; }
  .nav-btn { flex: 1; border: 1px solid var(--border); background: var(--surface);
    border-radius: 6px; padding: 10px; font-size: 14px; cursor: pointer; color: var(--text); }
  .nav-btn:hover { border-color: var(--accent); color: var(--accent); }

  .toolbar { background: var(--surface); border: 1px solid var(--border); border-radius: 8px;
    padding: 12px 16px; display: flex; gap: 12px; align-items: center; flex-wrap: wrap; font-size: 13px; }
  .toolbar label { color: var(--muted); display: flex; gap: 6px; align-items: center; }
  .toolbar .spacer { flex: 1; }
  .toolbar button { border: 1px solid var(--border); background: var(--surface);
    border-radius: 6px; padding: 6px 12px; cursor: pointer; font-size: 13px; color: var(--text); }
  .toolbar button:hover { border-color: var(--accent); color: var(--accent); }
  .toolbar select { border: 1px solid var(--border); background: var(--surface);
    border-radius: 6px; padding: 5px 8px; font-size: 13px; }
  .kbd-hint { color: var(--muted); font-size: 12px; margin-top: 12px; text-align: center; }
  .kbd-hint kbd { border: 1px solid var(--border); border-bottom-width: 2px; border-radius: 4px;
    padding: 1px 6px; font-family: "SF Mono", Menlo, monospace; font-size: 11px; background: var(--surface); }

  /* Term details popover */
  #term-pop { position: fixed; max-width: 460px; background: var(--surface);
    border: 1px solid var(--border); border-radius: 8px; padding: 14px 16px;
    box-shadow: 0 10px 25px rgba(15, 23, 42, 0.12); z-index: 100; display: none; font-size: 13px; }
  #term-pop .name { font-weight: 600; font-size: 14px; }
  #term-pop .uri { font-family: "SF Mono", Menlo, monospace; font-size: 11px; color: var(--muted); margin: 3px 0 10px 0; }
  #term-pop .definition { color: var(--text); line-height: 1.5; margin-bottom: 10px; }
  #term-pop .syn-label { font-size: 11px; color: var(--muted); text-transform: uppercase;
    letter-spacing: 0.05em; margin-bottom: 4px; }
  #term-pop .syn { display: inline-block; font-size: 11.5px; background: var(--bg);
    border-radius: 4px; padding: 2px 7px; margin: 2px 4px 2px 0; }
  #term-pop .close { position: absolute; top: 6px; right: 10px; cursor: pointer;
    color: var(--muted); font-size: 18px; line-height: 1; }
  #term-pop .open-ebi { font-size: 11.5px; color: var(--accent); text-decoration: none; }
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
    <label>Review status
      <select id="filter">
        <option value="all">All</option>
        <option value="unreviewed" selected>Unreviewed only</option>
        <option value="reviewed">Reviewed only</option>
      </select>
    </label>
    <label>Priority
      <select id="priority">
        <option value="claude_extra_goes_with" selected>Sonnet/Opus extras alongside a Gemma match  (156)</option>
        <option value="both_claude_extra_goes_with">Sonnet AND Opus agree, alongside a Gemma match  (31)</option>
        <option value="gpt_only_goes_with">GPT-4o extras alongside a Gemma match  (55)</option>
        <option value="all">All rows  (960)</option>
      </select>
    </label>
    <div class="spacer"></div>
    <button id="export">Download verdicts JSON</button>
    <button id="import">Import…</button>
    <input type="file" id="import-file" hidden accept=".json" />
  </div>

  <div class="nav nav-top">
    <button class="nav-btn" id="prev">← Previous</button>
    <button class="nav-btn" id="next">Next →</button>
  </div>

  <div id="card-mount"><div class="card">Loading…</div></div>

  <p class="kbd-hint">
    <kbd>←</kbd>/<kbd>→</kbd> navigate &middot;
    <kbd>1</kbd> correct &middot;
    <kbd>2</kbd> wrong &middot;
    <kbd>3</kbd> unsure &middot;
    <kbd>s</kbd> skip
  </p>
</main>

<div id="term-pop">
  <span class="close" onclick="document.getElementById('term-pop').style.display='none'">×</span>
  <div class="name" id="tp-name"></div>
  <div class="uri" id="tp-uri"></div>
  <div class="definition" id="tp-def"></div>
  <div id="tp-syn-wrap" style="display:none">
    <div class="syn-label">Synonyms</div>
    <div id="tp-syns"></div>
  </div>
  <a id="tp-link" class="open-ebi" target="_blank">Open in EBI / OBO ↗</a>
</div>

<script>
const DATA   = __ROWS_PLACEHOLDER__;
const TERMS  = __TERMS_PLACEHOLDER__;
const GEMMA  = __GEMMA_PLACEHOLDER__;
const STORAGE_KEY = "claude-cell-line-audit-verdicts-v1";
const GEMMA_URL = "https://gemma.msl.ubc.ca/expressionExperiment/showExpressionExperiment.html?shortName={gse}";

let verdicts = JSON.parse(localStorage.getItem(STORAGE_KEY) || "{}");
let filter   = "unreviewed";
let priority = "claude_extra_goes_with";  // default: extras that go with a same-LLM Gemma match
let pos = 0;

// Pre-compute per-GSE which LLMs have at least one row that matches Gemma
// (in_gemma=✓ AND that LLM=✓). Used by the "goes with a Gemma match" filters
// to restrict the audit to extras that accompany a confirmed correct call.
const GSE_LLM_MATCHED_GEMMA = (() => {
  const out = {};
  for (const r of DATA) {
    const g = r.sources[0].picked, c = r.sources[1].picked, o = r.sources[2].picked, p = r.sources[3].picked;
    if (!g) continue;
    const e = out[r.gse] ||= { claude: false, opus: false, gpt4o: false };
    if (c) e.claude = true;
    if (o) e.opus   = true;
    if (p) e.gpt4o  = true;
  }
  return out;
})();

function matchesPriority(r) {
  const g = r.sources[0].picked;  // Gemma
  const c = r.sources[1].picked;  // Claude Sonnet
  const o = r.sources[2].picked;  // Claude Opus
  const p = r.sources[3].picked;  // GPT-4o (published)
  const m = GSE_LLM_MATCHED_GEMMA[r.gse] || {};
  // For every audit-style filter below, also exclude rows where Gemma annotated
  // a cross-walk- or is_a-related concept on the same GSE — those are specificity
  // disagreements, not novel LLM extras worth curator attention.
  const novel = !g && !r.gemma_related;
  if (priority === "all")                          return true;
  if (priority === "claude_extra")                 return novel && (c || o);
  if (priority === "both_claude_extra")            return novel && c && o;
  if (priority === "gpt_only")                     return novel && p && !c && !o;
  // "Goes with a Gemma match" variants: the LLM picking this extra must ALSO have
  // at least one Gemma-matching prediction on the same GSE.
  if (priority === "claude_extra_goes_with")       return novel && ((c && m.claude) || (o && m.opus));
  if (priority === "both_claude_extra_goes_with")  return novel && c && o && m.claude && m.opus;
  if (priority === "gpt_only_goes_with")           return novel && p && !c && !o && m.gpt4o;
  return true;
}

function rowKey(r) { return r.gse + "::" + r.canonical_uri; }

function visibleRows() {
  return DATA.filter(r => {
    if (!matchesPriority(r)) return false;
    const v = verdicts[rowKey(r)];
    if (filter === "unreviewed") return !v || !v.verdict;
    if (filter === "reviewed")   return v && v.verdict;
    return true;
  });
}

function reviewedCount() {
  const c = DATA.filter(matchesPriority);
  return c.filter(r => verdicts[rowKey(r)] && verdicts[rowKey(r)].verdict).length;
}
function totalCount() { return DATA.filter(matchesPriority).length; }

function escapeHtml(s) {
  return (s || "").replace(/[&<>"']/g, c =>
    ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));
}

function canonicalize(uri) {
  // Convert URI or CURIE-like string to canonical PREFIX:LOCAL
  if (!uri) return "";
  let s = uri.trim();
  if (s.startsWith("http")) s = s.split("/").pop();
  if (s.indexOf("_") >= 0) {
    const [p, ...r] = s.split("_");
    return p.toUpperCase() + ":" + r.join("_");
  }
  if (s.indexOf(":") >= 0) {
    const [p, r] = s.split(":", 2);
    return p.toUpperCase() + ":" + r;
  }
  return s;
}

function renderQuoteSet(label, items) {
  if (!items || !items.length) return "";
  const li = items.map(q => {
    const srcKey = (q.source || "unknown").replace(/\\s+/g, "-").toLowerCase();
    const srcLabel = q.source || "unknown";
    return `<div class="evidence-item">
      <span class="src-tag ${srcKey}">${escapeHtml(srcLabel)}</span>
      <span class="text">${escapeHtml(q.text)}</span>
    </div>`;
  }).join("");
  return `<div class="evidence-from">${escapeHtml(label)}</div>${li}`;
}

function renderEvidence(r) {
  let out = "";
  out += renderQuoteSet("Sonnet 4.6 first-pass",    r.quotes.claude);
  out += renderQuoteSet("Opus 4.7 first-pass",      r.quotes.opus);
  out += renderQuoteSet("GEO sample characteristics", r.quotes.geo);
  if (!out) {
    return `<p class="no-evidence">No supporting quotes captured.
      Open the Gemma link below to inspect the experiment metadata directly.</p>`;
  }
  return out;
}

function renderGemma(gse) {
  const g = GEMMA[gse];
  if (!g) return `<div class="gemma"><p class="no-evidence">No Gemma metadata cached for this GSE.</p></div>`;
  if (g.error) return `<div class="gemma"><p class="no-evidence">Gemma: ${escapeHtml(g.error)}</p></div>`;
  const chars = (g.characteristics || []).map(c =>
    `<span class="chip"><span class="cat">${escapeHtml(c.category || "")}</span>${escapeHtml(c.value || "")}</span>`
  ).join("");
  const samples = (g.samples || []).slice(0, 12).map(s => {
    const sc = (s.characteristics || []).map(c =>
      `<span class="chip"><span class="cat">${escapeHtml(c.category)}</span>${escapeHtml(c.value)}</span>`
    ).join("");
    return `<div class="gemma-sample"><span class="name">${escapeHtml(s.name)}</span><br/>${sc}</div>`;
  }).join("");
  return `<details>
    <summary>Gemma metadata${g.bioAssayCount ? ` &middot; ${g.bioAssayCount} samples` : ""}${g.taxon ? ` &middot; ${escapeHtml(g.taxon)}` : ""}</summary>
    <div class="gemma-body">
      <div class="gemma-name">${escapeHtml(g.name)}</div>
      <div class="gemma-desc">${escapeHtml(g.description)}</div>
      ${chars ? `<div class="gemma-chars">${chars}</div>` : ""}
      ${samples ? `<div style="margin-top:10px"><div class="syn-label" style="font-size:11px; color:var(--muted); text-transform:uppercase; letter-spacing:0.05em;">Relevant samples</div>${samples}</div>` : ""}
    </div>
  </details>`;
}

// Group DATA by GSE once so the per-card "other rows" section is O(1) per row.
const ROWS_BY_GSE = (() => {
  const out = {};
  for (const r of DATA) (out[r.gse] ||= []).push(r);
  return out;
})();

function renderOtherRows(r) {
  const all = ROWS_BY_GSE[r.gse] || [];
  const others = all.filter(o => o.canonical_uri !== r.canonical_uri);
  if (others.length === 0) {
    return `<p class="row-meta">(this is the only annotation row for this GSE)</p>`;
  }
  // For each other row, show: canonical label, who picked, gemma-match badge
  return `<table class="other-rows">
    <thead><tr><th>cell-line annotation</th><th>G</th><th>S</th><th>O</th><th>P</th><th>picked by</th></tr></thead>
    <tbody>${others.map(o => {
      const g = o.sources[0].picked, c = o.sources[1].picked, oo = o.sources[2].picked, p = o.sources[3].picked;
      const cls = g ? "gemma-match" : (c||oo||p) ? "llm-extra" : "";
      const dot = picked => picked ? "●" : "·";
      return `<tr class="${cls}">
        <td><a class="term-link" data-uri="${escapeHtml(o.canonical_uri)}">${escapeHtml(o.canonical_label || o.canonical_uri || "(none)")}</a></td>
        <td class="${g?'on':'off'}">${dot(g)}</td>
        <td class="${c?'on':'off'}">${dot(c)}</td>
        <td class="${oo?'on':'off'}">${dot(oo)}</td>
        <td class="${p?'on':'off'}">${dot(p)}</td>
        <td class="muted">${escapeHtml(o.picked_by || "")}</td>
      </tr>`;
    }).join("")}</tbody>
  </table>`;
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
      <span class="uri ${s.uri ? "" : "empty"}">${s.uri ? `<a class="term-link" data-uri="${escapeHtml(s.uri)}">${escapeHtml(s.uri)}</a>` : "—"}</span>
    </div>`).join("");

  return `<div class="card">
    <h2><a target="_blank" href="${GEMMA_URL.replace("{gse}", r.gse)}">${r.gse}</a></h2>
    <p class="row-meta">picked by <code>${escapeHtml(r.picked_by || "(none)")}</code>${r.auto_accept ? " &middot; auto-accepted" : ""}${r.gemma_related ? " &middot; <span class='gemma-related-badge'>Gemma-related (specificity disagreement)</span>" : ""}</p>

    <div class="canonical">
      <div class="label term-link" data-uri="${escapeHtml(r.canonical_uri)}">${escapeHtml(r.canonical_label || "(no canonical label)")}</div>
      <div class="uri">${r.canonical_uri ? `<a class="term-link" data-uri="${escapeHtml(r.canonical_uri)}">${escapeHtml(r.canonical_uri)}</a>` : "(no URI)"}</div>
    </div>

    <div class="verdict">
      <button class="vote-btn correct ${v.verdict === "correct" ? "active" : ""}" data-vote="correct">✓ Correct</button>
      <button class="vote-btn wrong ${v.verdict === "wrong" ? "active" : ""}" data-vote="wrong">✗ Wrong</button>
      <button class="vote-btn unsure ${v.verdict === "unsure" ? "active" : ""}" data-vote="unsure">? Unsure</button>
      <input class="note-input" id="note" placeholder="Optional note…" value="${escapeHtml(v.note || "")}" />
    </div>

    <div class="section"><h3>Supporting evidence</h3>${renderEvidence(r)}</div>

    <div class="section"><h3>Predictions by source</h3>${sources}</div>

    <div class="section"><h3>Other annotations for ${escapeHtml(r.gse)}</h3>${renderOtherRows(r)}</div>

    <div class="section gemma"><h3>From the Gemma record</h3>${renderGemma(r.gse)}</div>
  </div>`;
}

function showTermPop(uri, anchor) {
  const cid = canonicalize(uri);
  const t = TERMS[cid];
  const pop = document.getElementById("term-pop");
  document.getElementById("tp-name").textContent = (t && t.label) ? t.label : cid;
  document.getElementById("tp-uri").textContent  = cid || uri;
  document.getElementById("tp-def").textContent  = (t && t.definition) ? t.definition : "(no definition captured)";
  const synWrap = document.getElementById("tp-syn-wrap");
  const syns    = (t && t.synonyms) || [];
  if (syns.length) {
    document.getElementById("tp-syns").innerHTML = syns.map(s => `<span class="syn">${escapeHtml(s)}</span>`).join("");
    synWrap.style.display = "";
  } else {
    synWrap.style.display = "none";
  }
  const link = document.getElementById("tp-link");
  link.href = (uri && uri.indexOf("http") === 0) ? uri :
              (cid.startsWith("EFO:")
                ? "http://www.ebi.ac.uk/efo/" + cid.replace(":","_")
                : "http://purl.obolibrary.org/obo/" + cid.replace(":","_"));
  // Position near anchor
  const r = anchor.getBoundingClientRect();
  pop.style.left = Math.min(window.innerWidth - 480, r.left) + "px";
  pop.style.top  = (r.bottom + 8) + "px";
  pop.style.display = "block";
}

function render() {
  const rows = visibleRows();
  if (rows.length === 0) pos = 0;
  else if (pos >= rows.length) pos = rows.length - 1;
  else if (pos < 0) pos = 0;
  const r = rows[pos];
  document.getElementById("card-mount").innerHTML = renderRow(r);
  document.querySelectorAll(".vote-btn").forEach(btn => {
    btn.addEventListener("click", () => {
      if (!r) return;
      const k = rowKey(r);
      const existing = verdicts[k] || {};
      verdicts[k] = {
        verdict: btn.dataset.vote,
        note:    existing.note || "",
        timestamp: new Date().toISOString(),
      };
      saveVerdicts();
      if (btn.dataset.vote !== "unsure") setTimeout(afterVote, 80);
      else render();
    });
  });
  const note = document.getElementById("note");
  if (note) note.addEventListener("input", () => {
    if (!r) return;
    const k = rowKey(r);
    verdicts[k] = verdicts[k] || { verdict: "" };
    verdicts[k].note = note.value;
    saveVerdicts();
  });
  document.querySelectorAll(".term-link").forEach(el => {
    el.addEventListener("click", e => {
      e.preventDefault();
      showTermPop(el.dataset.uri, el);
    });
  });

  const done = reviewedCount();
  const tot  = totalCount();
  document.getElementById("progress-text").textContent =
    `${done} / ${tot} reviewed ${rows.length ? `· row ${pos + 1} / ${rows.length}` : ""}`;
  document.getElementById("progress-fill").style.width = tot ? `${100 * done / tot}%` : "0%";
}

function saveVerdicts() { localStorage.setItem(STORAGE_KEY, JSON.stringify(verdicts)); }

function advance(delta) {
  // Plain navigation: increment pos by delta with wrap-around. Always moves.
  const rows = visibleRows();
  if (rows.length === 0) { pos = 0; render(); return; }
  pos = ((pos + delta) % rows.length + rows.length) % rows.length;
  render();
}

function afterVote() {
  // Auto-advance after a Correct/Wrong vote. If filter == 'unreviewed' the
  // row just voted has dropped out of visibleRows; pos already points at
  // the next row in the new list and we just re-render. Otherwise advance.
  if (filter === "unreviewed") render();
  else                          advance(1);
}

document.getElementById("prev").addEventListener("click", () => advance(-1));
document.getElementById("next").addEventListener("click", () => advance( 1));
document.getElementById("filter").addEventListener("change", e => { filter = e.target.value; pos = 0; render(); });
document.getElementById("priority").addEventListener("change", e => { priority = e.target.value; pos = 0; render(); });

document.getElementById("export").addEventListener("click", () => {
  const blob = new Blob([JSON.stringify({ version: "1.0", saved_at: new Date().toISOString(),
      total: totalCount(), completed: reviewedCount(), verdicts },
    null, 2)], { type: "application/json" });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = `curator_verdicts_${new Date().toISOString().slice(0,10)}.json`;
  a.click();
});
document.getElementById("import").addEventListener("click", () => document.getElementById("import-file").click());
document.getElementById("import-file").addEventListener("change", e => {
  const f = e.target.files[0]; if (!f) return;
  const reader = new FileReader();
  reader.onload = () => {
    try {
      const data = JSON.parse(reader.result);
      verdicts = { ...verdicts, ...(data.verdicts || data) };
      saveVerdicts(); render();
    } catch (err) { alert("Could not parse JSON: " + err.message); }
  };
  reader.readAsText(f);
});

document.addEventListener("click", e => {
  // close term popover when clicking outside it
  if (e.target.closest("#term-pop") || e.target.closest(".term-link")) return;
  document.getElementById("term-pop").style.display = "none";
});

document.addEventListener("keydown", e => {
  if (e.target && (e.target.tagName === "INPUT" || e.target.tagName === "TEXTAREA")) return;
  if (e.key === "ArrowLeft")  advance(-1);
  if (e.key === "ArrowRight") advance( 1);
  if (e.key === "1")          document.querySelector('.vote-btn.correct')?.click();
  if (e.key === "2")          document.querySelector('.vote-btn.wrong')?.click();
  if (e.key === "3")          document.querySelector('.vote-btn.unsure')?.click();
  if (e.key === "s")          advance(1);
  if (e.key === "Escape")     document.getElementById("term-pop").style.display = "none";
});

render();
</script>
</body>
</html>
"""


def main():
    rows, seen_uris = build_rows()
    terms = build_terms_map(seen_uris)
    gemma = build_gemma_map({r["gse"] for r in rows}, rows)
    n_total = len(rows)
    n_review = sum(1 for r in rows if not r["auto_accept"])
    print(f"loaded {n_total} rows ({n_review} for review, {n_total - n_review} auto-accepted)",
          file=sys.stderr)
    print(f"  term details:  {len(terms)} URIs", file=sys.stderr)
    print(f"  gemma records: {len(gemma)} GSEs", file=sys.stderr)
    page = (HTML_TEMPLATE
            .replace("__ROWS_PLACEHOLDER__",  json.dumps(rows,  ensure_ascii=False))
            .replace("__TERMS_PLACEHOLDER__", json.dumps(terms, ensure_ascii=False))
            .replace("__GEMMA_PLACEHOLDER__", json.dumps(gemma, ensure_ascii=False)))
    OUT.write_text(page, encoding="utf-8")
    print(f"wrote {OUT}  ({OUT.stat().st_size // 1024} KB)", file=sys.stderr)


if __name__ == "__main__":
    main()
