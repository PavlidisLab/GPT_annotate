"""Build a long-format curator sheet: one row per (GSE, cell-line equivalence class).

Each row collapses URIs from all four sources (Gemma, Sonnet, Opus, GPT-4o) into
cross-walk equivalence classes. For each class the row records:

  - the canonical id and display label,
  - actual URI used by each source (if any),
  - a check (✓) per source indicating whether that source picked any URI in the class,
  - a `picked_by` summary string.

Restricted to the 159 GSEs in Sonnet's `needs_review` bucket -- the experiments
for which the curator audit is needed.

Inputs:
  revisions/data/results_cl/claude-sonnet-4-6/summary_inherit.tsv
  revisions/data/results_cl/claude-sonnet-4-6/<gse>.json
  revisions/data/results_cl/claude-opus-4-7/<gse>.json
  revisions/data/sample_cell500.tsv
  revisions/data/cell_line_terms.json

Output:
  revisions/data/long_curation_sheet.tsv
"""
import csv
import json
import os
import sys

from cell_line_eval import (
    _canonical_id, CellLineXrefIndex,
    split_uris_to_ids, split_ids,
)

SONNET_DIR = "revisions/data/results_cl/claude-sonnet-4-6"
OPUS_DIR   = "revisions/data/results_cl/claude-opus-4-7"
INHERIT    = f"{SONNET_DIR}/summary_inherit.tsv"
SAMPLE_TSV = "revisions/data/sample_cell500.tsv"
TERMS_PATH = "revisions/data/cell_line_terms.json"
OUT_TSV    = "revisions/data/long_curation_sheet.tsv"

# Prefer CLO labels for display; fall back through EFO, BTO, CL.
PREFIX_PRIORITY = ("CLO", "EFO", "BTO", "CL")


_INPUT_CACHE: dict[str, dict] = {}


def _experiment_input(gse: str) -> dict:
    if gse in _INPUT_CACHE:
        return _INPUT_CACHE[gse]
    try:
        from geo_fetch import build_input
        _INPUT_CACHE[gse] = build_input(gse)
    except Exception:
        _INPUT_CACHE[gse] = {}
    return _INPUT_CACHE[gse]


def _flat(s) -> str:
    """Coerce a possibly-list field to a single case-folded string."""
    if s is None: return ""
    if isinstance(s, list): return " ".join(str(x) for x in s).lower()
    return str(s).lower()


def _quote_source(quote: str, exp: dict) -> str:
    """Identify which input field a verbatim quote was drawn from."""
    if not quote: return ""
    q = quote.strip().lower()
    if not q:    return ""
    for s in exp.get("samples", []):
        if q in _flat(s.get("characteristics")): return "characteristic"
        if q in _flat(s.get("protocol")):        return "protocol"
        if q in _flat(s.get("title")):           return "sample title"
    if q in _flat(exp.get("summary")):        return "summary"
    if q in _flat(exp.get("overall_design")): return "overall design"
    for p in exp.get("papers", []):
        if q in _flat(p.get("abstract")): return "paper abstract"
        if q in _flat(p.get("methods")):  return "paper methods"
        if q in _flat(p.get("title")):    return "paper title"
    return "unknown"


def _load_pred(d, gse):
    """Return (URI set, list of (canonical_id, [(source, quote_text), ...]))."""
    p = f"{d}/{gse}.json"
    if not os.path.exists(p): return set(), []
    r = json.load(open(p))
    if "error" in r: return set(), []
    fp   = r.get("first_pass", [])
    anns = r.get("annotations", [])
    by_name = {(e.get("cell_line_name") or "").strip().lower(): e.get("quote", []) for e in fp}
    exp = _experiment_input(gse)

    def tag(quotes):
        return [(_quote_source(q, exp), q) for q in (quotes or []) if q]

    pred_set = set()
    pairs: list[tuple[str, list[tuple[str, str]]]] = []
    for i, a in enumerate(anns):
        cid = _canonical_id(a.get("cell_line_ID", ""))
        if not cid:
            continue
        pred_set.add(cid)
        if i < len(fp) and len(anns) == len(fp):
            raw = fp[i].get("quote", [])
        else:
            raw = by_name.get((a.get("cell_line_name") or "").strip().lower(), [])
        pairs.append((cid, tag(raw)))
    return pred_set, pairs


def _quotes_for_class(pairs, eq_class, idx):
    """Collect tagged quotes ``(source, text)`` from `pairs` whose canonical
    id expands into eq_class. Output is a flat list of ``"[source] text"``
    strings; duplicates removed."""
    out: list[str] = []
    seen: set[str] = set()
    for cid, tagged in pairs:
        if idx.expand(cid) & eq_class:
            for src, q in tagged:
                line = f"[{src}] {q}" if src else q
                if line not in seen:
                    seen.add(line)
                    out.append(line)
    return out


def _names_for_class(eq_class, terms: dict) -> set[str]:
    """Return the case-folded union of names/synonyms/alt-labels for every
    canonical id in the equivalence class. Used to scan GEO characteristics
    for supporting evidence on rows where no LLM produced a quote."""
    out: set[str] = set()
    for cid in eq_class:
        t = terms.get(cid, {})
        for n in (t.get("names", []) + t.get("synonyms", []) +
                  t.get("alt_labels", [])):
            if n:
                out.add(n.lower().strip())
    return out


# Cache of (characteristics, sample_titles) per GSE so we walk each file once.
_GEO_FIELDS_CACHE: dict[str, dict] = {}


def _geo_fields(gse: str) -> dict:
    """Return ``{'characteristic': [...], 'sample title': [...]}`` for one GSE.

    These are the two sample-level text fields that most often carry the
    literal cell-line name; we use them to surface supporting evidence on
    rows where the LLM didn't emit a directly-matching quote."""
    if gse in _GEO_FIELDS_CACHE:
        return _GEO_FIELDS_CACHE[gse]
    chars: list[str] = []
    titles: list[str] = []
    try:
        from geo_fetch import build_input
        exp = build_input(gse)
        for s in exp.get("samples", []):
            title = (s.get("title", "") or "").strip()
            if title and title not in titles:
                titles.append(title)
            raw = s.get("characteristics", "")
            if isinstance(raw, list):
                items = raw
            else:
                raw = (raw or "").strip()
                if raw.startswith("[") and raw.endswith("]"):
                    try:
                        import ast
                        items = ast.literal_eval(raw)
                    except Exception:
                        items = [raw]
                else:
                    items = [raw]
            for item in items:
                item = (item or "").strip().strip("'").strip('"').strip()
                if item and item not in chars:
                    chars.append(item)
    except Exception:
        pass
    _GEO_FIELDS_CACHE[gse] = {"characteristic": chars, "sample title": titles}
    return _GEO_FIELDS_CACHE[gse]


def _geo_evidence_for_class(gse: str, eq_class, terms: dict,
                            max_hits: int = 6) -> list[str]:
    """Find GEO sample fields (titles + characteristics) that mention any
    name/synonym of the equivalence class. Word-boundary matching keeps
    short ontology names like ``H9`` recoverable without producing matches
    on substrings like ``ph9000``.

    Sample titles are a common landing spot for cell-line names (e.g.
    ``H9-cPC``) that the LLM sometimes uses to identify a line without
    echoing the title as a supporting quote."""
    import re
    needles = {n for n in _names_for_class(eq_class, terms) if n and len(n) >= 2}
    if not needles:
        return []
    patterns = [re.compile(rf"\b{re.escape(n)}\b") for n in needles]
    fields = _geo_fields(gse)
    seen: list[str] = []
    # Search titles first because they typically carry the line name verbatim.
    for source in ("sample title", "characteristic"):
        for line in fields.get(source, []):
            low = line.lower()
            if any(p.search(low) for p in patterns):
                tagged = f"[{source}] {line}"
                if tagged not in seen:
                    seen.append(tagged)
                    if len(seen) >= max_hits:
                        return seen
    return seen


def _pick_display(eq_class: frozenset[str], terms: dict, members_by_source: dict[str, set[str]]) -> tuple[str, str]:
    """Return (canonical_id, display_name) for an equivalence class."""
    members = sorted(eq_class)
    by_prefix: dict[str, list[str]] = {}
    for cid in members:
        prefix = cid.split(":",1)[0]
        by_prefix.setdefault(prefix, []).append(cid)
    for p in PREFIX_PRIORITY:
        if p in by_prefix:
            chosen = sorted(by_prefix[p])[0]
            t = terms.get(chosen, {})
            names = t.get("names", [])
            label = next((n for n in names if n), "") or t.get("primary_label","") or chosen
            return chosen, label
    chosen = members[0]
    t = terms.get(chosen, {})
    names = t.get("names", [])
    return chosen, (next((n for n in names if n), "") or chosen)


def main():
    idx = CellLineXrefIndex()
    terms = json.load(open(TERMS_PATH))
    needs_review = [r["gse"] for r in csv.DictReader(open(INHERIT), delimiter="\t")
                    if r["verdict"] == "needs_review"]
    print(f"GSEs in needs_review: {len(needs_review)}", file=sys.stderr)

    sample = {r["shortName"]: r for r in csv.DictReader(open(SAMPLE_TSV), delimiter="\t")}

    rows_out = []
    for gse in needs_review:
        truth  = split_uris_to_ids(sample.get(gse, {}).get("gemma_uri", ""))
        gpt    = split_ids(sample.get(gse, {}).get("gpt_term_id", ""))
        sonnet, sonnet_pairs = _load_pred(SONNET_DIR, gse)
        opus,   opus_pairs   = _load_pred(OPUS_DIR, gse)

        # Build equivalence classes over the union of all picks.
        all_ids = truth | sonnet | opus | gpt
        if not all_ids:
            rows_out.append({
                "shortName": gse, "canonical_label": "", "canonical_uri": "",
                "in_gemma":"", "in_claude":"", "in_opus":"", "in_gpt4o":"",
                "gemma_uri":"", "claude_uri":"", "opus_uri":"", "gpt4o_uri":"",
                "claude_quotes":"", "opus_quotes":"", "geo_evidence":"",
                "picked_by":"(no annotations)",
                "auto_accept":"",
                "curator_verdict":"", "notes":"",
            })
            continue

        classes: list[frozenset[str]] = []
        used = set()
        for cid in sorted(all_ids):
            if cid in used: continue
            ec = idx.expand(cid)
            classes.append(ec)
            used |= ec
        # Within a class, restrict to ids that are actually picked by some source
        # (so the display URI/name reflects a real pick, not a transitive expansion).
        for ec in classes:
            in_picks = ec & all_ids
            cid, lbl = _pick_display(in_picks, terms, {})

            def uri_of(s):
                hit = sorted(s & ec)
                return hit[0] if hit else ""

            in_g, in_c, in_o, in_p = bool(truth & ec), bool(sonnet & ec), bool(opus & ec), bool(gpt & ec)
            picked = []
            if in_g: picked.append("Gemma")
            if in_c: picked.append("Claude")
            if in_o: picked.append("Opus")
            if in_p: picked.append("GPT-4o")

            claude_q = _quotes_for_class(sonnet_pairs, ec, idx)
            opus_q   = _quotes_for_class(opus_pairs,   ec, idx)
            geo_q    = _geo_evidence_for_class(gse, ec, terms)

            # auto_accept: the row needs no fresh curator review. Three rules:
            #   A) Gemma + ≥1 frontier model agree on this row
            #      (Gemma is curator-validated, and an independent model
            #      agreeing further confirms; silence from other models is
            #      not used as negative evidence).
            #   B) No frontier model picked this row (Gemma-only or
            #      "no annotations"): we're not evaluating Gemma sensitivity
            #      in this pass, and there is nothing model-side to verify.
            #   C) Only GPT-4o picked this row: GPT-4o was already curated in
            #      the original study, so re-reviewing solo GPT-4o predictions
            #      here adds no information about Sonnet or Opus.
            ai_models = (in_c, in_o, in_p)
            agreeing_models = sum(ai_models)
            auto_accept = (
                (in_g and agreeing_models >= 1) or              # rule A
                (not in_c and not in_o and not in_p) or         # rule B
                (in_p and not in_c and not in_o and not in_g)   # rule C
            )

            rows_out.append({
                "shortName":      gse,
                "canonical_label":lbl,
                "canonical_uri":  cid,
                "in_gemma":       "✓" if in_g else "",
                "in_claude":      "✓" if in_c else "",
                "in_opus":        "✓" if in_o else "",
                "in_gpt4o":       "✓" if in_p else "",
                "gemma_uri":      uri_of(truth),
                "claude_uri":     uri_of(sonnet),
                "opus_uri":       uri_of(opus),
                "gpt4o_uri":      uri_of(gpt),
                "claude_quotes":  "❗".join(claude_q),
                "opus_quotes":    "❗".join(opus_q),
                "geo_evidence":   "❗".join(geo_q),
                "picked_by":      " + ".join(picked) if picked else "",
                "auto_accept":    "TRUE" if auto_accept else "",
                "curator_verdict":"",
                "notes":          "",
            })

    fields = list(rows_out[0].keys())
    with open(OUT_TSV, "w") as f:
        w = csv.DictWriter(f, fieldnames=fields, delimiter="\t")
        w.writeheader()
        for r in rows_out: w.writerow(r)
    print(f"Wrote {OUT_TSV}: {len(rows_out)} rows across {len(set(r['shortName'] for r in rows_out))} GSEs",
          file=sys.stderr)

    # Quick distribution print
    from collections import Counter
    dist = Counter(r["picked_by"] for r in rows_out)
    print("picked_by distribution:", file=sys.stderr)
    for k, v in dist.most_common(15):
        print(f"  {k or '(none)':<40} {v}", file=sys.stderr)


if __name__ == "__main__":
    main()
