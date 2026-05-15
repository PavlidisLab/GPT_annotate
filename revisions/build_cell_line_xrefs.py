"""Extract per-term cross-reference metadata from CLO.obo and EFO.obo.

Output: revisions/data/cell_line_terms.json
  { canonical_id : {
      "names":     [str, ...],     # lower-cased
      "synonyms":  [str, ...],     # lower-cased
      "alt_labels":[str, ...],     # IAO:0000118 alternative labels, lower-cased
      "xrefs":     [canonical_id, ...],   # OBO ``xref:`` entries
      "see_also":  [canonical_id, ...],   # ``property_value: seeAlso ...`` entries
      "alt_ids":   [canonical_id, ...],
   } }

This is a faithful machine-readable version of the metadata that
``analysis/cell_lines/07.post_curator_evaluation.R::get_related_terms`` reads
out of the same OBO files. Equivalence-class expansion happens at evaluation
time in ``cell_line_eval.py``, lazily, only for the IDs that appear in a
prediction or ground-truth set.
"""
from __future__ import annotations

import json
import os
import re
import sys
import time

CLO_OBO = "revisions/data/CLO.obo"
EFO_OBO = "revisions/data/efo_v3.79.0.obo"
OUT_PATH = "revisions/data/cell_line_terms.json"

ID_PREFIXES = {"CLO","EFO","CL","BTO","NCBITaxon","BFO","OBI","PR"}


def canonical(s):
    if not s: return ""
    s = s.strip()
    if s.startswith("http"):
        s = s.rsplit("/", 1)[-1]
    local = s.split(":", 1)[1] if ":" in s else s
    if "_" in local:
        prefix, _, rest = local.partition("_")
        return f"{prefix.upper()}:{rest}"
    if ":" in s:
        prefix, _, rest = s.partition(":")
        return f"{prefix.upper()}:{rest}"
    return s


def parse_obo(path):
    if not os.path.exists(path):
        return
    SYN_RE  = re.compile(r'"([^"]+)"')
    PROP_RE = re.compile(r'(\S+)\s+"([^"]+)"')
    DEF_RE  = re.compile(r'"([^"]+)"')
    cur = None
    skip_obsolete = False
    with open(path) as f:
        for line in f:
            line = line.rstrip("\n")
            if line.startswith("[Term]"):
                if cur is not None and cur["id"] and not skip_obsolete:
                    yield cur
                cur = {"id":"","names":[],"synonyms":[],"alt_labels":[],
                       "xrefs":[],"see_also":[],"alt_ids":[],"definition":""}
                skip_obsolete = False
                continue
            if cur is None: continue
            colon = line.find(":")
            if colon < 0: continue
            key = line[:colon]; val = line[colon+1:].strip()
            if key == "id":
                cur["id"] = canonical(val)
            elif key == "name":
                cur["names"].append(val.lower())
            elif key == "def":
                m = DEF_RE.match(val)
                cur["definition"] = (m.group(1) if m else val).strip()
            elif key == "is_obsolete" and val == "true":
                skip_obsolete = True
            elif key == "synonym":
                m = SYN_RE.search(val)
                if m: cur["synonyms"].append(m.group(1).lower())
            elif key == "alt_id":
                cid = canonical(val)
                if cid: cur["alt_ids"].append(cid)
            elif key == "xref":
                tok = val.split()[0].rstrip(",")
                cid = canonical(tok)
                if cid and cid.split(":",1)[0] in ID_PREFIXES:
                    cur["xrefs"].append(cid)
            elif key == "property_value":
                m = PROP_RE.match(val)
                if not m: continue
                pred, payload = m.group(1), m.group(2)
                if pred.endswith("IAO_0000118"):
                    cur["alt_labels"].append(payload.lower())
                elif pred.endswith("seeAlso"):
                    # payload e.g. "EFO: EFO_0001185" or "ATCC: CCL-2"
                    for tok in re.split(r"[,\s]+", payload):
                        tok = tok.strip().strip("()[];").rstrip(":,")
                        if ":" not in tok: continue
                        cid = canonical(tok)
                        if cid and cid.split(":",1)[0] in ID_PREFIXES:
                            cur["see_also"].append(cid)
        if cur is not None and cur["id"] and not skip_obsolete:
            yield cur


def main():
    out: dict[str, dict] = {}
    for path in (CLO_OBO, EFO_OBO):
        if not os.path.exists(path):
            print(f"WARN missing {path}", file=sys.stderr); continue
        t0 = time.time()
        n = 0
        for term in parse_obo(path):
            cid = term.pop("id")
            if not cid: continue
            # dedupe lists
            for k in ("names","synonyms","alt_labels"):
                term[k] = sorted(set(term[k]))
            for k in ("xrefs","see_also","alt_ids"):
                term[k] = sorted(set(term[k]))
            # merge if a term appears in both ontologies (rare but possible for CL:xxx)
            if cid in out:
                prev = out[cid]
                for k in term:
                    prev[k] = sorted(set(prev[k]) | set(term[k]))
            else:
                out[cid] = term
            n += 1
        print(f"  {path}: parsed {n} terms in {time.time()-t0:.1f}s", file=sys.stderr)

    # --- name-based EFO↔CLO mirror bridge ------------------------------------
    # The OBO files contain explicit xrefs between many EFO and CLO cell-line
    # entries, but a large fraction of the obvious 1:1 mirrors are not encoded
    # (e.g. EFO:0004905 "induced pluripotent stem cell" ≡ CLO:0037307
    # "induced pluripotent stem cell line cell"). We bridge these by string
    # matching: if an EFO term's name (or synonym/alt-label) matches a CLO
    # term's name (or synonym/alt-label) after stripping the trailing tokens
    # "cell", "cell line", "cell line cell" (case-folded), the two ids are
    # mutually xref-ed. The bridge is symmetric (added in both directions).
    SUFFIXES = (" cell line cell", " cell line", " cell")
    def normalize(name: str) -> str:
        n = name.lower().strip()
        for suf in SUFFIXES:
            if n.endswith(suf):
                return n[: -len(suf)]
        return n

    bridge_pairs = 0
    # Build per-namespace normalised-name → ids index.
    efo_name_idx: dict[str, set[str]] = {}
    clo_name_idx: dict[str, set[str]] = {}
    for cid, term in out.items():
        if cid.startswith("EFO:") or cid.startswith("efo:"):
            tgt = efo_name_idx
        elif cid.startswith("CLO:") or cid.startswith("clo:"):
            tgt = clo_name_idx
        else:
            continue
        for nm in term.get("names", []) + term.get("synonyms", []) + term.get("alt_labels", []):
            key = normalize(nm)
            if not key:
                continue
            tgt.setdefault(key, set()).add(cid)
    # Pair anything that normalises to the same root across namespaces.
    for key, efo_ids in efo_name_idx.items():
        clo_ids = clo_name_idx.get(key)
        if not clo_ids:
            continue
        for e in efo_ids:
            for c in clo_ids:
                ex = set(out[e]["xrefs"])
                cx = set(out[c]["xrefs"])
                if c not in ex:
                    out[e]["xrefs"] = sorted(ex | {c})
                    bridge_pairs += 1
                if e not in cx:
                    out[c]["xrefs"] = sorted(cx | {e})
    print(f"  EFO↔CLO name bridge: added {bridge_pairs} xref edges", file=sys.stderr)

    with open(OUT_PATH, "w") as f:
        json.dump(out, f)
    print(f"Wrote {OUT_PATH}: {len(out)} terms", file=sys.stderr)


if __name__ == "__main__":
    main()
