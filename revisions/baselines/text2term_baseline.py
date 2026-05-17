"""text2term baseline for the strain annotation task.

text2term (Wachtler et al., https://github.com/ccb-hms/ontology-mapper) maps
free-text strings to ontology terms via TFIDF, fuzzy, or BioPortal-style
matchers. We treat it as the "next step up" from the regular-expression
baseline reported in the original paper: it still operates purely on string
similarity (no LLM, no semantic embedding), but uses term name + synonyms +
TFIDF rather than literal substring containment.

Pipeline for the strain task:
  1. For each GSE, build a list of "candidate strings" from the same fields
     the paper's regex baseline searches: sample-level characteristics, the
     experiment summary, the paper abstract & methods.
  2. Run `text2term.map_terms(source_terms, target_ontology, mapper=TFIDF)`
     against the same 156-term EFO + TGEMO list we used for the LLM run.
  3. Keep candidate-term mappings with similarity >= threshold (default 0.7).
  4. Per-experiment prediction = union of mapped URIs.

Output: revisions/data/results/text2term_strain/{summary.tsv,per_gse/*.json}
"""
from __future__ import annotations

# Path bootstrap so flat `from strain_annotate import X` and other
# revisions/-local imports keep working after subdir reorganisation.
import sys as _sys
from pathlib import Path as _Path
_REV = _Path(__file__).resolve().parents[1]
for _sub in ("", "annotators", "runners", "baselines", "build", "analysis", "metrics"):
    _p = str(_REV / _sub) if _sub else str(_REV)
    if _p not in _sys.path:
        _sys.path.insert(0, _p)
del _sys, _Path, _REV, _sub, _p

import argparse
import csv
import json
import os
import re
import sys
import time

import pandas as pd
import text2term
from text2term import Mapper

from geo_fetch import build_input

OUT_DIR        = "revisions/data/results/text2term_strain"
STRAIN_LIST    = "revisions/data/strain_list.json"


def build_dictionary_owl(path: str) -> str:
    """Build a minimal OWL ontology containing 156 named classes (one per strain),
    each carrying rdfs:label and oboInOwl:hasExactSynonym annotations. text2term
    parses this via owlready2/rdflib in seconds. Output: /tmp/strain_dict.owl"""
    from rdflib import Graph, Namespace, RDF, RDFS, OWL, URIRef, Literal
    out_path = "/tmp/strain_dict.owl"
    with open(path) as f:
        sl = json.load(f)
    g = Graph()
    OBOINOWL = Namespace("http://www.geneontology.org/formats/oboInOwl#")
    ONT = URIRef("http://example.org/strain_dict")
    g.add((ONT, RDF.type, OWL.Ontology))
    g.bind("oboInOwl", OBOINOWL)
    g.bind("owl", OWL)
    n = 0
    for s in sl:
        uri = s.get("URI")
        label = (s.get("value") or "").strip()
        if not (uri and label): continue
        cls = URIRef(uri)
        g.add((cls, RDF.type, OWL.Class))
        g.add((cls, RDFS.label, Literal(label)))
        for syn in s.get("synonyms", []) or []:
            syn = (syn or "").strip()
            if syn:
                g.add((cls, OBOINOWL.hasExactSynonym, Literal(syn)))
        n += 1
    g.serialize(destination=out_path, format="xml")
    print(f"wrote {out_path}: {n} OWL classes", file=sys.stderr)
    return out_path


def build_dictionary_obo(path: str) -> str:
    """Convert strain_list.json -> a small OBO file that text2term can ingest as
    target_ontology. We synthesize one [Term] per strain with id, name, and
    synonyms; this is the minimum text2term needs to TFIDF-match."""
    out_path = "/tmp/strain_dict.obo"
    with open(path) as f:
        sl = json.load(f)
    # Map our http URIs back to obo-style ids
    def to_obo_id(uri: str) -> str:
        if uri.startswith("http://www.ebi.ac.uk/efo/"):
            return uri.rsplit("/", 1)[-1].replace("EFO_", "EFO:")
        if uri.startswith("http://gemma.msl.ubc.ca/ont/"):
            return uri.rsplit("/", 1)[-1].replace("TGEMO_", "TGEMO:")
        return uri.rsplit("/", 1)[-1]
    with open(out_path, "w") as f:
        f.write("format-version: 1.2\n\n")
        for s in sl:
            label = (s.get("value") or "").replace("\n", " ").strip()
            uri = s.get("URI", "")
            if not (label and uri): continue
            oid = to_obo_id(uri)
            f.write("[Term]\n")
            f.write(f"id: {oid}\n")
            f.write(f"name: {label}\n")
            for syn in s.get("synonyms", []) or []:
                syn = (syn or "").replace('"', "'").strip()
                if syn:
                    f.write(f'synonym: "{syn}" EXACT []\n')
            f.write("\n")
    print(f"wrote {out_path}: {len(sl)} [Term] stanzas", file=sys.stderr)
    return out_path


def extract_candidates(exp: dict) -> list[str]:
    """Pull a short list of candidate strings from the experiment data — the
    natural input to a string-matching baseline. Mirrors the regex-baseline's
    search corpus."""
    out: list[str] = []
    if exp.get("summary"):       out.append(exp["summary"])
    if exp.get("overall_design"):out.append(exp["overall_design"])
    for s in exp.get("samples", []):
        if s.get("characteristics"):
            cs = s["characteristics"]
            if isinstance(cs, list): out.extend(cs)
            else: out.append(cs)
        if s.get("title"): out.append(s["title"])
    for p in exp.get("papers", []) or []:
        if p.get("abstract"): out.append(p["abstract"])
        if p.get("methods"):  out.append(p["methods"])
    # dedupe and trim
    out = [c.strip() for c in out if isinstance(c, str) and c.strip()]
    out = list(dict.fromkeys(out))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sample", default="revisions/data/sample500.tsv")
    ap.add_argument("--n", type=int, default=0, help="0 = all rows in sample")
    ap.add_argument("--threshold", type=float, default=0.4,
                    help="TFIDF similarity threshold for retained mappings. "
                         "0.4 is the per-mention sweet spot: 0.65 only catches "
                         "literal substring hits and drops 70%% of correct picks; "
                         "below 0.4 the false-positive rate explodes.")
    ap.add_argument("--max-mappings", type=int, default=1,
                    help="top-K mappings to keep per candidate string. =1 mimics "
                         "regex-style 'pick whichever name matches best'.")
    args = ap.parse_args()

    os.makedirs(OUT_DIR, exist_ok=True)
    os.makedirs(f"{OUT_DIR}/per_gse", exist_ok=True)
    dict_obo = build_dictionary_owl(STRAIN_LIST)

    rows = list(csv.DictReader(open(args.sample), delimiter="\t"))
    if args.n: rows = rows[: args.n]
    print(f"Sample size: {len(rows)} GSEs", file=sys.stderr)

    summary = []
    n_ok = 0
    n_exact = 0
    n_any_overlap = 0
    t0 = time.time()

    for i, row in enumerate(rows):
        gse = row["shortName"]
        truth = {u for u in row["gemma_uri"].split(",") if u}
        try:
            exp = build_input(gse)
        except Exception as e:
            summary.append({"gse": gse, "error": f"build_input: {e!r}"})
            continue
        cands = extract_candidates(exp)
        if not cands:
            summary.append({"gse": gse, "pred": "", "truth": ",".join(sorted(truth)),
                            "exact": False, "any_overlap": False})
            continue
        try:
            df = text2term.map_terms(
                source_terms=cands,
                target_ontology=dict_obo,
                mapper=Mapper.TFIDF,
                min_score=args.threshold,
                max_mappings=args.max_mappings,
                save_mappings=False,
            )
        except Exception as e:
            summary.append({"gse": gse, "error": f"text2term: {e!r}"})
            continue
        pred = set()
        if df is not None and len(df) > 0:
            # df columns include "Mapped Term IRI" (the IRI we put in the dict)
            iri_col = next((c for c in df.columns if "iri" in c.lower()), None)
            if iri_col is not None:
                pred = set(df[iri_col].dropna().astype(str).tolist())
        exact = pred == truth
        any_overlap = bool(pred & truth)
        n_ok += 1
        n_exact += int(exact)
        n_any_overlap += int(any_overlap)
        summary.append({"gse": gse, "pred": ",".join(sorted(pred)),
                        "truth": ",".join(sorted(truth)),
                        "exact": exact, "any_overlap": any_overlap})
        with open(f"{OUT_DIR}/per_gse/{gse}.json", "w") as f:
            json.dump({"gse": gse, "pred": list(pred), "truth": list(truth),
                       "candidate_count": len(cands)}, f, indent=2)
        if (i + 1) % 25 == 0:
            elapsed = time.time() - t0
            rate = (i + 1) / elapsed
            eta = (len(rows) - i - 1) / rate
            print(f"  {i+1}/{len(rows)}  exact={n_exact}/{n_ok} ({n_exact/max(n_ok,1):.1%})  "
                  f"any_overlap={n_any_overlap}/{n_ok} ({n_any_overlap/max(n_ok,1):.1%})  "
                  f"ETA {eta/60:.1f} min", file=sys.stderr)

    out_path = f"{OUT_DIR}/summary.tsv"
    with open(out_path, "w") as f:
        fields = ["gse", "pred", "truth", "exact", "any_overlap", "error"]
        w = csv.DictWriter(f, fieldnames=fields, delimiter="\t", extrasaction="ignore")
        w.writeheader()
        for r in summary: w.writerow(r)

    print()
    print(f"Total GSEs:         {len(rows)}")
    print(f"Successful runs:    {n_ok}")
    print(f"Exact match:        {n_exact}/{n_ok} = {n_exact/max(n_ok,1):.1%}")
    print(f"Any overlap:        {n_any_overlap}/{n_ok} = {n_any_overlap/max(n_ok,1):.1%}")
    print(f"Wrote {out_path}")


if __name__ == "__main__":
    main()
