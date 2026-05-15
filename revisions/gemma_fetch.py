"""Fetch dataset + per-sample metadata from the Gemma REST API for each GSE
in the audit set; cache the trimmed result as JSON.

Pulls from the staging endpoint by default. Cached files live under
``revisions/data/gemma_cache/<gse>.json`` and are re-fetched only when
missing or when ``--force`` is passed.

Per-GSE record schema (trimmed):

  {
    "shortName":       "GSE19466",
    "name":            "...",
    "description":     "...",
    "curationNote":    "...",
    "taxon":           "homo sapiens",
    "characteristics": [
        {"category": "cell line", "value": "...", "valueUri": "..."},
        ...
    ],
    "samples": [
        {"name": "GM18526_RNA-SEQ_Rep2",
         "description": "...",
         "characteristics": [
             {"category": "cell line", "value": "GM18526", "valueUri": null},
             ...
         ]},
        ...
    ]
  }
"""
from __future__ import annotations

import argparse
import csv
import json
import os
import sys
import time
import urllib.request
import urllib.error

DEFAULT_BASE = "https://staging-gemma.msl.ubc.ca/rest/v2"
CACHE_DIR    = "revisions/data/gemma_cache"
INHERIT_TSV  = "revisions/data/results_cl/claude-sonnet-4-6/summary_inherit.tsv"


def _fetch(url: str, timeout: float = 30.0) -> dict:
    req = urllib.request.Request(url, headers={"Accept": "application/json",
                                                "User-Agent": "revisions-audit/1.0"})
    for attempt in range(5):
        try:
            with urllib.request.urlopen(req, timeout=timeout) as resp:
                return json.loads(resp.read().decode("utf-8"))
        except urllib.error.HTTPError as e:
            if e.code in (429, 500, 502, 503, 504) and attempt < 4:
                time.sleep(2 ** attempt)
                continue
            raise
        except urllib.error.URLError:
            if attempt < 4:
                time.sleep(2 ** attempt)
                continue
            raise


def _trim_characteristic(c: dict) -> dict:
    return {
        "category":     c.get("category", ""),
        "categoryUri":  c.get("categoryUri", ""),
        "value":        c.get("value", ""),
        "valueUri":     c.get("valueUri", ""),
    }


def fetch_one(gse: str, base: str) -> dict:
    d = _fetch(f"{base}/datasets/{gse}")
    if not d.get("data"):
        return {"shortName": gse, "error": "dataset not found in Gemma"}
    ds = d["data"][0]
    out = {
        "shortName":       ds.get("shortName") or gse,
        "name":            ds.get("name", ""),
        "description":     ds.get("description", "") or "",
        "curationNote":    ds.get("curationNote") or "",
        "taxon":           (ds.get("taxon") or {}).get("commonName") or
                           (ds.get("taxon") or {}).get("scientificName") or "",
        "bioAssayCount":   ds.get("bioAssayCount") or ds.get("numberOfBioAssays") or 0,
        "characteristics": [_trim_characteristic(c) for c in (ds.get("characteristics") or [])],
        "samples":         [],
    }
    try:
        s = _fetch(f"{base}/datasets/{gse}/samples")
        for entry in (s.get("data") or []):
            sample_block = entry.get("sample") or {}
            out["samples"].append({
                "name":            entry.get("name", ""),
                "description":     (entry.get("description") or "").strip(),
                "characteristics": [_trim_characteristic(c)
                                     for c in (sample_block.get("characteristics") or [])],
            })
    except urllib.error.HTTPError as e:
        out["samples_error"] = f"HTTP {e.code}"
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--base",  default=DEFAULT_BASE)
    ap.add_argument("--force", action="store_true",
                    help="Re-fetch even if a cached file exists.")
    ap.add_argument("--gses",  nargs="*", default=None,
                    help="Limit to specific GSEs. Default: every needs_review GSE in summary_inherit.tsv.")
    args = ap.parse_args()

    os.makedirs(CACHE_DIR, exist_ok=True)

    if args.gses:
        gses = list(args.gses)
    else:
        gses = [r["gse"] for r in csv.DictReader(open(INHERIT_TSV), delimiter="\t")
                if r["verdict"] == "needs_review"]
    print(f"fetching {len(gses)} GSEs from {args.base}", file=sys.stderr)

    n_ok = n_skip = n_err = 0
    for i, gse in enumerate(gses):
        path = os.path.join(CACHE_DIR, f"{gse}.json")
        if not args.force and os.path.exists(path):
            n_skip += 1
            continue
        try:
            rec = fetch_one(gse, args.base)
            with open(path, "w") as f:
                json.dump(rec, f, ensure_ascii=False, indent=1)
            n_ok += 1
            if rec.get("error") or rec.get("samples_error"):
                print(f"  {gse}: {rec.get('error') or rec.get('samples_error')}", file=sys.stderr)
        except Exception as e:
            n_err += 1
            print(f"  {gse}: {e}", file=sys.stderr)
        if (i + 1) % 25 == 0:
            print(f"  {i+1}/{len(gses)}", file=sys.stderr)

    print(f"done. fetched={n_ok}  cached={n_skip}  errors={n_err}", file=sys.stderr)


if __name__ == "__main__":
    main()
