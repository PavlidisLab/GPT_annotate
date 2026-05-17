"""Verifier pass for the strain task.

For each per-GSE prediction JSON written by ``run_sample.py``, check that
every verbatim ``quote`` Claude emitted actually appears as a case-folded
substring of the input the model was given. The result is two
complementary rates per model:

  * per-quote verification rate -- how often a quote is grounded.
  * per-prediction "fully grounded" rate -- the fraction of strain
    predictions whose entire quote list is grounded.

A quote that is *not* present in the input is treated as a hallucinated
attribution; the URI itself may still be correct, but the supporting
evidence the model claimed to be quoting is not in the input.

Output:
  revisions/data/analysis/09_verifier_strain.tsv   (per-model summary)
  revisions/data/analysis/09_verifier_strain_per_gse.tsv (per-GSE)
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

import csv
import glob
import json
import os
import sys
from typing import Iterable

from geo_fetch import build_input

MODELS = [
    "claude-sonnet-4-6",
    "claude-opus-4-7",
    "claude-haiku-4-5-20251001",
]

RESULTS_BASE = "revisions/data/results"
GPT4O_TSV    = "/tmp/gpt4o_strain_preds.tsv"  # exported from main_frame.rds
OUT_SUMMARY  = "revisions/data/analysis/09_verifier_strain.tsv"
OUT_PER_GSE  = "revisions/data/analysis/09_verifier_strain_per_gse.tsv"


def verify_gpt4o(tsv_path: str):
    """Mirror of verify_model() for the published GPT-4o predictions which
    live in a different on-disk format (TSV exported from main_frame.rds)."""
    if not os.path.exists(tsv_path):
        return None, []
    n_gse = 0
    n_preds_total = n_preds_with_quote = 0
    n_preds_fully_strict = n_preds_fully_norm = 0
    n_quotes_total = n_quotes_strict = n_quotes_norm = 0
    per_gse_rows = []
    with open(tsv_path) as f:
        rdr = csv.DictReader(f, delimiter="\t")
        for row in rdr:
            gse = row["shortName"]
            try:
                quotes_for_each_pred = json.loads(row.get("gpt_quote", "") or "[]")
            except json.JSONDecodeError:
                quotes_for_each_pred = []
            if not isinstance(quotes_for_each_pred, list):
                continue
            n_gse += 1
            hay, norm_hay = _haystack(gse)
            gse_preds = gse_preds_strict = gse_preds_norm = 0
            gse_qt = gse_q_strict = gse_q_norm = 0
            # main_frame.rds stores gpt_quote either as a flat list of quotes
            # (one prediction's worth) or as nested per-prediction lists; we
            # treat each top-level entry as one prediction.
            for entry in quotes_for_each_pred:
                if isinstance(entry, str):
                    # main_frame.rds joins per-prediction quote lists with
                    # ❗ (within a strain) and ❕ (between strains); split on
                    # both so each piece is one verbatim quote to verify.
                    pieces = []
                    for part in entry.split("❕"):
                        pieces.extend(part.split("❗"))
                    quote_list = [p.strip().strip('"').strip("'") for p in pieces if p.strip()]
                elif isinstance(entry, list):
                    quote_list = [q for q in entry if isinstance(q, str)]
                else:
                    continue
                n_preds_total += 1; gse_preds += 1
                quote_list = [q for q in quote_list if q]
                if not quote_list:
                    continue
                n_preds_with_quote += 1
                ok_strict = ok_norm = 0
                for q in quote_list:
                    n_quotes_total += 1; gse_qt += 1
                    if _verified_strict(q, hay):
                        n_quotes_strict += 1; gse_q_strict += 1; ok_strict += 1
                        n_quotes_norm   += 1; gse_q_norm   += 1; ok_norm   += 1
                    elif _verified_normalised(q, norm_hay):
                        n_quotes_norm += 1; gse_q_norm += 1; ok_norm += 1
                if ok_strict == len(quote_list): n_preds_fully_strict += 1; gse_preds_strict += 1
                if ok_norm   == len(quote_list): n_preds_fully_norm   += 1; gse_preds_norm   += 1
            per_gse_rows.append({
                "model": "gpt-4o (published)", "gse": gse,
                "preds": gse_preds,
                "preds_fully_strict": gse_preds_strict,
                "preds_fully_norm":   gse_preds_norm,
                "quotes": gse_qt,
                "quotes_strict": gse_q_strict,
                "quotes_norm":   gse_q_norm,
            })
    return ({
        "model": "gpt-4o (published)",
        "n_gse": n_gse,
        "preds_total": n_preds_total,
        "preds_with_quote": n_preds_with_quote,
        "preds_fully_strict": n_preds_fully_strict,
        "preds_fully_strict_rate": (n_preds_fully_strict / n_preds_with_quote) if n_preds_with_quote else 0.0,
        "preds_fully_norm":   n_preds_fully_norm,
        "preds_fully_norm_rate":   (n_preds_fully_norm / n_preds_with_quote) if n_preds_with_quote else 0.0,
        "quotes_total": n_quotes_total,
        "quotes_strict": n_quotes_strict,
        "quotes_strict_rate": (n_quotes_strict / n_quotes_total) if n_quotes_total else 0.0,
        "quotes_norm":   n_quotes_norm,
        "quotes_norm_rate":   (n_quotes_norm / n_quotes_total) if n_quotes_total else 0.0,
    }, per_gse_rows)


# Pre-compute one searchable haystack per GSE so we don't rebuild it
# for every model. Two flavours per GSE: the verbatim string and a
# normalised string (alphanumerics + lowercase, all other characters
# collapsed) used for the lenient verification path.
_HAYSTACK_CACHE: dict[str, str] = {}
_NORM_HAYSTACK_CACHE: dict[str, str] = {}

import re as _re
_NON_ALNUM = _re.compile(r"[^a-z0-9]+")


def _normalise(s: str) -> str:
    """Aggressive normalisation: lower-case, drop every non-alphanumeric
    character, collapse runs of whitespace into single spaces. This kills
    the en-dash / hyphen / added-period / multi-line-join failure modes
    documented for GPT-4o."""
    return _NON_ALNUM.sub(" ", s.lower()).strip()


def _flatten_text(blob) -> str:
    """Recursively unroll a possibly nested str/list/dict into a flat
    case-folded string used for substring lookup."""
    if blob is None: return ""
    if isinstance(blob, str): return blob.lower()
    if isinstance(blob, list):
        return " ".join(_flatten_text(x) for x in blob)
    if isinstance(blob, dict):
        return " ".join(_flatten_text(v) for v in blob.values())
    return str(blob).lower()


def _haystack(gse: str) -> tuple[str, str]:
    """Return (verbatim_haystack, normalised_haystack) for one GSE.
    Verbatim is whitespace-collapsed case-folded text; normalised drops
    every non-alphanumeric character so en-dash / hyphen / period
    differences don't matter."""
    if gse in _HAYSTACK_CACHE:
        return _HAYSTACK_CACHE[gse], _NORM_HAYSTACK_CACHE[gse]
    try:
        exp = build_input(gse)
    except Exception:
        _HAYSTACK_CACHE[gse] = ""
        _NORM_HAYSTACK_CACHE[gse] = ""
        return "", ""
    parts: list[str] = []
    for f in ("summary", "overall_design"):
        parts.append(_flatten_text(exp.get(f, "")))
    for s in exp.get("samples", []) or []:
        for f in ("title", "characteristics", "protocol"):
            parts.append(_flatten_text(s.get(f, "")))
    for p in exp.get("papers", []) or []:
        for f in ("title", "abstract", "methods"):
            parts.append(_flatten_text(p.get(f, "")))
    blob = " ".join(" ".join(p.split()) for p in parts)
    _HAYSTACK_CACHE[gse] = blob
    _NORM_HAYSTACK_CACHE[gse] = " ".join(_normalise(blob).split())
    return _HAYSTACK_CACHE[gse], _NORM_HAYSTACK_CACHE[gse]


def _verified_strict(quote: str, hay: str) -> bool:
    if not quote or not hay:
        return False
    q = " ".join(quote.lower().split())
    return q in hay


def _verified_normalised(quote: str, norm_hay: str) -> bool:
    if not quote or not norm_hay:
        return False
    q = " ".join(_normalise(quote).split())
    return bool(q) and q in norm_hay


def verify_model(model: str):
    res_dir = f"{RESULTS_BASE}/{model}"
    files = sorted(glob.glob(f"{res_dir}/*.json"))
    n_gse = 0
    n_preds_total = n_preds_with_quote = 0
    n_preds_fully_strict = n_preds_fully_norm = 0
    n_quotes_total = n_quotes_strict = n_quotes_norm = 0
    per_gse_rows = []
    for path in files:
        gse = os.path.basename(path)[:-5]
        try:
            r = json.load(open(path))
        except Exception:
            continue
        if "strains" not in r: continue
        n_gse += 1
        hay, norm_hay = _haystack(gse)
        # Robust to the same tool-input stringification quirk seen with Opus:
        # occasionally `strains` arrives as a JSON-encoded string instead of
        # a structured list.
        strains = r.get("strains", [])
        if isinstance(strains, str):
            try:
                strains = json.loads(strains)
                if isinstance(strains, dict) and "strains" in strains:
                    strains = strains["strains"]
            except json.JSONDecodeError:
                strains = []
        if not isinstance(strains, list):
            strains = []
        gse_preds = gse_preds_strict = gse_preds_norm = 0
        gse_qt = gse_q_strict = gse_q_norm = 0
        for s in strains:
            if not isinstance(s, dict):
                continue
            n_preds_total += 1; gse_preds += 1
            quotes = [q for q in (s.get("quote", []) or []) if q]
            if not quotes:
                continue
            n_preds_with_quote += 1
            ok_strict = ok_norm = 0
            for q in quotes:
                n_quotes_total += 1; gse_qt += 1
                if _verified_strict(q, hay):
                    n_quotes_strict += 1; gse_q_strict += 1; ok_strict += 1
                    n_quotes_norm   += 1; gse_q_norm   += 1; ok_norm   += 1
                elif _verified_normalised(q, norm_hay):
                    n_quotes_norm += 1; gse_q_norm += 1; ok_norm += 1
            if ok_strict == len(quotes): n_preds_fully_strict += 1; gse_preds_strict += 1
            if ok_norm   == len(quotes): n_preds_fully_norm   += 1; gse_preds_norm   += 1
        per_gse_rows.append({
            "model": model, "gse": gse,
            "preds": gse_preds,
            "preds_fully_strict": gse_preds_strict,
            "preds_fully_norm":   gse_preds_norm,
            "quotes": gse_qt,
            "quotes_strict": gse_q_strict,
            "quotes_norm":   gse_q_norm,
        })
    return {
        "model": model,
        "n_gse": n_gse,
        "preds_total": n_preds_total,
        "preds_with_quote": n_preds_with_quote,
        "preds_fully_strict": n_preds_fully_strict,
        "preds_fully_strict_rate": (n_preds_fully_strict / n_preds_with_quote) if n_preds_with_quote else 0.0,
        "preds_fully_norm":   n_preds_fully_norm,
        "preds_fully_norm_rate":   (n_preds_fully_norm / n_preds_with_quote) if n_preds_with_quote else 0.0,
        "quotes_total": n_quotes_total,
        "quotes_strict": n_quotes_strict,
        "quotes_strict_rate": (n_quotes_strict / n_quotes_total) if n_quotes_total else 0.0,
        "quotes_norm":   n_quotes_norm,
        "quotes_norm_rate":   (n_quotes_norm / n_quotes_total) if n_quotes_total else 0.0,
    }, per_gse_rows


def main():
    os.makedirs("revisions/data/analysis", exist_ok=True)

    summaries = []
    all_per_gse = []
    # Always include GPT-4o as the within-study baseline by reading the
    # published predictions from main_frame.rds (already on disk as TSV).
    # The summary fields are the same shape so they slot into the same table.

    for m in MODELS:
        if not os.path.isdir(f"{RESULTS_BASE}/{m}"):
            print(f"  skip: {m} (no results dir)", file=sys.stderr); continue
        print(f"verifying {m} ...", file=sys.stderr)
        s, per_gse = verify_model(m)
        summaries.append(s)
        all_per_gse.extend(per_gse)
        print(f"  {m}: strict {s['quotes_strict']}/{s['quotes_total']}"
              f" ({s['quotes_strict_rate']:.1%}); "
              f"normalised {s['quotes_norm']}/{s['quotes_total']}"
              f" ({s['quotes_norm_rate']:.1%}); "
              f"preds fully grounded {s['preds_fully_strict']}/{s['preds_with_quote']}"
              f" → {s['preds_fully_norm']}/{s['preds_with_quote']}"
              f" ({s['preds_fully_strict_rate']:.1%} → {s['preds_fully_norm_rate']:.1%})",
              file=sys.stderr)

    # Also verify the published GPT-4o predictions exported from main_frame.rds.
    if os.path.exists(GPT4O_TSV):
        print(f"verifying gpt-4o (published) ...", file=sys.stderr)
        s, per_gse = verify_gpt4o(GPT4O_TSV)
        if s:
            summaries.append(s); all_per_gse.extend(per_gse)
            print(f"  gpt-4o (published): strict {s['quotes_strict']}/{s['quotes_total']}"
                  f" ({s['quotes_strict_rate']:.1%}); "
                  f"normalised {s['quotes_norm']}/{s['quotes_total']}"
                  f" ({s['quotes_norm_rate']:.1%}); "
                  f"preds fully grounded {s['preds_fully_strict']}/{s['preds_with_quote']}"
                  f" → {s['preds_fully_norm']}/{s['preds_with_quote']}"
                  f" ({s['preds_fully_strict_rate']:.1%} → {s['preds_fully_norm_rate']:.1%})",
                  file=sys.stderr)

    fields = ["model", "n_gse", "preds_total", "preds_with_quote",
              "preds_fully_strict", "preds_fully_strict_rate",
              "preds_fully_norm",   "preds_fully_norm_rate",
              "quotes_total",
              "quotes_strict", "quotes_strict_rate",
              "quotes_norm",   "quotes_norm_rate"]
    with open(OUT_SUMMARY, "w") as f:
        w = csv.DictWriter(f, fieldnames=fields, delimiter="\t")
        w.writeheader()
        for s in summaries: w.writerow(s)
    print(f"wrote {OUT_SUMMARY}")

    if all_per_gse:
        with open(OUT_PER_GSE, "w") as f:
            w = csv.DictWriter(f, fieldnames=list(all_per_gse[0].keys()), delimiter="\t")
            w.writeheader()
            for r in all_per_gse: w.writerow(r)
        print(f"wrote {OUT_PER_GSE} ({len(all_per_gse)} rows)")


if __name__ == "__main__":
    main()
