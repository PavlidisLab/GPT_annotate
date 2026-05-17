"""SapBERT baseline for the strain and cell-line annotation tasks.

SapBERT (Liu et al. 2021, *NAACL*) is a BERT-base model fine-tuned with
self-aligned UMLS pretraining for biomedical concept normalisation. It is
the canonical non-LLM neural baseline for "free-text mention → ontology
term" mapping and is what reviewers typically expect to see compared
alongside an LLM pipeline.

We use the public ``cambridgeltl/SapBERT-from-PubMedBERT-fulltext``
checkpoint. The HuggingFace cache is redirected to ``~/Data/huggingface``
to keep large downloads out of the repo and project venv.

Workflow per task:
  1. Build a dictionary of ``(canonical_id, name)`` rows from the
     committed ontology JSON; one row per name + synonym.
  2. Embed every dictionary row with SapBERT (mean-pool last hidden
     state, L2-normalise) and cache the resulting matrix.
  3. For each GSE, extract candidate strings from the same input fields
     used by the regex baseline + ``text2term`` baseline (sample
     characteristics, sample titles, study summary / overall-design, paper
     abstract / methods).
  4. Embed each candidate string; cosine top-1 against the dictionary.
  5. Keep mappings with cosine ≥ ``--threshold`` (default 0.85, the
     operating point typically cited in the SapBERT paper for evaluation
     against UMLS).
  6. Per-experiment prediction = union of retained mappings.

The output directory mirrors ``text2term_baseline.py`` so the existing
evaluation script can score it (``per_gse/<gse>.json`` + ``summary.tsv``).
"""
from __future__ import annotations

import argparse
import csv
import json
import os
import re
import sys
import time
from typing import Iterable

# Pop big downloads to ~/Data (kept out of the repo and venv).
os.environ.setdefault("HF_HOME",            os.path.expanduser("~/Data/huggingface"))
os.environ.setdefault("TRANSFORMERS_CACHE", os.path.expanduser("~/Data/huggingface/transformers"))

import numpy as np
import torch
from transformers import AutoTokenizer, AutoModel

MODEL_ID = "cambridgeltl/SapBERT-from-PubMedBERT-fulltext"


def _device() -> str:
    if torch.backends.mps.is_available(): return "mps"
    if torch.cuda.is_available():         return "cuda"
    return "cpu"


def _load_model():
    tok = AutoTokenizer.from_pretrained(MODEL_ID)
    model = AutoModel.from_pretrained(MODEL_ID)
    model.eval()
    dev = _device()
    model.to(dev)
    print(f"loaded {MODEL_ID} on {dev}", file=sys.stderr)
    return tok, model, dev


@torch.inference_mode()
def embed_strings(strings: list[str], tok, model, dev: str,
                  batch_size: int = 64, max_length: int = 64) -> np.ndarray:
    """Mean-pool BERT last hidden state, L2-normalise → (N, 768) float32."""
    out_chunks: list[np.ndarray] = []
    for i in range(0, len(strings), batch_size):
        batch = strings[i:i + batch_size]
        enc = tok(batch, padding=True, truncation=True, max_length=max_length,
                  return_tensors="pt").to(dev)
        h = model(**enc).last_hidden_state
        mask = enc["attention_mask"].unsqueeze(-1).float()
        pooled = (h * mask).sum(dim=1) / mask.sum(dim=1).clamp(min=1)
        pooled = torch.nn.functional.normalize(pooled, p=2, dim=1)
        out_chunks.append(pooled.cpu().numpy().astype(np.float32))
    return np.concatenate(out_chunks, axis=0) if out_chunks else np.zeros((0, 768), dtype=np.float32)


# --- dictionary loaders --------------------------------------------------------

def _strain_dictionary() -> list[tuple[str, str]]:
    """Return ``[(canonical_id, surface_name), ...]`` for the strain task."""
    with open("revisions/data/strain_list.json") as f:
        terms = json.load(f)
    rows: list[tuple[str, str]] = []
    for t in terms:
        uri = t.get("URI", "")
        cid = uri.rsplit("/", 1)[-1].replace("_", ":")
        name = t.get("value") or ""
        if name:
            rows.append((cid, name))
        for syn in t.get("synonyms", []) or []:
            if syn and syn != name:
                rows.append((cid, syn))
    return rows


def _cell_line_dictionary() -> list[tuple[str, str]]:
    """Return ``[(canonical_id, surface_name), ...]`` for the cell-line task.

    Uses the committed retrieval dictionary so the candidate set matches the
    LLM-side RAG retrieval index exactly."""
    with open("revisions/data/cell_line_list.json") as f:
        terms = json.load(f)
    rows: list[tuple[str, str]] = []
    iterable = terms.values() if isinstance(terms, dict) else terms
    for t in iterable:
        cid = t.get("ID", "")
        if not cid: continue
        # Normalise to PREFIX:LOCAL
        if cid.startswith("http"):
            cid = cid.rsplit("/", 1)[-1].replace("_", ":")
        if "_" in cid and ":" not in cid:
            p, _, r = cid.partition("_")
            cid = f"{p.upper()}:{r}"
        elif ":" in cid:
            p, _, r = cid.partition(":")
            cid = f"{p.upper()}:{r}"
        name = t.get("value") or ""
        if name:
            rows.append((cid, name))
        for syn in t.get("synonyms", []) or []:
            if syn and syn != name:
                rows.append((cid, syn))
    return rows


# --- candidate extraction -----------------------------------------------------

_STOP_PUNCT = re.compile(r"[\\s\\u00A0]+")

def _candidates_from_experiment(exp: dict, max_cands: int = 80) -> list[str]:
    """Mirror the fields scanned by the regex baseline and ``text2term``:
    sample characteristics, sample titles, study summary / overall design,
    and (when present) paper abstract & methods."""
    out: list[str] = []
    def _add(s):
        if not s: return
        s = s.strip()
        if not s: return
        if s in out: return
        out.append(s)
    for s in exp.get("samples", []) or []:
        _add(s.get("title", ""))
        chars = s.get("characteristics", "")
        if isinstance(chars, list):
            for item in chars: _add(item)
        else:
            chars = (chars or "").strip()
            if chars.startswith("[") and chars.endswith("]"):
                try:
                    import ast
                    for item in ast.literal_eval(chars): _add(item)
                except Exception:
                    _add(chars)
            else:
                _add(chars)
        # Protocol is too long for SapBERT name-level matching; split into
        # sentence-ish chunks so the model sees focused mentions.
        proto = s.get("protocol", "")
        if isinstance(proto, list): proto = " ".join(str(x) for x in proto)
        for sent in re.split(r"(?<=[.!?])\\s+", str(proto)):
            if sent: _add(sent[:300])
    _add(exp.get("summary", ""))
    _add(exp.get("overall_design", ""))
    for p in exp.get("papers", []) or []:
        _add(p.get("title", ""))
        for sent in re.split(r"(?<=[.!?])\\s+", str(p.get("abstract", "") or "")):
            _add(sent[:300])
        for sent in re.split(r"(?<=[.!?])\\s+", str(p.get("methods", "")  or "")):
            _add(sent[:300])
    return out[:max_cands]


# --- task drivers -------------------------------------------------------------

def run_task(task: str, sample_tsv: str, out_dir: str, threshold: float,
             cache_dir: str):
    os.makedirs(out_dir, exist_ok=True)
    os.makedirs(os.path.join(out_dir, "per_gse"), exist_ok=True)
    os.makedirs(cache_dir, exist_ok=True)

    if task == "strain":
        rows = _strain_dictionary()
        truth_field = "gemma_uri"
    elif task == "cell_line":
        rows = _cell_line_dictionary()
        truth_field = "gemma_uri"
    else:
        raise SystemExit(f"unknown task: {task}")

    print(f"[{task}] dictionary: {len(rows)} (id, name) rows", file=sys.stderr)

    # Embed (or load cached) dictionary
    cache_path = os.path.join(cache_dir, f"sapbert_{task}_dict.npz")
    if os.path.exists(cache_path):
        d = np.load(cache_path, allow_pickle=True)
        dict_embs = d["embs"]
        dict_ids  = list(d["ids"])
        dict_names= list(d["names"])
        print(f"[{task}] loaded cached embeddings: {dict_embs.shape}", file=sys.stderr)
    else:
        tok, model, dev = _load_model()
        names = [r[1] for r in rows]
        ids   = [r[0] for r in rows]
        t0 = time.time()
        dict_embs = embed_strings(names, tok, model, dev)
        print(f"[{task}] embedded dictionary in {time.time()-t0:.1f}s", file=sys.stderr)
        np.savez_compressed(cache_path, embs=dict_embs, ids=np.array(ids), names=np.array(names))
        dict_ids   = ids
        dict_names = names

    # Tokeniser+model also needed for per-experiment candidate embeddings.
    tok, model, dev = _load_model()

    # Iterate experiments.
    sample_rows = list(csv.DictReader(open(sample_tsv), delimiter="\t"))
    print(f"[{task}] sample size: {len(sample_rows)} GSEs", file=sys.stderr)

    from geo_fetch import build_input
    n_ok = n_exact = n_any = 0
    summary = []
    t0 = time.time()
    for i, row in enumerate(sample_rows):
        gse = row["shortName"]
        truth_str = (row.get(truth_field, "") or "")
        truth_split = truth_str.split("|||") if "|||" in truth_str else truth_str.split(",")
        truth = set()
        for u in truth_split:
            u = u.strip()
            if not u: continue
            u = (u.replace("http://purl.obolibrary.org/obo/CLO_", "CLO:")
                  .replace("http://purl.obolibrary.org/obo/CL_",  "CL:")
                  .replace("http://www.ebi.ac.uk/efo/EFO_",       "EFO:"))
            if ":" not in u and "_" in u:
                p, _, r = u.partition("_")
                u = f"{p.upper()}:{r}"
            truth.add(u)

        try:
            exp = build_input(gse)
        except Exception as e:
            summary.append({"gse": gse, "error": f"build_input: {e!r}"})
            continue
        cands = _candidates_from_experiment(exp)
        if not cands:
            summary.append({"gse": gse, "pred": "", "truth": ",".join(sorted(truth)),
                            "exact": False, "any_overlap": False})
            continue

        cand_embs = embed_strings(cands, tok, model, dev)
        # Cosine similarity: candidates × dictionary
        sims = cand_embs @ dict_embs.T  # both unit-norm → cosine
        top_idx = sims.argmax(axis=1)
        top_sim = sims.max(axis=1)
        pred: set[str] = set()
        for k, (idx, s) in enumerate(zip(top_idx, top_sim)):
            if s >= threshold:
                pred.add(dict_ids[idx])
        exact = pred == truth
        any_overlap = bool(pred & truth)
        n_ok += 1
        n_exact += int(exact)
        n_any   += int(any_overlap)
        summary.append({"gse": gse,
                        "pred":  ",".join(sorted(pred)),
                        "truth": ",".join(sorted(truth)),
                        "exact": exact, "any_overlap": any_overlap})
        with open(os.path.join(out_dir, "per_gse", f"{gse}.json"), "w") as f:
            json.dump({"gse": gse, "pred": sorted(pred), "truth": sorted(truth),
                       "candidate_count": len(cands)}, f, indent=2)
        if (i + 1) % 25 == 0:
            elapsed = time.time() - t0
            rate    = (i + 1) / elapsed
            eta     = (len(sample_rows) - i - 1) / rate
            print(f"  {i+1}/{len(sample_rows)}  exact={n_exact}/{n_ok} ({n_exact/max(n_ok,1):.1%})  "
                  f"any={n_any}/{n_ok} ({n_any/max(n_ok,1):.1%})  ETA {eta/60:.1f} min", file=sys.stderr)

    out_path = os.path.join(out_dir, "summary.tsv")
    with open(out_path, "w") as f:
        fields = ["gse","pred","truth","exact","any_overlap","error"]
        w = csv.DictWriter(f, fieldnames=fields, delimiter="\t", extrasaction="ignore")
        w.writeheader()
        for r in summary: w.writerow(r)
    print()
    print(f"[{task}] total: {len(sample_rows)}  ok: {n_ok}")
    print(f"[{task}] exact match: {n_exact}/{n_ok} = {n_exact/max(n_ok,1):.1%}")
    print(f"[{task}] any overlap: {n_any}/{n_ok} = {n_any/max(n_ok,1):.1%}")
    print(f"[{task}] wrote {out_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--task", choices=("strain","cell_line"), required=True)
    ap.add_argument("--sample", default=None,
                    help="default depends on task: data/sample500.tsv (strain) or data/sample_cell500.tsv (cell_line)")
    ap.add_argument("--out",    default=None,
                    help="default: revisions/data/results/sapbert_<task>/")
    ap.add_argument("--threshold", type=float, default=0.85,
                    help="cosine similarity cutoff for retained mappings")
    ap.add_argument("--cache-dir", default=os.path.expanduser("~/Data/huggingface/sapbert_dict"),
                    help="where the per-task dictionary embedding matrix is cached")
    args = ap.parse_args()

    if args.sample is None:
        args.sample = ("revisions/data/sample_cell500.tsv" if args.task == "cell_line"
                       else "revisions/data/sample500.tsv")
    if args.out is None:
        args.out = f"revisions/data/results/sapbert_{args.task}"

    run_task(args.task, args.sample, args.out, args.threshold, args.cache_dir)


if __name__ == "__main__":
    main()
