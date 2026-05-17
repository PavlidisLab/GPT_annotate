"""Build a BM25 index over the 46,032 cell-line ontology terms.

Index document per term = `value + synonyms`, lower-cased and split on
non-alphanumeric. Persisted as a pickle alongside the existing
`cell_line_embeddings.npy` so the hybrid retriever can load both
without recomputing.
"""
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

import json
import pickle
import re
import sys
import time
from pathlib import Path

from rank_bm25 import BM25Okapi

LIST_PATH = Path("revisions/data/cell_line_list.json")
IDS_PATH  = Path("revisions/data/cell_line_embedding_ids.json")
OUT_PATH  = Path("revisions/data/cell_line_bm25.pkl")


_TOKEN_RE = re.compile(r"[a-z0-9]+")
def tokenize(text: str) -> list[str]:
    return _TOKEN_RE.findall((text or "").lower())


def main():
    print("loading cell-line list…", file=sys.stderr)
    terms_dict = json.load(open(LIST_PATH))
    # Iterate over the SAME id order used by the dense index so we can
    # combine ranks by position without remapping.
    ids = json.load(open(IDS_PATH))
    print(f"  {len(ids)} terms (dense index id order)", file=sys.stderr)

    print("tokenising…", file=sys.stderr)
    t0 = time.time()
    docs = []
    for tid in ids:
        t = terms_dict.get(tid, {})
        parts = [t.get("value") or ""] + [s for s in (t.get("synonyms") or []) if s]
        # description is too noisy for sparse retrieval; keep it for the dense channel only.
        docs.append(tokenize(" ".join(parts)))
    print(f"  tokenised in {time.time()-t0:.1f}s", file=sys.stderr)

    print("building BM25Okapi…", file=sys.stderr)
    t0 = time.time()
    bm = BM25Okapi(docs)
    print(f"  built in {time.time()-t0:.1f}s", file=sys.stderr)

    print(f"writing {OUT_PATH}…", file=sys.stderr)
    with open(OUT_PATH, "wb") as f:
        pickle.dump({"ids": ids, "bm25": bm}, f, protocol=pickle.HIGHEST_PROTOCOL)
    print(f"  wrote {OUT_PATH.stat().st_size/1e6:.1f} MB", file=sys.stderr)


if __name__ == "__main__":
    main()
