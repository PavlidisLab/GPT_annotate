"""Probe EFO releases to find the version whose mouse-strain count matches Rogic et al. (140)."""
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

import sys
import urllib.request

import networkx as nx
import obonet

CANDIDATE_VERSIONS = [
    "v3.65.0", "v3.66.0", "v3.67.0", "v3.68.0", "v3.69.0",
    "v3.70.0", "v3.71.0", "v3.72.0", "v3.73.0", "v3.74.0",
]
REMOVE = {"EFO:0004000", "EFO:0003013"}


def count_mouse_strains(url: str) -> int:
    path = f"/tmp/{url.rsplit('/', 1)[-1]}_{url.rsplit('/', 3)[-2]}"
    try:
        urllib.request.urlretrieve(url, path)
    except Exception as e:
        return -1
    try:
        g = obonet.read_obo(path)
    except Exception as e:
        print(f"  parse error: {e}", file=sys.stderr)
        return -2
    root = "NCBITaxon:10090"
    if root not in g:
        return -3
    desc = nx.ancestors(g, root)
    desc = {n for n in desc if not n.startswith("NCBITaxon")}
    desc = desc - REMOVE
    return len(desc)


for v in CANDIDATE_VERSIONS:
    url = f"https://github.com/EBISPOT/efo/releases/download/{v}/efo.obo"
    n = count_mouse_strains(url)
    print(f"{v}: {n} EFO mouse strains (target 140)")
