"""One-shot: build a per-term direct-parent + direct-child map across CLO and
EFO using `is_a` edges, save as JSON for the curator app builder to consume."""
import json
from pathlib import Path

import obonet

OUT = Path("revisions/data/cell_line_isa.json")
SRCS = [
    ("revisions/data/CLO.obo",        "CLO"),
    ("revisions/data/efo_v3.79.0.obo","EFO"),
]


def main():
    parents: dict[str, set[str]] = {}
    children: dict[str, set[str]] = {}
    for path, _ in SRCS:
        print(f"loading {path}")
        g = obonet.read_obo(path)
        # obonet uses graph nodes as ids, edges as (child, parent) for is_a
        # Iterate over edges - obonet's is_a edges have key 'is_a' label
        for u, v, k in g.edges(keys=True):
            if k != "is_a":
                continue
            parents.setdefault(u, set()).add(v)
            children.setdefault(v, set()).add(u)
    print(f"is_a parents recorded for {len(parents)} terms; children for {len(children)}")
    out = {
        "parents":  {k: sorted(v) for k, v in parents.items()},
        "children": {k: sorted(v) for k, v in children.items()},
    }
    OUT.write_text(json.dumps(out))
    print(f"wrote {OUT}  ({OUT.stat().st_size/1e6:.1f} MB)")


if __name__ == "__main__":
    main()
