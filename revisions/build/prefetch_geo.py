"""Pre-fetch GEO SOFT + PMC paper data for a sample TSV, with retry and modest concurrency.

Run once before launching the per-model LLM jobs so the Claude workers only read from disk.
"""
import argparse
import concurrent.futures
import csv
import os
import sys

from geo_fetch import build_input, fetch_pmc_passages, fetch_soft, parse_soft


def prefetch_one(gse: str) -> tuple[str, bool, str]:
    try:
        soft = fetch_soft(gse)
        parsed = parse_soft(soft)
        for pmid in parsed["pubmed_ids"]:
            fetch_pmc_passages(pmid)
        return gse, True, ""
    except Exception as e:
        return gse, False, repr(e)


def main(sample: str, workers: int = 2):
    with open(sample) as f:
        gses = [r["shortName"] for r in csv.DictReader(f, delimiter="\t")]
    print(f"Prefetching {len(gses)} GSEs with {workers} workers", file=sys.stderr)
    ok = 0
    failed = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as ex:
        for gse, success, err in ex.map(prefetch_one, gses):
            if success:
                ok += 1
            else:
                failed.append((gse, err))
            if (ok + len(failed)) % 25 == 0:
                print(f"  {ok + len(failed)}/{len(gses)}  ok={ok}  failed={len(failed)}", file=sys.stderr)
    print(f"\nDone: ok={ok}  failed={len(failed)}", file=sys.stderr)
    for gse, err in failed[:20]:
        print(f"  FAIL {gse}: {err}", file=sys.stderr)
    if failed:
        with open(f"{os.path.splitext(sample)[0]}.prefetch_failed.tsv", "w") as f:
            for gse, err in failed:
                f.write(f"{gse}\t{err}\n")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--sample", required=True)
    ap.add_argument("--workers", type=int, default=2)
    args = ap.parse_args()
    main(args.sample, args.workers)
