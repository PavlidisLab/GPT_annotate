"""Fetch GEO SOFT metadata + (optional) PMC article passages and build a Claude input."""
import gzip
import io
import json
import os
import random
import re
import time
import urllib.error
import urllib.request
from typing import Optional

CACHE_DIR = "revisions/data/geo_cache"
PAPER_DIR = "revisions/data/paper_cache"
os.makedirs(CACHE_DIR, exist_ok=True)
os.makedirs(PAPER_DIR, exist_ok=True)


def _soft_url(gse: str) -> str:
    # https://ftp.ncbi.nlm.nih.gov/geo/series/GSE9nnn/GSE9471/soft/GSE9471_family.soft.gz
    bucket = re.sub(r"\d{1,3}$", "nnn", gse)
    return f"https://ftp.ncbi.nlm.nih.gov/geo/series/{bucket}/{gse}/soft/{gse}_family.soft.gz"


def _urlopen_retry(url: str, timeout: int = 120, max_attempts: int = 6):
    """Open a URL with exponential backoff on transient HTTP/network errors."""
    last_exc = None
    for attempt in range(max_attempts):
        try:
            return urllib.request.urlopen(url, timeout=timeout)
        except urllib.error.HTTPError as e:
            last_exc = e
            if e.code in (429, 500, 502, 503, 504):
                pass
            else:
                raise
        except urllib.error.URLError as e:
            last_exc = e
        except (TimeoutError, ConnectionResetError) as e:
            last_exc = e
        # backoff: 1, 2, 4, 8, 16, 32 seconds + jitter
        delay = (2 ** attempt) + random.uniform(0, 1)
        time.sleep(delay)
    raise last_exc  # type: ignore[misc]


def fetch_soft(gse: str) -> str:
    path = os.path.join(CACHE_DIR, f"{gse}.soft")
    if os.path.exists(path):
        with open(path) as f:
            return f.read()
    url = _soft_url(gse)
    with _urlopen_retry(url) as r:
        raw = r.read()
    text = gzip.decompress(raw).decode("utf-8", errors="replace")
    with open(path, "w") as f:
        f.write(text)
    return text


def parse_soft(text: str) -> dict:
    """Parse a GSE family SOFT file into series + per-sample fields used in the paper's prompt."""
    series = {"title": [], "summary": [], "overall_design": [], "pubmed_id": []}
    samples = []  # list of {title, characteristics, protocol}
    current = None
    cur_sample = None
    for line in text.splitlines():
        if line.startswith("^"):
            kind, _, ident = line[1:].partition("=")
            kind = kind.strip()
            if kind == "SERIES":
                current = "series"
            elif kind == "SAMPLE":
                if cur_sample is not None:
                    samples.append(cur_sample)
                cur_sample = {"title": [], "characteristics": [], "protocol": []}
                current = "sample"
            else:
                current = "other"
            continue
        if not line.startswith("!"):
            continue
        key, _, val = line[1:].partition("=")
        key = key.strip()
        val = val.strip()
        if current == "series":
            if key == "Series_title":
                series["title"].append(val)
            elif key == "Series_summary":
                series["summary"].append(val)
            elif key == "Series_overall_design":
                series["overall_design"].append(val)
            elif key == "Series_pubmed_id":
                series["pubmed_id"].append(val)
        elif current == "sample" and cur_sample is not None:
            if key == "Sample_title":
                cur_sample["title"].append(val)
            elif key.startswith("Sample_characteristics"):
                cur_sample["characteristics"].append(val)
            elif key.startswith("Sample_extract_protocol"):
                cur_sample["protocol"].append(val)
    if cur_sample is not None:
        samples.append(cur_sample)

    # Flatten single-element lists like the R code's jsonlite behaviour
    def flatten(d):
        return {k: (v[0] if len(v) == 1 else v) for k, v in d.items()}

    out_samples = []
    for s in samples:
        out_samples.append({
            "title": s["title"][0] if s["title"] else "",
            "characteristics": s["characteristics"],
            "protocol": s["protocol"],
        })

    return {
        "title": " ".join(series["title"]),
        "summary": "\n".join(series["summary"]),
        "overall_design": "\n".join(series["overall_design"]),
        "pubmed_ids": series["pubmed_id"],
        "samples": out_samples,
    }


def fetch_pmc_passages(pmid: str) -> Optional[dict]:
    """Use the BioC PMC endpoint (same as the R pipeline) to get TITLE/ABSTRACT/METHODS."""
    path = os.path.join(PAPER_DIR, f"{pmid}.json")
    if not os.path.exists(path):
        url = f"https://www.ncbi.nlm.nih.gov/research/bionlp/RESTful/pmcoa.cgi/BioC_json/{pmid}/unicode"
        try:
            with _urlopen_retry(url, timeout=60) as r:
                body = r.read().decode("utf-8", errors="replace")
        except Exception as e:
            return None
        if body.startswith("[Error]") or body.startswith("No record"):
            return None
        with open(path, "w") as f:
            f.write(body)
    else:
        with open(path) as f:
            body = f.read()
    try:
        data = json.loads(body)
    except Exception:
        return None
    sections = {"TITLE": [], "ABSTRACT": [], "METHODS": []}
    docs = []
    if isinstance(data, list):
        for entry in data:
            docs.extend(entry.get("documents", []))
    elif isinstance(data, dict):
        docs.extend(data.get("documents", []))
    for doc in docs:
        for p in doc.get("passages", []):
            stype = (p.get("infons") or {}).get("section_type")
            if stype in sections:
                sections[stype].append(p.get("text", ""))
    return {
        "title": "\n".join(sections["TITLE"]),
        "abstract": "\n".join(sections["ABSTRACT"]),
        "methods": "\n".join(sections["METHODS"]),
    }


def build_input(gse: str) -> dict:
    """Build the per-experiment input dict matching the paper's prompt schema."""
    soft = fetch_soft(gse)
    parsed = parse_soft(soft)
    out = {
        "overall_design": parsed["overall_design"],
        "summary": parsed["summary"],
        "samples": parsed["samples"],
    }
    papers = []
    for pmid in parsed["pubmed_ids"]:
        p = fetch_pmc_passages(pmid)
        if p is not None and (p["title"] or p["abstract"] or p["methods"]):
            papers.append(p)
    if papers:
        out["papers"] = papers
    return out


if __name__ == "__main__":
    import sys
    gse = sys.argv[1] if len(sys.argv) > 1 else "GSE9471"
    inp = build_input(gse)
    print(json.dumps(inp, indent=2)[:3000])
    print("...")
    print(f"num samples: {len(inp['samples'])}")
    print(f"has paper: {'papers' in inp}")
