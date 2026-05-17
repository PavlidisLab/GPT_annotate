"""Two-pass cell-line annotation: Claude extraction → embedding RAG retrieval → Claude pick.

Mirrors analysis/cell_lines/{02.ask_gpt.R, 03.compare_to_vect.R, 04.rag_inputs.R, 05.rag_gpt.R}.
Embeddings: OpenAI text-embedding-3-large (paper parity).
LLM:        Claude Sonnet 4.6 by default (override with CLAUDE_MODEL).
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
import os
import subprocess
import sys
import time
from typing import Optional

import anthropic
import numpy as np
from openai import OpenAI

from geo_fetch import build_input

MODEL          = os.environ.get("CLAUDE_MODEL", "claude-sonnet-4-6")
EMB_MODEL      = "text-embedding-3-large"
PROMPT_P1_PATH = os.environ.get("CELL_LINE_PROMPT_P1", "revisions/cell_line_prompt_p1.txt")
PROMPT_P2_PATH = os.environ.get("CELL_LINE_PROMPT_P2", "revisions/cell_line_prompt_p2.txt")
LIST_PATH      = "revisions/data/cell_line_list.json"
EMB_PATH       = "revisions/data/cell_line_embeddings.npy"
IDS_PATH       = "revisions/data/cell_line_embedding_ids.json"
TOP_K          = 50  # paper uses top-50 candidates


# --- key handling --------------------------------------------------------------------

def _keychain(name: str) -> str:
    return subprocess.check_output(
        ["security", "find-generic-password", "-s", name, "-w"], text=True
    ).strip()

def get_anthropic_key() -> str:
    return os.environ.get("ANTHROPIC_API_KEY") or _keychain("ANTHROPIC_API_KEY")

def get_openai_key() -> str:
    return os.environ.get("OPENAI_API_KEY") or _keychain("OPENAI_API_KEY")


# --- structured-output schemas -------------------------------------------------------

TOOL_EXTRACT = {
    "name": "report_cell_lines_extracted",
    "description": "Report the free-text cell lines used in the experiment.",
    "input_schema": {
        "type": "object",
        "additionalProperties": False,
        "required": ["cell_lines"],
        "properties": {
            "cell_lines": {
                "type": "array",
                "items": {
                    "type": "object",
                    "additionalProperties": False,
                    "required": ["cell_line_name", "description", "quote"],
                    "properties": {
                        "cell_line_name": {"type": "string"},
                        "description":     {"type": "string"},
                        "quote":            {"type": "array", "items": {"type": "string"}},
                    },
                },
            }
        },
    },
}

TOOL_ANNOTATE = {
    "name": "report_cell_lines_annotated",
    "description": "Report ontology terms for the cell lines used, picked from the provided candidates only.",
    "input_schema": {
        "type": "object",
        "additionalProperties": False,
        "required": ["cell_lines"],
        "properties": {
            "cell_lines": {
                "type": "array",
                "items": {
                    "type": "object",
                    "additionalProperties": False,
                    "required": ["cell_line_name", "cell_line_ID"],
                    "properties": {
                        "cell_line_name": {"type": "string"},
                        "cell_line_ID":    {"type": "string"},
                    },
                },
            }
        },
    },
}


# --- shared state --------------------------------------------------------------------

_ANTHROPIC: Optional[anthropic.Anthropic] = None
_OPENAI:    Optional[OpenAI] = None
_EMB:       Optional[np.ndarray] = None
_ID_LIST:   Optional[list] = None
_ID_TO_IDX: Optional[dict] = None
_LIST:      Optional[dict] = None
_SYS_P1:    Optional[str] = None


def anthropic_client() -> anthropic.Anthropic:
    global _ANTHROPIC
    if _ANTHROPIC is None:
        _ANTHROPIC = anthropic.Anthropic(api_key=get_anthropic_key())
    return _ANTHROPIC


def openai_client() -> OpenAI:
    global _OPENAI
    if _OPENAI is None:
        _OPENAI = OpenAI(api_key=get_openai_key())
    return _OPENAI


def load_index() -> tuple[np.ndarray, list, dict]:
    global _EMB, _ID_LIST, _ID_TO_IDX
    if _EMB is None:
        _EMB = np.load(EMB_PATH)
        _ID_LIST = json.load(open(IDS_PATH))
        _ID_TO_IDX = {i: k for k, i in enumerate(_ID_LIST)}
    return _EMB, _ID_LIST, _ID_TO_IDX


def load_term_dict() -> dict:
    global _LIST
    if _LIST is None:
        _LIST = json.load(open(LIST_PATH))
    return _LIST


def system_prompt_p1() -> str:
    global _SYS_P1
    if _SYS_P1 is None:
        with open(PROMPT_P1_PATH) as f:
            _SYS_P1 = f.read().rstrip()
    return _SYS_P1


# --- core: per-experiment annotation -------------------------------------------------

def _claude_kwargs(**rest):
    if MODEL.startswith("claude-opus-4-7"):
        return rest
    return {**rest, "temperature": 0}


def _extract_cell_lines(tool_use) -> list:
    """Return a list of {cell_line_name, ...} dicts from a tool_use block.

    Opus 4.7 occasionally serializes the entire tool input as a JSON string
    (input == {"cell_lines": "<json-encoded-array>"}) rather than as native
    structured data, so we json.loads if needed.
    """
    if not tool_use:
        return []
    val = tool_use.input.get("cell_lines", []) if isinstance(tool_use.input, dict) \
        else json.loads(tool_use.input).get("cell_lines", [])
    if isinstance(val, str):
        try:
            val = json.loads(val)
        except json.JSONDecodeError:
            return []
        if isinstance(val, dict) and "cell_lines" in val:
            val = val["cell_lines"]
    return val if isinstance(val, list) else []


def first_pass(experiment: dict, max_tokens: int = 2048) -> dict:
    """Stage 1: Claude reads experiment JSON and emits free-text cell lines."""
    client = anthropic_client()
    sys_blocks = [{"type": "text", "text": system_prompt_p1(),
                   "cache_control": {"type": "ephemeral"}}]
    kwargs = _claude_kwargs(
        model=MODEL, max_tokens=max_tokens,
        system=sys_blocks,
        tools=[TOOL_EXTRACT],
        tool_choice={"type": "tool", "name": TOOL_EXTRACT["name"]},
        messages=[{"role": "user", "content": json.dumps(experiment)}],
    )
    msg = client.messages.create(**kwargs)
    tool_use = next((b for b in msg.content if b.type == "tool_use"), None)
    return {
        "cell_lines": _extract_cell_lines(tool_use),
        "usage": {
            "input_tokens": msg.usage.input_tokens,
            "output_tokens": msg.usage.output_tokens,
            "cache_read": getattr(msg.usage, "cache_read_input_tokens", 0) or 0,
            "cache_creation": getattr(msg.usage, "cache_creation_input_tokens", 0) or 0,
        },
    }


def retrieve_candidates(extractions: list[dict], top_k: int = TOP_K) -> dict[str, list[dict]]:
    """For each first-pass extraction, return the top-k ontology terms by cosine similarity.

    Matches analysis/cell_lines/03.compare_to_vect.R: query string is `name\\ndescription`.
    Returns {name: [{ID, value, description, synonyms, cosine_similarity}, ...]}.
    """
    if not extractions:
        return {}
    emb, ids, id_to_idx = load_index()
    terms = load_term_dict()
    queries = [
        f"{e.get('cell_line_name','')}\n{e.get('description','')}"
        for e in extractions
    ]
    resp = openai_client().embeddings.create(model=EMB_MODEL, input=queries)
    q = np.asarray([d.embedding for d in resp.data], dtype=np.float32)
    qn = q / np.linalg.norm(q, axis=1, keepdims=True).clip(min=1e-12)
    sims = qn @ emb.T  # (n_queries, N)
    out = {}
    for i, ext in enumerate(extractions):
        top_idx = np.argpartition(-sims[i], top_k)[:top_k]
        top_idx = top_idx[np.argsort(-sims[i, top_idx])]
        cand_list = []
        for j in top_idx:
            term_id = ids[j]
            term = terms.get(term_id, {})
            cand_list.append({
                "ID": term_id,
                "value": term.get("value", ""),
                "description": term.get("description", ""),
                "synonyms": term.get("synonyms", []),
                "cosine_similarity": float(sims[i, j]),
            })
        # key by the first-pass cell_line_name
        key = ext.get("cell_line_name") or f"_unnamed_{i}"
        out[key] = cand_list
    return out


def second_pass(experiment: dict, first_pass_result: list[dict],
                ontology_terms: dict[str, list[dict]],
                max_tokens: int = 2048) -> dict:
    """Stage 2: Claude picks ontology IDs from the retrieved candidates."""
    client = anthropic_client()
    with open(PROMPT_P2_PATH) as f:
        sys_text = f.read().rstrip()
    # Build the augmented experiment payload
    payload = dict(experiment)
    payload["gpt_inference"] = first_pass_result
    payload["ontology_terms"] = ontology_terms
    kwargs = _claude_kwargs(
        model=MODEL, max_tokens=max_tokens,
        system=sys_text,
        tools=[TOOL_ANNOTATE],
        tool_choice={"type": "tool", "name": TOOL_ANNOTATE["name"]},
        messages=[{"role": "user", "content": json.dumps(payload)}],
    )
    msg = client.messages.create(**kwargs)
    tool_use = next((b for b in msg.content if b.type == "tool_use"), None)
    return {
        "cell_lines": _extract_cell_lines(tool_use),
        "usage": {
            "input_tokens": msg.usage.input_tokens,
            "output_tokens": msg.usage.output_tokens,
            "cache_read": getattr(msg.usage, "cache_read_input_tokens", 0) or 0,
            "cache_creation": getattr(msg.usage, "cache_creation_input_tokens", 0) or 0,
        },
    }


def annotate(gse: str) -> dict:
    experiment = build_input(gse)

    t0 = time.time()
    p1 = first_pass(experiment)
    t_p1 = time.time() - t0

    cands = retrieve_candidates(p1["cell_lines"])
    t_retr = time.time() - t0 - t_p1

    if p1["cell_lines"]:
        p2 = second_pass(experiment, p1["cell_lines"], cands)
    else:
        p2 = {"cell_lines": [], "usage": {"input_tokens": 0, "output_tokens": 0,
                                          "cache_read": 0, "cache_creation": 0}}
    t_p2 = time.time() - t0 - t_p1 - t_retr

    return {
        "gse": gse,
        "model": MODEL,
        "embedding_model": EMB_MODEL,
        "first_pass": p1["cell_lines"],
        "annotations": p2["cell_lines"],
        "usage": {
            "p1": p1["usage"],
            "p2": p2["usage"],
        },
        "timings": {"p1_s": round(t_p1, 2), "retrieval_s": round(t_retr, 2), "p2_s": round(t_p2, 2)},
    }


if __name__ == "__main__":
    gse = sys.argv[1] if len(sys.argv) > 1 else "GSE1923"
    out = annotate(gse)
    print(json.dumps(out, indent=2))
