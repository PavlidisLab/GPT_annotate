# Hybrid dense + sparse retrieval for the cell-line second pass

Status: planning, 2026-05-15. Triggered by Mondal/Mittal concurrent-work
review.

## What the experiment tests

Mondal et al. (2025) and Mittal et al. (2026) both run dense semantic
retrieval **together with** a sparse channel (TF-IDF / BM25) for
ontology-term retrieval, then score candidates with reciprocal-rank
fusion. Both papers flag this as the precision-recovery move on
cell-line tasks where dense-only retrieval surfaces the wrong-prefix
mirror (`EFO:HeLa` vs `CLO:HeLa cell`) before the correct term.

Our current cell-line pipeline (FINDINGS §7) uses **dense-only
retrieval** with OpenAI `text-embedding-3-large` over the 46,032-term
cell-line ontology, returning top-50 candidates per first-pass
extraction. The hypothesis: replacing dense-only with
dense + BM25 + RRF will reduce the EFO ↔ CLO confusion in the top-50
window and lift Sonnet 4.6's headline cell-line accuracy from 58.8 %
toward GPT-4o's 64.5 %.

## Why this is cheap to run

1. **First-pass extractions are already cached** in
   `revisions/data/results_cl/claude-sonnet-4-6/<GSE>.json` for every
   GSE in the 500-sample. Schema (per file):
   ```json
   {"gse": "...", "first_pass": [{"cell_line_name": "...", "description": "...", "quote": [...]}, ...],
    "annotations": [{"cell_line_name": "...", "cell_line_ID": "..."}, ...]}
   ```
   The first-pass is identical regardless of which retrieval method we
   use downstream; we re-use it verbatim and only re-run stages 2+3.

2. **OpenAI embeddings (dense channel) are already cached** in
   `revisions/data/cell_line_embeddings.npy` (46032 × 3072 float32,
   L2-normalised). No re-embed needed; we just compute BM25 over the
   same term dict.

3. **Sonnet 4.6 second-pass cost** for the re-run: ~$0.034 / experiment
   × 500 = **~$17 API spend** (real-time, no batch — single-shot
   second-pass calls are small enough). First-pass call count = 0.

## Implementation sketch

1. **Build the BM25 index** over `{value + synonyms}` of each of the
   46,032 cell-line ontology terms in `cell_line_list.json`. Library:
   `rank_bm25.BM25Okapi`. Tokenisation: case-fold, alphanumeric split.
   Cache the index as a pickle alongside the existing `.npy`.

2. **Modify `retrieve_candidates()`** in `cell_line_annotate.py` to
   produce two ranked lists per query (dense via existing FAISS-less
   matmul; sparse via BM25), then fuse via reciprocal rank fusion:
   `score(id) = sum_over_retrievers (1 / (60 + rank_in_that_retriever))`.
   Take the top-50 fused IDs.

3. **Re-run only the second pass** on the 500 cell-line GSEs. New
   results directory:
   `revisions/data/results_cl/claude-sonnet-4-6_hybrid/<GSE>.json`.
   First-pass extractions are loaded from the existing dir; second-pass
   uses the new candidate set.

4. **Aggregate vs current Sonnet 4.6** under the same three scoring
   rules (exact-ID, cross-walk, curator-inheritance) from FINDINGS §7.

## Pre-registered prediction

- Exact-ID match (current 16.3 %) → 18–22 %.
- Cross-walk match (current 36.0 %) → 36–40 % (cross-walk already
  absorbs most prefix-mirror cases; the gain shrinks).
- Curator-inheritance perfect (current 58.8 %) → 60–63 %.

If the gain is < 1 pp under cross-walk scoring, the dense channel is
already capturing the relevant signal and hybrid retrieval doesn't
help on our task — that's a publishable null result.

## Where the result lands in the manuscript

- New SENSITIVITY_ANALYSES.md section: "Hybrid dense + sparse
  retrieval for the cell-line task".
- Brief mention in Discussion contextualising vs Mondal/Mittal.
- If the gain on exact-ID match is ≥ 3 pp, also worth a one-line note
  in §7 ("Updated Sonnet headline with hybrid retrieval: X %").
