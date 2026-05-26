# revisions/ — replication pipeline

End-to-end replication of every analysis in this manuscript revision.

```bash
# from the repository root
./revisions/replicate.sh                  # run everything
DRY_RUN=1 ./revisions/replicate.sh        # print commands; run nothing
PHASES="build,strain" ./revisions/replicate.sh   # subset
```

Wall-clock end-to-end on a single laptop is roughly **6–10 hours**, of
which about half is OpenAI Batch queue waiting (not CPU). Total API
spend at current pricing is **roughly $500**, dominated by Opus runs;
the section below itemises this so you can decide which phases to skip.

## Prerequisites

- **Python 3.9+** with the project venv at `revisions/.venv/`
  (`uv venv revisions/.venv && uv pip install -r revisions/requirements.txt`
  if you need to rebuild it).
- **R 4.x** with `magrittr`, `ontologyIndex`, `rdflib`, `jsonlite`.
- **API keys** in the macOS Keychain (the runners fall back through
  `<VAR>` env, then Keychain service name `<VAR>`):
  - `ANTHROPIC_API_KEY`
  - `OPENAI_API_KEY`     (tier 2+ required for Batch API enqueue cap)
  - `TOGETHERAI_API_KEY`
- **Source data** already shipped in the repository:
  - `data-raw/strain_data/main_frame.rds` (6 054 strain experiments)
  - `data-raw/cell_line_data/main_frame.rds` (3 383 cell-line experiments)
- **Internet** for ontology downloads, GEO SOFT files, PMC paper text,
  and the API calls.

## Phases at a glance

| Phase | What it does | Wall time | API spend | Output |
|---|---|---|---|---|
| `keys` | Verify API keys present | <1 s | $0 | (none) |
| `build` | Ontology dictionaries, dense/BM25/is_a indices, samples, main-frame TSV export | 5 min | $3 (OpenAI embeddings for the 46 k cell-line dictionary, one-time) | `revisions/data/` |
| `fetch` | Prefetch GEO SOFT + PMC paper text for the 1 020 GSEs across both samples | 10–30 min, network-bound | $0 | `revisions/data/{geo_cache,paper_cache}/` |
| `strain` | Annotate the 500-GSE strain sample with 5 LLMs (3 Claude + Llama + faithful GPT-4o) ± specificity-rule prompt + 3 non-LLM baselines | 1–3 h wall + batch wait | ~$190 (see breakdown) | `revisions/data/results/<model_tag>/` |
| `cell` | Cell-line sample with Sonnet + Opus dense, then hybrid (dense + BM25 + RRF) for all three frontier LLMs + SapBERT baseline | 1–2 h wall + batch wait | ~$295 | `revisions/data/results_cl/<model_tag>/` |
| `ab` | Paper-vs-no-paper A/B (GPT-4o + spec rule, with vs without the linked publication) | ~30 min batch | ~$5 | `…/gpt-4o-2024-11-20_specprompt_nopaper/` |
| `analyze` | Wilson CIs, paired McNemar, Cohen's κ, ensemble, top-K, spec-rule effect, quote verifier, cross-walk + curator-inheritance metric | 5–10 min | $0 | `revisions/data/analysis/*.tsv` |
| `curate` | Long curation sheet, per-GSE Sonnet sheet, refresh live-Gemma cache (`.suffix` fallback), build curator-app HTML | 5 min | $0 (only Gemma REST queries) | `revisions/data/long_curation_sheet.tsv`, `curator_app/index.html` |
| `figures` | Regenerate every SVG / PNG | 1 min | $0 | `revisions/figures/` |

Each phase is idempotent: re-running with the cache already in place
skips the LLM calls and recomputes only the aggregate tables.

## Itemised cost breakdown

API rates as of 2026-05. Real-time / Batch flag in parentheses.

### Strain phase — ~$190 total

| Step | Cost basis | Subtotal |
|---|---|---:|
| Claude Sonnet 4.6 baseline (real-time) | 500 × $0.034 | $17 |
| Claude Sonnet 4.6 + specificity rule (real-time) | 500 × $0.034 | $17 |
| Claude Opus 4.7 baseline (real-time) | 500 × $0.23 | $115 |
| Claude Haiku 4.5 baseline (real-time) | 500 × $0.011 | $6 |
| Llama 3.3 70B Instruct baseline (Together AI) | 500 × $0.020 | $10 |
| Llama 3.3 70B Instruct + spec rule (Together AI) | 500 × $0.020 | $10 |
| GPT-4o + spec rule, faithful re-run (OpenAI Batch) | 500 × $0.015 | $8 |
| Claude Opus 4.7 noise rerun (real-time) | 20 × $0.23 | $5 |
| text2term, SapBERT, BM25 baselines | local, $0 | $0 |

### Cell-line phase — ~$295 total

| Step | Cost basis | Subtotal |
|---|---|---:|
| Claude Sonnet 4.6 dense Stage 1+2 (real-time) | ~500 × $0.05 | $25 |
| Claude Opus 4.7 dense Stage 1+2 (real-time) | ~500 × $0.26 | $130 |
| Claude Sonnet 4.6 hybrid Stage 2 (cache reused) | ~500 × $0.034 | $17 |
| Claude Opus 4.7 hybrid Stage 2 (cache reused) | ~500 × $0.23 | $115 |
| GPT-4o cell-line hybrid Stage 2 (OpenAI Batch) | ~500 × $0.015 | $8 |
| SapBERT cell-line baseline | local, $0 | $0 |

### A/B + indices + analysis — ~$8 total

| Step | Cost basis | Subtotal |
|---|---|---:|
| GPT-4o no-paper Batch (paper-vs-no-paper A/B) | ~290 × $0.015 | $5 |
| OpenAI text-embedding-3-large index for 46 k cell-line terms (one-time) | one-time | $3 |
| All `analysis/`, `curate`, `figures` phases | local, $0 | $0 |

### Skipping Opus saves the most

The two Opus runs together account for **~$245 of the ~$500 total**
(roughly half). If you only need the frontier-vs-Sonnet comparison,
running:

```bash
PHASES="keys,build,fetch,strain,analyze,figures" \
  CLAUDE_OPUS_SKIP=1 \
  ./revisions/replicate.sh
```

would shave the bill to ~$250 (manually skip the Opus invocations in
`replicate.sh` if you adopt this pattern; the script doesn't currently
honour `CLAUDE_OPUS_SKIP`).

## OpenAI tier requirements

The faithful GPT-4o runs and the cell-line GPT-4o hybrid run go
through OpenAI's Batch API. At tier-2 (≥$50 lifetime spend on OpenAI,
≥7 days since account creation; automatic promotion) the per-batch
enqueued-token cap is ~1.35 M tokens, and the runner packs requests
into ~50-GSE chunks (~1 M tokens each). On tier-1 (default for new
accounts), the cap is 90 k enqueued tokens, which is too low for our
~22 k-token-per-call prompts; the strain run becomes infeasible. The
script will fail at the first batch submission on tier-1 with a clear
`Enqueued token limit` error.

If a chunk lands in `failed` state because the enqueue cap is full at
submission time, `replicate.sh` invokes the runner's `recover`
subcommand to resubmit. Each chunk's 24-hour SLA is honoured (usually
drains in 5–30 min); the script polls every 60 s.

## What gets produced

```
revisions/
├── data/
│   ├── strain_list.json                Strain ontology dictionary (156 terms)
│   ├── cell_line_list.json             Cell-line ontology dictionary (~46 k)
│   ├── cell_line_embeddings.npy        OpenAI text-embedding-3-large dense index
│   ├── cell_line_bm25.pkl              BM25 sparse index over the same terms
│   ├── cell_line_terms.json            Cross-walk equivalence graph
│   ├── cell_line_isa.json              CLO + EFO is_a parent/child map
│   ├── sample500.tsv                   Strain sample (500 GSEs)
│   ├── sample_cell500.tsv              Cell-line sample (500 GSEs)
│   ├── sample_noise20.tsv              20-GSE noise subsample for Opus rerun
│   ├── geo_cache/                      Cached GEO SOFT records
│   ├── paper_cache/                    Cached PMC paper passages
│   ├── gemma_cache/                    Live Gemma REST snapshots
│   ├── results/<model_tag>/            Per-experiment strain annotations + summary.tsv
│   ├── results_cl/<model_tag>/         Per-experiment cell-line annotations + summary.tsv
│   ├── long_curation_sheet.tsv         Per-(GSE, class) row for the curator app
│   └── sonnet_curation_sheet.tsv       Per-GSE Sonnet review sheet (original-format compatible)
├── figures/                            fig3 / fig4 / fig5 / fig6 / fig7 SVG + PNG
└── curator_app/index.html              Self-contained curator-review web app
```

## Directory layout

The replication scripts are grouped by role; each subdir is a flat
collection of `.py` and `.R` files. A small `sys.path` bootstrap at
the top of every moved Python file makes cross-subdir imports work via
flat module names (e.g. `from strain_annotate import STRAIN_TOOL`).

```
revisions/
├── runners/         Entry-point scripts that orchestrate full runs:
│                      run_sample.py             — strain (Claude)
│                      run_open_sample.py        — strain (OpenAI-compatible
│                                                   APIs: Together, Groq,
│                                                   OpenRouter, OpenAI, …)
│                      run_cell_line_sample.py   — cell-line dense, Stage 1+2
│                      run_hybrid_cell_line.py   — cell-line hybrid retrieval,
│                                                   Stage 2 only (re-uses cache)
│                      gpt4o_batch.py            — strain Stage 2 via Batch API
│                                                   (the original generation
│                                                   parameters; submit / poll /
│                                                   recover / finalize)
│                      gpt4o_cell_line_hybrid.py — cell-line Stage 2 via Batch API
│
├── annotators/      Per-experiment inference logic (called by runners):
│                      strain_annotate.py         strain_annotate_open.py
│                      cell_line_annotate.py      hybrid_retrieval.py
│
├── baselines/       Non-LLM comparators:
│                      text2term_baseline.py      sapbert_baseline.py
│                      bm25_strain_baseline.py    sapbert_eval_cellline.py
│
├── build/           Index, dictionary, sample, fetch-cache builders.
│                    Run once before the runners. Includes
│                    build_long_curation_sheet.py and
│                    build_sonnet_curation_sheet.py for the curator-review
│                    sheets, and prefetch_geo.py for the GEO/PMC warmup.
│
├── analysis/        Post-hoc analysis: McNemar, Cohen's κ, Wilson CIs,
│                    ensemble, top-K sensitivity, quote verifier, etc.
│
├── metrics/         Evaluation primitives:
│                      cell_line_eval.py          — cross-walk index + per-GSE
│                                                   exact + cross-walk match
│                      cell_line_inherit_curator.py — curator-inheritance
│                                                   rule
│
├── curator_app/     Static single-page web app for curator audit
│                    of LLM-flagged Gemma-silent annotations:
│                      build_app.py → index.html (self-contained, ~1.9 MB)
│
├── data/            Inputs, caches, sample TSVs, per-GSE result JSONs
├── figures/         SVG + PNG outputs (regenerated by make_figures.py)
│
├── geo_fetch.py     Shared GEO SOFT + PMC fetcher (used by every runner)
├── gemma_fetch.py   Live Gemma REST snapshotter with .suffix fallback
│                    for re-imported series
├── make_figures.py  Single script producing every figure
├── strain_prompt.txt, strain_prompt_specificity.txt
├── cell_line_prompt_p1.txt, cell_line_prompt_p2.txt
├── replicate.sh     End-to-end driver (this file's main entry point)
└── README.md        ← you are here
```

## Common adjustments

- **Smoke-test:** `DRY_RUN=1 ./revisions/replicate.sh` prints every
  command; spends $0 and runs nothing.
- **Run a subset:** `PHASES=build,fetch` exercises the builders
  without any API calls; `PHASES=figures` re-renders the figures from
  cached results.
- **Different model:** the runners read `CLAUDE_MODEL` /
  `OPEN_MODEL` / `OPEN_PROVIDER` from the environment, so you can
  prepend e.g. `CLAUDE_MODEL=claude-haiku-4-5-20251001 …` to a single
  invocation. The replicate script wires the canonical model set.
- **Larger / smaller sample:** rebuild a different sample with
  `Rscript revisions/build/sample_gse.R <N> <out_path> <seed>` and
  point the runners at it via `--sample`.
- **Force a re-run** instead of using the cache: every runner accepts
  `--force`. The replicate script does not pass this, so re-running is
  cheap.

## Curator app

`revisions/curator_app/index.html` is a self-contained web page. Open
it in any browser; verdicts persist to `localStorage` per browser.
Curators export their verdicts as JSON via the toolbar button when
done, and the JSONs can be re-imported into a master session for
aggregation. The page does not need a server. Four priority filters
at the top constrain the audit to the cases worth reviewing (LLM
extras that go with a Gemma match, with `gemma_related` specificity
disagreements already filtered out).
