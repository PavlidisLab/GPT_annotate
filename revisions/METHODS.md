 # Methods

## Overview

We extend the cell-line and mouse-strain annotation pipelines on the same
Gemma-derived corpus and curator-validated reference set used in the
original submission, substituting the OpenAI GPT-4o language model with three Anthropic
Claude 4 family models: Claude Sonnet 4.6, Claude Opus 4.7, and Claude Haiku
4.5. All other aspects of the pipeline — input construction, ontology
choices, RAG retrieval parameters, output schemas, and evaluation metrics —
are held fixed and follow the original protocol. The OpenAI
`text-embedding-3-large` model used for the cell-line RAG step is retained
unchanged.

## Reference data

We used the curator-validated reference set distributed with the original-submission
GitHub repository: `data-raw/strain_data/main_frame.rds` (6,054 mouse
transcriptomic experiments with at least one curator-validated strain
annotation) and `data-raw/cell_line_data/main_frame.rds` (3,383 experiments
with cell-line annotations). The `gemma_uri` / `gemma_term` columns from these
tables serve as ground truth and include the corrections made by curators
during the original submission (Table 1). The `gpt_uris` /
`gpt_cell_line_term_id` columns preserve the original GPT-4o predictions and
are used here as the OpenAI baseline.

## Ontologies

### Strain task

Mouse strains were drawn from the Experimental Factor Ontology (EFO) and the
in-house Gemma extension TGEMO. To match the original submission exactly, EFO was
pinned to release v3.79.0 (`http://www.ebi.ac.uk/efo/releases/v3.79.0/efo.owl`,
released 2025-06-16), contemporaneous with the strain-task commits in the
original-submission repository (commit `f76e6e4`, "strain eval", 2025-07-10). EFO was
parsed with the R `ontologyIndex` package (v2.12) using
`extract_tags = "everything"` and traversed with `get_descendants` rooted at
`NCBITaxon:10090` (`exclude_roots = TRUE`), mirroring
`analysis/strains/00.downloads.R` in the original repository. The meta-terms
`EFO:0004000` ("Mus musculus strain type") and `EFO:0003013` ("Mus musculus
subspecies") were removed, yielding 138 EFO strain terms. (An earlier
version of `build_strain_list.R` had a prefix-handling bug that allowed
these two meta-terms to leak through; the LLM runs were issued against
the 156-term pre-fix dictionary, but none of the three Claude models
ever selected either meta-term across the full 500-GSE sample, so the
LLM results are unaffected by the fix. The BM25 baseline was re-run on
the 154-term corrected dictionary.)

TGEMO was obtained from the `PavlidisLab/TGEMO` GitHub repository (commit
`95ecc733`, file `TGEMO.OWL`) and parsed with the R `rdflib` package via
SPARQL. Direct subclasses of `obo:NCBITaxon_10090` were retained, yielding 16
TGEMO strain terms. URIs were constructed as
`http://gemma.msl.ubc.ca/ont/TGEMO_<id>` to match the form used in the Gemma
annotation set.

The combined strain list contains 154 terms (138 EFO + 16 TGEMO) after
the meta-term fix; the LLM runs were issued against the 156-term pre-fix
dictionary. Each term is represented as `{URI, value, description,
synonyms}`. Build script: `revisions/build/build_strain_list.R`.

### Cell-line task

The cell-line ontology was assembled from four sources to match the
combined dictionary used in the original submission:

1. **Cell Line Ontology (CLO)** — full OBO release, parsed with `obonet`.
2. **EFO cell-line subtree** — descendants of `EFO:0000322` (cell line)
   from the same v3.79.0 EFO snapshot used for strains.
3. **BRENDA Tissue Ontology (BTO) cell-line subtree** — descendants of
   `BTO:0000214` (cell line). Included because curators occasionally
   reach for BTO ids when the same line is missing from CLO/EFO.
4. **Cell Ontology (CL) leaf cell types** — for the small number of
   cases where the curator-validated term is a primary cell type rather
   than a cell line (e.g. `CL:0000148` melanocyte).

The combined dictionary is built by `revisions/build/build_cell_line_list.R`. Each
term is represented as `{ID, value, description, synonyms}` and contributes
one row to the retrieval index.

### Cross-walk equivalence index

CLO and EFO mirror each other on many cell-line entries (e.g. `EFO:0001185
HeLa` and `CLO:0003684 HeLa cell`); curators typically annotate with CLO
while embedding retrieval favours EFO's shorter labels. To score equivalent
predictions as correct, we built a single cross-walk graph
(`revisions/build/build_cell_line_xrefs.py` → `cell_line_terms.json`) by combining
three signals:

1. **Explicit ontology edges.** Every `xref`, `alt_id`, `seeAlso`, and
   `IAO:0000118` annotation in the OBO releases of CLO, EFO, and BTO is
   read as a bidirectional equivalence edge.
2. **Shared exact names.** A case-folded reverse index over names,
   synonyms, and `IAO:0000118` alternative labels yields additional edges
   between terms that share an exact string in any of these fields.
3. **EFO ↔ CLO name bridge.** The OBO files leave many obvious 1:1
   mirrors unencoded — e.g. `EFO:0004905` "induced pluripotent stem
   cell" is semantically identical to `CLO:0037307` "induced pluripotent
   stem cell line cell" but neither term references the other. We
   normalise every EFO and CLO term name (and synonym/alt-label) by
   stripping the trailing tokens `cell`, `cell line`, or `cell line
   cell` (case-folded), then add a bidirectional xref between any EFO
   id and CLO id whose normalised strings match. This pass adds 591
   additional EFO ↔ CLO edges over the explicit annotations and
   recovers, among others, the pluripotent-stem-cell mirror above.

Two ontology IDs are considered equivalent if they are connected by any
edge in this combined graph. The expansion is implemented lazily, only for
IDs that appear in a prediction or ground-truth set, by
`cell_line_eval.py::CellLineXrefIndex`. Applied to the original GPT-4o
predictions on a 500-experiment random subset, the cross-walk recovers the
59 % headline accuracy reported in the main text (64.5 % on our 500-row
subset; see `SENSITIVITY_ANALYSES.md`).

## Input construction

For each experiment we constructed a JSON input mirroring the structure used
in the original submission (see `create_input` in the original `R/geo_utils.R`):

- `overall_design` and `summary` from the GEO series record;
- `samples`: an array of `{title, characteristics, protocol}` extracted from
  each `GSM` record;
- `papers` (when available): an array of `{title, abstract, methods}`
  extracted per PubMed ID associated with the series.

GEO SOFT family files were retrieved from the NCBI FTP service
(`ftp.ncbi.nlm.nih.gov/geo/series/.../<GSE>_family.soft.gz`) and parsed with
a line-based parser that mirrors the field set used by the R `GEOquery`
package. Article text was retrieved from the NCBI BioC PMC API
(`https://www.ncbi.nlm.nih.gov/research/bionlp/RESTful/pmcoa.cgi/BioC_json/<PMID>/unicode`),
identical to the endpoint used in the original submission; passages were grouped by
`section_type` (`TITLE`, `ABSTRACT`, `METHODS`). Fetches used exponential
backoff (1, 2, 4, …, 32 s with jitter, up to 6 attempts) for HTTP 429 / 5xx
responses. All retrieved artefacts are cached on disk to make runs
deterministic and re-entrant. Scripts: `revisions/geo_fetch.py`,
`revisions/build/prefetch_geo.py`.

## Language models and inference parameters

All Claude inference used the official Anthropic Python SDK
(`anthropic==0.102.0`).

| Role | Model | Model ID |
|---|---|---|
| Annotator | Claude Sonnet 4.6 | `claude-sonnet-4-6` |
| Annotator | Claude Opus 4.7 | `claude-opus-4-7` |
| Annotator | Claude Haiku 4.5 | `claude-haiku-4-5-20251001` |
| Annotator (open-weights) | Llama 3.3 70B Instruct | `meta-llama/Llama-3.3-70B-Instruct-Turbo` (Together AI) |
| Cell-line embedding | OpenAI `text-embedding-3-large` | identical to the original submission |

Common inference settings:

- `max_tokens`: 2,048 (strain task), comparable to the 1,024 / sample used
  for the GPT-4o runs, raised slightly to accommodate Claude's marginally
  more verbose JSON formatting.
- `temperature`: 0 for Sonnet 4.6 and Haiku 4.5. Opus 4.7 does not accept a
  temperature parameter (the API deprecates this control for that model);
  for Opus we use Anthropic's default decoding.
- Structured output: enforced with Anthropic's tool-use mechanism. We
  declare a single tool, `report_strains` (resp. `report_cell_lines`), with
  the same JSON schema used for the GPT-4o runs (`strain_output`,
  `cell_line_output`, `cell_line_annotation` from the original
  `R/output_schemas.R`). Model output is forced via
  `tool_choice={"type":"tool","name":...}`.
- System prompts: the original `analysis/strains/prompt`,
  `analysis/cell_lines/prompt`, and `analysis/cell_lines/rag_prompt` are
  used verbatim. The full strain list (for the strain task) or the top-K
  retrieved candidates (for the cell-line second pass) are appended to the
  system message.
- Prompt caching: for the strain task the system prompt (which contains the
  156-term ontology and is identical across experiments) is marked
  `cache_control: ephemeral`. With prompt caching ~95 % of the system
  prompt is served from cache on subsequent calls within the 5-minute TTL,
  reducing per-call cost by roughly 60 %.

## Strain annotation pipeline

For each experiment we issue a single Claude request containing:

1. the system prompt, terminating in the JSON-encoded list of 156 mouse
   strains;
2. a user message containing the experiment JSON described above.

The model is forced to emit a `report_strains` tool call whose `strains`
field is an array of `{value, URI, quote[]}`. An empty array denotes "no
strain identifiable from the provided ontology".

Script: `revisions/annotators/strain_annotate.py`. Parallel runner over a sample TSV:
`revisions/runners/run_sample.py`. Model selected via the `CLAUDE_MODEL` env var.

### Specificity-rule prompt variant

The strain prompt is also available in a variant that adds a single
sentence — *"Prefer the most general strain that is consistent with the
evidence; only return a specific sub-strain when the input explicitly
names the sub-strain or a vendor stock identifier"* — at
`revisions/strain_prompt_specificity.txt`. The variant addresses the
single dominant Claude failure mode (over-specification of `C57BL/6J`
where curators chose `C57BL/6`) and is evaluated separately.

## Cell-line annotation pipeline

We follow the two-stage retrieval-augmented protocol of the original submission.

**Step 1 — Open extraction.** Claude is presented with the experiment JSON
and asked to return a list of `{cell_line_name, description, quote}` triples
without seeing the ontology. The system prompt is the original
`analysis/cell_lines/prompt`.

**Step 2 — Embedding-based retrieval.** Each ontology term is represented
as the JSON-serialised `{ID, value, description, synonyms}` and embedded
once with OpenAI `text-embedding-3-large`. Each first-stage candidate is
embedded by serialising `{cell_line_name, description}`. Cosine similarity
yields the top-50 candidate ontology terms per first-stage extraction,
matching the 50-term ceiling used in the original submission.

**Step 3 — Constrained selection.** Claude is queried again with the
experiment JSON, the first-stage extraction, and the union of retrieved
candidates. The system prompt is the original
`analysis/cell_lines/rag_prompt`. The model emits a `report_cell_lines`
tool call returning `{cell_line_name, cell_line_ID}` pairs constrained to
the provided candidate list, or an empty array if none of the candidates
match. The structured-output handler in `cell_line_annotate.py` accepts
both native-object and JSON-string serialisations of the tool input — the
latter is occasionally emitted by Opus 4.7 — and parses both into the
canonical record format.

Script: `revisions/annotators/cell_line_annotate.py`. Parallel runner:
`revisions/runners/run_cell_line_sample.py`. The retrieval index is built by
`revisions/build/build_cell_line_index.py`.

## Evaluation

For each experiment we treat the predicted and curator-validated annotations
as sets of URIs (strain) or ontology IDs (cell line) and compute:

- **Exact match**: the predicted set equals the ground-truth set.
- **Any-overlap match** (the original submission's "basic match"): non-empty
  intersection between predicted and ground-truth sets.
- **Specificity / sensitivity**: a prediction is *specific* if it
  introduces no ground-truth-absent terms; *sensitive* if it omits no
  ground-truth-present terms.
- **Recall**, **precision** and **F1** computed per experiment, averaged
  across experiments. Experiments with no ground-truth terms are excluded
  from the recall average; experiments with no predicted terms from the
  precision average; experiments with both metrics defined and both zero
  are assigned F1 = 0.

The original GPT-4o predictions from `main_frame.rds` are evaluated
against the same ground-truth columns to serve as a within-study baseline.

### Cell-line scoring rules

The cell-line task is evaluated under three nested rules:

1. **Exact-ID match** — predicted URI set equals the Gemma `gemma_uri`
   set after canonicalisation (CURIE form, namespace-stripped; e.g. both
   `http://purl.obolibrary.org/obo/CLO_0003684` and `CLO:0003684`
   normalise to `CLO:0003684`).
2. **Cross-walk name match** — adds the cross-walk equivalence rule
   described above. A prediction is correct when its ID set, expanded
   through the cross-walk closure, contains every Gemma truth ID and
   only Gemma truth IDs.
3. **Curator inheritance** — when the prediction is cross-walk
   equivalent to the original GPT-4o prediction for the same GSE,
   inherit the original curator's per-experiment specific/sensitive
   verdict from `curation.tsv`. This reproduces the original-submission
   headline accuracy on the cell-line task without re-issuing curator
   reviews and isolates the residual experiments for which the model
   differs from both the truth set and the GPT-4o prediction (the
   `needs_review` bucket).

`revisions/metrics/cell_line_eval.py` implements rules 1 and 2;
`revisions/metrics/cell_line_inherit_curator.py` implements rule 3 and emits six
verdict buckets per GSE (auto_exact, auto_name, inherit_perfect,
inherit_wrong, gpt_exact_we_differ, needs_review).

### Targeted curator-overlay review (cell-line task, 2026-05-21)

To bound how much of the residual cross-walk error reflects scoring
gaps in the Gemma gold (vs. actual model error), we ran a targeted
manual review pass on 2026-05-21. One curator reviewed
**156 (GSE, predicted-URI) pairs across 58 GSEs**, drawn from the
union of Stage-1+2 system predictions on the 500-GSE cell-line
sample (GPT-4o, Sonnet 4.6, Opus 4.7) for cases where (a) at least
one system predicted a URI not cross-walk-equivalent
to `gemma_uri`, or (b) Gemma's gold was a single ambiguous term where
sibling lines in the same study could plausibly also be correct.

Verdicts (`correct`, `wrong`, `unsure`) were collected in
`revisions/curator_app/` (a small static web tool serving the
candidates from a JSON manifest) and exported as
`analysis/curator_verdicts_2026-05-21.json` (71 correct, 78 wrong, 7
unsure). The verdict file is keyed by `GSE::URI` pairs; the
"unsure" verdicts are treated as no-opinion in downstream scoring.

**Overlay scoring rule.** For each system prediction set `P` on
GSE `g`:

    overlay_correct(P, g)  :=
        ( any p ∈ P is in curator_correct[g] )
      OR
        ( any p ∈ P is cross-walk-equivalent to gemma_uri[g]
            AND p is not in curator_wrong[g] )

I.e., the curator-`correct` set expands the acceptable URIs for `g`;
the curator-`wrong` set rejects URIs the cross-walk rule would
otherwise have accepted. Outside the 58 reviewed GSEs the overlay
reduces to the original cross-walk rule, so the overlay numbers are
strict marginal additions to the published baseline.

In fig6 we draw the overlay credit as a lighter-shade extension
stacked on top of each system's published k_xw bar; published
baselines (and their Wilson CIs) are unchanged so the
"as-reported" numbers remain the visual anchor. MetaMuse-GPT-4o predictions were *not* in the candidate pool the
curator reviewed; MetaMuse is nonetheless rescored under the same
overlay rule (Sensitivity §"Targeted curator-overlay rescoring") for
completeness — its predictions are credited or rejected against the
same expanded gold built from the Stage-1+2-seeded review. Scripts:
`analysis/curator_overlay_score.py` (per-system rescoring on the
4 systems in `~/Data/metamuse-bench/scored/full500_per_gse.tsv`)
and `analysis/fig6_overlay_scores.py` (full 7-system table for
fig6, baseline pinned to the published k_exact / k_xw counts and
deltas computed against the curator-reviewed 58 GSEs only).

### Curator-review sheets

The `needs_review` bucket is serialised in two parallel formats for
curator inspection:

- **Per-GSE sheet** (`revisions/build/build_sonnet_curation_sheet.py` →
  `revisions/data/sonnet_curation_sheet.tsv`). One row per experiment,
  formatted to match the column schema of the original
  `curation.tsv` and extended with Opus 4.7's prediction plus an
  ensemble label (`A` = Opus matches Gemma; `B`/`C` = frontier
  consensus disagrees with Gemma; `D` = three-way disagreement; `E` =
  Opus extracted no cell line). Cross-walk-aware overlap summaries
  (`claude_vs_gemma`, `opus_vs_gemma`) report the count of Gemma
  annotations matched by each model.
- **Long-format sheet** (`revisions/build/build_long_curation_sheet.py` →
  `revisions/data/long_curation_sheet.tsv`). One row per (GSE,
  cross-walk-equivalence-class) pair, with check marks per source
  (Gemma / Claude / Opus / GPT-4o), the URI each source used, and a
  `picked_by` summary string. This expands the per-GSE residue to ~3
  rows per experiment and makes per-line agreement immediately
  visible.

## Sensitivity analyses

The replication setup is extended with five sensitivity analyses, each
holding the rest of the pipeline fixed.

### Specificity-rule prompt variant

The variant prompt described above is evaluated alongside the unmodified
baseline on the same 500-experiment strain sample. Script:
`revisions/analysis/test_specificity_gain.R`. The McNemar test is paired on the
binary correctness outcome.

### Inter-model agreement and ensemble (cell-line task)

`revisions/analysis/disagreement_analysis.py` produces the 2×2 correctness matrix
between Sonnet 4.6 and the original GPT-4o predictions and decomposes
the both-wrong cases into "models converge on the same wrong prediction"
versus "models pick different wrong predictions".

`revisions/analysis/ensemble_analysis.py` joins per-GSE predictions across
Sonnet 4.6, Opus 4.7, and the original GPT-4o predictions and reports:

- Pairwise McNemar (continuity-corrected) on the binary
  cross-walk-correctness outcome;
- Cohen's κ on that same outcome;
- Pairwise URI-set agreement under cross-walk equivalence;
- All-three-agree ensemble precision over coverage;
- 2-of-3 majority-vote precision over coverage.

The 2-of-3 majority rule predicts whichever URI set is matched under
cross-walk equivalence by ≥ 2 of the three models; it abstains when no
two models agree. Reported precision is the fraction of covered
experiments for which the majority verdict is correct under the same
cross-walk rule.

### Top-K retrieval sensitivity (cell-line task)

The cell-line second pass is re-run at K ∈ {10, 25, 50, 100, 200} on a
100-GSE subset (`revisions/analysis/topk_sensitivity.py`). First-pass
extractions and OpenAI embeddings are reused from the main Sonnet 4.6
run; only the candidate window and the second-pass prompt are
re-issued. Aggregation: `revisions/analysis/aggregate_topk.py`.

### Regex baseline (strain task)

The original-submission Methods describe the regex baseline only
qualitatively ("text-parsing produced a large number of erroneous
annotations") despite citing 6 % elsewhere. We restate it here with
explicit numbers. The baseline matches each of 156 strain terms
(EFO mouse-strain descendants + TGEMO) against the lower-cased GSE
metadata (sample characteristics, sample titles, study summary /
overall design, and paper abstract + methods when available). For
each strain, a per-term alternation
`((name)|(synonym₁)|(synonym₂)|…)` is built by `ogbox::regexMerge`,
synonyms of ≤3 characters are dropped, and the strains `NOR` and
`CAST/EiJ` are removed (their primary names produced uncontrolled
hits — `NOR` is a 3-letter English word; the slash in `CAST/EiJ`
turns its alternation noisy). The match is case-insensitive substring
(no word-boundary, no escaping), and *every* strain whose pattern
matches anywhere is emitted as a prediction; the top-1 is the first
hit by file order. Code: `R/regex.R::find_and_quote`,
`analysis/strains/01.ask_regex.R`. On the 500-GSE sample, the regex
baseline recovers the correct ontology term **in the candidate set
in 79.2 % of GSEs** (any-overlap) but selects it as the **top-1 in
only 6.4 %** (exact match) — the 73 pp gap quantifies what
"erroneous annotations" meant in the original Methods, and the
6.4 % is the same number that surfaced as "6 %" in the original
Results. The headroom between the two is the contribution available
to a contextual model that performs the same lexical lookup and then
adjudicates between co-occurring mentions.

### text2term TFIDF baseline (strain task)

text2term (Wachtler et al., TFIDF mapper) is run against the
pre-fix 156-term strain dictionary on the full 500-GSE strain sample
(text2term was not re-run after the meta-term fix; it selected a
meta-term in only 3 / 500 GSEs, so the impact on its 5.4 % headline
is < 1 pp)
(`revisions/baselines/text2term_baseline.py`). The dictionary is synthesised as
a minimal OWL document (`oboInOwl:hasExactSynonym` + `rdfs:label`) so
that text2term's `owlready2` back-end can parse it. Candidate strings
are extracted from the same fields the paper's regex baseline searches
(characteristics, study summary, paper abstract / methods).
Predictions are retained at TFIDF similarity ≥ 0.4 with at most one
mapping per candidate string (per-mention top-1). The threshold was
swept across {0.65, 0.5, 0.4}; 0.4 is the empirical sweet spot —
above 0.5, true substring matches like
`background strain: C57BL/6` → `C57BL/6` (0.487) drop out, while
below 0.4 the false-positive rate dominates. See
`SENSITIVITY_ANALYSES.md` for the sweep numbers.

### SapBERT neural baseline

SapBERT (Liu et al. 2021, *NAACL*; canonical checkpoint
`cambridgeltl/SapBERT-from-PubMedBERT-fulltext`) is run on both the
strain and cell-line tasks as a non-LLM neural reference. Every
ontology term in the task-specific dictionary — its name and every
synonym — is mean-pooled from the model's last hidden state,
L2-normalized, and cached: 156 terms (post-fix; see *Strain task /
Ontologies*) for the strain dictionary, 46,032 terms for the cell-line
dictionary.

**Candidate construction.** For each experiment we assemble a set of
candidate input strings:

- each sample-level `characteristic` value enters as one candidate
  string (most of these are short — `"strain: C57BL/6J"`, `"cell line:
  HeLa"` — and contain the strain or cell-line directly);
- the study summary, overall design, and (when available) the paper
  title enter as single candidates;
- the per-sample protocol, paper abstract and paper methods are
  *split at sentence boundaries* (regex `(?<=[.!?])\s+`, each sentence
  trimmed to 300 characters) so that SapBERT sees focused mentions
  ("the C57BL/6J mice were…") rather than 1 000-character paragraphs.

The candidate set is capped at 80 strings per experiment. Every
candidate is embedded in a single batched forward pass; the cosine
top-1 dictionary term for each candidate is retained if its score
exceeds 0.85, and the GSE's prediction is the union of accepted terms
across all candidates. The 0.85 cutoff is the standard SapBERT
high-confidence threshold reported by the original paper.

Scripts: `revisions/baselines/sapbert_baseline.py` (both tasks; the
`--task` flag selects the dictionary), `revisions/baselines/sapbert_eval_cellline.py`
(cell-line cross-walk-aware rescoring).

> The sentence-boundary split was previously implemented as
> `re.split(r"(?<=[.!?])\\s+", …)` — an extra backslash that made the
> regex match the literal string `\s+` instead of whitespace, so the
> splitter never fired and SapBERT saw only the first 300 characters
> of each free-text field as a single blob. The bug was caught in the
> revision-round audit and fixed; the post-fix numbers reported here
> are 293/500 = 58.6 % strain exact-match and 58/498 = 11.6 %
> cell-line exact-ID (vs. pre-fix 296/500 = 59.2 % and 61/498 = 12.2 %
> — well within Wilson noise on either task).

### BM25 standalone baseline (strain task)

BM25 (Robertson & Walker, TREC-3, 1994; `rank_bm25.BM25Okapi`) is run
as a probabilistic IR scoring method that refines the TFIDF family
text2term implements. Both algorithms predate the LLMs we compare
against here; BM25 is included as a sparse-retrieval reference because
it remains the standard probabilistic baseline in modern IR systems
(including the BM25 channel of our hybrid retriever in the cell-line
task). The index is built once over each ontology
term's `value + synonyms` (description excluded; BM25 length
normalisation amplifies long descriptions unfairly); tokenisation is
case-folded alphanumeric runs. Per-experiment, the GSE's full input
document (overall_design + summary + per-sample characteristics +
paper text when available) is tokenised the same way and scored
against all 154 terms (corrected post-fix dictionary; see Strain task
Ontologies); we take the top-1. Script:
`revisions/baselines/bm25_strain_baseline.py`. Multi-mapping variants
were swept (top-K with relative score thresholds) and uniformly
underperformed top-1 on the exact-match metric.

### Hybrid dense + sparse retrieval (cell-line task)

The cell-line Stage 2 candidate set is also evaluated under a hybrid
retriever combining the existing dense (OpenAI
`text-embedding-3-large`) and a BM25 sparse channel via reciprocal
rank fusion. Each channel returns its top-200 per query; the fused
score for an id is the sum of `1 / (60 + rank_in_that_retriever)`
across the two channels (a candidate that does not make either
retriever's top-200 contributes 0). The top-50 by fused score is then
handed to the Stage-2 LLM. The Stage-1 extractions, the Stage-2
prompt, model parameters, and the evaluation rule are unchanged;
only the candidate set differs. Run on three frontier LLMs (Sonnet
4.6, Opus 4.7, and a faithful GPT-4o re-run via the OpenAI
Batch API). Scripts: `revisions/build/build_bm25_index.py` (one-off
BM25 index build), `revisions/annotators/hybrid_retrieval.py`
(retrieval primitive), `revisions/runners/run_hybrid_cell_line.py`
(Claude Stage-2 reruns), `revisions/runners/gpt4o_cell_line_hybrid.py`
(GPT-4o Stage-2 reruns).

### GPT-4o + specificity-rule re-run

To control for the prompt-vs-model confound in the original §5 result —
which compared *specificity-prompted Sonnet 4.6* against *baseline-prompted
GPT-4o* — we re-ran GPT-4o on the same 500-GSE sample using both the
baseline and specificity-rule prompts. Inference uses
`gpt-4o-2024-11-20` with the original submission's exact generation
parameters: `temperature=0`, `seed=1`, `top_p=1`, `max_tokens=1024`, and
structured output via `response_format = {"type":"json_schema", "strict":
true, "name":"mouse_strain"}` (the same JSON schema the original
submission submits via `inst/gpt.py::ask_gpt`, transliterated to native
Python in `revisions/runners/gpt4o_batch.py::STRAIN_RESPONSE_FORMAT`).
Submission goes through the **OpenAI Batch API** (`/v1/chat/completions`
endpoint, 24-hour completion window, 50 % discounted pricing), matching
the original submission's channel as well as its generation parameters.

Because OpenAI's tier-2 batch enqueue cap is ~1.35 M tokens, the 500
requests (≈11 M tokens total) are split into 11 chunks of ~50 GSEs
each (~1 M tokens per chunk, leaving safety headroom). The chunked
submitter (`revisions/runners/gpt4o_batch.py`) bin-packs greedily,
auto-resubmits any chunk that OpenAI rejects for enqueue-cap reasons
once previous chunks complete, and stitches the per-chunk JSONL
outputs back into per-GSE result files identical in shape to the
real-time Claude / Llama runs.

A 100-GSE *baseline-prompt* sanity check against the predictions
in `main_frame.rds` confirms setup-equivalence: 83 %
of GSEs produced URI-identical predictions, 87 % produced the same
correct/wrong verdict against ground truth, and the residual
disagreements were symmetric (7 GSEs where our re-run is closer to
truth, 6 where the original prediction is closer). Our exact-match
accuracy on the 100-GSE subset is 69 % vs the original 72 % — well
within the Wilson 95 % CI (60 %–77 %) and consistent with the
run-to-run stochasticity OpenAI documents for `temperature=0` +
`seed=1` calls on `gpt-4o-2024-11-20`.

### Open-weights baseline (strain task)

We additionally run **Llama 3.3 70B Instruct** as an open-weights
baseline on the same 500-GSE strain sample. Inference uses
`meta-llama/Llama-3.3-70B-Instruct-Turbo` served through Together AI's
OpenAI-compatible HTTP API (`https://api.together.xyz/v1`), with the
official `openai` Python SDK (v2.36) pointed at that base URL. The
Anthropic-shaped `report_strains` tool is translated to OpenAI
function-calling shape (`type=function`, identical JSON schema in
`parameters`); the request uses `tool_choice={"type":"function",
"function":{"name":"report_strains"}}` to force a single tool call.
Inference settings are otherwise identical to the Claude runs:
`max_tokens=2048`, `temperature=0`, system prompt and 156-term strain
list verbatim. Authentication resolves `TOGETHERAI_API_KEY` from the
macOS Keychain. The runner (`revisions/annotators/strain_annotate_open.py` +
`revisions/runners/run_open_sample.py`) supports presets for OpenRouter, Groq,
Fireworks, and DeepSeek and accepts an `OPEN_BASE_URL` override for
arbitrary endpoints (e.g. local Ollama / vLLM) so the same code can be
repointed at any OpenAI-compatible deployment without modification. We
evaluate the model under the strain baseline prompt and the
specificity-rule variant on the same 500-GSE sample; two of 500
specprompt requests returned a transient `xgrammar` tool-decoding
422 error on the first pass and were re-issued successfully, so all
n=500 are reported.

### Effect of associated publication on annotation performance

To quantify the contribution of the linked publication to annotation
accuracy, we ran a within-experiment A/B on
the 291 strain-task GSEs for which a PubMed-Central paper was
retrievable and its `{title, abstract, methods}` fields were
populated. The same GPT-4o + specificity-rule setup (model
`gpt-4o-2024-11-20`, `temperature=0`, `seed=1`, `top_p=1`,
`max_tokens=1024`, `response_format` = `mouse_strain` JSON schema)
was issued twice per experiment: once with the default input
(GEO metadata + the paper's title, abstract, and Methods section
concatenated as a single `papers` field in the user message), and
once with the `papers` field omitted. Submission via the OpenAI
Batch API in 6 chunks under the tier-2 enqueued-token cap
(`revisions/runners/gpt4o_batch.py submit --strip-papers --suffix
specprompt_nopaper`). Pairing is at the GSE level. The paired test
is McNemar with Edwards continuity correction on the binary
correctness outcome.

### MetaMuse architecture benchmark (cell-line task)

We benchmark MetaMuse (Mittal et al. 2026), a concurrent agentic GEO
metadata curation system that performs per-sample LLM curation +
SapBERT/FAISS-anchored ontology normalisation + LLM arbitrator.

**Sample.** The full 500-GSE cell-line task sample, stratified by
membership in the ARCHS4 cohort: 199 ARCHS4 GSEs, 299 non-ARCHS4
(microarrays, older bulk RNA-seq, single-cell). MetaMuse processes
individual GSMs (samples), not GSEs; we cap at 3 random GSMs per GSE
(seed `20260520`, sampled from cached GEO SOFT files), yielding
1,500 GSMs total. Per-GSE predictions are constructed by set-union
over the 3 GSMs' `cell_line_normalized_id` outputs (dropping
`None reported` and `No term found`).

**Software.** MetaMuse v0.1 (`Ekansh-Mittal1/MetaMuse`, `main`
branch, 2026-05-20) with four patches:

1. Auth + base-URL auto-selection by destination
   (`src/metamuse_cli.py`).
2. Outbound-body sanitiser stripping OpenRouter-only extras
   (`reasoning_effort`, `provider.order`, `verbosity`) that
   OpenAI direct rejects with HTTP 400 (custom
   `httpx.AsyncBaseTransport`).
3. Per-stage model providers honour `$METAMUSE_MODEL` rather than
   hard-coded Gemini routes (`src/workflows/batch_targets.py`,
   `batch_samples_efficient.py`).
4. `TARGET_FIELD_CONFIG` patched to curate + normalise only the
   `cell_line` field, omitting MetaMuse's default 5-field schema
   (disease, organ, cell_type, treatment, cell_line).

**LLM.** GPT-4o (`gpt-4o-2024-11-20`, OpenAI direct, $2.50 in /
$10 out per M tokens). Claude Sonnet 4.6 via Anthropic's
OpenAI-compatible endpoint returns a server-side "compiled grammar
is too large" error on MetaMuse's combined-field normalizer schema
and was not benchmarked.

**Inference parameters.** `conditional_mode=eval`,
`max_iterations=2`, `batch_size=10`, `max_workers=3`.

**Scoring.** Identical to the rest of the cell-line task: exact-ID
match, cross-walk match via `cell_line_eval.py::CellLineXrefIndex`,
any-overlap, and within-coverage precision (defined as cross-walk
precision computed over the subset of GSEs on which MetaMuse
returned a non-null prediction).

**LLM-call counts.** Per GSM, MetaMuse issues ~5 LLM calls
(sample-type curator + cell_line curator + arbitrator iter 1–2 +
normalizer); each call carries ~10–30 k input tokens. At 3 GSMs/GSE
this is ~15 calls/GSE. The cell500 sample contains 13,174 GSMs
total (mean 26.5, median 16, max 480 per GSE); at full per-sample
coverage MetaMuse would issue ~66,000 LLM calls over the 500 GSEs.
The Stage-1+2 pipeline issues 2 LLM calls per GSE regardless of
sample count (1,000 calls over the 500 GSEs). MetaMuse uses
synchronous streaming chat completions and cannot use the OpenAI
Batch API (the agentic loop is sequential per GSM: curator output
feeds arbitrator, which conditionally re-curates).

### Quote-verifier pass (strain task)

Every strain prediction carries a `quote: list[str]` field in which
the model copies the input span(s) that motivated the call. To
quantify how often those supporting quotes are actually grounded in
the input — as opposed to paraphrased or fabricated — we run a
post-hoc verifier
(`revisions/analysis/verify_quotes_strain.py`) over every per-GSE
prediction JSON produced on the 500-GSE strain sample by the three
Claude runs (Sonnet 4.6, Opus 4.7, Haiku 4.5 — files under
`revisions/data/results/<model_id>/<GSE>.json`), plus the original
GPT-4o predictions for the same 500 GSEs exported from
`main_frame.rds` as a TSV of `(shortName, gpt_quote)` pairs. All
four models are verified against identical inputs, so the rates are
directly comparable across models.

For each GSE the verifier rebuilds the exact input the model was
given by re-invoking `geo_fetch.build_input` and concatenating the
`summary`, `overall_design`, per-sample `{title, characteristics,
protocol}`, and per-paper `{title, abstract, methods}` fields. The
concatenated string is case-folded and whitespace-collapsed to form
a verbatim haystack; a second, more permissive haystack additionally
strips every non-alphanumeric character (collapsing en-dash vs
hyphen, terminal-period insertions, multi-line joining, and
capitalisation into a single equivalence class). Each emitted quote
is then checked against both haystacks:

- **Strict** — case-folded, whitespace-collapsed substring match.
- **Normalised** — alphanumeric-only substring match.

We report two rates per model: the per-quote verification rate and
the per-prediction fully-grounded rate (a prediction is "fully
grounded" iff *every* one of its quotes verifies). The strict /
normalised gap measures how much of the unverified bucket is pure
character-set normalisation versus paraphrase or fabrication.

The same procedure is applied to the original GPT-4o predictions
after splitting `main_frame.rds`'s quote-list serialisation on its
`❗` (within-strain) and `❕` (between-strains) separators so each
fragment is checked as one verbatim quote.

### Opus 4.7 inference-time noise

Opus 4.7 does not accept a `temperature` parameter. To quantify
inference-time stochasticity we re-run Opus 4.7 on a 20-GSE strain
subsample (`revisions/build/sample_for_noise.R`, seed 13579) and report the
run-to-run identical-prediction rate plus paired exact-match accuracy.
Sonnet 4.6 and Haiku 4.5 are run at `temperature = 0`; we do not
separately estimate their stochasticity as their deterministic-decoding
behaviour is empirically stable on the same 20-GSE subsample (no
observed disagreement).

## Statistical analysis

All statistical analyses for the strain task are implemented in
`revisions/analysis/analyze_strain_results.R`. The script reads the per-model
`summary.tsv` files written by `run_sample.py` and emits eight machine-
readable tables (`revisions/data/analysis/01_..._.tsv` through
`08_..._.tsv`) covering Wilson 95 % score intervals, paired McNemar tests,
Cohen's κ, error overlap, and the specificity-rule effect. Cell-line
disagreement and ensemble statistics are emitted by
`disagreement_analysis.py` and `ensemble_analysis.py`.

**Pairing.** Every model is evaluated on the *same* sample. Cross-model
comparisons are paired on the binary correctness outcome per experiment.

**Confidence intervals.** Wilson 95 % score intervals (Wilson 1927;
Agresti & Coull 1998). The Wald (normal-approximation) interval
under-covers at small n and produces nonsensical bounds near 0 or 1;
Wilson has near-nominal coverage across the full range of p and n we
encounter.

**Pairwise comparisons.** Marginal homogeneity of the paired-correctness
outcome is tested with McNemar's test (McNemar 1947) with Edwards
continuity correction. McNemar is the paired-binary analogue of a
two-proportion test.

**Multiple comparisons.** We compute six pairwise McNemar tests (`{Opus,
Sonnet, GPT-4o, Haiku}` taken two at a time on the strain task; three
pairs on the cell-line task) plus three tests for the specificity-rule
analysis. We report unadjusted p-values. The substantive conclusions
hold under a Bonferroni adjustment over all nine strain-task tests.
Wilson intervals are not adjusted for multiplicity because they
describe single-model accuracy, not contrasts.

**Inter-model agreement.** Raw percent agreement on the correct/wrong
outcome plus Cohen's κ (Cohen 1960), which corrects for chance
agreement. We additionally report the percentage of experiments on
which two models produced *identical* predicted URI sets (a stricter
criterion than matching correct/wrong outcomes).

**Recall, precision, F1.** Per experiment with predicted and
ground-truth URI sets, then averaged. Experiments with `TP + FN = 0`
are omitted from the recall average; experiments with `TP + FP = 0`
from the precision average; experiments with both metrics defined and
both zero are assigned F1 = 0.

## Sampling

We drew a uniform random sample of 500 experiments from the 6,054
strain-curated rows and 500 experiments from the 3,383 cell-line-curated
rows (seed `20260513`, scripts `revisions/build/sample_gse.R` and
`revisions/build/sample_cell_lines.R`). All Claude models were evaluated on
identical samples to eliminate sampling variance from the comparison.
Sample size was chosen as a trade-off between statistical resolution and
API spend; at n = 500 a 10-percentage-point difference in exact-match
rate is statistically distinguishable from no difference at α = 0.05.

## Software and reproducibility

The replication code is contained in the `revisions/` directory. Python
dependencies are pinned in `revisions/.venv` (Python 3.9.13;
`anthropic` 0.102.0, `openai`, `obonet` 1.1.1, `rdflib` 7.6.0,
`networkx`, `text2term`). Per-experiment model responses are saved as
JSON in `revisions/data/results/<model_id>/<GSE>.json` (strain) and
`revisions/data/results_cl/<model_id>/<GSE>.json` (cell line) and
aggregated into per-model `summary.tsv` files. The random samples and
the strain ontology snapshot are committed alongside the code so that
the entire downstream evaluation is reproducible without re-issuing API
calls. The cell-line ontology, embedding index, and OBO dumps are
regenerable via the `build_*` scripts and are excluded from version
control.
