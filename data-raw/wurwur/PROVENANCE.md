# Vendored corpus source files

These files are the immediate source for the `rollins_corpus` and `fm_corpus`
package datasets, built by `../add_wurwur_corpora.R`. They were taken from the
**wurwur** package (<https://github.com/mcfrank/wurwur>, MIT-licensed), a
modern reimplementation of the Frank, Goodman & Tenenbaum (2009) word-learning
model.

| file(s) | from wurwur | underlying dataset |
|---|---|---|
| `rollins_corpus.csv`, `rollins_gold.csv` | exported from `matlab_code/data/{world,corpus,gold_standard}.mat` | CHILDES / Rollins (2003) corpus, as used by Frank, Goodman & Tenenbaum (2009) |
| `FMcorpus/*.csv` | `data/FMcorpus/*.csv` (verbatim) | Frank, Tenenbaum & Fernald corpus (Frank, Tenenbaum & Fernald, 2013) |
| `fm_gold_curated.tsv` | `fm_corpus_out/gold_curated.tsv` | hand-curated gold lexicon for the FM corpus |
| `fm_gold_strict.tsv`, `fm_gold_permissive.tsv` | `fm_corpus_out/gold_{strict,permissive}.tsv` | gold lexicons auto-derived from the coded intentions at tighter / looser thresholds |

`rollins_corpus.csv` columns: `utt_num`, `words` (space-separated tokens of the
utterance), `objects` (space-separated objects present in the scene).

The FM CSVs also carry `objects.referred` (the coded referential intention,
imported as `fm_corpus$intents`) and gaze/hand attentional-cue columns
(`mom.eyes` etc., **not** imported).

## Citations

- Frank, M. C., Goodman, N. D., & Tenenbaum, J. B. (2009). Using speakers'
  referential intentions to model early cross-situational word learning.
  *Psychological Science, 20*(5), 578-585.
- Frank, M. C., Tenenbaum, J. B., & Fernald, A. (2013). Social and discourse
  contributions to the determination of reference in cross-situational word
  learning. *Language Learning and Development, 9*(1), 1-24.
