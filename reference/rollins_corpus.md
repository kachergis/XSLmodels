# CHILDES/Rollins naturalistic word-learning corpus (Frank et al. 2009)

The corpus the intentional Bayesian model of Frank, Goodman & Tenenbaum
(2009) was fit to: 619 mother-to-infant utterances from the Rollins
corpus in CHILDES, each paired with the set of objects present in the
scene (six toys rotated in groups). 416 word types, 22 object types.

## Usage

``` r
rollins_corpus
```

## Format

An object of class `list` of length 3.

## Source

Frank, M. C., Goodman, N. D., & Tenenbaum, J. B. (2009). Using speakers'
referential intentions to model early cross-situational word learning.
*Psychological Science, 20*(5), 578-585. Corpus originally from Rollins
(2003), CHILDES.

## Details

A list with:

- `data`:

  an
  [xslData](https://kachergis.github.io/XSLmodels/reference/xslData-class.md)
  object with the 619 training utterances (`data$train$words[[t]]` /
  `data$train$objects[[t]]` are character vectors). No `accuracy` or
  `test` – there is no human referent-selection data for this corpus.

- `gold`:

  the gold-standard lexicon as `list(words, objects)` (34 word-object
  pairs), for scoring a learned matrix with
  [`get_fscore()`](https://kachergis.github.io/XSLmodels/reference/get_fscore.md),
  [`get_roc()`](https://kachergis.github.io/XSLmodels/reference/get_roc.md),
  [`get_roc_max()`](https://kachergis.github.io/XSLmodels/reference/get_roc.md),
  or
  [`get_tp()`](https://kachergis.github.io/XSLmodels/reference/get_tp.md).

- `reference`:

  the citation string.

Not part of
[xsl_datasets](https://kachergis.github.io/XSLmodels/reference/xsl_datasets.md)
(which is scored by SSE against a human accuracy vector these corpora
don't have). Imported from the wurwur package
(<https://github.com/mcfrank/wurwur>); see
`data-raw/add_wurwur_corpora.R`.

## Examples

``` r
m <- suppressWarnings(
  xsl_run(baseline(), rollins_corpus$data)$fits[[1]]$matrix)
get_roc_max(m, gold_lexicon = rollins_corpus$gold)
#> [1] 0.3768116
```
