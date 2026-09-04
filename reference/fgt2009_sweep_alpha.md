# Sweep the lexicon-size prior alpha for an FGT model

[`fgt2009()`](https://kachergis.github.io/XSLmodels/reference/fgt2009.md)'s
`alpha` trades lexicon size against fit and must scale with corpus size,
and the fit-vs-`alpha` curve is single-peaked – so a coarse sweep is the
right way to choose it (and to evaluate the model fairly), rather than a
general-purpose optimizer. This runs
[`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md)
for each `alpha` and returns the per-value SSE against the dataset's
human accuracy.

## Usage

``` r
fgt2009_sweep_alpha(
  data,
  alphas,
  rsa = FALSE,
  gold = NULL,
  ...,
  control = xslControl()
)
```

## Arguments

- data:

  An `xslData` object (or list of them).

- alphas:

  Numeric vector of `alpha` values to try.

- rsa:

  Logical; if `TRUE`, sweep
  [`fgt2009_rsa()`](https://kachergis.github.io/XSLmodels/reference/fgt2009_rsa.md)
  instead of
  [`fgt2009()`](https://kachergis.github.io/XSLmodels/reference/fgt2009.md).

- gold:

  Optional gold-standard lexicon as `list(words, objects)`. When given,
  the result gains an `f_max` column (best-threshold F-score against
  `gold`). `data` must be a single `xslData` in this case.

- ...:

  Further arguments to the model constructor (e.g. `gamma`, `kappa`,
  sampler controls).

- control:

  Control arguments passed to
  [`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md).

## Value

A data frame with columns `alpha` and `sse` (plus `unweighted_sse` when
`data` is a list, or `f_max` when `gold` is given).

## Details

For a naturalistic corpus (e.g.
[rollins_corpus](https://kachergis.github.io/XSLmodels/reference/rollins_corpus.md),
[fm_corpus](https://kachergis.github.io/XSLmodels/reference/fm_corpus.md))
there is no human accuracy vector; pass `gold` (a `list(words, objects)`
lexicon) to also get the best-threshold F-score of the learned matrix
against it (via
[`get_roc_max()`](https://kachergis.github.io/XSLmodels/reference/get_roc.md))
at each `alpha`. Remember such corpora also want the naturalistic-speech
`gamma = 0.1`, `kappa = 0.05` rather than the defaults (see
[`fgt2009()`](https://kachergis.github.io/XSLmodels/reference/fgt2009.md)).

## Examples

``` r
fgt2009_sweep_alpha(get_example_ambiguous_condition(),
                    alphas = c(1, 2, 4, 8))
#>   alpha          sse
#> 1     1 0.0002831451
#> 2     2 0.0238093922
#> 3     4 0.0132282220
#> 4     8 0.2031250000
```
