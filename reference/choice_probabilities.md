# Choice probabilities for each test item, with human response counts

For each test item, returns the alternatives offered, the correct
object, the model's probability of choosing each alternative, and how
many participants chose each one. This is the full choice distribution
behind the accuracy that
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
scores: the probability of the correct alternative is exactly that
item's entry in `perf`. It is what a multinomial likelihood
(`xslControl(objective = "multinomial")`) compares with the data, and it
shows which wrong objects a model expects learners to choose.

## Usage

``` r
choice_probabilities(m, data, d = NULL, lapse = 0)
```

## Arguments

- m:

  A word-by-object matrix, e.g. `xsl_run(mod, data)$fits[[1]]$matrix`.

- data:

  The `xslData` condition `m` was learned from.

- d:

  Optional choice exponent (a model's `ch_dec`; see
  [`get_perf()`](https://www.kachergis.com/XSLmodels/reference/get_perf.md)).

- lapse:

  Lapse rate: the probability of guessing uniformly among the
  alternatives, mixed into `p` (0, the default, leaves `p` as the
  model's choice rule gives it).

## Value

A tibble with one row per test item: `item`, `correct` (the correct
object's label), and list columns `alternatives` (object labels), `p`
(choice probabilities over `alternatives`) and `counts` (participants
choosing each alternative; `NULL` entries if `data` has no
`response_matrix`).

## Details

Items are scored as in
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md):
on a condition with a `test`, by Luce choice over each test trial's
listed objects (see
[`mafc_test()`](https://www.kachergis.com/XSLmodels/reference/mafc_test.md));
otherwise among all objects, with word i's correct referent the i-th
object (see
[`get_perf()`](https://www.kachergis.com/XSLmodels/reference/get_perf.md)).

## Examples

``` r
dat <- xsl_datasets[[1]]
fit <- xsl_run(uncfam(X = .1, B = 3, C = .97), dat)$fits[[1]]
ch <- choice_probabilities(fit$matrix, dat)
ch$p[[1]]       # the model's choice distribution for item 1
#>  [1] 0.42412064 0.03117284 0.05779498 0.03232234 0.03281867 0.01819889
#>  [7] 0.01876175 0.05414666 0.03658513 0.04601400 0.03447850 0.03281867
#> [13] 0.03848299 0.02629217 0.01819889 0.03374124 0.02055692 0.04349470
ch$counts[[1]]  # what participants chose
#>  [1] 7 3 2 0 0 0 2 0 0 0 1 1 1 0 2 3 2 1
```
