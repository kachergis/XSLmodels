# Evaluate m-alternative forced choice test

Scores a set of test trials against a model's word-object matrix,
returning the probability of choosing the correct object on each trial.
Each trial presents one word and a set of candidate referents; the
correct object is the one whose id matches the word's id (the package's
diagonal convention), unless the test gives an explicit `correct` object
for that trial (e.g. to test a word's association with a second,
co-occurring referent, with its own referent absent). A thin wrapper
over
[`predict_referent()`](https://www.kachergis.com/XSLmodels/reference/predict_referent.md).

## Usage

``` r
mafc_test(m, test, ...)
```

## Arguments

- m:

  A matrix representing model memory with words as rows and referents as
  columns.

- test:

  A list representing the test trials: `words` (one word per trial),
  `objects` (the candidate referents on each trial), and optionally
  `correct` (the correct referent on each trial; defaults to the word's
  own id).

- ...:

  Further arguments passed to
  [`predict_referent()`](https://www.kachergis.com/XSLmodels/reference/predict_referent.md)
  (e.g. `pragmatic`, `threshold`).

## Value

A vector with the probability of choosing the correct object, given each
word.

## Examples

``` r
dat <- xsl_datasets[[10]]
x <- xsl_run(baseline(), dat)
mat <- x$fits[[1]]$matrix
mafc_test(mat, dat$test)
#>  [1] 0.6666667 0.8571429 0.7500000 0.6000000 0.8571429 0.6666667 0.8571429
#>  [8] 0.6000000 0.8571429 0.6666667 0.6666667 0.5454545 0.8571429 0.8571429
#> [15] 0.6000000 0.6000000 0.6000000 0.6666667
```
