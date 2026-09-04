# Evaluate m-alternative forced choice test

Scores a set of test trials against a model's word-object matrix,
returning the probability of choosing the correct object on each trial.
Each trial presents one word and a set of candidate referents; the
correct object is the one whose id matches the word's id (the package's
diagonal convention). A thin wrapper over
[`predict_referent()`](https://kachergis.github.io/XSLmodels/reference/predict_referent.md).

## Usage

``` r
mafc_test(m, test, ...)
```

## Arguments

- m:

  A matrix representing model memory with words as rows and referents as
  columns.

- test:

  A list representing the test trials, each containing a word and its
  associated referents.

- ...:

  Further arguments passed to
  [`predict_referent()`](https://kachergis.github.io/XSLmodels/reference/predict_referent.md)
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
