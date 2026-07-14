# Evaluate m-alternative forced choice test

This function evaluates a given set of test trials using the provided
model memory matrix (word x referent). Each test trial is assumed to
present one word and a set of referents of size less than the width of
the model memory matrix.

## Usage

``` r
mafc_test(m, test)
```

## Arguments

- m:

  A matrix representing model memory with words as rows and referents as
  columns.

- test:

  A list representing the test trials, each containing a word and its
  associated referents.

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
