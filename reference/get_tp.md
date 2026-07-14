# Get true positives (TP), given a knowledge matrix and a gold-standard lexicon

This function iterates over words in a given gold lexicon and
accumulates the associative strength (can be integral e.g. 1, or
real-valued) in the knowledge matrix for the intended referents (present
in the gold lexicon). Returns the number of expected true positives (TP)
for this gold lexicon and knowledge matrix.

## Usage

``` r
get_tp(m, gold_lexicon)
```

## Arguments

- m:

  A matrix representing the knowledge matrix with words as rows and
  referents as columns.

- gold_lexicon:

  A data frame or list where each row/element represents a word-object
  pair in the gold lexicon.

## Value

A single value with the expected number of true positives.

## Examples

``` r
dat <- xsl_datasets[[10]]
x <- xsl_run(baseline(), dat)
mat <- x$fits[[1]]$matrix
lex <- list(words = rep(1:18), objects = rep(1:18))
get_tp(mat, lex)
#> [1] 108
```
