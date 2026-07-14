# Calculate F-score, precision, recall, and specificity for a knowledge matrix at a given threshold

This function calculates the F-score, precision, recall, and specificity
for a given knowledge matrix at a specified threshold. It uses the
concept of true positives, false positives, and false negatives,
determined from the knowledge matrix and an optional gold lexicon. The
function is useful for evaluating the performance of a model in terms of
its ability to correctly identify associations between words and
referents.

## Usage

``` r
get_fscore(m, threshold, gold_lexicon = NULL)
```

## Arguments

- m:

  A matrix representing the knowledge matrix with words as rows and
  referents as columns.

- threshold:

  A numeric value representing the threshold for considering an
  association between a word and a referent as positive.

- gold_lexicon:

  Optional; a data frame or list where each row/element represents a
  word-object pair in the gold lexicon. If provided, it is used to
  calculate true positives, false positives, and false negatives.

## Value

A tibble with columns for threshold, precision, recall, specificity, and
F-score.

## Examples

``` r
dat <- xsl_datasets[[1]]
x <- xsl_run(baseline(), dat)
mat <- x$fits[[1]]$matrix
lex <- list(words = rep(1:18), objects = rep(1:18))
get_fscore(mat, 0.5, lex)
#> # A tibble: 1 × 5
#>   threshold precision recall fscore specificity
#>       <dbl>     <dbl>  <dbl>  <dbl>       <dbl>
#> 1       0.5     0.075      1  0.140       0.275
```
