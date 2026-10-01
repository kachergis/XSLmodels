# Calculate Luce choice (proportion correct) for each item in a model knowledge matrix

This function computes the Luce choice, or the proportion of correct
selections, for each item in a given model knowledge matrix. It assesses
the probability of correctly identifying each referent based on the
knowledge matrix, providing a measure of model performance per item.

## Usage

``` r
get_perf(m, d = NULL)
```

## Arguments

- m:

  A square matrix representing the model's knowledge, where rows
  correspond to words and columns correspond to referents. The diagonal
  elements represent correct associations, and off-diagonal elements
  represent incorrect associations.

- d:

  Exponent for exponentiated choice rule

## Value

A named numeric vector where each element corresponds to an item in the
matrix. The value of each element represents the proportion of correct
selections (Luce choice) for that item, calculated as the ratio of the
correct association (diagonal element) to the total associations for
that item.

## Details

Negative associations (possible in error-driven models such as
[`rescorla_wagner()`](https://www.kachergis.com/XSLmodels/reference/rescorla_wagner.md)
and
[`kalman_filter()`](https://www.kachergis.com/XSLmodels/reference/kalman_filter.md))
are treated as 0 before the choice rule is applied – Luce choice is
undefined for negative values, and would otherwise yield "probabilities"
outside \[0, 1\] – matching
[`predict_referent()`](https://www.kachergis.com/XSLmodels/reference/predict_referent.md).
A word whose associations are then all 0 is answered at chance
(`1 / ncol(m)`).

## Examples

``` r
x <- xsl_run(baseline(), get_example_ambiguous_condition())
mat <- x$fits[[1]]$matrix
get_perf(mat)
#>   1   2   3   4 
#> 0.5 0.5 0.5 0.5 
```
