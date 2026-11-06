# Softmax choice weights

Transforms a word-by-object association matrix so that Luce choice over
it (as in
[`get_perf()`](https://www.kachergis.com/XSLmodels/reference/get_perf.md),
[`mafc_test()`](https://www.kachergis.com/XSLmodels/reference/mafc_test.md)
and
[`choice_probabilities()`](https://www.kachergis.com/XSLmodels/reference/choice_probabilities.md))
is a softmax choice rule with inverse temperature `phi`: a word's
probability of choosing object `o` among a set of alternatives is
`exp(phi * m[w, o]) / sum(exp(phi * m[w, alternatives]))`. Unlike Luce
choice, softmax is defined for negative associations (which error-driven
models such as
[`rescorla_wagner()`](https://www.kachergis.com/XSLmodels/reference/rescorla_wagner.md)
and
[`kalman_filter()`](https://www.kachergis.com/XSLmodels/reference/kalman_filter.md)
can learn) and gives objects a word has never been paired with nonzero
probability.

## Usage

``` r
softmax_weights(m, phi)
```

## Arguments

- m:

  A word-by-object association matrix.

- phi:

  Inverse temperature (choice sensitivity), `phi >= 0`: 0 is uniform
  guessing, and larger values choose the strongest association more
  deterministically.

## Value

A matrix of positive choice weights with `m`'s dimensions and names.
Each row is shifted by its maximum before exponentiating, which leaves
the choice probabilities unchanged and avoids overflow.

## Details

[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
applies it automatically to a model with a `phi` parameter (e.g.
`rescorla_wagner(..., phi = 5)`), to the mean of the simulations'
matrices.

## Examples

``` r
m <- matrix(c(1, -.5, 0, .2), 2, dimnames = list(1:2, 1:2))
get_perf(softmax_weights(m, phi = 3))
#>         1         2 
#> 0.9525741 0.8909032 
```
