# Predictive biased associative model (item-level prediction error)

A variant of
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
(Kachergis et al. 2012's uncertainty- and familiarity-biased associative
model) that adds an item-level, Rescorla-Wagner-style prediction error
term: on each trial, the amount learned about a word-object association
is scaled by how much the word's predicted association strength (summed
over the trial's objects) falls short of the maximum value, rather than
normalizing to distribute a fixed amount of associative weight across
the trial. This lets initially mis-paired ("surprising") items draw more
learning than the un-normalized original model allows.

## Usage

``` r
uncfam_predictive(X, B, C)
```

## Arguments

- X:

  Associative weight to distribute

- B:

  Weighting of uncertainty vs. familiarity

- C:

  Decay

## Value

An object of class xslMod

## Examples

``` r
mod <- uncfam_predictive(X = .1, C = 1, B = .98)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> NULL
#> 
#> $fits[[1]]$responses
#>           [,1]      [,2]     [,3]     [,4]
#> [1,] 0.3145218 0.3145218 0.427946 0.427946
#> 
#> $fits[[1]]$perf
#>         1         2         3         4 
#> 0.3145218 0.3145218 0.4279460 0.4279460 
#> 
#> $fits[[1]]$matrix
#>            1          2          3          4
#> 1 0.01695734 0.01695734 0.01000000 0.01000000
#> 2 0.01695734 0.01695734 0.01000000 0.01000000
#> 3 0.01000000 0.01000000 0.05939244 0.05939244
#> 4 0.01000000 0.01000000 0.05939244 0.05939244
#> 
#> $fits[[1]]$sse
#> [1] 0.0791879
#> 
#> $fits[[1]]$data
#> xslData object with label "example condition" and condition "ambiguous"
#>   training trials: 2
#>       test trials: 0
#>             words: 4
#>           objects: 4
#>        accuracies: 4
#> 
#> 
#> 
#> $sse
#> [1] 0.0791879
#> 
#> $unweighted_sse
#> [1] 0.0791879
#> 
```
