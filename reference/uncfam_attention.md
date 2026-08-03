# Biased associative model with attention scaled to trial uncertainty

A variant of
[`uncfam()`](https://kachergis.github.io/XSLmodels/reference/uncfam.md)
(Kachergis et al. 2012's uncertainty- and familiarity-biased associative
model) in which the associative weight distributed on a trial is
additionally scaled by the ratio of that trial's mean object entropy to
the mean entropy of all objects seen so far. This implements the
"system-level" theory proposed by Fitneva & Christiansen (2015): that
learners are more alert (i.e. allocate a higher learning rate) on trials
with more uncertain items, such as in a Low Initial Accuracy condition
where many items have been mis-paired.

## Usage

``` r
uncfam_attention(X, B, C)
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
mod <- uncfam_attention(X = .1, C = 1, B = .98)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.3888889 0.3888889 0.3926256 0.3926256
#> 
#> $matrix
#>       1     2          3          4
#> 1 0.035 0.035 0.01000000 0.01000000
#> 2 0.035 0.035 0.01000000 0.01000000
#> 3 0.010 0.010 0.03656604 0.03656604
#> 4 0.010 0.010 0.03656604 0.03656604
#> 
#> $traj
#> $traj[[1]]
#>       1     2 3 4
#> 1 0.035 0.035 0 0
#> 2 0.035 0.035 0 0
#> 3 0.000 0.000 0 0
#> 4 0.000 0.000 0 0
#> 
#> $traj[[2]]
#>       1     2          3          4
#> 1 0.035 0.035 0.01000000 0.01000000
#> 2 0.035 0.035 0.01000000 0.01000000
#> 3 0.010 0.010 0.03656604 0.03656604
#> 4 0.010 0.010 0.03656604 0.03656604
#> 
#> 
#> $sse
#> numeric(0)
#> 
#> attr(,"class")
#> [1] "xslFit" "list"  
#> 
#> 
#> $fits[[1]]$perf
#>         1         2         3         4 
#> 0.3888889 0.3888889 0.3926256 0.3926256 
#> 
#> $fits[[1]]$matrix
#>       1     2          3          4
#> 1 0.035 0.035 0.01000000 0.01000000
#> 2 0.035 0.035 0.01000000 0.01000000
#> 3 0.010 0.010 0.03656604 0.03656604
#> 4 0.010 0.010 0.03656604 0.03656604
#> 
#> $fits[[1]]$sse
#> [1] 0.04774988
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
#> [1] 0.04774988
#> 
#> $unweighted_sse
#> [1] 0.04774988
#> 
```
