# Bayesian decay model

Bayesian decay model

## Usage

``` r
bayesian_decay(alpha, delta, ch_dec)
```

## Arguments

- alpha:

  Decay for word/object non-co-occurrence (0.1, 0.5, 0.9)

- delta:

  Multiplier for word/object co-occurrence (1 for no increase)

- ch_dec:

  ecision parameter (e.g., 1)

## Value

An object of class xslMod

## Examples

``` r
mod <- bayesian_decay(alpha = 0.5, delta = 1, ch_dec = 1)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#>      [,1] [,2] [,3] [,4]
#> [1,]  0.4  0.4  0.4  0.4
#> 
#> $matrix
#>     1   2   3   4
#> 1 0.4 0.4 0.1 0.1
#> 2 0.4 0.4 0.1 0.1
#> 3 0.1 0.1 0.4 0.4
#> 4 0.1 0.1 0.4 0.4
#> 
#> $traj
#> $traj[[1]]
#>           1         2         3         4
#> 1 0.3333333 0.3333333 0.1666667 0.1666667
#> 2 0.3333333 0.3333333 0.1666667 0.1666667
#> 3 0.1666667 0.1666667 0.3333333 0.3333333
#> 4 0.1666667 0.1666667 0.3333333 0.3333333
#> 
#> $traj[[2]]
#>     1   2   3   4
#> 1 0.4 0.4 0.1 0.1
#> 2 0.4 0.4 0.1 0.1
#> 3 0.1 0.1 0.4 0.4
#> 4 0.1 0.1 0.4 0.4
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
#>   1   2   3   4 
#> 0.4 0.4 0.4 0.4 
#> 
#> $fits[[1]]$matrix
#>     1   2   3   4
#> 1 0.4 0.4 0.1 0.1
#> 2 0.4 0.4 0.1 0.1
#> 3 0.1 0.1 0.4 0.4
#> 4 0.1 0.1 0.4 0.4
#> 
#> $fits[[1]]$sse
#> [1] 0.04
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
#> [1] 0.04
#> 
#> $unweighted_sse
#> [1] 0.04
#> 
```
