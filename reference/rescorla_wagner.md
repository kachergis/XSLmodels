# Rescorla-Wagner (1972) error-driven associative model

Rescorla-Wagner (1972) error-driven associative model

## Usage

``` r
rescorla_wagner(C, alpha, beta, lambda)
```

## Arguments

- C:

  Decay

- alpha:

  Salience (fix at 1 unless manipulated)

- beta:

  Learning rate – a proportion of lambda

- lambda:

  Maximum associative value that a CS can achieve - should be larger
  than learning rate

## Value

An object of class xslMod

## Examples

``` r
mod <- rescorla_wagner(C = 1, alpha = 1, beta = 0.1, lambda = 3)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#>      [,1] [,2] [,3] [,4]
#> [1,]  0.5  0.5  0.5  0.5
#> 
#> $matrix
#>     1   2   3   4
#> 1 0.9 0.9 0.0 0.0
#> 2 0.9 0.9 0.0 0.0
#> 3 0.0 0.0 0.9 0.9
#> 4 0.0 0.0 0.9 0.9
#> 
#> $traj
#> $traj[[1]]
#>     1   2 3 4
#> 1 0.9 0.9 0 0
#> 2 0.9 0.9 0 0
#> 3 0.0 0.0 0 0
#> 4 0.0 0.0 0 0
#> 
#> $traj[[2]]
#>     1   2   3   4
#> 1 0.9 0.9 0.0 0.0
#> 2 0.9 0.9 0.0 0.0
#> 3 0.0 0.0 0.9 0.9
#> 4 0.0 0.0 0.9 0.9
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
#> 0.5 0.5 0.5 0.5 
#> 
#> $fits[[1]]$matrix
#>     1   2   3   4
#> 1 0.9 0.9 0.0 0.0
#> 2 0.9 0.9 0.0 0.0
#> 3 0.0 0.0 0.9 0.9
#> 4 0.0 0.0 0.9 0.9
#> 
#> $fits[[1]]$sse
#> [1] 0
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
#> [1] 0
#> 
#> $unweighted_sse
#> [1] 0
#> 
```
