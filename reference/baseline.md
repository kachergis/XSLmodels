# Baseline model

Baseline model

## Usage

``` r
baseline()
```

## Value

An object of class xslMod

## Examples

``` r
mod <- baseline()
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
#>   1 2 3 4
#> 1 1 1 0 0
#> 2 1 1 0 0
#> 3 0 0 1 1
#> 4 0 0 1 1
#> 
#> $traj
#> $traj[[1]]
#>   1 2 3 4
#> 1 1 1 0 0
#> 2 1 1 0 0
#> 3 0 0 0 0
#> 4 0 0 0 0
#> 
#> $traj[[2]]
#>   1 2 3 4
#> 1 1 1 0 0
#> 2 1 1 0 0
#> 3 0 0 1 1
#> 4 0 0 1 1
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
#>   1 2 3 4
#> 1 1 1 0 0
#> 2 1 1 0 0
#> 3 0 0 1 1
#> 4 0 0 1 1
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
