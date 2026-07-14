# Kachergis 2012

Kachergis et al. 2012 uncertainty- and familiarity-biased associative
model

## Usage

``` r
uncfam(X, B, C, variant = c("entropy", "novelty", "uncertainty-only"))
```

## Arguments

- X:

  Associative weight to distribute

- B:

  Weighting of uncertainty vs. familiarity

- C:

  Decay

- variant:

  Which variant of the model to fit (one of "entropy", "novelty",
  "uncertainty-only").

## Value

An object of class xslMod

## Examples

``` r
mod <- uncfam(X = .1, C = 1, B = .98)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.3888889 0.3888889 0.3888889 0.3888889
#> 
#> $matrix
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
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
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
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
#> 0.3888889 0.3888889 0.3888889 0.3888889 
#> 
#> $fits[[1]]$matrix
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
#> 
#> $fits[[1]]$sse
#> [1] 0.04938272
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
#> [1] 0.04938272
#> 
#> $unweighted_sse
#> [1] 0.04938272
#> 

mod <- uncfam(X = .1, C = 1, B = .98, variant = "novelty")
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.3888889 0.3888889 0.3888889 0.3888889
#> 
#> $matrix
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
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
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
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
#> 0.3888889 0.3888889 0.3888889 0.3888889 
#> 
#> $fits[[1]]$matrix
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
#> 
#> $fits[[1]]$sse
#> [1] 0.04938272
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
#> [1] 0.04938272
#> 
#> $unweighted_sse
#> [1] 0.04938272
#> 

mod <- uncfam(X = .1, C = 1, B = 0) # familiarity-only
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.3888889 0.3888889 0.3888889 0.3888889
#> 
#> $matrix
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
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
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
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
#> 0.3888889 0.3888889 0.3888889 0.3888889 
#> 
#> $fits[[1]]$matrix
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
#> 
#> $fits[[1]]$sse
#> [1] 0.04938272
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
#> [1] 0.04938272
#> 
#> $unweighted_sse
#> [1] 0.04938272
#> 

mod <- uncfam(X = .1, C = 1, B = .98, variant = "uncertainty-only")
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.3888889 0.3888889 0.3888889 0.3888889
#> 
#> $matrix
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
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
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
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
#> 0.3888889 0.3888889 0.3888889 0.3888889 
#> 
#> $fits[[1]]$matrix
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
#> 
#> $fits[[1]]$sse
#> [1] 0.04938272
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
#> [1] 0.04938272
#> 
#> $unweighted_sse
#> [1] 0.04938272
#> 
```
