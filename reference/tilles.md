# Tilles and Fontanari (2013) reinforcement model

Tilles and Fontanari (2013) reinforcement model

## Usage

``` r
tilles(x, b, alpha_0)
```

## Arguments

- x:

  Reinforcement parameter for stimuli in current context

- b:

  Inference parameter, regulating ME and prior info integration (applies
  either to known words that do not appear in the current context or to
  new words in the current context)

- alpha_0:

  Baseline efficiency corresponding to maximal uncertainty about
  referent of target word

## Value

An object of class xslMod

## Examples

``` r
mod <- tilles(x = 0.6, b = 0.8, alpha_0 = 0.85)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#> [1]        NA        NA 0.4434419 0.4434419
#> 
#> $matrix
#>            [,1]       [,2]      [,3]      [,4]
#> [1,]         NA         NA        NA        NA
#> [2,]         NA         NA        NA        NA
#> [3,] 0.05655814 0.05655814 0.4434419 0.4434419
#> [4,] 0.05655814 0.05655814 0.4434419 0.4434419
#> 
#> $traj
#> list()
#> 
#> $sse
#> numeric(0)
#> 
#> attr(,"class")
#> [1] "xslFit" "list"  
#> 
#> 
#> $fits[[1]]$perf
#> [1]        NA        NA 0.4434419 0.4434419
#> 
#> $fits[[1]]$matrix
#>            [,1]       [,2]      [,3]      [,4]
#> [1,]         NA         NA        NA        NA
#> [2,]         NA         NA        NA        NA
#> [3,] 0.05655814 0.05655814 0.4434419 0.4434419
#> [4,] 0.05655814 0.05655814 0.4434419 0.4434419
#> 
#> $fits[[1]]$sse
#> [1] NA
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
#> [1] NA
#> 
#> $unweighted_sse
#> [1] NA
#> 
```
