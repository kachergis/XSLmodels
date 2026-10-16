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
#> NULL
#> 
#> $fits[[1]]$responses
#>            1       2         3         4
#> [1,] 0.48125 0.48125 0.4546598 0.4546598
#> 
#> $fits[[1]]$perf
#>         1         2         3         4 
#> 0.4812500 0.4812500 0.4546598 0.4546598 
#> 
#> $fits[[1]]$matrix
#>            1          2         3         4
#> 1 0.48125000 0.48125000 0.0187500 0.0187500
#> 2 0.48125000 0.48125000 0.0187500 0.0187500
#> 3 0.04534017 0.04534017 0.4546598 0.4546598
#> 4 0.04534017 0.04534017 0.4546598 0.4546598
#> 
#> $fits[[1]]$sse
#> [1] 0.004814588
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
#> [1] 0.004814588
#> 
#> $unweighted_sse
#> [1] 0.004814588
#> 
```
