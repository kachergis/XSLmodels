# Fazly et al. 2010 probablistic associative model

Fazly et al. 2010 probablistic associative model

## Usage

``` r
fazly(lambda, beta, alpha = 10, epsilon = 0.001, theta = NULL)
```

## Arguments

- lambda:

  Small smoothing factor (e.g. 1e-5)

- beta:

  Upper bound on number of symbol types to expect (e.g. 8500)

- alpha:

  Fixed smoothing parameter (e.g. 10)

- epsilon:

  Fixed smoothing parameter (e.g. 0.001)

- theta:

  Threshold to determine when associations enter the lexicon (defaults
  to NULL, i.e. model variant without thresholding).

## Value

An object of class xslMod

## Examples

``` r
mod <- fazly(lambda = 1e-5, beta = 8500)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.4999542 0.4999542 0.4999542 0.4999542
#> 
#> $matrix
#>              1            2            3            4
#> 1 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 2 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 3 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 4 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 
#> $traj
#> $traj[[1]]
#>           1         2            3            4
#> 1 0.3599496 0.3599496 3.296079e-05 3.296079e-05
#> 2 0.3599496 0.3599496 3.296079e-05 3.296079e-05
#> 3 0.0000000 0.0000000 0.000000e+00 0.000000e+00
#> 4 0.0000000 0.0000000 0.000000e+00 0.000000e+00
#> 
#> $traj[[2]]
#>              1            2            3            4
#> 1 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 2 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 3 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 4 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
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
#> 0.4999542 0.4999542 0.4999542 0.4999542 
#> 
#> $fits[[1]]$matrix
#>              1            2            3            4
#> 1 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 2 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 3 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 4 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 
#> $fits[[1]]$sse
#> [1] 8.383632e-09
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
#> [1] 8.383632e-09
#> 
#> $unweighted_sse
#> [1] 8.383632e-09
#> 

mod_t <- fazly(lambda = 1e-5, beta = 8500, theta = 0.7)
xsl_run(mod_t, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> $fits[[1]]$sims[[1]]
#> $perf
#>      [,1] [,2] [,3] [,4]
#> [1,]    0    0    0    0
#> 
#> $matrix
#>              1            2            3            4
#> 1 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 2 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 3 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 4 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 
#> $traj
#> $traj[[1]]
#>           1         2            3            4
#> 1 0.3599496 0.3599496 3.296079e-05 3.296079e-05
#> 2 0.3599496 0.3599496 3.296079e-05 3.296079e-05
#> 3 0.0000000 0.0000000 0.000000e+00 0.000000e+00
#> 4 0.0000000 0.0000000 0.000000e+00 0.000000e+00
#> 
#> $traj[[2]]
#>              1            2            3            4
#> 1 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 2 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 3 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 4 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
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
#> 0.4999542 0.4999542 0.4999542 0.4999542 
#> 
#> $fits[[1]]$matrix
#>              1            2            3            4
#> 1 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 2 3.599496e-01 3.599496e-01 3.296079e-05 3.296079e-05
#> 3 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 4 3.296079e-05 3.296079e-05 3.599496e-01 3.599496e-01
#> 
#> $fits[[1]]$sse
#> [1] 8.383632e-09
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
#> [1] 8.383632e-09
#> 
#> $unweighted_sse
#> [1] 8.383632e-09
#> 
```
