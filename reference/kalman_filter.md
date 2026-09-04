# Kalman filter associative model

A Kalman-filter generalization of
[`rescorla_wagner()`](https://kachergis.github.io/XSLmodels/reference/rescorla_wagner.md)'s
error-driven update rule (Dayan & Kakade, 2000; Kruschke, 2008;
Gershman, 2015): each word-object association is tracked as a Gaussian
belief (mean `mu` and variance `sigma2`), and each trial's update is
scaled by that pair's own Kalman gain, `sigma2 / (sigma2 + sigma2_obs)`.
This makes the effective learning rate adaptive rather than fixed –
large (fast learning) while an association is still uncertain, and
automatically shrinking as confidence accumulates – and it emerges from
three interpretable parameters rather than being hand-set. Between
observations, every association's variance grows by `tau2` (the model
assumes true associations drift slowly over time), which keeps the model
able to revise a belief rather than converging to a fixed point.

## Usage

``` r
kalman_filter(tau2, sigma2_obs, sigma2_0)
```

## Arguments

- tau2:

  Process (diffusion) noise: how much uncertainty about every
  association grows per trial, whether or not it was observed

- sigma2_obs:

  Observation noise: how uninformative a single co-occurrence
  observation is (larger = slower learning per trial)

- sigma2_0:

  Initial (prior) uncertainty about every association, before any
  training

## Value

An object of class xslMod

## Details

Note that the three parameters share a scale redundancy: multiplying
`tau2`, `sigma2_obs` and `sigma2_0` together by any constant leaves
every Kalman gain – and hence the learned matrix – unchanged. Only two
of the three are identifiable; when fitting, hold one fixed
(conventionally `sigma2_obs`).

## Examples

``` r
mod <- kalman_filter(tau2 = .01, sigma2_obs = .5, sigma2_0 = 1)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> NULL
#> 
#> $fits[[1]]$responses
#>      [,1] [,2] [,3] [,4]
#> [1,]  0.5  0.5  0.5  0.5
#> 
#> $fits[[1]]$perf
#>   1   2   3   4 
#> 0.5 0.5 0.5 0.5 
#> 
#> $fits[[1]]$matrix
#>           1         2         3         4
#> 1 0.6688742 0.6688742 0.0000000 0.0000000
#> 2 0.6688742 0.6688742 0.0000000 0.0000000
#> 3 0.0000000 0.0000000 0.6710526 0.6710526
#> 4 0.0000000 0.0000000 0.6710526 0.6710526
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
