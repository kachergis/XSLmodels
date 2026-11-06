# Kalman filter associative model

A Kalman-filter generalization of
[`rescorla_wagner()`](https://www.kachergis.com/XSLmodels/reference/rescorla_wagner.md)'s
error-driven update rule (Dayan & Kakade, 2001; Kruschke, 2008;
Gershman, 2015): word- object associations are tracked as Gaussian
beliefs, and each trial's update is scaled by a Kalman gain,
`sigma2 / (sigma2 + sigma2_obs)`. This makes the effective learning rate
adaptive rather than fixed – large (fast learning) while an association
is still uncertain, and automatically shrinking as confidence
accumulates – and it emerges from three interpretable parameters rather
than being hand-set. Between observations, every association's variance
grows by `tau2` (the model assumes true associations drift slowly over
time), which keeps the model able to revise a belief rather than
converging to a fixed point.

## Usage

``` r
kalman_filter(
  tau2,
  sigma2_obs,
  sigma2_0,
  covariance = c("independent", "full"),
  outcomes = c("present", "all"),
  phi = NULL
)
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

- covariance:

  `"independent"` (the default): one filter per word-object pair.
  `"full"`: one filter per object, over all words, with a full
  covariance.

- outcomes:

  `"present"` (the default) updates only the objects present on a trial;
  `"all"` also updates absent objects toward 0.

- phi:

  Optional softmax inverse temperature for the choice rule. `NULL` (the
  default) uses Luce choice.

## Value

An object of class xslMod

## Details

By default (`covariance = "independent"`), every word-object association
is its own one-dimensional filter that observes 1 whenever the pair
co-occurs. Associations then don't interact: there is no competition
between a trial's words, and an association is a saturating function of
how often and how recently its pair co-occurred. With a small gain it is
simply co-occurrence counting.

## Options

- `covariance = "full"` is the Kalman filter of the associative-learning
  literature (Dayan & Kakade, 2001; Kruschke, 2008). Each object
  (outcome) has a weight vector over words with a full covariance
  matrix. The present words jointly predict the object (the sum of their
  weights), so they share one prediction error, and the gain is a vector
  over words. Words that appear together come to have negatively
  correlated weights, so later evidence about one revises the other:
  explaining away and backward blocking (retrospective revaluation), as
  when learning that one word on an earlier trial named an object
  implies the trial's other words did not. Memory and time grow with the
  square of the vocabulary.

- `outcomes = "all"` treats every object as an outcome on every trial,
  observing 1 if it is present and 0 if absent, as in the canonical
  filter, rather than updating only the objects present. Associations
  can then become negative, which Luce choice treats as 0; `phi` gives a
  choice rule defined for them.

- `phi` replaces Luce choice with a softmax choice rule with inverse
  temperature `phi` (see
  [`softmax_weights()`](https://www.kachergis.com/XSLmodels/reference/softmax_weights.md)).

Note that the three noise parameters share a scale redundancy:
multiplying `tau2`, `sigma2_obs` and `sigma2_0` together by any constant
leaves every Kalman gain – and hence the learned matrix – unchanged.
Only two of the three are identifiable; when fitting, hold one fixed
(conventionally `sigma2_obs`).

## References

Dayan, P., & Kakade, S. (2001). Explaining away in weight space. In
*Advances in Neural Information Processing Systems 13* (pp. 451-457).

Gershman, S. J. (2015). A unifying probabilistic view of associative
learning. *PLoS Computational Biology, 11*(11), e1004567.

Kruschke, J. K. (2008). Bayesian approaches to associative learning:
From passive to active learning. *Learning & Behavior, 36*(3), 210-226.

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
# the full-covariance filter, with absent objects as outcomes
mod <- kalman_filter(tau2 = .001, sigma2_obs = 1, sigma2_0 = .5,
                     covariance = "full", outcomes = "all")
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
#>           1         2        3        4
#> 1 0.2502498 0.2502498 0.000000 0.000000
#> 2 0.2502498 0.2502498 0.000000 0.000000
#> 3 0.0000000 0.0000000 0.250499 0.250499
#> 4 0.0000000 0.0000000 0.250499 0.250499
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
