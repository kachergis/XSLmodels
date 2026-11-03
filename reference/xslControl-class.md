# xslControl S3 class

Control arguments for
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)

## Usage

``` r
xslControl(
  reps = 1,
  start_matrix = NULL,
  test_noise = 0,
  n_sim = 500,
  verbose = FALSE,
  keep_traj = FALSE,
  keep_sims = FALSE,
  objective = c("sse", "binomial", "multinomial"),
  lapse = 0.01
)

new_xslControl(x = list())
```

## Arguments

- reps:

  Number of times to repeat training

- start_matrix:

  Starting matrix

- test_noise:

  Test noise

- n_sim:

  Number of simulations for stochastic models

- verbose:

  Verbosity

- keep_traj:

  Whether each model run should record its per-trial association matrix
  (`xslFit$traj`). Off by default: the trajectory is not used by any
  function in the package, and for a long corpus it is a serious memory
  cost (one word-by-object matrix per trial, per simulation). Set `TRUE`
  only when you want to inspect the learning trajectory yourself.

- keep_sims:

  Whether
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  should retain the full list of per-simulation `xslFit` objects
  (`fits[[i]]$sims`). Off by default; `fits[[i]]$responses` (an `n_sim`
  x n-words matrix of each simulated participant's final per-word
  accuracy) is always returned instead, at a fraction of the memory.

- objective:

  What
  [`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)
  minimizes, and what
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  scores:

  - `"sse"` (the default): the `n_subj`-weighted sum of squared errors
    between model and human accuracy per test item.

  - `"binomial"`: the negative log-likelihood of how many participants
    got each test item right, given the model's probability of a correct
    choice. Counts come from the condition's `response_matrix`, or
    otherwise from `accuracy` and `n_subj`.

  - `"multinomial"`: the negative log-likelihood of which object each
    participant chose on each test item, given the model's full choice
    distribution over the item's alternatives (see
    [`choice_probabilities()`](https://www.kachergis.com/XSLmodels/reference/choice_probabilities.md)).
    Needs a `response_matrix`.

  The likelihoods treat every response as an independent draw from the
  model's prediction, ignoring individual differences, so they overstate
  the evidence for one model over another; rankings are more trustworthy
  than the absolute size of AIC or likelihood-ratio differences.

- lapse:

  For the likelihood objectives: the probability that a response is a
  uniform guess among the item's alternatives rather than the model's
  choice. Keeps the likelihood finite when a model is certain and a
  participant chooses otherwise. Fit it by naming `"lapse"` in
  [`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)'s
  `free`. Ignored by `"sse"`.

- x:

  List with elements train, test, accuracy, n_subj, label, condition

## Value

An object of class xslControl
