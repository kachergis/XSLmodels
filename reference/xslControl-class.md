# xslControl S3 class

Control arguments for
[`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md)

## Usage

``` r
xslControl(
  reps = 1,
  start_matrix = NULL,
  test_noise = 0,
  n_sim = 500,
  verbose = FALSE,
  keep_traj = FALSE,
  keep_sims = FALSE
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
  [`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md)
  should retain the full list of per-simulation `xslFit` objects
  (`fits[[i]]$sims`). Off by default; `fits[[i]]$responses` (an `n_sim`
  x n-words matrix of each simulated participant's final per-word
  accuracy) is always returned instead, at a fraction of the memory.

- x:

  List with elements train, test, accuracy, n_subj, label, condition

## Value

An object of class xslControl
