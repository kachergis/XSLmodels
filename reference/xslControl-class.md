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
  verbose = FALSE
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

- x:

  List with elements train, test, accuracy, n_subj, label, condition

## Value

An object of class xslControl
