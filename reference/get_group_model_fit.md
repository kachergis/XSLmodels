# Get group model fit for a specific model

Fits a model to all available datasets and returns the group-level fit.
This function provides a convenient way to get pre-computed model fits
for common models.

## Usage

``` r
get_group_model_fit(
  model_name,
  datasets = NULL,
  control = xslControl(),
  deoptim_control = DEoptim::DEoptim.control(reltol = 0.001, NP = 100, itermax = 100),
  n_restarts = NULL,
  seed = NULL
)
```

## Arguments

- model_name:

  Name of the model to fit (see
  [`show_models()`](https://www.kachergis.com/XSLmodels/reference/show_models.md))

- datasets:

  Optional list of datasets to fit to (defaults to all available)

- control:

  Control arguments passed to
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  (via
  [`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md));
  notably `n_sim`, the number of simulations averaged per evaluation of
  a stochastic model (default 500 – lowering this can speed up fitting
  of stochastic models substantially).

- deoptim_control:

  Control parameters passed to `DEoptim()` (via
  [`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)).

- n_restarts:

  Number of DEoptim runs to keep the best of; defaults to the model's
  registry default (5 for `uncfam_predictive`, 3 for `tilles`, otherwise
  1).

- seed:

  Optional random seed for reproducible fits.

## Value

A list containing the fitted model results

## Examples

``` r
# Get group fit for the uncfam model. A small dataset subset and a
# reduced DEoptim search keep this example fast; drop those arguments
# for a real, thorough fit.
group_fit <- get_group_model_fit(
  "uncfam", datasets = xsl_datasets[1:3],
  deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 5))
#> Warning: For many problems it is best to set 'NP' (in 'control') to be at least ten times the length of the parameter vector. 
#> Iteration: 1 bestvalit: 5.155958 bestmemit:    0.572436    4.886378    0.964311
#> Iteration: 2 bestvalit: 5.155958 bestmemit:    0.572436    4.886378    0.964311
#> Iteration: 3 bestvalit: 4.683360 bestmemit:    1.070998    3.482776    0.990162
#> Iteration: 4 bestvalit: 3.017839 bestmemit:    0.400891    3.482776    0.990162
#> Iteration: 5 bestvalit: 3.017839 bestmemit:    0.400891    3.482776    0.990162
```
