# Get cross-validated model fit

Performs k-fold cross-validation on a model using the available
datasets: the datasets are split into `n_folds` groups, and for each
fold the model is fit (via
[`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md))
to the other folds and evaluated (via
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md))
on the held-out fold.

## Usage

``` r
get_crossvalidated_model_fit(
  model_name,
  n_folds = 5,
  datasets = NULL,
  control = xslControl(),
  deoptim_control = DEoptim::DEoptim.control(reltol = 0.001, NP = 100, itermax = 100)
)
```

## Arguments

- model_name:

  Name of the model to cross-validate (see
  [`show_models()`](https://www.kachergis.com/XSLmodels/reference/show_models.md))

- n_folds:

  Number of folds for cross-validation (default: 5)

- datasets:

  Optional list of datasets to use (defaults to all available)

- control:

  Control arguments passed to
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)/[`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md);
  notably `n_sim`, the number of simulations averaged per evaluation of
  a stochastic model (default 500 – lowering this can speed up
  cross-validation of stochastic models substantially).

- deoptim_control:

  Control parameters passed to `DEoptim()` (via
  [`cross_validated_group_fits()`](https://www.kachergis.com/XSLmodels/reference/cross_validated_group_fits.md)).

## Value

A list containing cross-validation results (see
[`cross_validated_group_fits()`](https://www.kachergis.com/XSLmodels/reference/cross_validated_group_fits.md))

## Examples

``` r
# Get cross-validated fit for the decay model. A reduced DEoptim search
# keeps this example fast; drop deoptim_control for a real, thorough fit.
cv_fit <- get_crossvalidated_model_fit(
  "decay", n_folds = 2, datasets = xsl_datasets[1:6],
  deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 5))
#> Iteration: 1 bestvalit: 0.180096 bestmemit:    0.990199
#> Iteration: 2 bestvalit: 0.179639 bestmemit:    0.992927
#> Iteration: 3 bestvalit: 0.179639 bestmemit:    0.992927
#> Iteration: 4 bestvalit: 0.179482 bestmemit:    0.993901
#> Iteration: 5 bestvalit: 0.178656 bestmemit:    0.999459
#> Iteration: 1 bestvalit: 0.337464 bestmemit:    0.957351
#> Iteration: 2 bestvalit: 0.337464 bestmemit:    0.919373
#> Iteration: 3 bestvalit: 0.337464 bestmemit:    0.919373
#> Iteration: 4 bestvalit: 0.337464 bestmemit:    0.919373
#> Iteration: 5 bestvalit: 0.337464 bestmemit:    0.919373
```
