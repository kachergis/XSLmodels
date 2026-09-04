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
  deoptim_control = DEoptim::DEoptim.control(reltol = 0.001, NP = 100, itermax = 100)
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
#> Iteration: 1 bestvalit: 0.227401 bestmemit:    0.066012    0.864247    0.985724
#> Iteration: 2 bestvalit: 0.227213 bestmemit:    0.066012    0.837244    0.985724
#> Iteration: 3 bestvalit: 0.227213 bestmemit:    0.066012    0.837244    0.985724
#> Iteration: 4 bestvalit: 0.227213 bestmemit:    0.066012    0.837244    0.985724
#> Iteration: 5 bestvalit: 0.227213 bestmemit:    0.066012    0.837244    0.985724
```
