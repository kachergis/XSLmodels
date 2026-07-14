# Perform Cross-Validation on Group Fits of a Model

This function runs k-fold cross-validation for group fits of a specified
model using a set of conditions. On each fold, the model is fit (via
[`xsl_fit()`](https://kachergis.github.io/XSLmodels/reference/xsl_fit.md))
to the training conditions, and the fitted model is then evaluated (via
[`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md))
on the held-out test conditions to obtain an out-of-sample SSE.

## Usage

``` r
cross_validated_group_fits(
  model,
  combined_data,
  lower,
  upper,
  n_folds = 5,
  control = xslControl(),
  deoptim_control = DEoptim::DEoptim.control(reltol = 0.001, NP = 100, itermax = 100),
  seed = NULL
)
```

## Arguments

- model:

  An object of class `xslMod` giving the model to cross-validate (its
  starting parameter values are only used to determine how many
  parameters
  [`xsl_fit()`](https://kachergis.github.io/XSLmodels/reference/xsl_fit.md)
  should optimize).

- combined_data:

  A list of `xslData` conditions used for cross-validation.

- lower:

  Numeric vector of lower bounds for the model's parameters.

- upper:

  Numeric vector of upper bounds for the model's parameters.

- n_folds:

  Number of folds (default: 5).

- control:

  Control arguments passed to
  [`xsl_fit()`](https://kachergis.github.io/XSLmodels/reference/xsl_fit.md)/[`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md).

- deoptim_control:

  Control parameters passed to `DEoptim()` (via
  [`xsl_fit()`](https://kachergis.github.io/XSLmodels/reference/xsl_fit.md)).

- seed:

  Optional random seed, for reproducible fold assignment.

## Value

A list containing cross-validation results:

- `folds`: a list (one per fold) with the fitted `params`, `train_sse`
  (in-sample), and `test_sse` (out-of-sample).

- `params`: a matrix of fitted parameters, one row per fold.

- `train_sse` / `test_sse`: numeric vectors of per-fold SSE.

## Examples

``` r
# A reduced DEoptim search keeps this example fast; drop
# deoptim_control for a real, thorough fit.
cross_validated_group_fits(decay(C = 0.98), xsl_datasets[1:6],
                           lower = 0.8, upper = 1.0, n_folds = 2,
                           deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 5))
#> Iteration: 1 bestvalit: 0.277897 bestmemit:    0.980795
#> Iteration: 2 bestvalit: 0.276413 bestmemit:    0.989890
#> Iteration: 3 bestvalit: 0.276413 bestmemit:    0.989890
#> Iteration: 4 bestvalit: 0.276413 bestmemit:    0.989890
#> Iteration: 5 bestvalit: 0.276413 bestmemit:    0.989890
#> Iteration: 1 bestvalit: 0.233286 bestmemit:    0.889506
#> Iteration: 2 bestvalit: 0.233286 bestmemit:    0.889506
#> Iteration: 3 bestvalit: 0.233286 bestmemit:    0.889506
#> Iteration: 4 bestvalit: 0.233286 bestmemit:    0.889506
#> Iteration: 5 bestvalit: 0.233286 bestmemit:    0.889506
#> $folds
#> $folds$`1`
#> $folds$`1`$params
#>      par1 
#> 0.9898899 
#> 
#> $folds$`1`$train_sse
#> [1] 0.2764133
#> 
#> $folds$`1`$test_sse
#> [1] 0.2332857
#> 
#> 
#> $folds$`2`
#> $folds$`2`$params
#>      par1 
#> 0.8895055 
#> 
#> $folds$`2`$train_sse
#> [1] 0.2332857
#> 
#> $folds$`2`$test_sse
#> [1] 0.3035583
#> 
#> 
#> 
#> $params
#>        par1
#> 1 0.9898899
#> 2 0.8895055
#> 
#> $train_sse
#>         1         2 
#> 0.2764133 0.2332857 
#> 
#> $test_sse
#>         1         2 
#> 0.2332857 0.3035583 
#> 
```
