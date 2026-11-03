# Fit XSL model using differential evolution

Fits a model to provided data using the Differential Evolution
optimization algorithm. By default it optimizes the model parameters to
minimize the sum of squared errors (SSE) between the model's predictions
and human accuracy data; with
`control = xslControl(objective = "binomial")` (or `"multinomial"`) it
instead maximizes the likelihood of the human test responses (see
[`xslControl()`](https://www.kachergis.com/XSLmodels/reference/xslControl-class.md)).

## Usage

``` r
xsl_fit(
  model,
  data,
  lower,
  upper,
  by_data = FALSE,
  control = xslControl(),
  deoptim_control = DEoptim::DEoptim.control(reltol = 0.001, NP = 100, itermax = 100),
  free = NULL,
  n_restarts = 1,
  seed = NULL
)
```

## Arguments

- model:

  An object of class xslMod.

- data:

  An object (or list of objects) of class xslData.

- lower:

  Numeric vector of lower bounds for the model's parameters.

- upper:

  Numeric vector of upper bounds for the model's parameters.

- by_data:

  Logical indicating whether to fit to each entry in data separately.

- control:

  Control parameters passed to
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md).

- deoptim_control:

  Control parameters passed to `DEoptim()`.

- free:

  Optional names of the parameters that `lower`/`upper` bound, in order
  (see
  [`update_params()`](https://www.kachergis.com/XSLmodels/reference/update_params.md));
  by default, the model's leading parameters. With a likelihood
  objective, `"lapse"` fits the control's lapse rate along with the
  model's parameters (e.g. bounds 0 to 1).

- n_restarts:

  Number of independent DEoptim runs per fit; the best (lowest SSE) is
  kept. Some models' objectives have poor local basins that a single run
  lands in a good fraction of the time (e.g.
  [`uncfam_predictive()`](https://www.kachergis.com/XSLmodels/reference/uncfam_predictive.md)),
  so a few restarts make fits far more reliable.

- seed:

  Optional random seed, set once before fitting, so DEoptim's random
  initial populations (and a stochastic model's simulations) are
  reproducible.

## Value

A list with one element per fit (one in total, or one per dataset with
`by_data = TRUE`), each the `DEoptim` result of the best run, which
includes the best parameters found (`optim$bestmem`, named) and their
SSE (`optim$bestval`; the negative log-likelihood with a likelihood
objective). With `n_restarts > 1`, `restarts` holds every run's best
value. With a likelihood objective, each fit also has `loglik`, `n_obs`
(the number of human responses), `k` (the number of fitted parameters,
including a fitted lapse), and `aic` and `bic`.

## Examples

``` r
fit <- xsl_fit(decay(C = .98), xsl_datasets[1:2], lower = .8, upper = 1,
               n_restarts = 2, seed = 1,
               deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 5, trace = FALSE))
fit[[1]]$optim$bestmem
#>         C 
#> 0.9269144 
fit[[1]]$restarts
#> [1] 0.2686 0.2686

# fit by binomial likelihood, with the lapse rate as a free parameter
fit <- xsl_fit(decay(C = .98), xsl_datasets[1:2], lower = c(.8, 0), upper = c(1, .5),
               free = c("C", "lapse"), control = xslControl(objective = "binomial"),
               seed = 1,
               deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 5, trace = FALSE))
#> Warning: For many problems it is best to set 'NP' (in 'control') to be at least ten times the length of the parameter vector. 
fit[[1]]$optim$bestmem
#>         C     lapse 
#> 0.8920193 0.2176450 
fit[[1]]$aic
#> [1] 189.0766
```
