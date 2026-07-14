# Fit XSL model using differential evolution

Fits a model to provided data using the Differential Evolution
optimization algorithm. It optimizes the model parameters to minimize
the sum of squared errors (SSE) between the model's predictions and
human accuracy data.

## Usage

``` r
xsl_fit(
  model,
  data,
  lower,
  upper,
  by_data = FALSE,
  control = xslControl(),
  deoptim_control = DEoptim::DEoptim.control(reltol = 0.001, NP = 100, itermax = 100)
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
  [`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md).

- deoptim_control:

  Control parameters passed to `DEoptim()`.

## Value

An object of class `DEoptim` representing the fitting result, which
includes the best set of parameters found and the corresponding SSE
value.
