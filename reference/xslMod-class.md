# Constructor for xsl_model S3 class

Constructor for xsl_model S3 class

xslMod S3 class

Constructor for xslMod S3 class

## Usage

``` r
new_xslFit(x = list())

xslMod(
  name = character(),
  description = character(),
  model,
  params = numeric(),
  stochastic = logical(),
  supports_start_matrix = FALSE
)

new_xslMod(x = list())
```

## Arguments

- x:

  List with elements perf, matrix, traj, sse

- name:

  Name

- description:

  Description

- model:

  Model fitting function

- params:

  List of parameters

- stochastic:

  Logical indicating whether model is stochastic

- supports_start_matrix:

  Logical: does `model` initialize from `control$start_matrix`?
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  errors if a start matrix is supplied to a model that doesn't, rather
  than letting it be silently ignored.

## Value

An object of class xslMod
