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
  stochastic = logical()
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

## Value

An object of class xslMod
