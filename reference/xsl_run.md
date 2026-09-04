# Run XSL model

Run XSL model

## Usage

``` r
xsl_run(model, data, control = xslControl())
```

## Arguments

- model:

  An object of class xslMod.

- data:

  An object (or list of objects) of class xslData.

- control:

  Control arguments returned by `xsl_control()`.

## Value

A list with `sse`, `unweighted_sse`, and `fits` (one entry per dataset).
Each fit has `matrix` (the word-by-object matrix, summed over
simulations for a stochastic model), `perf`, `sse`, `data`, `responses`
(an `n_sim` x n-words matrix of each simulated participant's final
per-word accuracy), and `sims` (the full per-simulation `xslFit` list,
`NULL` unless `control` had `keep_sims = TRUE`).
