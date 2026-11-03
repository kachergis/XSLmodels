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

With a likelihood objective (`xslControl(objective = "binomial")` or
`"multinomial"`), each fit also has `loglik`, the log-likelihood of that
condition's human test responses, and the result has `loglik` (summed
over conditions) and `nll` (its negative, which
[`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)
minimizes). Unlike `sse`, which averages conditions weighted by
`n_subj`, the likelihood sums over every response, so larger conditions
count for more in the same way.
