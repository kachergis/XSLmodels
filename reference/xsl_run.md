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

For a multi-block condition (an `xslData` whose `accuracy` has one row
per block; see
[`xslData()`](https://www.kachergis.com/XSLmodels/reference/xslData-class.md)),
block `b` is scored on a run trained on `b` repetitions of the training
sequence. Its fit's `perf` then has one row per block, `sse` (and
`loglik`) sum over blocks, `matrix`, `responses` and `sims` are the last
block's, and `blocks` holds each block's own fit. Each block is a
separate run, so for a stochastic model the blocks' predictions come
from independent simulated learners: right on average, but not one
learner's trajectory.

With a likelihood objective (`xslControl(objective = "binomial")` or
`"multinomial"`), each fit also has `loglik`, the log-likelihood of that
condition's human test responses, and the result has `loglik` (summed
over conditions) and `nll` (its negative, which
[`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)
minimizes). Unlike `sse`, which averages conditions weighted by
`n_subj`, the likelihood sums over every response, so larger conditions
count for more in the same way.
