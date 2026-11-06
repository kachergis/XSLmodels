# Update parameters of xslMod

Replaces some of `xsl_mod`'s parameter values with `params` – e.g. a raw
numeric vector such as `DEoptim`'s `bestmem`. The result keeps
`xsl_mod`'s original parameter names.

## Usage

``` r
update_params(xsl_mod, params, free = NULL)
```

## Arguments

- xsl_mod:

  Object of class xslMod

- params:

  Vector or list of parameter values, in the same order as the leading
  elements of `xsl_mod$params`, or as `free`

- free:

  Optional character vector of parameter names that `params` replaces,
  in order

## Value

An object of class xslMod

## Details

By default `params` replaces the *leading* parameters, matched by
position (not by name): `params` may be shorter than `xsl_mod$params`,
in which case only the first `length(params)` parameters are replaced.
This is what lets
[`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)
optimize a leading subset of a model's parameters (e.g.
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)'s
numeric `X`/`B`/`C` but not its categorical `variant`).

With `free`, `params` instead replaces the named parameters, in the
order given – so any subset can be fit while the rest stay fixed (e.g.
[`rescorla_wagner()`](https://www.kachergis.com/XSLmodels/reference/rescorla_wagner.md)'s
`beta`, `C` and `lambda` with `alpha` fixed at 1).

## Examples

``` r
rw <- rescorla_wagner(C = .98, alpha = 1, beta = .5, lambda = 1)
update_params(rw, c(.2, .95, .8), free = c("beta", "C", "lambda"))$params
#> $C
#> [1] 0.95
#> 
#> $alpha
#> [1] 1
#> 
#> $beta
#> [1] 0.2
#> 
#> $lambda
#> [1] 0.8
#> 
#> $outcomes
#> [1] "present"
#> 
#> $cues
#> [1] "words"
#> 
```
