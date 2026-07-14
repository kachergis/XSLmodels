# Update parameters of xslMod

Replaces `xsl_mod`'s leading parameter values with `params`, matched by
position (not by name) – e.g. a raw numeric vector such as `DEoptim`'s
`bestmem`. The result keeps `xsl_mod`'s original parameter names.

## Usage

``` r
update_params(xsl_mod, params)
```

## Arguments

- xsl_mod:

  Object of class xslMod

- params:

  Vector or list of parameter values, in the same order as the leading
  elements of `xsl_mod$params`

## Value

An object of class xslMod

## Details

`params` may be shorter than `xsl_mod$params`, in which case only the
first `length(params)` parameters are replaced and the rest keep their
existing values. This is what lets
[`xsl_fit()`](https://kachergis.github.io/XSLmodels/reference/xsl_fit.md)
optimize a leading subset of a model's parameters (e.g.
[`uncfam()`](https://kachergis.github.io/XSLmodels/reference/uncfam.md)'s
numeric `X`/`B`/`C` but not its categorical `variant`) by giving DEoptim
`lower`/`upper` bounds shorter than the model's full parameter list.
