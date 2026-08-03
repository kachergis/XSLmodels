# Shannon entropy after clamping negative associations to 0

Unlike the other models in this package, `uncfam_predictive_model()`'s
update rule has no normalization step, so an association can be driven
below 0 by a large, repeated negative prediction error. Association
strength below 0 isn't meaningful for entropy purposes, so it's treated
as 0 here (matching the effect, though not the value, of the
associations the "0" is standing in for).

## Usage

``` r
nonneg_shannon_entropy(p)
```
