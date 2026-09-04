# Rational Speech Act pragmatic listener and speaker

A small, self-contained RSA recursion over a word-by-object meaning
matrix (Frank & Goodman, 2012; Smith, Goodman & Frank, 2013), used by
[`predict_referent()`](https://www.kachergis.com/XSLmodels/reference/predict_referent.md)
with `pragmatic = TRUE`. `rsa_listener()` returns `P(object | word)` for
a listener who reasons about an informative speaker; `rsa_speaker()`
returns `P(word | object)` for that speaker. Both operate on any
non-negative `(word x object)` matrix, reading entries as literal
meaning weights.

## Usage

``` r
rsa_listener(lex, prior, ...)

rsa_speaker(lex, prior, ...)
```

## Arguments

- lex:

  A `(word x object)` matrix of literal meaning weights.

- prior:

  Numeric prior over objects (one per column of `lex`). Objects with
  prior 0 are dropped from the reasoning and get listener probability 0.

- ...:

  Further arguments to the recursion: `alpha` (speaker rationality /
  softmax temperature, default 3), `cost` (optional per-word production
  cost vector), and `depth` (pragmatic recursions beyond the literal
  level; `0` gives the prior-weighted literal listener, `1` the usual
  one-step pragmatic one).

## Value

`rsa_listener()`: a `(word x object)` matrix of `P(object | word)`.
`rsa_speaker()`: a `(word x object)` matrix of `P(word | object)`.

## Examples

``` r
# "wug" names either object; a pragmatic listener resolves it to the second
lex <- matrix(c(1, 0, 1, 1), 2, byrow = TRUE)
rsa_listener(lex, c(0.5, 0.5), alpha = 3, depth = 1)
#>      [,1] [,2]
#> [1,]  1.0  0.0
#> [2,]  0.1  0.9
```
