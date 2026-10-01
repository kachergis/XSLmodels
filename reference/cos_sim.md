# Cosine similarity of two vectors

Used by the distributed-memory models
([`minerva2()`](https://www.kachergis.com/XSLmodels/reference/minerva2.md),
[`todam()`](https://www.kachergis.com/XSLmodels/reference/todam.md)):
`a . b / (||a|| ||b||)`. Returns 0 (rather than `NaN`) when either
vector has zero norm, since that just means "no signal yet" for these
models (e.g. an empty memory trace before any training).

## Usage

``` r
cos_sim(a, b)
```

## Arguments

- a, b:

  Numeric vectors of the same length
