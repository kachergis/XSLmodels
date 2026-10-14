# Koehne, Trueswell, & Gleitman (2013) multiple-meaning word learning

Four conditions from Koehne, Trueswell, & Gleitman (2013), *Multiple
proposal memory in observational word learning*, Proceedings of the 35th
Annual Conference of the Cognitive Science Society: 8 words, 1 word and
4 objects per trial, each word with one referent present whenever it is
heard and one present half the time; the order of trials with (P) and
without (A) the 50% referent is blocked (AAAPPP, PPPAAA) or alternating
(APAPAP, PAPAPA).

## Usage

``` r
koehne2013
```

## Format

A named list of 4
[xslData](https://www.kachergis.com/XSLmodels/reference/xslData-class.md)
objects, one per condition.

## Details

Only each condition's reported mean accuracy is available (no per-item
or per-participant data), so `accuracy` is that mean replicated across
items. Held out of
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md)
(see
[xsl_holdout_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_holdout_datasets.md)),
as in the original bake-off: use it to test whether a model fit
elsewhere generalizes, by comparing its mean predicted accuracy with the
reported mean.

## Examples

``` r
sapply(koehne2013, \(d) d$accuracy[1])
#> Koehne2013-aaappp Koehne2013-apapap Koehne2013-papapa Koehne2013-pppaaa 
#>            0.1795            0.2305            0.2305            0.3215 
```
