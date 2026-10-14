# Suanda, Mugwanya, & Namy (2014) cross-situational learning in children

Three conditions (low, medium, and high contextual diversity) from
Suanda, Mugwanya, & Namy (2014), *Cross-situational statistical word
learning in young children*, Journal of Experimental Child Psychology,
126, 395-411
([doi:10.1016/j.jecp.2014.06.003](https://doi.org/10.1016/j.jecp.2014.06.003)
): 8 word-object pairs, 2 words and 2 objects per training trial,
children aged 5-7.

## Usage

``` r
suanda2014
```

## Format

A named list of 3
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
sapply(suanda2014, \(d) d$accuracy[1])
#> Suanda2014-lowCD Suanda2014-medCD  Suanda2014-hiCD 
#>             0.34             0.39             0.48 
fit <- xsl_run(uncfam(X = .1, B = 2, C = .97), suanda2014[["Suanda2014-hiCD"]])
mean(fit$fits[[1]]$perf)
#> [1] 0.6294132
```
