# Benitez et al. (2020) temporal-structure-of-naming experiment data

A named list of 7
[xslData](https://www.kachergis.com/XSLmodels/reference/xslData-class.md)
objects from Benitez, V. L., et al. (2020), "The temporal structure of
naming events differentially affects children's and adults'
cross-situational word learning" (data/materials:
<https://osf.io/2hmxr/>): one per condition (Interleaved, Massed,
Unstructured) x age-group (kids, adults) combination, except
Unstructured's first training order, which only has child data.
Unstructured has two training orders; its second and third were
confirmed identical by the original extraction and are pooled as "order
2/3".

## Usage

``` r
benitez2020
```

## Format

An object of class `list` of length 7.

## Details

8 symmetric word-object pairs (word `i` pairs with object `i`), 2 pairs
per training trial, 12 trials, followed by an 8-item 2AFC test.
`accuracy` and `response_matrix` are real per-word 2AFC accuracy and raw
response counts for that condition/age-group combination (not an
estimate) – structurally this could join
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md),
but is kept standalone deliberately as held-out generalization-test
data; see `xsl_holdout_datasets`.
`data-raw/Benitez2020-rep/Benitez2020_XSLdata_extraction.R` extracts the
raw OSF data into training orders, 2AFC test trials, and response
matrices; `data-raw/add_benitez2020.R` builds the `xslData` objects from
those.

## Examples

``` r
benitez2020[["Interleaved, kids"]]$accuracy
#>         1         2         3         4         5         6         7         8 
#> 0.6739130 0.7173913 0.5434783 0.5869565 0.6521739 0.5869565 0.7173913 0.5000000 
m <- xsl_run(baseline(), benitez2020[["Massed, adults"]])$fits[[1]]$matrix
mafc_test(m, benitez2020[["Massed, adults"]]$test)
#> [1] 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75
```
