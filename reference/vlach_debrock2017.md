# Vlach & DeBrock (2017) spaced/massed repetition experiment data

An
[xslData](https://www.kachergis.com/XSLmodels/reference/xslData-class.md)
object from Vlach, H. A., & DeBrock, C. A. (2017). "Remember dax?
Relations between children's cross-situational word learning, memory,
and language abilities." *Journal of Memory and Language, 93*, 217-230.
12 to-be-learned word-object pairs (symmetric design: word `i` pairs
with object `i`), 2 pairs per training trial, 36 trials; pairs vary in
whether their 6 repetitions fall on consecutive trials (massed) or are
spread out (spaced), the paper's memory/spacing manipulation. `n_subj`
is 47 children (ages 2-5).

## Usage

``` r
vlach_debrock2017
```

## Format

An object of class `xslData` (inherits from `list`) of length 8.

## Details

No per-word accuracy is available from this dataset, only the paper's
overall mean across all pairs and participants (accuracy = .5583, sd =
.1975; see `data-raw/XSL-dataset-fields.csv`), so `accuracy` is `NA`
throughout. **Not included in
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md)**
and not scorable by
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)'s
built-in per-item SSE – usable for running/fitting models and comparing
simulated performance against the one real number this paper reports.
See `data-raw/add_vlach_debrock2017.R` for construction details.

## Examples

``` r
m <- xsl_run(baseline(), vlach_debrock2017)$fits[[1]]$matrix
dim(m)
#> [1] 12 12
```
