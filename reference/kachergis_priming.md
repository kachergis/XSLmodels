# Kachergis, Yu & Shiffrin priming experiments: 4-block learning trajectories

Three experiments run alongside Kachergis, Yu & Shiffrin's frequency and
contextual-diversity studies. Participants studied the same
cross-situational training sequence four times, taking an 18AFC test on
all 18 words after each repetition. Each condition is therefore a
multi-block
[xslData](https://www.kachergis.com/XSLmodels/reference/xslData-class.md):
`train` is one block, `accuracy` is a 4 x 18 matrix (one row per block),
and `response_matrix` is a list of four 18 x 18 confusion matrices.
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
scores block `b` on a model trained on `b` repetitions of the sequence,
so a model's learning trajectory can be compared with people's.

## Usage

``` r
kachergis_priming
```

## Format

An object of class `list` of length 3.

## Details

A named list of three conditions:

- `"4x4 6x"`: 4 words and 4 objects per trial, 27 trials, every pair 6
  times (n = 54). The sequence is that of the original 4x4 condition,
  which differs from `xsl_datasets` condition `223` only in words 4 and
  16 trading places between trials 11 and 24.

- `"4x4 3/6/9x"`: 4 words and 4 objects per trial, 27 trials, 6 pairs
  each 3, 6 and 9 times (n = 44); the training of `3_x8_369_4x4`.

- `"3x3 3/6/9x"`: 3 words and 3 objects per trial, 36 trials, 6 pairs
  each 3, 6 and 9 times with low contextual diversity (n = 66); the
  training of `freq369-3x3loCD`.

The participants are different from those in `xsl_datasets` (until
XSLmodels PR \#30, their first block had been pooled into `3_x8_369_4x4`
and `freq369-3x3loCD`). Participants whose accuracy across all four
blocks was below 2/18 were excluded in the original analysis. Not
included in
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md);
see `data-raw/add_kachergis_priming.R` for construction details.

## Examples

``` r
d <- kachergis_priming[["4x4 6x"]]
d
#> xslData object with label "priming 4x4 6x" and condition "4x4 6x"
#>   training trials: 27
#>       test trials: 0
#>             words: 18
#>           objects: 18
#>            blocks: 4 (training repeated, tested after each)
#>        accuracies: 72
#>          subjects: 54
rowMeans(d$accuracy)   # people's learning curve
#>    block1    block2    block3    block4 
#> 0.1841564 0.3652263 0.5514403 0.6965021 
fit <- xsl_run(uncfam(X = .2, B = 3, C = .97), d)
rowMeans(fit$fits[[1]]$perf)   # the model's
#> [1] 0.4364966 0.6848688 0.8194688 0.8945512
```
