# Gangwani, Kachergis & Yu simultaneous category + object name learning

Experiment 1 of Gangwani, Kachergis & Yu, "Simultaneous
Cross-situational Learning of Category and Object Names" (an
undergraduate-journal version is Gangwani & Kachergis, *Indiana
Undergraduate Journal of Cognitive Science* 5, 2010). A **hierarchical**
cross-situational design: on every training trial the learner sees 2
objects and hears 3 words – the two objects' own 1-to-1 names *plus* one
1-to-many "category" label that names those two objects and two further
objects seen on other trials. Every object is thus part of a 2-to-1
word-referent mapping (a basic name and a category label), and the
paper's finding is that learners violate mutual exclusivity: they learn
*both* labels for the same object.

## Usage

``` r
gangwani2011_category
```

## Format

An object of class `list` of length 3.

## Details

A named list of three
[xslData](https://www.kachergis.com/XSLmodels/reference/xslData-class.md)
objects, one per training block – `"block 1 (ad hoc)"`,
`"block 2 (natural)"`, `"block 3 (ad hoc)"` – run in that fixed order.
All three share the same 36-trial co-occurrence schedule (only the
stimuli differ); each carries its own block-specific human data.

Per object: `train$words[[t]]` is `c(name_i, name_j, category_label)`
and `train$objects[[t]]` is `c(i, j)`. Words 1-12 are object names (name
`i` pairs with object `i`); words 13-15 are the category labels
(13/14/15 name objects 1-4 / 5-8 / 9-12). `accuracy` is per-word human
P(correct referent \| word), length 15: positions 1-12 are 12-AFC name
accuracy, positions 13-15 are 3-AFC category-membership accuracy (n = 34
canonical-block-order participants; the paper reports 33, dropping one
more low performer). `response_matrix` (15 x 12, row-normalised) is the
full human choice distribution including the category labels. `test`
holds only the 12 name trials (12-AFC).

**Not part of
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md)**,
and not scorable by the built-in
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
SSE: the association matrix is 15 words x 12 objects, so
[`get_perf()`](https://www.kachergis.com/XSLmodels/reference/get_perf.md)
/
[`mafc_test()`](https://www.kachergis.com/XSLmodels/reference/mafc_test.md)
(which score via the diagonal `m[w, w]`) cover only words 1-12. Score
the category labels – and the ME-violation measure that is the paper's
point – with `score_gangwani2011_category()` in
`tests/bakeoff_comparison/gangwani2011_category_fit.R`. The
natural-category feature-similarity effect (block 2) and generalization
to novel objects are out of scope for the package's amodal, index-based
models and are not represented. See
`data-raw/add_gangwani2011_category.R` for construction details.

## Examples

``` r
block1 <- gangwani2011_category[["block 1 (ad hoc)"]]
# 15 x 12 matrix; suppressWarnings() because get_perf()'s diagonal rule
# warns on the non-square matrix (the built-in SSE is not meaningful here).
m <- suppressWarnings(xsl_run(baseline(), block1))$fits[[1]]$matrix
mafc_test(m, block1$test)                           # names (words 1-12) only
#>  [1] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
```
