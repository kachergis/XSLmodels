# Kachergis, Grimmick, & Gureckis initial accuracy experiment data

A list of two `xslData` objects ("High Initial Accuracy" and "Low
Initial Accuracy") from an unpublished MTurk cross-situational word
learning experiment manipulating how many of 18 words are first shown
("familiarized") with a different object than the one they are then
cross-situationally studied with. The study pairing is the test-correct
one, and is on the diagonal (`m[w, w]`); an initially inaccurate word's
familiarization object is off-diagonal. The first 18 training trials are
the familiarization phase. Not included in
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md)
(see `data-raw/add_kachergis_initial_accuracy.R` for construction
details and `tests/bakeoff_comparison/kachergis_initial_accuracy_fit.R`
for an example model comparison).

## Usage

``` r
kachergis_initial_accuracy
```

## Format

An object of class `list` of length 2.
