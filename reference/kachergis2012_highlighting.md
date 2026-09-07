# Kachergis (2012) highlighting experiment data

A list of two `xslData` objects ("words as cues" and "objects as cues")
from the highlighting experiment in Kachergis (2012, CogSci), "Learning
Nouns with Domain-General Associative Learning Mechanisms". Training
trials are read directly from the real per-trial ordering file used for
the published experiment, not reconstructed from the paper's prose:
every trial is a single word-object pair (not a simultaneous joint-cue
trial), with two words each trained equally often (7x) with their own
object and with a shared object, one entering the schedule 9 trials
before the other – a primacy/recency design, not the blocked early/late
structure the paper's Figure 2 describes. No `accuracy` is set: both
ambiguous words have two legitimate targets, which doesn't fit
`xslData`'s one-correct-object-per-word convention, and forcing a number
on either would be arbitrary. Not included in
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md)
as a result – see `data-raw/add_kachergis2012_highlighting.R` for the
full construction rationale, and
`tests/bakeoff_comparison/kachergis2012_highlighting_fit.R` for how to
evaluate a model against this design (reading its predicted preference
for each word's own vs. shared target directly off the association
matrix, rather than via
`accuracy`/[`mafc_test()`](https://www.kachergis.com/XSLmodels/reference/mafc_test.md)).

## Usage

``` r
kachergis2012_highlighting
```

## Format

An object of class `list` of length 2.
