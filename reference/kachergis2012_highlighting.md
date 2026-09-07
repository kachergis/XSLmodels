# Kachergis (2012) highlighting experiment data

A list of two `xslData` objects ("words as cues" and "objects as cues")
from the highlighting experiment in Kachergis (2012, CogSci), "Learning
Nouns with Domain-General Associative Learning Mechanisms". Training
trials (genuine simultaneous 2-cue trials, matching the paper's Figure
2: 2 words + 1 object, or 2 objects + 1 word, per trial) are read
directly from the real per-trial ordering file for the real N=67 dataset
behind the paper's published statistics – confirmed by reproducing its
exact reported response proportions and chi-square values. See
`data-raw/add_kachergis2012_highlighting.R` for the full construction
rationale, including index relabeling needed to work around a
`uncfam_model()` indexing limitation.

## Usage

``` r
kachergis2012_highlighting
```

## Format

An object of class `list` of length 2.

## Details

Each condition replicates the classic 3-role highlighting structure (PE,
PL, I) twice. Words-as-cues' PL items have their real target at a
different index than their own word index, and its I items have two
legitimate targets – neither fits `xslData`'s one-correct-object-per-
word `accuracy` convention, so only its two PE items get a real accuracy
value (the rest are `NA`). Objects-as-cues has no such issue (every
tested word's own index is among its trained objects after relabeling),
so all 4 of its items have real accuracy. Because of the partial-NA
accuracy vector, this dataset is kept standalone rather than appended to
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md):
an `NA` would silently poison every other dataset's aggregate fit in
[`get_group_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_group_model_fit.md)/[`get_crossvalidated_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_crossvalidated_model_fit.md),
which sum SSE across all of `xsl_datasets`. See
`tests/bakeoff_comparison/kachergis2012_highlighting_fit.R` for a custom
scorer that fits a model against all four of the paper's reported
proportions (PE-E, PL-L, I-E, I-L) directly off the association matrix,
the way the paper's own (non-package) fitting procedure did.
