#' Cross-situational word learning experiment data
"xsl_datasets"

#' Kachergis (2012) highlighting experiment data
#'
#' A list of two `xslData` objects ("words as cues" and "objects as cues")
#' from the highlighting experiment in Kachergis (2012, CogSci), "Learning
#' Nouns with Domain-General Associative Learning Mechanisms". Not included
#' in [xsl_datasets]: the "words as cues" condition's ambiguous "I" item has
#' two legitimate target objects with different empirical response rates,
#' which doesn't fit `xslData`'s one-correct-object-per-word convention (its
#' accuracy is left `NA`) -- see `data-raw/add_kachergis2012_highlighting.R`
#' for details, and
#' `tests/bakeoff_comparison/kachergis2012_highlighting_fit.R` for a custom
#' scoring function that fits all of the paper's reported response
#' proportions (including the "I" item's early/late split) directly.
"kachergis2012_highlighting"

#' Kachergis, Grimmick, & Gureckis initial accuracy experiment data
#'
#' A list of two `xslData` objects ("High Initial Accuracy" and "Low Initial
#' Accuracy") from an unpublished MTurk cross-situational word learning
#' experiment manipulating how many of 18 word-object pairs are "switched"
#' (i.e. studied with the wrong partner) relative to an initial
#' familiarization phase. Not included in [xsl_datasets] (see
#' `data-raw/add_kachergis_initial_accuracy.R` for construction details and
#' `tests/bakeoff_comparison/kachergis_initial_accuracy_fit.R` for an example
#' model comparison).
"kachergis_initial_accuracy"
