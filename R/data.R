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
