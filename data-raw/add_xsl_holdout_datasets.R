# Build a registry of every dataset in the package that is NOT part of
# xsl_datasets, and therefore never seen by get_group_model_fit()/
# get_crossvalidated_model_fit() -- i.e. everything available for testing a
# fitted model's generalization to genuinely held-out data.
#
# This is a lookup table (object name -> how to score it), not a copy of the
# data itself: the datasets already exist as their own package objects, and
# duplicating them here would just double their storage in the installed
# package. Scoring methods vary because the whole reason most of these are
# standalone is some structural mismatch with xslData's one-correct-object-
# per-word / diagonal-scoring convention -- see each dataset's own ?help page
# (and its data-raw/add_*.R) for the full rationale.
#
# Run from the package root: Rscript data-raw/add_xsl_holdout_datasets.R

devtools::load_all(quiet = TRUE)

xsl_holdout_datasets <- list(
  benitez2020 = list(
    scorable = TRUE,
    scoring = "xsl_run() + mafc_test() / get_perf() (standard diagonal scoring)",
    note = "Real per-word accuracy; structurally could join xsl_datasets but kept out deliberately as generalization-test data."
  ),
  kachergis_initial_accuracy = list(
    scorable = TRUE,
    scoring = "xsl_run() + mafc_test() / get_perf() (standard diagonal scoring)",
    note = "Kept standalone for the initial-accuracy manipulation's own analysis, not a structural issue."
  ),
  vlach_debrock2017 = list(
    scorable = FALSE,
    scoring = "No per-item accuracy; compare simulated performance only to the source's overall mean (.5583, sd .1975)",
    note = "accuracy is NA for every word."
  ),
  kachergis2012_highlighting = list(
    scorable = FALSE,
    scoring = "score with tests/bakeoff_comparison/kachergis2012_highlighting_fit.R's custom scorer",
    note = "Partial-NA accuracy; some items have 2 legitimate targets."
  ),
  gangwani2011_category = list(
    scorable = FALSE,
    scoring = "score_gangwani2011_category() in tests/bakeoff_comparison/gangwani2011_category_fit.R",
    note = "15 x 12 (word x object) association matrix -- non-square, breaks diagonal scoring for the category-label words."
  ),
  rollins_corpus = list(
    scorable = FALSE,
    scoring = "get_fscore() / get_roc() / get_roc_max() against $gold (no human accuracy data at all, a naturalistic corpus)",
    note = "Access via rollins_corpus$data."
  ),
  fm_corpus = list(
    scorable = FALSE,
    scoring = "get_fscore() / get_roc() / get_roc_max() against $gold or $gold_variants (no human accuracy data at all, a naturalistic corpus)",
    note = "Access via fm_corpus$data."
  )
)

usethis::use_data(xsl_holdout_datasets, overwrite = TRUE)
