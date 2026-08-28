# Changelog

## XSLmodels 0.3.0

### New Features

- Added two new model variants of
  [`uncfam()`](https://kachergis.github.io/XSLmodels/reference/uncfam.md):
  [`uncfam_attention()`](https://kachergis.github.io/XSLmodels/reference/uncfam_attention.md)
  (BAM+Attention), which scales a trial’s learning rate by that trial’s
  object uncertainty relative to the running mean (implementing Fitneva
  & Christiansen 2015’s system-level attention account), and
  [`uncfam_predictive()`](https://kachergis.github.io/XSLmodels/reference/uncfam_predictive.md)
  (BAM+Prediction), which replaces the normalized update rule with an
  item-level, Rescorla-Wagner-style prediction-error term (floored at 0
  to prevent the unnormalized update from diverging over trials)
- Added two standalone datasets, documented but deliberately not
  appended to `xsl_datasets`: `kachergis2012_highlighting` (Kachergis,
  2012, CogSci) has an ambiguous test item with two legitimate target
  objects that doesn’t fit `xslData`’s one-correct-object-per-word
  convention, so its accuracy is left `NA` – appending it would silently
  poison every other dataset’s aggregate fit in
  [`get_group_model_fit()`](https://kachergis.github.io/XSLmodels/reference/get_group_model_fit.md)/[`get_crossvalidated_model_fit()`](https://kachergis.github.io/XSLmodels/reference/get_crossvalidated_model_fit.md),
  which sum/average SSE across all of `xsl_datasets`;
  `kachergis_initial_accuracy` (an unpublished MTurk initial-accuracy
  manipulation) has no such blocker but is kept standalone for now. See
  their [`?help`](https://rdrr.io/r/utils/help.html) pages and
  `data-raw/add_kachergis2012_highlighting.R`/`add_kachergis_initial_accuracy.R`
  for full construction details

## XSLmodels 0.2.0

### New Features

- Added helper functions
  [`show_models()`](https://kachergis.github.io/XSLmodels/reference/show_models.md),
  [`show_datasets()`](https://kachergis.github.io/XSLmodels/reference/show_datasets.md),
  [`get_group_model_fit()`](https://kachergis.github.io/XSLmodels/reference/get_group_model_fit.md),
  and
  [`get_crossvalidated_model_fit()`](https://kachergis.github.io/XSLmodels/reference/get_crossvalidated_model_fit.md)
  (now actually exported and working)
- Re-implemented k-fold cross-validation
  ([`cross_validated_group_fits()`](https://kachergis.github.io/XSLmodels/reference/cross_validated_group_fits.md),
  [`get_train_test_split()`](https://kachergis.github.io/XSLmodels/reference/get_train_test_split.md))
  for the current `xslMod`/`xslData` architecture
- Added 9 new conditions to `xsl_datasets` (44 -\> 53): 2 from
  Kachergis, Yu, & Shiffrin (2009) (temporal contiguity), 3 from Suanda,
  Mugwanya, & Namy (2014), and 4 from Koehne, Trueswell, & Gleitman
  (2013)

### Bug Fixes

- [`xslData()`](https://kachergis.github.io/XSLmodels/reference/xslData-class.md)
  no longer rejects data with no `response_matrix` supplied – this was
  breaking the vignette, most function examples, and
  [`get_example_ambiguous_condition()`](https://kachergis.github.io/XSLmodels/reference/get_example_ambiguous_condition.md)/[`get_example_unambiguous_condition()`](https://kachergis.github.io/XSLmodels/reference/get_example_unambiguous_condition.md)
- [`get_group_model_fit()`](https://kachergis.github.io/XSLmodels/reference/get_group_model_fit.md)/[`get_crossvalidated_model_fit()`](https://kachergis.github.io/XSLmodels/reference/get_crossvalidated_model_fit.md)
  called
  [`uncfam_sampling()`](https://kachergis.github.io/XSLmodels/reference/uncfam_sampling.md),
  [`multi_sampling()`](https://kachergis.github.io/XSLmodels/reference/multi_sampling.md),
  [`tilles()`](https://kachergis.github.io/XSLmodels/reference/tilles.md),
  and
  [`fazly()`](https://kachergis.github.io/XSLmodels/reference/fazly.md)
  with incorrect parameter names, causing them to error for those models
- [`update_params()`](https://kachergis.github.io/XSLmodels/reference/update_params.md)
  was dropping the parameter list’s names/type, which made
  [`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md)
  error for every model except
  [`bayesian_decay()`](https://kachergis.github.io/XSLmodels/reference/bayesian_decay.md)
  whenever it was called after fitting (e.g. from
  [`xsl_fit()`](https://kachergis.github.io/XSLmodels/reference/xsl_fit.md)
  or cross-validation)
- [`xsl_fit()`](https://kachergis.github.io/XSLmodels/reference/xsl_fit.md)
  now treats a model erroring or returning `NA`/`NaN` SSE for a given
  parameter draw as an infinitely bad fit instead of aborting the whole
  optimization run
- Fixed
  [`tilles()`](https://kachergis.github.io/XSLmodels/reference/tilles.md)
  mislabeling its model name as `"rescorla_wagner"`
- [`create_cooc_matrix()`](https://kachergis.github.io/XSLmodels/reference/create_cooc_matrix.md)
  test had an incorrect expected value
- [`plot_training_trials()`](https://kachergis.github.io/XSLmodels/reference/plot_training_trials.md)
  now gives an informative error if the optional `viridis`/`gganimate`
  packages aren’t installed, instead of a raw namespace-loading error
- [`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md)
  checked `!is.null(dat$test)` to decide whether to use
  [`mafc_test()`](https://kachergis.github.io/XSLmodels/reference/mafc_test.md),
  but
  [`xslData()`](https://kachergis.github.io/XSLmodels/reference/xslData-class.md)’s
  `test` defaults to [`list()`](https://rdrr.io/r/base/list.html) (not
  `NULL`), so any dataset built without explicit test trials – including
  the package’s own
  [`get_example_ambiguous_condition()`](https://kachergis.github.io/XSLmodels/reference/get_example_ambiguous_condition.md)/[`get_example_unambiguous_condition()`](https://kachergis.github.io/XSLmodels/reference/get_example_unambiguous_condition.md)
  – was silently routed through `mafc_test(mat, list())`. That returned
  `numeric(0)` rather than erroring (an R quirk: `1:0` counts down
  instead of being empty, and `vec[i] <- numeric(0)` truncates the
  vector rather than erroring), producing a false `sse = 0` “perfect
  fit” with no warning. Now checks `length(dat$test) > 0`.
- [`guess_and_test()`](https://kachergis.github.io/XSLmodels/reference/guess_and_test.md)
  had an inverted guard clause (`if (length(which(m[w,]==1))) next`)
  that made its hypothesis-disconfirmation step permanently unreachable,
  since it was only reached for words that already have a stored
  hypothesis (guaranteeing the guard’s condition was always true).
  Hypotheses were therefore never disconfirmed once formed, silently
  disabling a core part of the model (“…on later encounters, learners
  attempt to retrieve this hypothesis from memory and test it against a
  new context, updating it only if it is disconfirmed”). Found by
  comparing the package’s fits against the original pre-package research
  code’s fits for the same 44-condition dataset.

### Documentation

- Enhanced README with working examples
- Updated vignette with proper model categorization
- Added comprehensive function documentation
- Documented models and datasets
