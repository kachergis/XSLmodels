# Changelog

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

### Documentation

- Enhanced README with working examples
- Updated vignette with proper model categorization
- Added comprehensive function documentation
- Documented models and datasets
