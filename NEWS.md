# XSLmodels 0.2.0

## New Features
- Added helper functions `show_models()`, `show_datasets()`, `get_group_model_fit()`, and `get_crossvalidated_model_fit()` (now actually exported and working)
- Re-implemented k-fold cross-validation (`cross_validated_group_fits()`, `get_train_test_split()`) for the current `xslMod`/`xslData` architecture
- Added 9 new conditions to `xsl_datasets` (44 -> 53): 2 from Kachergis, Yu, & Shiffrin (2009) (temporal contiguity), 3 from Suanda, Mugwanya, & Namy (2014), and 4 from Koehne, Trueswell, & Gleitman (2013)

## Bug Fixes
- `xslData()` no longer rejects data with no `response_matrix` supplied -- this was breaking the vignette, most function examples, and `get_example_ambiguous_condition()`/`get_example_unambiguous_condition()`
- `get_group_model_fit()`/`get_crossvalidated_model_fit()` called `uncfam_sampling()`, `multi_sampling()`, `tilles()`, and `fazly()` with incorrect parameter names, causing them to error for those models
- `update_params()` was dropping the parameter list's names/type, which made `xsl_run()` error for every model except `bayesian_decay()` whenever it was called after fitting (e.g. from `xsl_fit()` or cross-validation)
- `xsl_fit()` now treats a model erroring or returning `NA`/`NaN` SSE for a given parameter draw as an infinitely bad fit instead of aborting the whole optimization run
- Fixed `tilles()` mislabeling its model name as `"rescorla_wagner"`
- `create_cooc_matrix()` test had an incorrect expected value
- `plot_training_trials()` now gives an informative error if the optional `viridis`/`gganimate` packages aren't installed, instead of a raw namespace-loading error
- `xsl_run()` checked `!is.null(dat$test)` to decide whether to use `mafc_test()`, but `xslData()`'s `test` defaults to `list()` (not `NULL`), so any dataset built without explicit test trials -- including the package's own `get_example_ambiguous_condition()`/`get_example_unambiguous_condition()` -- was silently routed through `mafc_test(mat, list())`. That returned `numeric(0)` rather than erroring (an R quirk: `1:0` counts down instead of being empty, and `vec[i] <- numeric(0)` truncates the vector rather than erroring), producing a false `sse = 0` "perfect fit" with no warning. Now checks `length(dat$test) > 0`.
- `guess_and_test()` had an inverted guard clause (`if (length(which(m[w,]==1))) next`) that made its hypothesis-disconfirmation step permanently unreachable, since it was only reached for words that already have a stored hypothesis (guaranteeing the guard's condition was always true). Hypotheses were therefore never disconfirmed once formed, silently disabling a core part of the model ("...on later encounters, learners attempt to retrieve this hypothesis from memory and test it against a new context, updating it only if it is disconfirmed"). Found by comparing the package's fits against the original pre-package research code's fits for the same 44-condition dataset.

## Documentation
- Enhanced README with working examples
- Updated vignette with proper model categorization
- Added comprehensive function documentation
- Documented models and datasets