# XSLmodels 0.2.0

## New Features
- Added helper functions `show_models()`, `show_datasets()`, `get_group_model_fit()`, and `get_crossvalidated_model_fit()` (now actually exported and working)
- Re-implemented k-fold cross-validation (`cross_validated_group_fits()`, `get_train_test_split()`) for the current `xslMod`/`xslData` architecture

## Bug Fixes
- `xslData()` no longer rejects data with no `response_matrix` supplied -- this was breaking the vignette, most function examples, and `get_example_ambiguous_condition()`/`get_example_unambiguous_condition()`
- `get_group_model_fit()`/`get_crossvalidated_model_fit()` called `uncfam_sampling()`, `multi_sampling()`, `tilles()`, and `fazly()` with incorrect parameter names, causing them to error for those models
- `update_params()` was dropping the parameter list's names/type, which made `xsl_run()` error for every model except `bayesian_decay()` whenever it was called after fitting (e.g. from `xsl_fit()` or cross-validation)
- `xsl_fit()` now treats a model erroring or returning `NA`/`NaN` SSE for a given parameter draw as an infinitely bad fit instead of aborting the whole optimization run
- Fixed `tilles()` mislabeling its model name as `"rescorla_wagner"`
- `create_cooc_matrix()` test had an incorrect expected value
- `plot_training_trials()` now gives an informative error if the optional `viridis`/`gganimate` packages aren't installed, instead of a raw namespace-loading error

## Documentation
- Enhanced README with working examples
- Updated vignette with proper model categorization
- Added comprehensive function documentation
- Documented models and datasets