# Package index

## Models

Cross-situational word learning models

- [`baseline()`](https://kachergis.github.io/XSLmodels/reference/baseline.md)
  : Baseline model
- [`decay()`](https://kachergis.github.io/XSLmodels/reference/decay.md)
  : Decay model
- [`uncfam()`](https://kachergis.github.io/XSLmodels/reference/uncfam.md)
  : Kachergis 2012
- [`uncfam_sampling()`](https://kachergis.github.io/XSLmodels/reference/uncfam_sampling.md)
  : Kachergis 2012
- [`multi_sampling()`](https://kachergis.github.io/XSLmodels/reference/multi_sampling.md)
  : Multi-sampling associative model
- [`propose_but_verify()`](https://kachergis.github.io/XSLmodels/reference/propose_but_verify.md)
  : Trueswell et al. 2013 propose-but-verify model
- [`pursuit()`](https://kachergis.github.io/XSLmodels/reference/pursuit.md)
  : Stevens et al. 2014 pursuit model
- [`fazly()`](https://kachergis.github.io/XSLmodels/reference/fazly.md)
  : Fazly et al. 2010 probablistic associative model
- [`guess_and_test()`](https://kachergis.github.io/XSLmodels/reference/guess_and_test.md)
  : Guess and test model
- [`rescorla_wagner()`](https://kachergis.github.io/XSLmodels/reference/rescorla_wagner.md)
  : Rescorla-Wagner (1972) error-driven associative model
- [`tilles()`](https://kachergis.github.io/XSLmodels/reference/tilles.md)
  : Tilles and Fontanari (2013) reinforcement model
- [`bayesian_decay()`](https://kachergis.github.io/XSLmodels/reference/bayesian_decay.md)
  : Bayesian decay model

## Running and fitting models

- [`xsl_run()`](https://kachergis.github.io/XSLmodels/reference/xsl_run.md)
  : Run XSL model
- [`xsl_fit()`](https://kachergis.github.io/XSLmodels/reference/xsl_fit.md)
  : Fit XSL model using differential evolution
- [`update_params()`](https://kachergis.github.io/XSLmodels/reference/update_params.md)
  : Update parameters of xslMod
- [`get_group_model_fit()`](https://kachergis.github.io/XSLmodels/reference/get_group_model_fit.md)
  : Get group model fit for a specific model
- [`get_crossvalidated_model_fit()`](https://kachergis.github.io/XSLmodels/reference/get_crossvalidated_model_fit.md)
  : Get cross-validated model fit
- [`cross_validated_group_fits()`](https://kachergis.github.io/XSLmodels/reference/cross_validated_group_fits.md)
  : Perform Cross-Validation on Group Fits of a Model
- [`get_train_test_split()`](https://kachergis.github.io/XSLmodels/reference/get_train_test_split.md)
  : Split Data into Training and Testing Sets Based on Indices

## Datasets and data structures

- [`xsl_datasets`](https://kachergis.github.io/XSLmodels/reference/xsl_datasets.md)
  : Cross-situational word learning experiment data
- [`xslData()`](https://kachergis.github.io/XSLmodels/reference/xslData-class.md)
  [`new_xslData()`](https://kachergis.github.io/XSLmodels/reference/xslData-class.md)
  : xslData S3 class
- [`new_xslFit()`](https://kachergis.github.io/XSLmodels/reference/xslMod-class.md)
  [`xslMod()`](https://kachergis.github.io/XSLmodels/reference/xslMod-class.md)
  [`new_xslMod()`](https://kachergis.github.io/XSLmodels/reference/xslMod-class.md)
  : Constructor for xsl_model S3 class
- [`xslControl()`](https://kachergis.github.io/XSLmodels/reference/xslControl-class.md)
  [`new_xslControl()`](https://kachergis.github.io/XSLmodels/reference/xslControl-class.md)
  : xslControl S3 class
- [`xslFit()`](https://kachergis.github.io/XSLmodels/reference/xslFit-class.md)
  : xslFit S3 class
- [`get_example_ambiguous_condition()`](https://kachergis.github.io/XSLmodels/reference/get_example_ambiguous_condition.md)
  : Get example ambiguous condition
- [`get_example_unambiguous_condition()`](https://kachergis.github.io/XSLmodels/reference/get_example_unambiguous_condition.md)
  : Get example unambiguous condition
- [`show_datasets()`](https://kachergis.github.io/XSLmodels/reference/show_datasets.md)
  : Show available datasets in the package
- [`show_models()`](https://kachergis.github.io/XSLmodels/reference/show_models.md)
  : Show available models in the package
- [`create_cooc_matrix()`](https://kachergis.github.io/XSLmodels/reference/create_cooc_matrix.md)
  : Creates co-occurrence matrix from training trials

## Evaluating model performance

- [`get_perf()`](https://kachergis.github.io/XSLmodels/reference/get_perf.md)
  : Calculate Luce choice (proportion correct) for each item in a model
  knowledge matrix
- [`mafc_test()`](https://kachergis.github.io/XSLmodels/reference/mafc_test.md)
  : Evaluate m-alternative forced choice test
- [`get_tp()`](https://kachergis.github.io/XSLmodels/reference/get_tp.md)
  : Get true positives (TP), given a knowledge matrix and a
  gold-standard lexicon
- [`get_fscore()`](https://kachergis.github.io/XSLmodels/reference/get_fscore.md)
  : Calculate F-score, precision, recall, and specificity for a
  knowledge matrix at a given threshold
- [`get_roc()`](https://kachergis.github.io/XSLmodels/reference/get_roc.md)
  [`plot_roc()`](https://kachergis.github.io/XSLmodels/reference/get_roc.md)
  [`get_roc_max()`](https://kachergis.github.io/XSLmodels/reference/get_roc.md)
  : Calculate receiver operating characteristic (ROC) scores for a model
  association Matrix
- [`plot_training_trials()`](https://kachergis.github.io/XSLmodels/reference/plot_training_trials.md)
  : Create Animated Plot of Co-Occurrence Matrix from Training Trials
