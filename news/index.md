# Changelog

## XSLmodels 0.3.0

### New Features

- Added two new model variants of
  [`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md):
  [`uncfam_attention()`](https://www.kachergis.com/XSLmodels/reference/uncfam_attention.md)
  (BAM+Attention), which scales a trial’s learning rate by that trial’s
  object uncertainty relative to the running mean (implementing Fitneva
  & Christiansen 2015’s system-level attention account), and
  [`uncfam_predictive()`](https://www.kachergis.com/XSLmodels/reference/uncfam_predictive.md)
  (BAM+Prediction), which replaces the normalized update rule with an
  item-level, Rescorla-Wagner-style prediction-error term (floored at 0
  to prevent the unnormalized update from diverging over trials)
- Added two standalone datasets, documented but deliberately not
  appended to `xsl_datasets`: `kachergis2012_highlighting` (Kachergis,
  2012, CogSci) has an ambiguous test item with two legitimate target
  objects that doesn’t fit `xslData`’s one-correct-object-per-word
  convention, so its accuracy is left `NA` – appending it would silently
  poison every other dataset’s aggregate fit in
  [`get_group_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_group_model_fit.md)/[`get_crossvalidated_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_crossvalidated_model_fit.md),
  which sum/average SSE across all of `xsl_datasets`;
  `kachergis_initial_accuracy` (an unpublished MTurk initial-accuracy
  manipulation) has no such blocker but is kept standalone for now. See
  their [`?help`](https://rdrr.io/r/utils/help.html) pages and
  `data-raw/add_kachergis2012_highlighting.R`/`add_kachergis_initial_accuracy.R`
  for full construction details
- Added
  [`kalman_filter()`](https://www.kachergis.com/XSLmodels/reference/kalman_filter.md),
  a Kalman-filter generalization of
  [`rescorla_wagner()`](https://www.kachergis.com/XSLmodels/reference/rescorla_wagner.md)’s
  error-driven update: each word-object association is tracked as a
  Gaussian belief (mean and variance), giving an adaptive learning rate
  that starts high under uncertainty and shrinks with confidence, rather
  than a fixed rate (Dayan & Kakade, 2000; Kruschke, 2008)
- Added
  [`softmax_rl()`](https://www.kachergis.com/XSLmodels/reference/softmax_rl.md),
  a Q-learning-style reinforcement learning model. Unlike every other
  model in the package (which updates every word-object pair presented
  together on a trial), it makes a discrete choice per word – sampling a
  candidate referent from a softmax policy over its current values, then
  updating only that guess via a TD/delta-rule reward (1 if the guess
  was present in the scene, 0 otherwise). This mirrors
  [`propose_but_verify()`](https://www.kachergis.com/XSLmodels/reference/propose_but_verify.md)/[`pursuit()`](https://www.kachergis.com/XSLmodels/reference/pursuit.md)’s
  propose-and-verify logic but replaces their threshold-based
  keep/discard rules with graded value learning and explicit softmax
  exploration
- Added two naturalistic corpora imported from the wurwur package
  (github.com/mcfrank/wurwur), as standalone objects (not appended to
  `xsl_datasets`, which is scored by SSE against a human accuracy vector
  these don’t have): `rollins_corpus` – the CHILDES/Rollins corpus the
  Frank, Goodman & Tenenbaum (2009) model was fit to (619 mother-infant
  utterances, 416 words, 22 objects) – and `fm_corpus` – the larger
  Frank, Tenenbaum & Fernald corpus (4763 utterances, 1122 words, 30
  objects), which additionally carries the speaker’s hand-coded
  referential intention per utterance (`fm_corpus$intents`). Each is a
  list bundling the training `xslData` (`$data`) with a gold-standard
  lexicon (`$gold`, plus `$gold_variants` for FM) to be scored with
  [`get_fscore()`](https://www.kachergis.com/XSLmodels/reference/get_fscore.md)/[`get_roc()`](https://www.kachergis.com/XSLmodels/reference/get_roc.md)/[`get_tp()`](https://www.kachergis.com/XSLmodels/reference/get_tp.md)
  rather than SSE. Gaze/hand attentional cues in the FM source are not
  imported. See `data-raw/add_wurwur_corpora.R`
- Added
  [`predict_referent()`](https://www.kachergis.com/XSLmodels/reference/predict_referent.md),
  a model-agnostic trial-level prediction: given any model’s word-object
  matrix, a heard word, and the objects present, it returns the
  probability distribution over those objects
  (`P(object | word) proportional to m[word, object] * prior(object)`,
  normalized over the objects present) – the quantity to compare against
  a participant’s choice on a referent-selection trial. Supports a
  non-uniform `prior` (set an entry to 0 to remove an object the speaker
  has no epistemic access to) and a `pragmatic = TRUE` mode that
  resolves the objects with a one-step Rational Speech Act pragmatic
  listener
  ([`rsa_listener()`](https://www.kachergis.com/XSLmodels/reference/rsa_listener.md)/[`rsa_speaker()`](https://www.kachergis.com/XSLmodels/reference/rsa_listener.md),
  also exported), yielding strong mutual exclusivity when a heard word
  is lexically ambiguous and a competitor word names one of the
  candidates.
  [`mafc_test()`](https://www.kachergis.com/XSLmodels/reference/mafc_test.md)
  is now a thin wrapper over it (behavior unchanged; gains a `...`
  passthrough for `pragmatic`/`threshold`)
- Added
  [`fgt2009()`](https://www.kachergis.com/XSLmodels/reference/fgt2009.md)
  and
  [`fgt2009_rsa()`](https://www.kachergis.com/XSLmodels/reference/fgt2009_rsa.md),
  a pure-R port of the intentional Bayesian word-learning model of
  Frank, Goodman & Tenenbaum (2009) and its Rational Speech Act (RSA)
  pragmatic extension, from the Python package `wordlearn`
  (github.com/mcfrank/wurwur). Unlike every other model in the package –
  which tally word-object associations incrementally, trial by trial –
  this is a **batch** model: it does joint MCMC inference (blocked
  per-object Gibbs plus MI-biased single-edge Metropolis moves) over the
  whole corpus at once, asking which lexicon best explains *all* the
  data, so `xslFit$matrix` holds posterior edge marginals
  `P(word names object | corpus)` rather than a running tally. It
  therefore has no learning trajectory and ignores `control$reps`. The
  scoring kernel is verified to match the Python reference to numerical
  precision. `gamma`/`kappa` default to the values used for the
  analogous cross-situational simulation in the source package
  (`gamma = 1`, since every word in a controlled XSL experiment is a
  referential label). The lexicon-size prior `alpha` scales with corpus
  size and its fit-vs-`alpha` curve is single-peaked, so it is not wired
  into `xsl_model_registry()`’s DEoptim bounds like the other models;
  use the new
  [`fgt2009_sweep_alpha()`](https://www.kachergis.com/XSLmodels/reference/fgt2009_sweep_alpha.md)
  helper instead (which takes an optional `gold` argument to report the
  best-threshold F-score against a gold lexicon at each `alpha`, for
  corpora with no human accuracy vector)
- Added a vignette, “Running and fitting models on naturalistic
  corpora”, covering how to run any model on
  `rollins_corpus`/`fm_corpus`, score the learned matrix against a gold
  lexicon with
  [`get_fscore()`](https://www.kachergis.com/XSLmodels/reference/get_fscore.md)/[`get_roc()`](https://www.kachergis.com/XSLmodels/reference/get_roc.md)/[`get_roc_max()`](https://www.kachergis.com/XSLmodels/reference/get_roc.md),
  and choose
  [`fgt2009()`](https://www.kachergis.com/XSLmodels/reference/fgt2009.md)’s
  `alpha` by sweeping. It ends with a quick model comparison
  (`bayesian_decay` and `fazly` recover the gold lexicon best on both
  corpora, with `fgt2009` close behind and highest-precision). The
  comparison script and its outputs are in
  `tests/bakeoff_comparison/corpus_gold_comparison.R` (every model at
  registry defaults); a companion, `corpus_gold_fits.R`, fits each
  model’s parameters to each corpus (parallel DEoptim maximizing
  gold-lexicon F, since these corpora have no accuracy vector;
  [`fgt2009()`](https://www.kachergis.com/XSLmodels/reference/fgt2009.md)
  by an `alpha` sweep). Fitting lifts every model – dramatically for the
  ones whose registry defaults were tuned on controlled experiments
  (`tilles` +0.34, `uncfam_predictive` +0.15/+0.32) – and
  `bayesian_decay` fit to the FM corpus reaches F = 0.56.
  `tests/bakeoff_comparison/corpus_report.md` is a rendered write-up of
  all of this

### Other changes

- [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  is now far more memory-frugal on long inputs (corpora). Previously it
  built the full list of every simulation’s `xslFit` – each carrying a
  word-by-object matrix *per training trial* – and only reduced them at
  the end, so a stochastic model on the 4763-utterance FM corpus at
  `n_sim = 50` needed tens of GB and simply failed. Now: (1) the
  per-trial trajectory (`xslFit$traj`) is only recorded when
  `xslControl(keep_traj = TRUE)` – it is off by default and is not used
  by any function in the package; (2)
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  accumulates the summed matrix one simulation at a time rather than
  holding all `n_sim` at once; (3) the full per-simulation list
  (`fits[[i]]$sims`) is dropped unless `xslControl(keep_sims = TRUE)`,
  replaced by `fits[[i]]$responses`, a compact `n_sim` x n-words matrix
  of each simulated participant’s final per-word accuracy. `matrix`,
  `perf`, and `sse` are numerically unchanged.

### Bug Fixes

- [`guess_and_test()`](https://www.kachergis.com/XSLmodels/reference/guess_and_test.md)
  errored (`if` with a zero- or multiple-length condition at the
  disconfirmation step) on any utterance that repeats a word – common in
  a naturalistic corpus, impossible in a controlled trial – because the
  repeated word was visited twice and the first pass could clear or
  double-set its hypothesis. It now de-duplicates a trial’s words and
  objects. Its disconfirmation check also compared a column *position*
  to the object *labels* (only equivalent when objects are labelled
  `1..N`), and it applied new hypotheses to `need_hypoths[w]` rather
  than the actual stored word `store[w]` (setting hypotheses for the
  wrong words whenever only a subset rolled below the storage
  threshold); both are fixed.
- [`tilles()`](https://www.kachergis.com/XSLmodels/reference/tilles.md)
  coerced each trial’s words/objects with
  [`as.integer()`](https://rdrr.io/r/base/integer.html) (it indexes its
  state by position), turning any non-integer label into `NA` and
  leaving the matrix all-zero; it also never set `dimnames` on its
  matrix, so the result could not be scored against a gold lexicon by
  label. It now maps labels to positions with
  [`match()`](https://rdrr.io/r/base/match.html) and names its matrix.
  `xsl_datasets` results are unchanged (the labels there are already
  `1..N`).
- [`softmax_rl()`](https://www.kachergis.com/XSLmodels/reference/softmax_rl.md)
  checked its sampled guess against the trial’s objects with
  `proposal %in% tr_o`, comparing a column *position* (`1..ref_sz`)
  against the objects’ *labels*. For `xsl_datasets` the object labels
  are `1..N` so it happened to work, but on any dataset whose objects
  have non-integer (or non-contiguous) labels – e.g. either bundled
  corpus – every reward was 0 and the learned Q-matrix stayed
  identically zero. It now maps the objects present to positions
  (`match(tr_o, ref)`) before the check, and skips a trial with no
  objects present. `xsl_datasets` results are unchanged.
- [`uncfam_sampling()`](https://www.kachergis.com/XSLmodels/reference/uncfam_sampling.md)
  and
  [`multi_sampling()`](https://www.kachergis.com/XSLmodels/reference/multi_sampling.md)
  errored ([`sample()`](https://rdrr.io/r/base/sample.html): “too few
  positive probabilities”) on any dataset containing an utterance with
  no objects present – e.g. the ~320 non-referential utterances in
  `fm_corpus`, which made both models unable to run on a naturalistic
  corpus. Such a trial now leaves the association matrix unchanged apart
  from decay (as
  [`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
  already did), by skipping the per-word sampling when its weights are
  all zero and only applying the normalized update when something was
  sampled. Behaviour on datasets without objectless trials (all of
  `xsl_datasets`) is unchanged.
- [`tilles()`](https://www.kachergis.com/XSLmodels/reference/tilles.md)
  was essentially unfittable – three separate bugs (not the single one
  originally suspected) made a quick DEoptim search return `Inf` for
  every draw: (1) its `alpha` term was recomputed fresh every trial
  instead of persisting per-word across trials, so words absent from the
  current trial (needed by the “other old words” update) always had
  `alpha = NA`; (2) an internal matrix was sized `voc_sz x voc_sz`
  instead of `voc_sz x ref_sz`, a copy-paste bug that only breaks on
  asymmetric datasets (word count != object count); (3) a flux
  computation divided by a quantity that can legitimately be zero (a
  previously-seen word paired with an entirely new-to-it set of
  objects), producing `Inf * 0 = NaN`. One dataset failed on 100% of a
  40-draw sweep regardless of parameters before these fixes; after them,
  a 100-draw sweep shows a residual ~2% failure rate confined to extreme
  parameter-space tails, on par with other models’ known numerical edges

## XSLmodels 0.2.0

### New Features

- Added helper functions
  [`show_models()`](https://www.kachergis.com/XSLmodels/reference/show_models.md),
  [`show_datasets()`](https://www.kachergis.com/XSLmodels/reference/show_datasets.md),
  [`get_group_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_group_model_fit.md),
  and
  [`get_crossvalidated_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_crossvalidated_model_fit.md)
  (now actually exported and working)
- Re-implemented k-fold cross-validation
  ([`cross_validated_group_fits()`](https://www.kachergis.com/XSLmodels/reference/cross_validated_group_fits.md),
  [`get_train_test_split()`](https://www.kachergis.com/XSLmodels/reference/get_train_test_split.md))
  for the current `xslMod`/`xslData` architecture
- Added 9 new conditions to `xsl_datasets` (44 -\> 53): 2 from
  Kachergis, Yu, & Shiffrin (2009) (temporal contiguity), 3 from Suanda,
  Mugwanya, & Namy (2014), and 4 from Koehne, Trueswell, & Gleitman
  (2013)

### Bug Fixes

- [`xslData()`](https://www.kachergis.com/XSLmodels/reference/xslData-class.md)
  no longer rejects data with no `response_matrix` supplied – this was
  breaking the vignette, most function examples, and
  [`get_example_ambiguous_condition()`](https://www.kachergis.com/XSLmodels/reference/get_example_ambiguous_condition.md)/[`get_example_unambiguous_condition()`](https://www.kachergis.com/XSLmodels/reference/get_example_unambiguous_condition.md)
- [`get_group_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_group_model_fit.md)/[`get_crossvalidated_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_crossvalidated_model_fit.md)
  called
  [`uncfam_sampling()`](https://www.kachergis.com/XSLmodels/reference/uncfam_sampling.md),
  [`multi_sampling()`](https://www.kachergis.com/XSLmodels/reference/multi_sampling.md),
  [`tilles()`](https://www.kachergis.com/XSLmodels/reference/tilles.md),
  and
  [`fazly()`](https://www.kachergis.com/XSLmodels/reference/fazly.md)
  with incorrect parameter names, causing them to error for those models
- [`update_params()`](https://www.kachergis.com/XSLmodels/reference/update_params.md)
  was dropping the parameter list’s names/type, which made
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  error for every model except
  [`bayesian_decay()`](https://www.kachergis.com/XSLmodels/reference/bayesian_decay.md)
  whenever it was called after fitting (e.g. from
  [`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)
  or cross-validation)
- [`xsl_fit()`](https://www.kachergis.com/XSLmodels/reference/xsl_fit.md)
  now treats a model erroring or returning `NA`/`NaN` SSE for a given
  parameter draw as an infinitely bad fit instead of aborting the whole
  optimization run
- Fixed
  [`tilles()`](https://www.kachergis.com/XSLmodels/reference/tilles.md)
  mislabeling its model name as `"rescorla_wagner"`
- [`create_cooc_matrix()`](https://www.kachergis.com/XSLmodels/reference/create_cooc_matrix.md)
  test had an incorrect expected value
- [`plot_training_trials()`](https://www.kachergis.com/XSLmodels/reference/plot_training_trials.md)
  now gives an informative error if the optional `viridis`/`gganimate`
  packages aren’t installed, instead of a raw namespace-loading error
- [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  checked `!is.null(dat$test)` to decide whether to use
  [`mafc_test()`](https://www.kachergis.com/XSLmodels/reference/mafc_test.md),
  but
  [`xslData()`](https://www.kachergis.com/XSLmodels/reference/xslData-class.md)’s
  `test` defaults to [`list()`](https://rdrr.io/r/base/list.html) (not
  `NULL`), so any dataset built without explicit test trials – including
  the package’s own
  [`get_example_ambiguous_condition()`](https://www.kachergis.com/XSLmodels/reference/get_example_ambiguous_condition.md)/[`get_example_unambiguous_condition()`](https://www.kachergis.com/XSLmodels/reference/get_example_unambiguous_condition.md)
  – was silently routed through `mafc_test(mat, list())`. That returned
  `numeric(0)` rather than erroring (an R quirk: `1:0` counts down
  instead of being empty, and `vec[i] <- numeric(0)` truncates the
  vector rather than erroring), producing a false `sse = 0` “perfect
  fit” with no warning. Now checks `length(dat$test) > 0`.
- [`guess_and_test()`](https://www.kachergis.com/XSLmodels/reference/guess_and_test.md)
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
