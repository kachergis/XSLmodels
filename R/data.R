#' Cross-situational word learning experiment data
"xsl_datasets"

#' Kachergis (2012) highlighting experiment data
#'
#' A list of two `xslData` objects ("words as cues" and "objects as cues")
#' from the highlighting experiment in Kachergis (2012, CogSci), "Learning
#' Nouns with Domain-General Associative Learning Mechanisms". Training
#' trials (genuine simultaneous 2-cue trials, matching the paper's Figure
#' 2: 2 words + 1 object, or 2 objects + 1 word, per trial) are read
#' directly from the real per-trial ordering file for the real N=67
#' dataset behind the paper's published statistics -- confirmed by
#' reproducing its exact reported response proportions and chi-square
#' values. See `data-raw/add_kachergis2012_highlighting.R` for the full
#' construction rationale, including index relabeling needed to work
#' around a `uncfam_model()` indexing limitation.
#'
#' Each condition replicates the classic 3-role highlighting structure
#' (PE, PL, I) twice. Words-as-cues' PL items have their real target at a
#' different index than their own word index, and its I items have two
#' legitimate targets -- neither fits `xslData`'s one-correct-object-per-
#' word `accuracy` convention, so only its two PE items get a real
#' accuracy value (the rest are `NA`). Objects-as-cues has no such issue
#' (every tested word's own index is among its trained objects after
#' relabeling), so all 4 of its items have real accuracy. Because of the
#' partial-NA accuracy vector, this dataset is kept standalone rather than
#' appended to [xsl_datasets]: an `NA` would silently poison every other
#' dataset's aggregate fit in
#' `get_group_model_fit()`/`get_crossvalidated_model_fit()`, which sum SSE
#' across all of `xsl_datasets`. See
#' `tests/bakeoff_comparison/kachergis2012_highlighting_fit.R` for a custom
#' scorer that fits a model against all four of the paper's reported
#' proportions (PE-E, PL-L, I-E, I-L) directly off the association matrix,
#' the way the paper's own (non-package) fitting procedure did.
"kachergis2012_highlighting"

#' Kachergis, Grimmick, & Gureckis initial accuracy experiment data
#'
#' A list of two `xslData` objects ("High Initial Accuracy" and "Low Initial
#' Accuracy") from an unpublished MTurk cross-situational word learning
#' experiment manipulating how many of 18 words are first shown
#' ("familiarized") with a different object than the one they are then
#' cross-situationally studied with. The study pairing is the test-correct
#' one, and is on the diagonal (`m[w, w]`); an initially inaccurate word's
#' familiarization object is off-diagonal. The first 18 training trials are
#' the familiarization phase. Not included in [xsl_datasets] (see
#' `data-raw/add_kachergis_initial_accuracy.R` for construction details and
#' `tests/bakeoff_comparison/kachergis_initial_accuracy_fit.R` for an example
#' model comparison).
"kachergis_initial_accuracy"

#' Gangwani, Kachergis & Yu simultaneous category + object name learning
#'
#' Experiment 1 of Gangwani, Kachergis & Yu, "Simultaneous Cross-situational
#' Learning of Category and Object Names" (an undergraduate-journal version is
#' Gangwani & Kachergis, *Indiana Undergraduate Journal of Cognitive Science*
#' 5, 2010). A **hierarchical** cross-situational design: on every training
#' trial the learner sees 2 objects and hears 3 words -- the two objects' own
#' 1-to-1 names *plus* one 1-to-many "category" label that names those two
#' objects and two further objects seen on other trials. Every object is thus
#' part of a 2-to-1 word-referent mapping (a basic name and a category label),
#' and the paper's finding is that learners violate mutual exclusivity: they
#' learn *both* labels for the same object.
#'
#' A named list of three [xslData-class] objects, one per training block --
#' `"block 1 (ad hoc)"`, `"block 2 (natural)"`, `"block 3 (ad hoc)"` -- run in
#' that fixed order. All three share the same 36-trial co-occurrence schedule
#' (only the stimuli differ); each carries its own block-specific human data.
#'
#' Per object: `train$words[[t]]` is `c(name_i, name_j, category_label)` and
#' `train$objects[[t]]` is `c(i, j)`. Words 1-12 are object names (name `i`
#' pairs with object `i`); words 13-15 are the category labels (13/14/15 name
#' objects 1-4 / 5-8 / 9-12). `accuracy` is per-word human P(correct referent |
#' word), length 15: positions 1-12 are 12-AFC name accuracy, positions 13-15
#' are 3-AFC category-membership accuracy (n = 34 canonical-block-order
#' participants; the paper reports 33, dropping one more low performer).
#' `response_matrix` (15 x 12, row-normalised) is the full human choice
#' distribution including the category labels. `test` holds only the 12 name
#' trials (12-AFC).
#'
#' **Not part of [xsl_datasets]**, and not scorable by the built-in
#' `xsl_run()` SSE: the association matrix is 15 words x 12 objects, so
#' `get_perf()` / `mafc_test()` (which score via the diagonal `m[w, w]`) cover
#' only words 1-12. Score the category labels -- and the ME-violation measure
#' that is the paper's point -- with `score_gangwani2011_category()` in
#' `tests/bakeoff_comparison/gangwani2011_category_fit.R`. The natural-category
#' feature-similarity effect (block 2) and generalization to novel objects are
#' out of scope for the package's amodal, index-based models and are not
#' represented. See `data-raw/add_gangwani2011_category.R` for construction
#' details.
#'
#' @examples
#' block1 <- gangwani2011_category[["block 1 (ad hoc)"]]
#' # 15 x 12 matrix; suppressWarnings() because get_perf()'s diagonal rule
#' # warns on the non-square matrix (the built-in SSE is not meaningful here).
#' m <- suppressWarnings(xsl_run(baseline(), block1))$fits[[1]]$matrix
#' mafc_test(m, block1$test)                           # names (words 1-12) only
"gangwani2011_category"

#' CHILDES/Rollins naturalistic word-learning corpus (Frank et al. 2009)
#'
#' The corpus the intentional Bayesian model of Frank, Goodman & Tenenbaum
#' (2009) was fit to: 619 mother-to-infant utterances from the Rollins corpus
#' in CHILDES, each paired with the set of objects present in the scene (six
#' toys rotated in groups). 416 word types, 22 object types.
#'
#' A list with:
#' \describe{
#'   \item{`data`}{an [xslData-class] object with the 619 training utterances
#'     (`data$train$words[[t]]` / `data$train$objects[[t]]` are character
#'     vectors). No `accuracy` or `test` -- there is no human
#'     referent-selection data for this corpus.}
#'   \item{`gold`}{the gold-standard lexicon as `list(words, objects)` (34
#'     word-object pairs), for scoring a learned matrix with [get_fscore()],
#'     [get_roc()], [get_roc_max()], or [get_tp()].}
#'   \item{`reference`}{the citation string.}
#' }
#'
#' Not part of [xsl_datasets] (which is scored by SSE against a human accuracy
#' vector these corpora don't have). Imported from the wurwur package
#' (\url{https://github.com/mcfrank/wurwur}); see
#' `data-raw/add_wurwur_corpora.R`.
#'
#' @source Frank, M. C., Goodman, N. D., & Tenenbaum, J. B. (2009). Using
#'   speakers' referential intentions to model early cross-situational word
#'   learning. *Psychological Science, 20*(5), 578-585. Corpus originally from
#'   Rollins (2003), CHILDES.
#'
#' @examples
#' m <- suppressWarnings(
#'   xsl_run(baseline(), rollins_corpus$data)$fits[[1]]$matrix)
#' get_roc_max(m, gold_lexicon = rollins_corpus$gold)
"rollins_corpus"

#' Frank, Tenenbaum & Fernald naturalistic word-learning corpus
#'
#' 4763 caregiver utterances (24 mother-infant sessions), each paired with the
#' objects present in the scene. 1122 word types, 30 object types present in
#' scenes; roughly half the utterances are non-referential. Unlike
#' [rollins_corpus], the speaker's referential intention is hand-coded per
#' utterance.
#'
#' A list with:
#' \describe{
#'   \item{`data`}{an [xslData-class] object with the 4763 training utterances
#'     (character-vector `words`/`objects` per trial). No `accuracy`/`test`.}
#'   \item{`intents`}{a length-4763 list; `intents[[t]]` is the object(s) the
#'     speaker referred to on utterance `t` (character vector, empty for
#'     non-referential utterances). A per-utterance gold signal.}
#'   \item{`gold`}{a hand-curated gold lexicon as `list(words, objects)` (41
#'     pairs), for [get_fscore()] / [get_roc()] / [get_tp()].}
#'   \item{`gold_variants`}{`list(strict, permissive)` -- two auto-derived
#'     alternatives (39 and 116 pairs) from looser vs. tighter co-occurrence
#'     thresholds on the coded intentions.}
#'   \item{`reference`}{the citation string.}
#' }
#'
#' Not part of [xsl_datasets]. Gaze/hand attentional cues present in the source
#' CSVs are not imported. Imported from the wurwur package
#' (\url{https://github.com/mcfrank/wurwur}); see
#' `data-raw/add_wurwur_corpora.R`.
#'
#' @source Frank, M. C., Tenenbaum, J. B., & Fernald, A. (2013). Social and
#'   discourse contributions to the determination of reference in
#'   cross-situational word learning. *Language Learning and Development,
#'   9*(1), 1-24.
#'
#' @examples
#' length(fm_corpus$data$train$words)
#' fm_corpus$intents[[1]]
#' fm_corpus$gold_variants$strict$words
"fm_corpus"

#' Vlach & DeBrock (2017) spaced/massed repetition experiment data
#'
#' An [xslData-class] object from Vlach, H. A., & DeBrock, C. A. (2017).
#' "Remember dax? Relations between children's cross-situational word
#' learning, memory, and language abilities." *Journal of Memory and
#' Language, 93*, 217-230. 12 to-be-learned word-object pairs (symmetric
#' design: word `i` pairs with object `i`), 2 pairs per training trial, 36
#' trials; pairs vary in whether their 6 repetitions fall on consecutive
#' trials (massed) or are spread out (spaced), the paper's memory/spacing
#' manipulation. `n_subj` is 47 children (ages 2-5).
#'
#' No per-word accuracy is available from this dataset, only the paper's
#' overall mean across all pairs and participants (accuracy = .5583, sd =
#' .1975; see `data-raw/XSL-dataset-fields.csv`), so `accuracy` is `NA`
#' throughout. **Not included in [xsl_datasets]** and not scorable by
#' `xsl_run()`'s built-in per-item SSE -- usable for running/fitting models
#' and comparing simulated performance against the one real number this
#' paper reports. See `data-raw/add_vlach_debrock2017.R` for construction
#' details.
#'
#' @examples
#' m <- xsl_run(baseline(), vlach_debrock2017)$fits[[1]]$matrix
#' dim(m)
"vlach_debrock2017"

#' Benitez et al. (2020) temporal-structure-of-naming experiment data
#'
#' A named list of 7 [xslData-class] objects from Benitez, V. L., et al.
#' (2020), "The temporal structure of naming events differentially affects
#' children's and adults' cross-situational word learning" (data/materials:
#' \url{https://osf.io/2hmxr/}): one per condition (Interleaved, Massed,
#' Unstructured) x age-group (kids, adults) combination, except
#' Unstructured's first training order, which only has child data.
#' Unstructured has two training orders; its second and third were confirmed
#' identical by the original extraction and are pooled as "order 2/3".
#'
#' 8 symmetric word-object pairs (word `i` pairs with object `i`), 2 pairs
#' per training trial, 12 trials, followed by an 8-item 2AFC test.
#' `accuracy` and `response_matrix` are real per-word 2AFC accuracy and raw
#' response counts for that condition/age-group combination (not an
#' estimate) -- structurally this could join [xsl_datasets], but is kept
#' standalone deliberately as held-out generalization-test data; see
#' `xsl_holdout_datasets`. `data-raw/Benitez2020-rep/Benitez2020_XSLdata_extraction.R`
#' extracts the raw OSF data into training orders, 2AFC test trials, and
#' response matrices; `data-raw/add_benitez2020.R` builds the `xslData`
#' objects from those.
#'
#' @examples
#' benitez2020[["Interleaved, kids"]]$accuracy
#' m <- xsl_run(baseline(), benitez2020[["Massed, adults"]])$fits[[1]]$matrix
#' mafc_test(m, benitez2020[["Massed, adults"]]$test)
"benitez2020"

#' Registry of datasets held out from xsl_datasets
#'
#' A named list, one entry per dataset in the package that is **not** part of
#' [xsl_datasets] and therefore never seen by
#' [get_group_model_fit()]/[get_crossvalidated_model_fit()] -- i.e. every
#' dataset available for testing a fitted model's generalization to
#' genuinely held-out data. This is a lookup table (dataset name -> how to
#' score it), not a copy of the data itself -- look up the dataset by its own
#' name (e.g. `benitez2020`, `vlach_debrock2017`) to use it.
#'
#' Each entry has:
#' \describe{
#'   \item{`scorable`}{Whether the dataset can be scored with the package's
#'     standard diagonal convention (`xsl_run()` + `mafc_test()`/`get_perf()`)
#'     out of the box.}
#'   \item{`scoring`}{How to actually score a model's fit against it.}
#'   \item{`note`}{Why it's held out, or any caveat for using it.}
#' }
#'
#' Most of these are standalone for a structural reason (partial-`NA`
#' accuracy, a non-square association matrix, or no human accuracy data at
#' all, e.g. the naturalistic corpora) -- see each dataset's own `?help`
#' page for the full rationale. `benitez2020` and `kachergis_initial_accuracy`
#' are the exceptions: both are directly scorable the same way
#' `xsl_datasets` conditions are, and are kept out of the group/CV fitting
#' pool specifically to have real held-out data to test generalization
#' against.
#'
#' @examples
#' xsl_holdout_datasets$benitez2020$scoring
#' names(xsl_holdout_datasets)[sapply(xsl_holdout_datasets, `[[`, "scorable")]
"xsl_holdout_datasets"
