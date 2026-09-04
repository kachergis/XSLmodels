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
