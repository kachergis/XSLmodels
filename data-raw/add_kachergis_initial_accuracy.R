# Add the "initial accuracy" MTurk experiment from Kachergis, Grimmick, &
# Gureckis ("Modeling error-driven cross-situational word learning",
# unpublished manuscript; see https://github.com/kachergis/initial_accuracyXSL)
# as its own package dataset `kachergis_initial_accuracy`.
#
# Design (from paper/ms.Rmd): 18 word-object pairs, familiarized one at a
# time (unambiguous), then studied cross-situationally (2 words + 2 objects
# per trial, 3 blocks x 9 trials = 27 trials, 3 exposures/pair). In the High
# Initial Accuracy (HIA) condition, 12/18 pairs are studied with their true
# familiarization partner (66.6% "initially accurate"); in the Low Initial
# Accuracy (LIA) condition, only 6/18 are (33.3% "initially accurate") --
# the rest are studied "switched" with another initially-inaccurate item's
# partner. Test is 18AFC (all 18 studied objects) using the *familiarization*
# pairing as ground truth, regardless of what was (mis)paired at study.
#
# Reconstructing the item-level design directly from the raw per-subject data
# in analysis/data/preprocessed_data.Rdata (not the paper text) because the
# real word/object *content* was randomly assigned per subject, but the
# *structural* switch design (which canonical item indices are accurate vs.
# inaccurate, and which get swapped with which) turns out to be identical
# for every subject within a condition -- i.e. a fixed, shared template. This
# was confirmed empirically: aggregating study-phase word x object
# co-occurrence counts by condition gives exact integer (0 or 3) cell counts
# for every subject, with zero cross-subject variance.
#
# xslData's convention requires a tested word's correct referent to share its
# index (`m[w, w]`), but the familiarization-correct pairing here is not
# word_i <-> object_i -- it's a fixed derangement pairing adjacent indices
# (word i <-> object (i xor 1), i.e. 0<->1, 2<->3, ..., 16<->17). We relabel
# *object* indices by `xor(o, 1)` throughout, which makes the
# familiarization-correct pairing become the diagonal, satisfying the
# package's convention while preserving the actual co-occurrence structure
# (a relabeling of columns is a lossless isomorphism).
#
# Familiarization is represented explicitly as 18 real (unambiguous,
# single-word/single-object) training trials prepended to the 27 study
# trials, rather than by hacking a model-specific `start_matrix`: because
# familiarization always pairs word i with its own relabeled diagonal
# partner, every model in the package builds up the diagonal from these
# familiarization trials using its own normal per-trial dynamics (decay,
# entropy, etc.) -- this works uniformly across every model in the package,
# including ones (like rescorla_wagner()) that don't support `start_matrix`.
#
# Study-phase trial order was randomized per subject (only the resulting
# word->object pairing counts were fixed by design, not the specific
# yoking of trials): we use one representative, non-memory-aid-excluded
# subject's real trial sequence per condition, extracted from `study`.
#
# Per-item accuracy is the group mean (@ 35 -> 34/31 subjects after
# excluding 5 participants who reported using an external memory aid, as in
# ms.Rmd's `analysis/paper/ms.Rmd` reported analysis) of `test$correct`,
# keyed by the canonical (un-relabeled, since accuracy is per *word*) item
# index -- reproducing the paper's Figure 2 crossover pattern almost exactly:
# HIA accurate M=.56 > LIA accurate M=.48; HIA inaccurate M=.30 < LIA
# inaccurate M=.34.
#
# Run with data-raw/ as the working directory. Requires
# ../../initial_accuracyXSL/analysis/data/preprocessed_data.Rdata (a sibling
# checkout of https://github.com/kachergis/initial_accuracyXSL).

pkgload::load_all("..")
suppressMessages(library(dplyr))

raw_data_path <- "../../initial_accuracyXSL/analysis/data/preprocessed_data.Rdata"
stopifnot(file.exists(raw_data_path))
load(raw_data_path) # study, test, qdat1, qdat2, fam, stud_long

# ---- exclude the 5 participants who reported using a memory aid (ms.Rmd) ----
memaid <- subset(qdat2, memory_aid == "yes") %>% rename(uniqueId = uniqueid)
test <- subset(test, !is.element(uniqueId, memaid$uniqueId))

relabel_obj <- function(o) bitwXor(as.integer(o), 1L) # 0<->1, 2<->3, ..., 16<->17

# ---- per-item human accuracy (group mean by canonical word index 0-17) ----
item_accuracy <- function(cond) {
  t2 <- subset(test, condition == cond & !is.na(init_word_ind))
  a <- aggregate(correct ~ init_word_ind, data = t2, mean)
  a[order(a$init_word_ind), ]$correct
}

n_subj <- function(cond) length(unique(subset(test, condition == cond)$uniqueId))

# ---- familiarization: 18 unambiguous trials, word i with its own (relabeled) partner ----
familiarization_trials <- list(
  words = as.list(1:18),
  objects = as.list(1:18)
)

# ---- study: one representative subject's real 27-trial sequence, relabeled ----
build_study_trials <- function(cond) {
  s <- subset(study, condition == cond & uniqueId %in% unique(test$uniqueId))
  subj <- sort(unique(s$uniqueId))[1]
  s <- subset(s, uniqueId == subj)
  s <- s[order(s$trial), ]
  stopifnot(nrow(s) == 27)
  list(
    words = lapply(seq_len(nrow(s)), \(i) c(s$w1ind[i], s$w2ind[i]) + 1L),
    objects = lapply(seq_len(nrow(s)), \(i) relabel_obj(c(s$o1ind[i], s$o2ind[i])) + 1L)
  )
}

build_condition <- function(cond, label) {
  study_trials <- build_study_trials(cond)
  train <- list(
    words = c(familiarization_trials$words, study_trials$words),
    objects = c(familiarization_trials$objects, study_trials$objects)
  )
  xslData(
    train = train,
    accuracy = item_accuracy(cond),
    n_subj = n_subj(cond),
    label = label,
    condition = cond,
    description = paste(
      "Kachergis, Grimmick, & Gureckis, 'Modeling error-driven",
      "cross-situational word learning' (unpublished ms.). 18",
      "word-object pairs: familiarized one at a time (18 unambiguous",
      "trials, included here as the first 18 training trials), then",
      "studied cross-situationally (2 words + 2 objects/trial, 3",
      "blocks x 9 trials = 27 trials, included as the remaining",
      "training trials; 3 exposures/pair). In the", label,
      "condition,", if (cond == "High Initial Accuracy") "12/18" else "6/18",
      "pairs are studied with their true familiarization partner",
      "('initially accurate'); the rest are studied switched with",
      "another initially-inaccurate item's partner. Object indices are",
      "relabeled (word i's familiarization partner is object",
      "xor(i-1, 1) + 1, not object i) so the diagonal m[w, w] is",
      "always the familiarization-correct (i.e. test-correct) pairing,",
      "per xslData convention. Test is 18AFC; accuracy is the group",
      "mean per item (n_subj subjects, after excluding 5 participants",
      "who reported using a memory aid)."
    )
  )
}

kachergis_initial_accuracy <- list(
  `High Initial Accuracy` = build_condition("High Initial Accuracy", "High Initial Accuracy"),
  `Low Initial Accuracy` = build_condition("Low Initial Accuracy", "Low Initial Accuracy")
)

print(kachergis_initial_accuracy[["High Initial Accuracy"]])
print(kachergis_initial_accuracy[["Low Initial Accuracy"]])

usethis::use_data(kachergis_initial_accuracy, overwrite = TRUE)
