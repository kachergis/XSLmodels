# Add the "initial accuracy" MTurk experiment from Kachergis, Grimmick, &
# Gureckis ("Modeling error-driven cross-situational word learning",
# unpublished manuscript; see https://github.com/kachergis/initial_accuracyXSL)
# as its own package dataset `kachergis_initial_accuracy`.
#
# Design: 18 word-object pairs are first shown one at a time
# ("familiarization"), then studied cross-situationally (2 words + 2 objects
# per trial, 3 blocks x 9 trials = 27 trials, 3 exposures per pair). For
# "initially accurate" items the study pairing matches the familiarization
# pairing; for "initially inaccurate" items it doesn't -- the word was
# familiarized with a *different* object, which is then consistently studied
# with some other word. 12/18 items are initially accurate in the High Initial
# Accuracy (HIA) condition, 6/18 in Low Initial Accuracy (LIA). Test is
# 19AFC (18 studied objects + 1 novel), and the correct answer is always the
# *study* pairing -- confirmed against the raw data: for every test trial,
# `correctAns` is the object the tested word co-occurred with at study, and
# for every initially inaccurate item it differs from that word's
# familiarization object.
#
# The raw data index words/objects by per-participant canonical indices 0-17
# (`w1ind`/`o1ind` in `study`, `word_ind` in `test`); stimulus *content* was
# randomized per participant, but the structural design (which indices are
# switched, and with what) is identical for every participant in a condition.
# In those indices, the familiarization pairing is word v <-> object
# xor(v, 1), and the study pairing is a condition-specific permutation.
#
# xslData's convention is that a tested word's correct referent shares its
# index (m[w, w]). We therefore relabel every *object* by the word it is
# studied with, so the study (= test-correct) pairing is the diagonal. The
# familiarization trials then pair each initially inaccurate word with an
# off-diagonal object -- the one it was first (mis)taught.
#
# (An earlier version of this script put the *familiarization* pairing on the
# diagonal, and keyed per-item accuracy by `init_word_ind` -- the word
# originally shown with the tested word's object -- rather than by the tested
# word itself. That scored every model's accuracy on initially inaccurate
# items as its probability of choosing the wrong, familiarization object.)
#
# Familiarization is represented as 18 real, unambiguous (1 word, 1 object)
# training trials preceding the 27 study trials, so every model learns from
# it with its own ordinary per-trial update. Study-phase trial order was
# randomized per participant (only the pairing structure was fixed): this
# dataset uses one representative, non-excluded participant's real trial
# sequence per condition. For per-participant fits using each participant's
# own sequence, see analysis/model/fit_with_XSLmodels.R in initial_accuracyXSL.
#
# Per-item accuracy is the group mean of `test$correct` for each tested word,
# after excluding the 5 participants who reported using a memory aid and the
# 4 whose median test response time was under 400 ms.
#
# Run with data-raw/ as the working directory. Requires
# ../../initial_accuracyXSL/analysis/data/preprocessed_data.Rdata (a sibling
# checkout of https://github.com/kachergis/initial_accuracyXSL).

pkgload::load_all("..")
suppressMessages(library(dplyr))

raw_data_path <- "../../initial_accuracyXSL/analysis/data/preprocessed_data.Rdata"
stopifnot(file.exists(raw_data_path))
load(raw_data_path) # study, test, qdat1, qdat2, fam, stud_long

# ---- exclusions (as in the initial_accuracyXSL manuscript) ----
# the 5 participants who reported using a memory aid, and the 4 whose median
# test response time was under 400 ms (too fast to have searched a
# 19-object display; all scored at or near 0)
memaid <- subset(qdat2, memory_aid == "yes")$uniqueid
fast <- test %>% filter(!uniqueId %in% memaid) %>% group_by(uniqueId) %>%
  summarise(med_rt = median(rt), .groups = "drop") %>% filter(med_rt < 400) %>% pull(uniqueId)
test <- subset(test, !uniqueId %in% c(memaid, fast))
study <- subset(study, uniqueId %in% unique(test$uniqueId))

# word (0-17) -> the object (0-17) it is studied with; identical for every
# participant in a condition, which is checked here
study_map <- function(cond) {
  maps <- lapply(split(subset(study, condition == cond), ~uniqueId), function(s) {
    m <- tapply(c(s$o1ind, s$o2ind), c(s$w1ind, s$w2ind), unique)
    stopifnot(all(lengths(m) == 1))
    unlist(m)[as.character(0:17)]
  })
  stopifnot(all(vapply(maps, identical, logical(1), maps[[1]])))
  maps[[1]]
}

build_condition <- function(cond) {
  o_study <- study_map(cond)
  relab <- setNames(0:17 + 1L, o_study) # object -> (1-based) word it is studied with
  R <- function(o) unname(relab[as.character(o)])

  s <- subset(study, condition == cond)
  s <- subset(s, uniqueId == sort(unique(s$uniqueId))[1])
  s <- s[order(s$trial), ]
  stopifnot(nrow(s) == 27)

  train <- list(
    # familiarization: word v with object xor(v, 1)
    words = c(as.list(1:18), lapply(seq_len(nrow(s)), \(i) c(s$w1ind[i], s$w2ind[i]) + 1L)),
    objects = c(as.list(R(bitwXor(0:17, 1L))),
                lapply(seq_len(nrow(s)), \(i) R(c(s$o1ind[i], s$o2ind[i]))))
  )

  t2 <- subset(test, condition == cond & !is.na(init_acc))
  acc <- aggregate(correct ~ word_ind, data = t2, mean)
  stopifnot(identical(acc$word_ind, 0:17))
  # sanity: initially accurate items are exactly those familiarized on the diagonal
  n_accurate <- sum(unlist(train$objects[1:18]) == 1:18)
  stopifnot(n_accurate == if (cond == "High Initial Accuracy") 12 else 6)

  xslData(
    train = train,
    accuracy = acc$correct,
    n_subj = length(unique(t2$uniqueId)),
    label = cond,
    condition = cond,
    description = paste(
      "Kachergis, Grimmick, & Gureckis, 'Modeling error-driven",
      "cross-situational word learning' (unpublished ms.). 18",
      "word-object pairs: familiarized one at a time (the first 18",
      "training trials), then studied cross-situationally (2 words + 2",
      "objects/trial, 27 trials, 3 exposures/pair). In the", cond,
      "condition,", n_accurate, "of 18 words are familiarized with the",
      "object they are later studied with ('initially accurate'); the",
      "rest are familiarized with a different object. Objects are",
      "indexed by the word they are studied with, so the diagonal m[w, w]",
      "is the study (= test-correct) pairing; an initially inaccurate",
      "word's familiarization object is off-diagonal. Test is 19AFC;",
      "accuracy is the group mean per tested word (n_subj participants,",
      "after excluding 5 who reported using a memory aid and 4 with",
      "median test response times under 400 ms)."
    )
  )
}

kachergis_initial_accuracy <- list(
  `High Initial Accuracy` = build_condition("High Initial Accuracy"),
  `Low Initial Accuracy` = build_condition("Low Initial Accuracy")
)

print(kachergis_initial_accuracy[["High Initial Accuracy"]])
print(kachergis_initial_accuracy[["Low Initial Accuracy"]])

usethis::use_data(kachergis_initial_accuracy, overwrite = TRUE)
