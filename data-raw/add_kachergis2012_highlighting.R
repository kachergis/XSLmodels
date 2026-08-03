# Add the highlighting experiment from Kachergis (2012, CogSci) --
# "Learning Nouns with Domain-General Associative Learning Mechanisms" --
# as its own package dataset `kachergis2012_highlighting`.
#
# Training trials are built directly from the paper's own description
# (Experiment: Highlighting; Figure 2; Results & Discussion), NOT from the
# raw per-trial file in the associative_word_learning repo. That file's
# item-numbering turns out not to match the minimal 3-role/replication
# design the paper describes (e.g. two supposedly-independent "early" words
# both target the very same object, and the paper's own reported chi-square
# sample sizes (N=79, N=73...) aren't reproducible from the 47 (pre-exclusion)
# or 34 (post-exclusion) subjects present in that export) -- most likely
# because that .txt reflects a broader, related data collection (it also
# contains illusory-correlation, memory-decay, and infer-then-remember
# conditions never reported in this paper) rather than a clean re-derivable
# log of exactly this experiment. Reconstructing the design from the
# published text avoids guessing at that undocumented coding.
#
# IMPORTANT LIMITATION -- NOT added to the shared `xsl_datasets` collection:
# In the words-as-cues condition, the ambiguous "I" item (highlighted cue)
# is *tested* directly, and has two legitimate target objects (early and
# late) with different empirical rates (.51 vs .25) -- but xslData's
# `accuracy`/`mafc_test()` convention hard-codes exactly one "correct"
# object per tested word (the object sharing its index, via `m[w, w]`).
# There is no way to make that diagonal simultaneously mean "the early
# object" for the I item without colliding with the early object's own
# dedicated word (PE), which already legitimately owns that diagonal slot.
# So the I items' accuracy is left NA here. Because `get_crossvalidated_group_fits()`
# and `get_group_model_fit()` sum/average sse across every dataset in
# `xsl_datasets`, an NA would silently poison every other dataset's
# aggregate fit -- so this condition is kept as its own standalone object
# instead of being appended to `xsl_datasets`. It remains fully usable via
# `xsl_run()` / `xsl_fit()` on its own, and see
# tests/bakeoff_comparison/kachergis2012_highlighting_fit.R for a custom
# scoring function that fits all reported response proportions (including
# the I item's early/late split) directly, the way the original paper did.
#
# Run with data-raw/ as the working directory.

devtools::load_all("..")

build_replication <- function(pe_word, pl_word, i_word, e_obj, l_obj, n_early = 7, n_late = 7) {
  early <- replicate(n_early, list(words = c(pe_word, i_word), objects = e_obj), simplify = FALSE)
  late <- replicate(n_late, list(words = c(pl_word, i_word), objects = l_obj), simplify = FALSE)
  c(early, late)
}

## ---- Words as cues: 2 words + 1 object per trial; 6 words, 4 objects ----
## Replication A: words 1 (PE), 2 (PL), 3 (I); objects 1 (E), 2 (L)
## Replication B: words 4 (PE), 5 (PL), 6 (I); objects 3 (E), 4 (L)
words_cues_trials <- c(
  build_replication(1, 2, 3, 1, 2),
  build_replication(4, 5, 6, 3, 4)
)
words_cues_train <- list(
  words = lapply(words_cues_trials, `[[`, "words"),
  objects = lapply(words_cues_trials, `[[`, "objects")
)

# Reported response proportions (Results & Discussion): PE-E = .69, PL-L =
# .82, I preferring E over L = .51 vs .25 (collapsed across the two
# replications, per the paper's own footnote). I items -> NA (see above).
words_as_cues <- xslData(
  train = words_cues_train,
  accuracy = c(0.69, 0.82, NA, 0.69, 0.82, NA),
  n_subj = 67,
  label = "Kachergis2012-highlighting-words",
  condition = "words as cues (2 words, 1 object per trial)",
  description = paste(
    "Highlighting design: early stage has cues PE and I jointly predicting",
    "outcome E (7 trials); late stage has PL and I jointly predicting L (7",
    "trials); replicated twice (words 1-3/objects 1-2, words 4-6/objects",
    "3-4) for 28 total training trials, 6 words, 4 objects. Test is 6AFC",
    "(choose the best object for each word). Accuracy for the ambiguous I",
    "items (words 3, 6) is NA -- see the limitation noted in",
    "data-raw/add_kachergis2012_highlighting.R.",
    "Kachergis, G. (2012). Learning Nouns with Domain-General Associative",
    "Learning Mechanisms. Proceedings of the 34th Annual Meeting of the",
    "Cognitive Science Society."
  )
)

## ---- Objects as cues: 2 objects + 1 word per trial; 4 words, 6 objects ----
## Replication A: objects 1 (PE), 2 (PL), 3 (I); words 1 (E), 2 (L)
## Replication B: objects 4 (PE), 5 (PL), 6 (I); words 3 (E), 4 (L)
## (train$words/objects are swapped relative to words-as-cues since here
## objects are the cues and the word is the single outcome per trial; the
## *test* is still "given word, choose best object" in both conditions.)
build_replication_obj_cues <- function(pe_obj, pl_obj, i_obj, e_word, l_word, n_early = 7, n_late = 7) {
  early <- replicate(n_early, list(objects = c(pe_obj, i_obj), words = e_word), simplify = FALSE)
  late <- replicate(n_late, list(objects = c(pl_obj, i_obj), words = l_word), simplify = FALSE)
  c(early, late)
}
objects_cues_trials <- c(
  build_replication_obj_cues(1, 2, 3, 1, 2),
  build_replication_obj_cues(4, 5, 6, 3, 4)
)
objects_cues_train <- list(
  words = lapply(objects_cues_trials, `[[`, "words"),
  objects = lapply(objects_cues_trials, `[[`, "objects")
)

# Reported response proportions: PE-E = .60, PL-L = .71 (both words are
# unambiguously *tested*, even though object 3/6 -- the ambiguous cue --
# pulls some of their probability mass; that's exactly what the model
# should reproduce, so no NA is needed here, unlike words-as-cues).
objects_as_cues <- xslData(
  train = objects_cues_train,
  accuracy = c(0.60, 0.71, 0.60, 0.71),
  n_subj = 67,
  label = "Kachergis2012-highlighting-objects",
  condition = "objects as cues (2 objects, 1 word per trial)",
  description = paste(
    "Highlighting design with cue/outcome roles reversed relative to",
    "Kachergis2012-highlighting-words: early stage has cues PE and I",
    "(objects) jointly predicting outcome E (word, 7 trials); late stage",
    "has PL and I predicting L (7 trials); replicated twice (objects",
    "1-3/words 1-2, objects 4-6/words 3-4) for 28 total training trials, 4",
    "words, 6 objects. Test is still 'given word, choose best object'",
    "(6AFC); accuracy is well-defined for both tested words here since the",
    "ambiguous item (I) is a cue, not a tested word.",
    "Kachergis, G. (2012). Learning Nouns with Domain-General Associative",
    "Learning Mechanisms. Proceedings of the 34th Annual Meeting of the",
    "Cognitive Science Society."
  )
)

kachergis2012_highlighting <- list(
  `words as cues` = words_as_cues,
  `objects as cues` = objects_as_cues
)

usethis::use_data(kachergis2012_highlighting, overwrite = TRUE)
