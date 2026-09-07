# Add the highlighting experiment from Kachergis (2012, CogSci) --
# "Learning Nouns with Domain-General Associative Learning Mechanisms" --
# as its own package dataset `kachergis2012_highlighting`.
#
# Training trials are read directly from the real per-trial ordering file
# associative_word_learning/orderings/highlighting.txt (confirmed, over
# several rounds of back-and-forth with the dataset's author, to be the
# actual trial order used for the published experiment -- as opposed to the
# same repo's "1x1" files, which belong to a different/related data
# collection). Key facts established about that file's format:
#
#   - Every physical trial is a single (word, object) pair (per the "1x1"
#     naming convention elsewhere in that repo: "1 word, 1 object per
#     trial"). There is no simultaneous joint-cue trial -- the highlighting
#     effect here arises from *sequential* retraining of a word onto a
#     different object over the course of the session, not from
#     within-trial attentional competition between simultaneous cues.
#   - Column headers "o1 w1 o2 w2": "o" indexes objects (columns of the
#     association matrix), "w" indexes words (rows). Each row packs two
#     independent single-pair trials side by side: (object=o1, word=w1) and
#     (object=o2, word=w2).
#   - Stream 1 (o1, w1) is the canonical highlighting manipulation: word 1
#     and word 3 are each trained with their own object (1 and 2
#     respectively) *and* with a shared object (3), 7 trials apiece (28
#     trials total). Word 1 gets a 9-trial head start before word 3 is
#     introduced, and the back end of the sequence is increasingly
#     dominated by word 3 -- a primacy/recency structure (see
#     figures/primacy1x1.pdf, figures/recency1x1.pdf), not a cleanly
#     blocked early/then-late design. (Word "2" never appears in the
#     original file -- see the relabeling note below -- so in the resulting
#     xslData object these are words 1 and 2, not 1 and 3.)
#   - Stream 2 (o2, w2) is unambiguous filler (word 4-object 4, word
#     5-object 5, word 6-object 6, always self-paired) included "to
#     encourage participants to actually learn 1-to-1 mappings" -- not part
#     of the analytic manipulation.
#   - The objects-as-cues condition has no separate real ordering file in
#     that repo; it is constructed here as an exact word/object role swap
#     of the same 28-row schedule, per the paper's own description of it as
#     that reversal.
#
# Both streams' trials are interleaved trial-by-trial in the order given
# (row 1's canonical trial, row 1's filler trial, row 2's canonical trial,
# ...), for 56 total training trials per condition.
#
# NOTE on accuracy: no `accuracy` is set. Both word 1 and word 3 are
# genuinely ambiguous (each trained equally often with its own object and
# the shared one), and xslData's accuracy/mafc_test() convention has no way
# to represent an item with two legitimate targets (see the long-form
# discussion of this limitation in this file's git history / prior
# revision) -- forcing a single number on either item would be arbitrary.
# Use tests/bakeoff_comparison/kachergis2012_highlighting_fit.R, which reads
# the model's own-target vs. shared-target preference directly off the
# returned association matrix, to evaluate model fit to this dataset.
#
# NOT added to the shared `xsl_datasets` collection, consistent with the
# above -- an empty accuracy vector here is fine on its own (xslData allows
# it), but xsl_run() falls back to get_perf()'s diag()-based scoring when
# `test` is unset, which isn't meaningful for this design either, and
# get_crossvalidated_group_fits() would then aggregate that meaningless
# score across every dataset in xsl_datasets.
#
# Run with data-raw/ as the working directory.

devtools::load_all("..")

ordering_path <- "../../associative_word_learning/orderings/highlighting.txt"
raw_lines <- readLines(ordering_path)
raw_lines <- raw_lines[!grepl("^\\s*(#|$)", raw_lines)] # drop comment/blank lines
ordering <- read.table(text = raw_lines, col.names = c("o1", "w1", "o2", "w2"))
stopifnot(nrow(ordering) == 28)

# Word "2" is never used (nobody's own-target *or* shared-target index),
# which leaves a gap in the word vocabulary {1,3,4,5,6}. uncfam_model()
# indexes its association matrix positionally (via bare numeric row/column
# numbers, not by name), so a non-consecutive vocabulary causes an
# out-of-bounds error partway through training -- close the gap by
# relabeling words to consecutive integers (order-preserving); objects
# {1..6} are already consecutive and need no relabeling.
word_relabel <- c(`1` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5)
relabel_words <- function(x) unname(word_relabel[as.character(x)])

# Interleave: each row contributes one canonical (stream 1) trial and one
# filler (stream 2) trial, in that order.
words_cues_train <- list(
  words = as.vector(rbind(relabel_words(ordering$w1), relabel_words(ordering$w2))),
  objects = as.vector(rbind(ordering$o1, ordering$o2))
) |> lapply(as.list)

# Objects as cues: exact role swap (word <-> object) of the same schedule,
# per the paper's description of it as the reversal of words-as-cues.
objects_cues_train <- list(
  words = words_cues_train$objects,
  objects = words_cues_train$words
)

words_as_cues <- xslData(
  train = words_cues_train,
  n_subj = 67,
  label = "Kachergis2012-highlighting-words",
  condition = "words as cues (sequential 1 word + 1 object trials)",
  description = paste(
    "56 training trials (28 canonical + 28 unambiguous filler, interleaved),",
    "read directly from associative_word_learning/orderings/highlighting.txt",
    "(word indices relabeled to consecutive integers; original word 3 is",
    "word 2 here -- word 2 in the original numbering was never used).",
    "Canonical manipulation: word 1 and word 2 are each trained 7x with their",
    "own object (1, 2) and 7x with a shared object (3); word 1 precedes word",
    "2 into the schedule by 9 trials, and the tail of the schedule is",
    "increasingly dominated by word 2 (primacy/recency structure). Filler:",
    "words 3/4/5 each trained only with their own same-numbered object",
    "(objects 4/5/6 respectively).",
    "No accuracy vector -- see the limitation noted in",
    "data-raw/add_kachergis2012_highlighting.R.",
    "Kachergis, G. (2012). Learning Nouns with Domain-General Associative",
    "Learning Mechanisms. Proceedings of the 34th Annual Meeting of the",
    "Cognitive Science Society."
  )
)

objects_as_cues <- xslData(
  train = objects_cues_train,
  n_subj = 67,
  label = "Kachergis2012-highlighting-objects",
  condition = "objects as cues (word/object role swap of the words-as-cues schedule)",
  description = paste(
    "Exact word/object role swap of Kachergis2012-highlighting-words' 56",
    "training trials, per the paper's description of this condition as that",
    "reversal. No separate real ordering file for this condition exists in",
    "associative_word_learning/orderings/.",
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
