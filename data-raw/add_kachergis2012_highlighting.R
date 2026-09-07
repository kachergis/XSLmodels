# Add the highlighting experiment from Kachergis (2012, CogSci) --
# "Learning Nouns with Domain-General Associative Learning Mechanisms" --
# as its own package dataset `kachergis2012_highlighting`.
#
# This supersedes an earlier version of this file that was built from a
# different, earlier/pilot study (single word-object-pair-per-trial
# sequential design, from associative_word_learning's root directory --
# orderings/highlighting.txt et al.). That data does NOT reproduce the
# paper's reported statistics. The real dataset, confirmed by reproducing
# the paper's exact reported numbers to 2-3 decimal places (including its
# specific chi-square values), is:
#
#   - associative_word_learning/illcorr_hl2/HLow_HLwo_67Ss.RData: test-phase
#     responses for exactly the paper's N=67 subjects, in conditions "HLwo"
#     (words as cues: 1 object + 2 words/trial) and "HLow" (objects as
#     cues: 2 objects + 1 word/trial).
#   - associative_word_learning/illcorr_hl2/orders/highlighting.txt: the
#     real 28-trial training order, in genuine joint-cue trials (unlike the
#     superseded pilot data). Columns are "o w w" for the words-as-cues
#     reading (object, cue word 1, cue word 2); the original analysis
#     script (assoc_learning_NEW_HL.R) re-reads the *same* file as "w o o"
#     for objects-as-cues (word, cue object 1, cue object 2) -- i.e.
#     objects-as-cues is the same 28-trial schedule with the single/paired
#     roles swapped between words and objects, not a separately-designed
#     schedule.
#
# Design (confirmed against both files): each condition replicates the
# classic 3-role highlighting structure twice. Words-as-cues: replication A
# uses words 1 (PE), 2 (PL), 3 (I) and objects 1 (E), 3 (L) (object 2 is an
# untrained foil); replication B uses words 4 (PE), 5 (PL), 6 (I) and
# objects 4 (E), 6 (L) (object 5 is a foil). Early stage: 7 trials of
# {PE, I} -> E; late stage: 7 trials of {PL, I} -> L; 28 trials total, 6
# words, 4 objects -- exactly as the paper states. Objects-as-cues is the
# same schedule with words/objects swapped: 4 words, 6 objects.
#
# Reproduction check (run interactively, not asserted below since it
# depends on the exact response matrix construction): the collapsed
# (both-replications-averaged) response proportions from this data are
# PE-E = .692, PL-L = .817, I-E = .510, I-L = .250 for words-as-cues
# (paper: .69, .82, .51, .25) and PE-E = .598, PL-L = .713, I-E = .287,
# I-L = .159 for objects-as-cues (paper: .60, .71, .28, .16); the paper's
# own reported chi-squares (e.g. chi^2(1,N=79)=9.23 for words-as-cues I,
# chi^2(1,N=73)=6.04 for objects-as-cues I) are exactly reproduced by
# chisq.test() on this data's raw (uncollapsed) counts.
#
# ACCURACY: words-as-cues' PL items (words 2, 5) have their real target
# (the shared late object, 3 and 6 respectively) at a *different* index
# than their own word index -- xslData's accuracy/mafc_test() convention
# requires a word's correct object to share its index, so PL's accuracy
# can't be represented this way (same underlying issue as the I items,
# which have two legitimate targets and so were already NA in the
# superseded version). Only the two PE items (unambiguous, and coincide
# with their own index) get a real accuracy value; PL/I are NA.
# Objects-as-cues has no such issue -- every tested word's own index is
# among its trained objects -- so all 4 of its items get real accuracy.
#
# Because of that partial-NA accuracy vector, this dataset is (as before)
# kept standalone rather than appended to `xsl_datasets`: an NA would
# silently poison every other dataset's aggregate fit in
# `get_group_model_fit()`/`get_crossvalidated_model_fit()`, which sum SSE
# across all of `xsl_datasets`. See
# tests/bakeoff_comparison/kachergis2012_highlighting_fit.R for a custom
# scorer that evaluates a model against all four reported proportions
# (PE-E, PL-L, I-E, I-L) directly off the association matrix, the way the
# original paper's own fitting procedure did.
#
# Run with data-raw/ as the working directory.

devtools::load_all("..")

hl_dir <- "../../associative_word_learning/illcorr_hl2"
ordering <- read.table(file.path(hl_dir, "orders/highlighting.txt"), skip = 1,
                       col.names = c("c1", "c2", "c3"))
stopifnot(nrow(ordering) == 28)

## ---- Words as cues: column 1 = object (single), columns 2/3 = the two ----
## cue words, exactly as the file's own header ("o w w") states. Objects 2
## and 5 are foils, never trained, leaving a gap in the object vocabulary
## ({1,3,4,6}); relabel to consecutive integers for the same reason as the
## word relabeling below.
object_relabel_wc <- c(`1` = 1, `3` = 2, `4` = 3, `6` = 4)
relabel_wc_objects <- function(x) unname(object_relabel_wc[as.character(x)])

words_cues_train <- list(
  words = lapply(seq_len(nrow(ordering)), \(i) c(ordering$c2[i], ordering$c3[i])),
  objects = as.list(relabel_wc_objects(ordering$c1))
)

## ---- Objects as cues: the *same* schedule, columns reinterpreted as ----
## "w o o" (word, then the two cue objects) -- exactly how the original
## analysis script (assoc_learning_NEW_HL.R) re-reads this same file for
## this condition, not a separately-derived design. Words 2 and 5 are
## unused as words here (they're foil objects on the words-as-cues side),
## leaving a gap {1,3,4,6}; as with the superseded pilot dataset,
## uncfam_model() indexes its matrix positionally (see that model's source)
## so a non-consecutive vocabulary throws "subscript out of bounds" --
## close the gap by relabeling words to consecutive integers.
word_relabel_oc <- c(`1` = 1, `3` = 2, `4` = 3, `6` = 4)
relabel_oc_words <- function(x) unname(word_relabel_oc[as.character(x)])

## The raw numbering also reuses words-as-cues' word indices for these
## positions, which means each tested word's *own* unique cue object isn't
## its same-numbered object here: raw word 1's cues are {1 (its own), 3
## (shared with raw word 2)}; raw word 3's are {2 (its own), 3 (shared)};
## raw word 4's are {4 (its own), 6 (shared with raw word 6)}; raw word 6's
## are {5 (its own), 6 (shared)}. Relabel objects so each word's own target
## aligns with its (relabeled) word index (1->1, 2->2, 4->3, 5->4), with
## the two shared/ambiguous objects (3, 6) moved out of that range (5, 6).
object_relabel_oc <- c(`1` = 1, `2` = 2, `4` = 3, `5` = 4, `3` = 5, `6` = 6)
relabel_oc_objects <- function(x) unname(object_relabel_oc[as.character(x)])

objects_cues_train <- list(
  words = as.list(relabel_oc_words(ordering$c1)),
  objects = lapply(seq_len(nrow(ordering)),
                   \(i) relabel_oc_objects(c(ordering$c2[i], ordering$c3[i])))
)

## ---- Human accuracy, computed directly from the real N=67 test data ----
load(file.path(hl_dir, "HLow_HLwo_67Ss.RData")) # loads `all`

response_matrix <- function(data) {
  wmap <- sort(unique(data$Word))
  omap <- sort(unique(data$ObjectSelected))
  m <- matrix(0, length(wmap), length(omap), dimnames = list(wmap, omap))
  for (w in wmap) for (o in omap) {
    m[as.character(w), as.character(o)] <- sum(data$Word == w & data$ObjectSelected == o)
  }
  m / rowSums(m)
}

hlwo_props <- response_matrix(subset(all, Cond == "HLwo"))
hlow_props <- response_matrix(subset(all, Cond == "HLow"))

# Words-as-cues: PE items (1, 4) have their own index as their real target;
# PL items (2, 5)'s real target (3, 6) doesn't share their index, and I
# items (3, 6) are genuinely ambiguous -- both NA (see header comment).
words_accuracy <- c(
  hlwo_props["1", "1"], NA, NA,
  hlwo_props["4", "4"], NA, NA
)

# Objects-as-cues: every tested word has one object unique to it (its real
# target) and shares its other cue object with a different tested word
# (the ambiguous cue) -- e.g. raw word 1's cues are {1 (unique), 3 (shared
# with raw word 2)}, so word 1's target is object 1, not 3. Only for E
# items (raw words 1, 4) does "own index" coincide with "unique cue"; for L
# items (raw words 3, 6) the unique cue is a *different* object (2 and 5
# respectively) -- the raw numbering reuses words-as-cues' word indices for
# these positions, not a same-index convention here.
objects_accuracy <- c(
  hlow_props["1", "1"], hlow_props["3", "2"],
  hlow_props["4", "4"], hlow_props["6", "5"]
)

words_as_cues <- xslData(
  train = words_cues_train,
  accuracy = words_accuracy,
  n_subj = length(unique(subset(all, Cond == "HLwo")$Subject)),
  label = "Kachergis2012-highlighting-words",
  condition = "words as cues (2 words, 1 object per trial)",
  description = paste(
    "28 real training trials (2 replications x 7 early + 7 late), read from",
    "associative_word_learning/illcorr_hl2/orders/highlighting.txt (object",
    "indices relabeled to consecutive integers -- original objects 1/3/4/6",
    "are objects 1/2/3/4 here, since 2 and 5 were unused foils). Early:",
    "{PE, I} jointly predict E (words 1&3 -> object 1; words 4&6 -> object",
    "3); late: {PL, I} jointly predict L (words 2&3 -> object 2; words 5&6",
    "-> object 4). Accuracy computed directly from the real N=67 test data",
    "(HLow_HLwo_67Ss.RData); PL and I items are NA -- see the limitation",
    "noted in data-raw/add_kachergis2012_highlighting.R.",
    "Kachergis, G. (2012). Learning Nouns with Domain-General Associative",
    "Learning Mechanisms. Proceedings of the 34th Annual Meeting of the",
    "Cognitive Science Society."
  )
)

objects_as_cues <- xslData(
  train = objects_cues_train,
  accuracy = objects_accuracy,
  n_subj = length(unique(subset(all, Cond == "HLow")$Subject)),
  label = "Kachergis2012-highlighting-objects",
  condition = "objects as cues (2 objects, 1 word per trial)",
  description = paste(
    "The same 28-trial schedule as Kachergis2012-highlighting-words, with",
    "cue/outcome roles swapped: 2 objects + 1 word per trial (4 words, 6",
    "objects; word and object indices both relabeled to consecutive",
    "integers and, for objects, so each word's own unique cue object",
    "shares its index -- original words 1/3/4/6 are words 1/2/3/4 here;",
    "original objects 1/2/4/5 (each word's own unique target) become",
    "1/2/3/4, and the two shared/ambiguous objects (original 3, 6) become",
    "5/6). Words 1 and 3 here (original 1, 4) are tested unambiguously (E,",
    "own target 1/3 here); words 2 and 4 (original 3, 6) are tested",
    "unambiguously (L, own target 2/4 here); the ambiguity is in the shared",
    "cue objects (5, 6 here), not in a tested word, so all 4 tested words",
    "get a real accuracy value, computed directly from the real N=67 test",
    "data (HLow_HLwo_67Ss.RData).",
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
