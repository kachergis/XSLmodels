# Import the two naturalistic corpora bundled with the `wurwur` package
# (github.com/mcfrank/wurwur) as standalone XSLmodels datasets:
#
#   rollins_corpus - the CHILDES/Rollins corpus used by Frank, Goodman &
#     Tenenbaum (2009): 619 mother-infant utterances, each paired with the
#     objects visible in the scene.
#   fm_corpus - the Frank, Tenenbaum & Fernald corpus: 4763 utterances with
#     the speaker's referential intention hand-coded per utterance.
#
# Neither corpus has human referent-selection accuracy or mAFC test trials,
# so -- like kachergis2012_highlighting -- they are NOT appended to
# `xsl_datasets` (whose group-fit helpers sum/average SSE against a human
# accuracy vector that these don't have). Instead each ships as a list
# bundling the training `xslData` with a gold-standard lexicon, to be scored
# with get_fscore() / get_roc() / get_tp() rather than SSE.
#
# Source files are vendored under data-raw/wurwur/ (see data-raw/wurwur/
# PROVENANCE.md). wurwur is MIT-licensed; the underlying corpora are the
# published research datasets cited in each object's documentation.
#
# Run with data-raw/ as the working directory:
#   setwd("data-raw"); source("add_wurwur_corpora.R")

devtools::load_all("..")

src <- "wurwur"
split_tokens <- function(x) {
  x[is.na(x)] <- ""
  lapply(strsplit(trimws(x), "\\s+"), function(t) t[nzchar(t)])
}

# ---------------------------------------------------------------------------
# CHILDES / Rollins  (Frank, Goodman & Tenenbaum, 2009)
# ---------------------------------------------------------------------------

rollins_raw <- read.csv(file.path(src, "rollins_corpus.csv"),
                        colClasses = "character")
rollins_gold_raw <- read.csv(file.path(src, "rollins_gold.csv"),
                             colClasses = "character")

rollins_corpus <- list(
  data = xslData(
    train = list(words = split_tokens(rollins_raw$words),
                 objects = split_tokens(rollins_raw$objects)),
    label = "Rollins",
    condition = "CHILDES naturalistic corpus (Frank, Goodman & Tenenbaum, 2009)",
    description = paste(
      "619 mother-to-infant utterances from the Rollins corpus in CHILDES,",
      "each paired with the set of objects present in the scene (6 toys",
      "rotated in groups). 419 word types, 22 object types. This is the",
      "corpus the intentional Bayesian model of Frank, Goodman & Tenenbaum",
      "(2009) was fit to. No human referent-selection data: evaluate the",
      "learned matrix against `rollins_corpus$gold` with get_fscore() /",
      "get_roc(). Imported from the wurwur package (github.com/mcfrank/wurwur).",
      "Frank, M. C., Goodman, N. D., & Tenenbaum, J. B. (2009). Using speakers'",
      "referential intentions to model early cross-situational word learning.",
      "Psychological Science, 20(5), 578-585."
    )
  ),
  gold = list(words = rollins_gold_raw$word, objects = rollins_gold_raw$object),
  reference = paste(
    "Frank, M. C., Goodman, N. D., & Tenenbaum, J. B. (2009).",
    "Using speakers' referential intentions to model early cross-situational",
    "word learning. Psychological Science, 20(5), 578-585."
  )
)

stopifnot(
  length(rollins_corpus$data$train$words) == 619,
  # 416 word types actually occur in the 619 utterances; wurwur's world.mat
  # vocabulary (419) additionally lists a few words that never appear.
  length(unique(unlist(rollins_corpus$data$train$words))) == 416,
  length(unique(unlist(rollins_corpus$data$train$objects))) == 22,
  length(rollins_corpus$gold$words) == 34
)

# ---------------------------------------------------------------------------
# Frank, Tenenbaum & Fernald corpus
# ---------------------------------------------------------------------------

fm_files <- sort(list.files(file.path(src, "FMcorpus"), pattern = "\\.csv$",
                            full.names = TRUE))
fm_rows <- do.call(rbind, lapply(fm_files, function(p) {
  r <- read.csv(p, colClasses = "character", check.names = FALSE)
  # A29.csv has a header typo (backtick instead of "video") -- the first
  # column is the video id whatever it is named.
  data.frame(
    utt = r[["utt"]],
    objects_present = r[["objects.present"]],
    objects_referred = r[["objects.referred"]],
    stringsAsFactors = FALSE
  )
}))

fm_words <- split_tokens(fm_rows$utt)
fm_objects <- split_tokens(fm_rows$objects_present)
fm_intents <- split_tokens(fm_rows$objects_referred)

# Drop the (defensive) utterances with no words, mirroring build_fm_dataset().
keep <- lengths(fm_words) > 0
fm_words <- fm_words[keep]
fm_objects <- fm_objects[keep]
fm_intents <- fm_intents[keep]

read_gold <- function(name) {
  g <- read.delim(file.path(src, name), colClasses = "character")
  list(words = g$word, objects = g$object)
}

fm_corpus <- list(
  data = xslData(
    train = list(words = fm_words, objects = fm_objects),
    label = "FM",
    condition = "Frank, Tenenbaum & Fernald naturalistic corpus",
    description = paste(
      "4763 caregiver utterances (24 mother-infant sessions), each paired",
      "with the objects present in the scene. 1122 word types, 33 object",
      "types; ~half the utterances are non-referential. Unlike the Rollins",
      "corpus, the speaker's referential intention is hand-coded per",
      "utterance -- see `fm_corpus$intents` (a list of the object(s)",
      "referred to on each utterance, empty for non-referential ones).",
      "No human referent-selection data: score the learned matrix against",
      "`fm_corpus$gold` (a hand-curated lexicon) or `fm_corpus$gold_variants`",
      "with get_fscore() / get_roc(). Imported from the wurwur package",
      "(github.com/mcfrank/wurwur).",
      "Frank, M. C., Goodman, N. D., & Tenenbaum, J. B. (2009). Using speakers'",
      "referential intentions to model early cross-situational word learning.",
      "Psychological Science, 20(5), 578-585.",
      "Frank, M. C., Tenenbaum, J. B., & Fernald, A. (2013). Social and",
      "discourse contributions to the determination of reference in",
      "cross-situational word learning. Language Learning and Development,",
      "9(1), 1-24."
    )
  ),
  intents = fm_intents,
  gold = read_gold("fm_gold_curated.tsv"),
  gold_variants = list(
    strict = read_gold("fm_gold_strict.tsv"),
    permissive = read_gold("fm_gold_permissive.tsv")
  ),
  reference = paste(
    "Frank, M. C., Tenenbaum, J. B., & Fernald, A. (2013). Social and discourse",
    "contributions to the determination of reference in cross-situational word",
    "learning. Language Learning and Development, 9(1), 1-24."
  )
)

stopifnot(
  length(fm_corpus$data$train$words) == 4763,
  length(fm_corpus$intents) == 4763,
  length(unique(unlist(fm_corpus$data$train$words))) == 1122,
  # objects.present spans 30 types; the 3 others in wurwur's inventory
  # (e.g. firetruck) only ever appear in objects.referred.
  length(unique(unlist(fm_corpus$data$train$objects))) == 30,
  length(fm_corpus$gold$words) == 41,
  length(fm_corpus$gold_variants$strict$words) == 39,
  length(fm_corpus$gold_variants$permissive$words) == 116
)

usethis::use_data(rollins_corpus, overwrite = TRUE, compress = "xz")
usethis::use_data(fm_corpus, overwrite = TRUE, compress = "xz")
