# Build `kachergis_priming`: the three "priming" experiments run alongside
# Kachergis, Yu & Shiffrin's frequency / contextual-diversity studies.
# Participants studied the same cross-situational training sequence four
# times, taking an 18AFC test on all 18 words after each repetition, so each
# condition is a 4-block learning trajectory (a multi-block xslData).
#
# Sources:
# - data-raw/data/priming_all_trajectory.RData: every test response, already
#   cleaned in the original analysis (participants whose accuracy across all 4
#   blocks was below 2/18 excluded; this matches all 186 participants in the
#   raw files, freqCD/ALL analysis/priming/*.txt). 164 participants remain,
#   each tested on all 18 words in all 4 blocks.
# - data-raw/data/priming_orders/: the three training sequences, copied from
#   freqCD/ALL analysis/orders/priming/. The original models of these data
#   trained on each sequence four times (reps = 4 in
#   model_for_paper_revisions/main_fazly.R).
#
# Two of the sequences are also xsl_datasets conditions, with different
# participants: 3_x8_369.txt is `3_x8_369_4x4` and
# block1_3_6_9-3x3-lo_cd.txt is `freq369-3x3loCD`. (Until XSLmodels PR #30 the
# priming participants' block 1 was pooled into those two conditions.) The
# third, orig_order4x4.txt, is the sequence of the dropped `orig_4x4`, which
# differs from condition `223` only in words 4 and 16 trading places between
# trials 11 and 24.
#
# Run from the package root: Rscript data-raw/add_kachergis_priming.R

suppressMessages({
  devtools::load_all(quiet = TRUE)
  library(dplyr)
})

e <- new.env()
load("data-raw/data/priming_all_trajectory.RData", e)
raw <- e$all

read_order <- function(file) {
  m <- as.matrix(read.table(file.path("data-raw/data/priming_orders", file)))
  trials <- lapply(seq_len(nrow(m)), \(t) unname(m[t, ]))
  list(words = trials, objects = trials)
}

conditions <- list(
  `4x4 6x` = list(exp = "4 Pairs/Trial, 6x", order = "orig_order4x4.txt",
                  design = "4 words + 4 objects per trial, 27 trials, every pair 6 times"),
  `4x4 3/6/9x` = list(exp = "4 Pairs/Trial 3,6,9x", order = "3_x8_369.txt",
                      design = "4 words + 4 objects per trial, 27 trials, 6 pairs each 3, 6 and 9 times (as condition 3_x8_369_4x4)"),
  `3x3 3/6/9x` = list(exp = "3 Pairs/Trial 3,6,9x", order = "block1_3_6_9-3x3-lo_cd.txt",
                      design = "3 words + 3 objects per trial, 36 trials, 6 pairs each 3, 6 and 9 times, low contextual diversity (as condition freq369-3x3loCD)")
)

build_condition <- function(name) {
  spec <- conditions[[name]]
  train <- read_order(spec$order)
  d <- filter(raw, Exp == spec$exp)
  stopifnot(all(d$Correct == (d$Response == d$CorrectAns)),
            all(count(d, Subject)$n == 72),                      # 4 blocks x 18 words
            all(count(d, Subject, Block, CorrectAns)$n == 1))
  # every pair's training frequency matches the order file
  freq <- table(factor(unlist(train$words), levels = 1:18))
  stopifnot(all(distinct(d, CorrectAns, Freq) |> with(freq[CorrectAns] == Freq)))

  lv <- as.character(1:18)
  accuracy <- d |> group_by(Block, CorrectAns) |> summarise(acc = mean(Correct), .groups = "drop") |>
    with(tapply(acc, list(Block, CorrectAns), identity))
  dimnames(accuracy) <- list(block = paste0("block", 1:4), item = lv)
  response_matrix <- lapply(1:4, \(b) {
    db <- filter(d, Block == b)
    m <- unclass(table(factor(db$CorrectAns, levels = 1:18), factor(db$Response, levels = 1:18)))
    dimnames(m) <- list(item = lv, object = lv)
    m
  })
  # the response matrices reproduce the accuracies exactly
  stopifnot(all(abs(t(sapply(response_matrix, \(m) diag(m) / rowSums(m))) - accuracy) < 1e-12))

  xslData(
    train = train,
    accuracy = accuracy,
    n_subj = n_distinct(d$Subject),
    response_matrix = response_matrix,
    label = paste("priming", name),
    condition = name,
    description = paste(
      "Kachergis, Yu & Shiffrin 'priming' experiment (unpublished; run with",
      "their frequency / contextual-diversity studies).", spec$design, "per",
      "block. The same sequence was studied 4 times, with an 18AFC test on",
      "all 18 words after each block; accuracy has one row per block."
    )
  )
}

kachergis_priming <- lapply(setNames(names(conditions), names(conditions)), build_condition)
for (d in kachergis_priming) {
  print(d)
  print(round(rowMeans(d$accuracy), 3))
}

usethis::use_data(kachergis_priming, overwrite = TRUE)
