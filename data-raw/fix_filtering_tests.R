# Give the 12 filtering conditions (Kachergis, Yu, & Shiffrin, 2012; filt*E_*L)
# the tests participants actually had.
#
# In these designs every word has a partner: each late word always appears
# with one particular early word (or, in filt0E_*, with one other late word),
# so, e.g., word 1 co-occurs with both object 1 and object 7 during training.
# Each word was tested twice, so that each participant's learning of both
# associations could be measured separately:
#   - "AtoA": word 1 tested with object 7 absent (measures w1-o1);
#   - "AtoB": word 1 tested with object 1 absent (measures w1-o7).
# In the raw data (data-raw/data/filtering_data.RData), no participant ever
# chose the partner object on an AtoA test, nor (outside the coding error
# below) the word's own object on an AtoB test.
#
# xsl_datasets previously held only the AtoA accuracies, with no test
# specification, so they were scored with get_perf() over all 12 objects --
# counting a model's association with the absent partner object against it,
# and capping filt0E_* (where the two pairs are symmetric) at .5. Each
# condition now has 24 test items, scored by mafc_test(): the 12 AtoA items
# (each word's own object plus the 10 objects that are neither it nor its
# partner) followed by the 12 AtoB items (the partner object correct, plus
# the 10 objects that are neither the word's own nor its partner's).
# `accuracy` is ordered the same way.
#
# Coding error: in all four *_6L conditions, participants 610 and 621 have
# their late-word AtoA tests coded with the early words' indices (1-6) and
# their early-word AtoB tests coded with nonexistent objects (13-18), i.e.
# shifted by 6. Whether their responses were shifted too can't be determined,
# so those 8 sessions are excluded (n_subj 25 -> 23 in each *_6L condition).
# This changes those conditions' AtoA accuracies slightly; the other 8
# conditions' AtoA accuracies are reproduced exactly.

devtools::load_all(".")
library(dplyr)

e <- new.env()
load("data-raw/data/filtering_data.RData", e)
raw <- e$raw |> mutate(order = paste0("filt", EarlyRepetitions, "E_", Late, "L"))
partner_of <- function(w) ifelse(w <= 6, w + 6, w - 6)

# sessions with the shifted coding (a late-word AtoA test coded as word 1-6,
# or an AtoB answer beyond object 12)
bad <- raw |>
  filter((testtype == "lateatoa" & Word <= 6) | CorrectAns > 12) |>
  distinct(order, Subject, Session)
stopifnot(nrow(bad) == 8, all(grepl("_6L$", bad$order)), setequal(bad$Subject, c(610, 621)))
clean <- anti_join(raw, bad, by = c("order", "Subject", "Session"))
stopifnot(all(clean$CorrectAns == ifelse(clean$AtoA, clean$Word, partner_of(clean$Word))),
          sum(clean$AtoA & clean$Response == partner_of(clean$Word)) == 0,
          sum(!clean$AtoA & clean$Response == clean$Word) == 0)
acc <- clean |>
  group_by(order, AtoA, Word) |>
  summarise(acc = mean(Correct), n = n(), n_sess = n_distinct(paste(Subject, Session)), .groups = "drop")
stopifnot(all(acc$n == acc$n_sess)) # each session tested each word once per test type

ix <- which(vapply(xsl_datasets, \(d) grepl("^filt", d$label), logical(1)))
stopifnot(length(ix) == 12)
for (i in ix) {
  d <- xsl_datasets[[i]]
  W <- lapply(d$train$words, \(x) unlist(x, use.names = FALSE))
  words <- sort(unique(unlist(W)))
  stopifnot(identical(as.numeric(words), as.numeric(1:12)))
  # a word's partner: the only word a late word ever appears with; an early
  # word's partner is the late word that appears only with it
  cooc <- lapply(words, \(w) unique(unlist(lapply(W[vapply(W, \(x) w %in% x, logical(1))], \(x) setdiff(x, w)))))
  partner <- vapply(seq_along(words), function(k) {
    if (length(cooc[[k]]) == 1) return(as.numeric(cooc[[k]]))
    as.numeric(words[vapply(cooc, \(co) length(co) == 1 && co == words[k], logical(1))])
  }, numeric(1))
  stopifnot(identical(partner, as.numeric(partner_of(1:12))))

  a <- acc |> filter(order == d$label)
  atoa <- a |> filter(AtoA) |> arrange(Word)
  atob <- a |> filter(!AtoA) |> arrange(Word)
  stopifnot(nrow(atoa) == 12, nrow(atob) == 12, length(unique(c(atoa$n, atob$n))) == 1)
  if (!grepl("_6L$", d$label)) stopifnot(isTRUE(all.equal(atoa$acc, d$accuracy)), atoa$n[1] == d$n_subj)

  d$test <- list(
    words = as.list(as.numeric(c(words, words))),
    objects = c(lapply(seq_along(words), \(k) as.numeric(setdiff(words, partner[k]))),
                lapply(seq_along(words), \(k) as.numeric(setdiff(words, words[k])))),
    correct = as.list(as.numeric(c(words, partner)))
  )
  d$accuracy <- c(atoa$acc, atob$acc)
  d$n_subj <- atoa$n[1]
  xsl_datasets[[i]] <- d
}

usethis::use_data(xsl_datasets, overwrite = TRUE)
