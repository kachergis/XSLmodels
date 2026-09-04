# hypothesis-testing model based on Medina, Snedeker,
# Trueswell, & Gleitman, 2011's verbal description:
# one-trial / "fast mapping" hypothesis:
#  i) learners hypothesize a single meaning based on their first encounter with a word
# ii) learners neither weight nor even store back-up alternative meanings
# iii) on later encounters, learners attempt to retrieve this hypothesis from memory and test it against a new context, updating it only if it is disconfirmed
# Thus, they do not accrue a "best" final hypothesis by comparing multiple episodic memories of prior contexts or multiple semantic hypotheses.

# for ICDL 2012 was: hypoth_model.R
# similar to Blythe, Smith, and Smith's guess-and-test model

guess_and_test_model <- function(params, data, control) {
  f <- params[["f"]] # forget at retrieval
  sa <- params[["sa"]] # prob of storage (slow learning down)
  reps <- control[["reps"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  m <- matrix(0, voc_sz, ref_sz) # hypothesis matrix
  colnames(m) <- ref
  rownames(m) <- voc
  keep_traj <- isTRUE(control[["keep_traj"]])
  traj <- list()
  perf <- matrix(0, nrow = reps, ncol = voc_sz) # a row for each block
  freq <- rep(0, voc_sz) # number of occurrences per pair, so far
  names(freq) <- voc
  for (rep in 1:reps) {
    for (t in seq_along(data$words)) {
      # a word or object heard twice in one utterance is still one word /
      # one object for hypothesis testing -- without de-duplicating, a
      # repeated word is visited twice below, and the first visit can clear
      # or double-set its hypothesis, leaving which(m[w, ] == 1) non-scalar
      # (a corpus has repeated words per utterance; controlled trials never do)
      tr_w <- unlist(data$words[t])
      tr_w <- unique(tr_w[!is.na(tr_w) & tr_w != ""])
      tr_o <- unlist(data$objects[t])
      tr_o <- unique(tr_o[!is.na(tr_o)])
      if (length(tr_o) == 0) {
        index <- (rep - 1) * length(data$words) + t
        if (keep_traj) traj[[index]] <- m
        next
      }
      tr_o_pos <- match(tr_o, ref)   # column positions of this trial's objects
      freq[tr_w] <- freq[tr_w] + 1
      # forget randomly-selected hypotheses
      forget <- tr_w[runif(length(tr_w)) < f]
      m[forget, ] <- m[forget, ] * 0
      if (length(tr_w) == 1) {
        have_hypoths <- tr_w[which(sum(m[tr_w, ]) != 0)]
      } else {
        have_hypoths <- tr_w[which(rowSums(m[tr_w, ]) != 0)] # throw out inconsistent ones
      }
      for (w in have_hypoths) {
        # disconfirm if the word's single stored hypothesis (a column
        # position) is not among the objects present on this trial
        if (!is.element(which(m[w, ] == 1), tr_o_pos)) m[w, ] <- m[w, ] * 0
      }

      # make new hypotheses
      if (length(tr_w) == 1) {
        need_hypoths <- tr_w[which(sum(m[tr_w, ]) == 0)]
      } else {
        need_hypoths <- tr_w[which(rowSums(m[tr_w, ]) == 0)]
      }
      store <- need_hypoths[runif(length(need_hypoths)) < sa]
      # tr_o[sample.int(...)] rather than sample(tr_o, ...) -- the latter
      # samples from 1:tr_o when tr_o is a single number (a 1-object trial)
      new_hyps <- tr_o[sample.int(length(tr_o), length(store), replace = TRUE)]
      for (w in seq_along(store)) {
        # store[w] is the word getting a hypothesis; new_hyps[w] its guessed
        # object (indexing need_hypoths here set hypotheses for the wrong
        # words whenever only a subset rolled below the storage threshold)
        m[store[w], new_hyps[w]] <- 1
      }
      index <- (rep - 1) * length(data$words) + t  # index for learning trajectory
      if (keep_traj) traj[[index]] <- m
    }
    perf[rep, ] <- get_perf(m + 1e-12)
  }
  xslFit(perf = perf, matrix = m + 1e-12, traj = traj)
}

#' Guess and test model
#'
#' Trueswell & Gleitman 2011 guess-and-test model
#'
#' @param f forget at retrieval
#' @param sa prob of storage (slow learning down)
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- guess_and_test(f = .1, sa = .5)
#' xsl_run(mod, get_example_ambiguous_condition())
guess_and_test <- function(f, sa) {
  xslMod(
    name = "guess_and_test",
    description = "Trueswell & Gleitman 2011 guess-and-test model",
    model = guess_and_test_model,
    params = list(f = f, sa = sa),
    stochastic = TRUE
  )
}

# TODO: sa = s_a
