#' Shannon entropy after clamping negative associations to 0
#'
#' Unlike the other models in this package, `uncfam_predictive_model()`'s
#' update rule has no normalization step, so an association can be driven
#' below 0 by a large, repeated negative prediction error. Association
#' strength below 0 isn't meaningful for entropy purposes, so it's treated
#' as 0 here (matching the effect, though not the value, of the associations
#' the "0" is standing in for).
#'
#' @keywords internal
nonneg_shannon_entropy <- function(p) {
  p[p < 0] <- 0
  shannon_entropy(p)
}

uncfam_predictive_model <- function(params, data, control) {
  X <- params[["X"]] # learning rate (associative weight)
  B <- params[["B"]] # weighting of uncertainty vs. familiarity
  C <- params[["C"]] # decay
  beta <- 1 # maximum association value (fixed, per Kachergis et al.)

  reps <- control[["reps"]]
  start_matrix <- control[["start_matrix"]]
  test_noise <- control[["test_noise"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  traj <- list()
  if (!is.null(start_matrix)) {
    m <- start_matrix
  } else {
    m <- matrix(0, voc_sz, ref_sz) # association matrix
  }
  colnames(m) <- ref
  rownames(m) <- voc
  perf <- matrix(0, reps, voc_sz) # a row for each block
  # training
  for (rep in 1:reps) { # for trajectory experiments, train multiple times
    for (t in seq_along(data$words)) {

      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]

      m <- update_known(m, tr_w, tr_o) # what's been seen so far?

      # objects' uncertainty, computed before decay
      ent_o <- exp(B * apply(as.matrix(m[, tr_o]), 2, nonneg_shannon_entropy))

      m <- m * C # decay everything

      # for each word on the trial, update its associations to this trial's
      # objects in proportion to the prediction error (beta - predicted
      # value), rather than normalizing to distribute a fixed amount of
      # associative weight across the trial -- this lets initially-surprising
      # (mis-predicted) items draw more learning than a normalized model
      # would allow
      for (w in tr_w) {
        pred <- sum(m[w, tr_o])
        ent_w <- exp(B * nonneg_shannon_entropy(m[w, ]))
        m[w, tr_o] <- m[w, tr_o] + X * m[w, tr_o] * (beta - pred) * ent_w * ent_o
        # without normalization, a large learning rate can overshoot an
        # association below 0; left uncorrected this is unstable -- a
        # negative association inflates the next trial's (beta - pred) term,
        # which drives the association more negative still, diverging
        # exponentially over trials (observed reaching +-1e300 and beyond
        # within a 45-trial run for some candidate parameter draws during
        # DEoptim fitting). Association strength isn't meaningful below 0
        # (beta is described as the *maximum* value), so floor it at 0,
        # which removes the runaway feedback loop entirely.
        m[w, tr_o] <- pmax(m[w, tr_o], 0)
      }

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      traj[[index]] <- m
    }
    m_test <- m + test_noise # test noise constant k
    perf[rep, ] <- get_perf(m_test)
  }
  xslFit(perf = perf, matrix = m, traj = traj)
}

#' Predictive biased associative model (item-level prediction error)
#'
#' A variant of [uncfam()] (Kachergis et al. 2012's uncertainty- and
#' familiarity-biased associative model) that adds an item-level,
#' Rescorla-Wagner-style prediction error term: on each trial, the amount
#' learned about a word-object association is scaled by how much the word's
#' predicted association strength (summed over the trial's objects) falls
#' short of the maximum value, rather than normalizing to distribute a fixed
#' amount of associative weight across the trial. This lets initially
#' mis-paired ("surprising") items draw more learning than the un-normalized
#' original model allows.
#'
#' @inheritParams uncfam
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam_predictive(X = .1, C = 1, B = .98)
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam_predictive <- function(X, B, C) {
  xslMod(
    name = "uncfam_predictive_model",
    description = paste(
      "Biased associative model (Kachergis et al. 2012) with an added",
      "item-level, Rescorla-Wagner-style prediction error learning term"
    ),
    model = uncfam_predictive_model,
    params = list(X = X, B = B, C = C),
    stochastic = FALSE
  )
}
