uncfam_attention_model <- function(params, data, control) {
  X <- params[["X"]] # associative weight to distribute
  B <- params[["B"]] # weighting of uncertainty vs. familiarity
  C <- params[["C"]] # decay

  reps <- control[["reps"]]
  start_matrix <- control[["start_matrix"]]
  test_noise <- control[["test_noise"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  keep_traj <- isTRUE(control[["keep_traj"]])
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

      # entropy of every word/object (not just this trial's), so that the
      # per-trial learning rate can be scaled relative to the overall mean
      ent_w_all <- apply(m, 1, shannon_entropy)
      ent_o_all <- apply(m, 2, shannon_entropy)

      # scale the associative weight distributed this trial by how uncertain
      # (high-entropy) this trial's objects are, relative to the mean
      # uncertainty of all objects seen so far -- representing the theory
      # that learners are more alert on trials with more uncertain items
      # (e.g. in the Low Initial Accuracy condition)
      tr_mean_ent_o <- mean(ent_o_all[tr_o], na.rm = TRUE)
      mean_ent_o <- mean(ent_o_all, na.rm = TRUE)
      # before any entropy has accumulated (e.g. very early in training),
      # mean_ent_o can be exactly 0 or NA; fall back to the unscaled rate
      # rather than dividing by zero
      scaled_X <- if (!is.finite(mean_ent_o) || mean_ent_o == 0) {
        X
      } else {
        X * (tr_mean_ent_o / mean_ent_o)
      }

      ent_w <- exp(B * ent_w_all[tr_w])
      ent_o <- exp(B * ent_o_all[tr_o])
      assocs <- m[tr_w, tr_o]
      terms <- assocs * (ent_w %*% t(ent_o))

      m <- m * C # decay everything
      # update associations on this trial
      m[tr_w, tr_o] <- m[tr_w, tr_o] + (scaled_X * terms) / sum(terms)

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      if (keep_traj) traj[[index]] <- m
    }
    m_test <- m + test_noise # test noise constant k
    perf[rep, ] <- get_perf(m_test)
  }
  xslFit(perf = perf, matrix = m, traj = traj)
}

#' Biased associative model with attention scaled to trial uncertainty
#'
#' A variant of [uncfam()] (Kachergis et al. 2012's uncertainty- and
#' familiarity-biased associative model) in which the associative weight
#' distributed on a trial is additionally scaled by the ratio of that
#' trial's mean object entropy to the mean entropy of all objects seen so
#' far. This implements the "system-level" theory proposed by Fitneva &
#' Christiansen (2015): that learners are more alert (i.e. allocate a higher
#' learning rate) on trials with more uncertain items, such as in a Low
#' Initial Accuracy condition where many items have been mis-paired.
#'
#' @inheritParams uncfam
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam_attention(X = .1, C = 1, B = .98)
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam_attention <- function(X, B, C) {
  xslMod(
    name = "uncfam_attention_model",
    description = paste(
      "Biased associative model (Kachergis et al. 2012) with learning rate",
      "scaled by relative trial uncertainty (Fitneva & Christiansen 2015",
      "system-level attention account)"
    ),
    model = uncfam_attention_model,
    params = list(X = X, B = B, C = C),
    stochastic = FALSE
  )
}
