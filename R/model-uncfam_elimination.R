uncfam_elimination_model <- function(params, data, control) {
  X <- params[["X"]] # associative weight to distribute
  B <- params[["B"]] # weighting of uncertainty vs. familiarity
  C <- params[["C"]] # decay
  eps <- params[["eps"]] # weight of elimination (mutual-exclusivity) inference

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
      assocs <- m[tr_w, tr_o, drop = FALSE]

      # uncfam()'s allocation: familiarity x uncertainty, normalized over the trial
      term_w <- apply(m[tr_w, , drop = FALSE], 1, shannon_entropy)
      term_o <- apply(m[, tr_o, drop = FALSE], 2, shannon_entropy)
      terms <- assocs * (exp(B * term_w) %*% t(exp(B * term_o)))
      bam_share <- terms / sum(terms)

      # elimination: how confidently each on-trial pair (w', o') already go
      # together, P(o'|w') * P(w'|o'); a pair (w, o) is supported to the
      # extent that the trial's *other* words and objects are accounted for
      # by each other (summing over w' != w, o' != o)
      p_o_given_w <- assocs / rowSums(m[tr_w, , drop = FALSE])
      p_w_given_o <- t(t(assocs) / colSums(m[, tr_o, drop = FALSE]))
      q <- p_o_given_w * p_w_given_o
      support <- sum(q) - outer(rowSums(q), colSums(q), "+") + q
      support <- pmax(support, 0) # guard against floating-point negatives

      share <- (bam_share + eps * support) / (1 + eps * sum(support))

      m <- m * C # decay everything
      # update associations on this trial
      m[tr_w, tr_o] <- m[tr_w, tr_o] + X * share

      index <- (rep - 1) * length(data$words) + t  # index for learning trajectory
      if (keep_traj) traj[[index]] <- m
    }
    m_test <- m + test_noise # test noise constant k
    perf[rep, ] <- get_perf(m_test)
  }
  xslFit(perf = perf, matrix = m, traj = traj)
}

#' Biased associative model with learning by elimination
#'
#' A variant of [uncfam()] (Kachergis et al. 2012's uncertainty- and
#' familiarity-biased associative model) that adds inference by elimination
#' (mutual exclusivity) to how a trial's associative weight is allocated. If
#' the other words and objects on a trial are already confidently paired with
#' each other, the remaining word and object probably go together, so their
#' pairing should draw attention even though it is still weak. In
#' [uncfam()], by contrast, an already-strong pair on the trial draws weight
#' *away* from such a pairing.
#'
#' On each trial, each on-trial pair \eqn{(w', o')} has a mutual confidence
#' \eqn{q(w', o') = P(o'|w') P(w'|o')}, from the row- and column-normalized
#' associations. A pair \eqn{(w, o)} gets elimination support
#' \eqn{s(w, o) = \sum_{w' \ne w, o' \ne o} q(w', o')}: how much of the rest
#' of the trial is accounted for by other pairs. The share of the trial's
#' weight \eqn{X} given to \eqn{(w, o)} is then
#' \deqn{\frac{b(w, o) + \epsilon s(w, o)}{1 + \epsilon \sum s}}
#' where \eqn{b(w, o)} is [uncfam()]'s normalized familiarity-and-uncertainty
#' share, so `eps = 0` reproduces [uncfam()] exactly.
#'
#' Motivated by the initial-accuracy experiment
#' ([kachergis_initial_accuracy]), where learners were more accurate on an
#' initially mis-paired word the more of its study trials it shared with a
#' word they already knew.
#'
#' @param X Associative weight to distribute
#' @param B Weighting of uncertainty vs. familiarity
#' @param C Decay
#' @param eps Weight of elimination inference (0 = identical to [uncfam()])
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam_elimination(X = .1, B = .98, C = 1, eps = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam_elimination <- function(X, B, C, eps) {
  xslMod(
    name = "uncfam_elimination_model",
    description = "uncfam() with learning by elimination (mutual exclusivity)",
    model = uncfam_elimination_model,
    params = list(X = X, B = B, C = C, eps = eps),
    stochastic = FALSE
  )
}
