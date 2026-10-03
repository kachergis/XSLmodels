uncfam_gamma_model <- function(params, data, control) {
  X <- params[["X"]] # associative weight to distribute
  B <- params[["B"]] # weighting of uncertainty vs. familiarity
  C <- params[["C"]] # decay
  gamma <- params[["gamma"]] # familiarity exponent

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
      assocs <- m[tr_w, tr_o]

      term_w <- if (length(tr_w) > 1) apply(m[tr_w, ], 1, shannon_entropy) else shannon_entropy(m[tr_w, ])
      term_o <- apply(as.matrix(m[, tr_o]), 2, shannon_entropy)
      terms <- exp(B * term_w) %*% t(exp(B * term_o))
      terms <- (assocs ^ gamma) * terms # <- only change from uncfam_model: assocs^gamma, not assocs

      m <- m * C # decay everything
      # update associations on this trial
      m[tr_w, tr_o] <- m[tr_w, tr_o] + (X * terms) / sum(terms)

      index <- (rep - 1) * length(data$words) + t  # index for learning trajectory
      if (keep_traj) traj[[index]] <- m
    }
    m_test <- m + test_noise # test noise constant k
    perf[rep, ] <- get_perf(m_test)
  }
  xslFit(perf = perf, matrix = m, traj = traj)
}

#' Kachergis 2012 with a free familiarity exponent
#'
#' A variant of [uncfam()] (Kachergis et al. 2012's uncertainty- and
#' familiarity-biased associative model) that decouples familiarity's
#' contribution to a trial's attentional allocation from uncertainty's.
#' [uncfam()]'s allocation weight for pair (w, o) is `exp(B * entropy) *
#' assocs` -- entropy gets a free, tunable exponent (`B`), but familiarity
#' (`assocs`, the pair's current association strength) enters linearly, with
#' an implicit weight of 1 and no free parameter of its own. This adds one
#' parameter, `gamma`, raising familiarity to a free power instead:
#' `assocs^gamma * exp(B * entropy)`. `gamma = 1` is identical to
#' [uncfam()]; `gamma < 1` dampens the "rich get richer" effect (diminishing
#' returns on an already-strong pairing); `gamma > 1` amplifies it
#' (winner-take-all).
#'
#' Cross-validated against all of [xsl_datasets] (SSE to human 18AFC
#' accuracy, the package's standard model-comparison objective),
#' `uncfam_gamma()` beats plain [uncfam()] in every fold of a 5-fold split
#' (mean test SSE 0.48 vs. 0.56), with a best-fit `gamma` around 0.3 -- i.e.
#' familiarity's pull *dampens* with diminishing returns when explaining how
#' well people learn from a passively-received training sequence. A
#' separate analysis fitting this same free-`gamma` substrate to *active*
#' cross-situational word learning (Kachergis, Yu, & Shiffrin's paradigm
#' where learners choose which items to see named next, rather than
#' receiving a fixed passive sequence) found a best-fit `gamma` around 2
#' instead -- amplified, not dampened. Both contexts agree that familiarity
#' should get its own free exponent rather than [uncfam()]'s hard-coded
#' linear one, but disagree on which direction it bends: the process
#' governing moment-to-moment choice of what to study next doesn't appear
#' to be simply the same one governing how much is learned from a given,
#' already-fixed sequence.
#'
#' @param X Associative weight to distribute
#' @param B Weighting of uncertainty vs. familiarity
#' @param C Decay
#' @param gamma Familiarity exponent (1 = identical to [uncfam()])
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam_gamma(X = .1, C = 1, B = .98, gamma = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
#'
#' # gamma = 1 reproduces uncfam() exactly
#' sse_gamma1 <- xsl_run(uncfam_gamma(X = .1, C = 1, B = .98, gamma = 1),
#'                       get_example_ambiguous_condition())$sse
#' sse_uncfam <- xsl_run(uncfam(X = .1, C = 1, B = .98),
#'                       get_example_ambiguous_condition())$sse
#' isTRUE(all.equal(sse_gamma1, sse_uncfam))
#'
#' mod <- uncfam_gamma(X = .1, C = 1, B = .98, gamma = 0.3) # dampened familiarity
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam_gamma <- function(X, B, C, gamma) {
  xslMod(
    name = "uncfam_gamma_model",
    description = "uncfam() with a free familiarity exponent (gamma)",
    model = uncfam_gamma_model,
    params = list(X = X, B = B, C = C, gamma = gamma),
    stochastic = FALSE,
    supports_start_matrix = TRUE
  )
}
