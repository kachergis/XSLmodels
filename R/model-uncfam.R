uncfam_model <- function(params, data, control) {
  X <- params[["X"]] # associative weight to distribute
  B <- params[["B"]] # weighting of uncertainty vs. familiarity
  C <- params[["C"]] # decay
  variant <- params[["variant"]] # model variant

  reps <- control[["reps"]]
  start_matrix <- control[["start_matrix"]]
  test_noise <- control[["test_noise"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  freq_w <- rep(0, voc_sz) # freq[i] = times word i has appeared
  freq_o <- rep(0, ref_sz)
  names(freq_w) <- voc
  names(freq_o) <- ref
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

      if (variant %in% c("entropy", "uncertainty-only")) {
        term_w <- if (length(tr_w) > 1) apply(m[tr_w, ], 1, shannon_entropy) else shannon_entropy(m[tr_w, ])
        term_o <- apply(as.matrix(m[, tr_o]), 2, shannon_entropy)
      } else if (variant == "novelty") {
        freq_w[tr_w] <- freq_w[tr_w] + 1
        freq_o[tr_o] <- freq_o[tr_o] + 1
        term_w <- 1 / (1 + freq_w[tr_w])
        term_o <- 1 / (1 + freq_o[tr_o])
      }

      terms <- exp(B * term_w) %*% t(exp(B * term_o))
      if (variant != "uncertainty-only") terms <- assocs * terms

      m <- m * C # decay everything
      # update associations on this trial
      m[tr_w, tr_o] <- m[tr_w, tr_o] + (X * terms) / sum(terms)

      index <- (rep - 1) * length(data$words) + t  # index for learning trajectory
      traj[[index]] <- m
    }
    m_test <- m + test_noise # test noise constant k
    perf[rep, ] <- get_perf(m_test)
  }
  xslFit(perf = perf, matrix = m, traj = traj)
}

#' Kachergis 2012
#'
#' Kachergis et al. 2012 uncertainty- and familiarity-biased associative model
#'
#' @param X Associative weight to distribute
#' @param B Weighting of uncertainty vs. familiarity
#' @param C Decay
#' @param variant Which variant of the model to fit (one of "entropy",
#'   "novelty", "uncertainty-only").
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- uncfam(X = .1, C = 1, B = .98)
#' xsl_run(mod, get_example_ambiguous_condition())
#'
#' mod <- uncfam(X = .1, C = 1, B = .98, variant = "novelty")
#' xsl_run(mod, get_example_ambiguous_condition())
#'
#' mod <- uncfam(X = .1, C = 1, B = 0) # familiarity-only
#' xsl_run(mod, get_example_ambiguous_condition())
#'
#' mod <- uncfam(X = .1, C = 1, B = .98, variant = "uncertainty-only")
#' xsl_run(mod, get_example_ambiguous_condition())
uncfam <- function(X, B, C, variant = c("entropy", "novelty",
                                        "uncertainty-only")) {
  variant <- match.arg(variant)
  xslMod(
    name = "uncfam_model",
    description = "Kachergis et al. 2012 uncertainty- and familiarity-biased associative model",
    model = uncfam_model,
    params = list(X = X, B = B, C = C, variant = variant),
    stochastic = FALSE
  )
}

# TODO: X = chi, B = lambda, C = alpha
