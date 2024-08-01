# Rescorla-Wagner (1972) associative learning model
# adapted for cross-situational word learning by
# George Kachergis  george.kachergis@gmail.com

rescorla_wagner_model <- function(params, data, control) {
  C <- params[["C"]]
  alpha <- params[["alpha"]]
  lambda <- params[["lambda"]]
  beta <- params[["beta"]] * lambda

  reps <- control[["reps"]]
  test_noise <- control[["test_noise"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  traj <- list()
  m <- matrix(0, voc_sz, ref_sz) # association matrix
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

      # if objects are cues that predict words, then we want colSums;
      # if words are cues, use rowSums--but only of the currently-presented stimuli
      if (length(tr_w) == 1) {
        pred <- m[tr_w, tr_o] # should prediction be based on entire col of obj assocs?
      } else if (length(tr_o) == 1) {
        pred <- sum(m[tr_w, tr_o])
      } else {
        pred <- colSums(m[tr_w, tr_o])
      }
      delta <- alpha * beta * (lambda - pred)
      m[tr_w, tr_o] <- m[tr_w, tr_o] + delta

      m <- m * C

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      traj[[index]] <- m
    }

    m_test <- m + test_noise # test noise constant k
    perf[rep, ] <- get_perf(m_test)
  }

  xslFit(perf = perf, matrix = m, traj = traj)
}

#' Rescorla-Wagner (1972) error-driven associative model
#'
#' @param C Decay
#' @param alpha Salience (fix at 1 unless manipulated)
#' @param beta Learning rate -- a proportion of lambda
#' @param lambda Maximum associative value that a CS can achieve - should be
#'   larger than learning rate
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- rescorla_wagner(C = 1, alpha = 1, beta = 0.1, lambda = 3)
#' xsl_run(mod, get_example_ambiguous_condition())
rescorla_wagner <- function(C, alpha, beta, lambda) {
  xslMod(
    name = "rescorla_wagner",
    description = "Rescorla-Wagner (1972) error-driven associative model",
    model = rescorla_wagner_model,
    params = list(C = C, alpha = alpha, beta = beta, lambda = lambda),
    stochastic = FALSE
  )
}
