kalman_filter_model <- function(params, data, control) {
  tau2 <- params[["tau2"]] # process (diffusion) noise: uncertainty growth per trial
  sigma2_obs <- params[["sigma2_obs"]] # observation noise: how uninformative a single co-occurrence is
  sigma2_0 <- params[["sigma2_0"]] # initial (prior) uncertainty about every association
  full <- identical(params[["covariance"]] %||% "independent", "full")
  all_outcomes <- identical(params[["outcomes"]] %||% "present", "all")
  phi <- params[["phi"]]

  reps <- control[["reps"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects

  mu <- matrix(0, voc_sz, ref_sz) # association strength estimate
  sigma2 <- matrix(sigma2_0, voc_sz, ref_sz) # uncertainty (variance) about each estimate
  colnames(mu) <- ref; rownames(mu) <- voc
  colnames(sigma2) <- ref; rownames(sigma2) <- voc
  # full covariance: each object's weights over words have a covariance
  # matrix. Covariance updates don't depend on the outcome, so with outcomes =
  # "all" every object's is the same (one matrix); otherwise one per object
  if (full) {
    S <- if (all_outcomes) diag(sigma2_0, voc_sz) else replicate(ref_sz, diag(sigma2_0, voc_sz), simplify = FALSE)
  }
  keep_traj <- isTRUE(control[["keep_traj"]])
  traj <- list()
  perf <- matrix(0, reps, voc_sz) # a row for each block

  for (rep in 1:reps) { # for trajectory experiments, train multiple times
    for (t in seq_along(data$words)) {
      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]
      wi <- match(tr_w, voc)
      oi <- match(tr_o, ref)
      # outcomes updated this trial: the present objects (target 1), plus,
      # with outcomes = "all", every absent object (target 0)
      outs <- if (all_outcomes) seq_len(ref_sz) else oi
      target <- as.numeric(outs %in% oi)

      if (!full) {
        # diffusion: every association's uncertainty grows with the passage of
        # time (a trial), whether or not it was observed on it -- this is what
        # keeps the model able to keep learning/revise rather than converging
        # to a fixed point, the Kalman-filter analog of "decay"
        sigma2 <- sigma2 + tau2

        # each present word's association with each outcome is its own
        # filter, observing the target (1 for a co-occurrence)
        kalman_gain <- sigma2[wi, outs, drop = FALSE] / (sigma2[wi, outs, drop = FALSE] + sigma2_obs)
        pred_error <- matrix(target, length(wi), length(outs), byrow = TRUE) - mu[wi, outs, drop = FALSE]
        mu[wi, outs] <- mu[wi, outs, drop = FALSE] + kalman_gain * pred_error
        sigma2[wi, outs] <- (1 - kalman_gain) * sigma2[wi, outs, drop = FALSE]
      } else {
        # diffusion, as above, for every object's covariance
        if (all_outcomes) S <- S + diag(tau2, voc_sz) else S <- lapply(S, \(Sj) Sj + diag(tau2, voc_sz))
        # the present words jointly predict each outcome (the sum of their
        # weights), with one prediction error per outcome and a vector Kalman
        # gain over words (Dayan & Kakade, 2001)
        x <- numeric(voc_sz); x[wi] <- 1
        if (!length(wi)) {
          # no cues: nothing is predicted or learned
        } else if (all_outcomes) {
          Sx <- drop(S %*% x)
          k <- Sx / (sum(x * Sx) + sigma2_obs)
          mu <- mu + outer(k, target - colSums(mu[wi, , drop = FALSE]))
          S <- S - tcrossprod(k, Sx)
        } else {
          for (n in seq_along(outs)) {
            j <- outs[n]
            Sx <- drop(S[[j]] %*% x)
            k <- Sx / (sum(x * Sx) + sigma2_obs)
            mu[, j] <- mu[, j] + k * (target[n] - sum(mu[wi, j]))
            S[[j]] <- S[[j]] - tcrossprod(k, Sx)
          }
        }
      }

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      if (keep_traj) traj[[index]] <- mu
    }
    perf[rep, ] <- get_perf(if (is.null(phi)) mu else softmax_weights(mu, phi))
  }
  xslFit(perf = perf, matrix = mu, traj = traj)
}

#' Kalman filter associative model
#'
#' A Kalman-filter generalization of [rescorla_wagner()]'s error-driven
#' update rule (Dayan & Kakade, 2001; Kruschke, 2008; Gershman, 2015): word-
#' object associations are tracked as Gaussian beliefs, and each trial's
#' update is scaled by a Kalman gain, `sigma2 / (sigma2 + sigma2_obs)`. This
#' makes the effective learning rate adaptive rather than fixed -- large (fast
#' learning) while an association is still uncertain, and automatically
#' shrinking as confidence accumulates -- and it emerges from three
#' interpretable parameters rather than being hand-set. Between observations,
#' every association's variance grows by `tau2` (the model assumes true
#' associations drift slowly over time), which keeps the model able to revise
#' a belief rather than converging to a fixed point.
#'
#' By default (`covariance = "independent"`), every word-object association
#' is its own one-dimensional filter that observes 1 whenever the pair
#' co-occurs. Associations then don't interact: there is no competition
#' between a trial's words, and an association is a saturating function of
#' how often and how recently its pair co-occurred. With a small gain it is
#' simply co-occurrence counting.
#'
#' @section Options:
#' - `covariance = "full"` is the Kalman filter of the associative-learning
#'   literature (Dayan & Kakade, 2001; Kruschke, 2008). Each object (outcome)
#'   has a weight vector over words with a full covariance matrix. The
#'   present words jointly predict the object (the sum of their weights), so
#'   they share one prediction error, and the gain is a vector over words.
#'   Words that appear together come to have negatively correlated weights,
#'   so later evidence about one revises the other: explaining away and
#'   backward blocking (retrospective revaluation), as when learning that one
#'   word on an earlier trial named an object implies the trial's other words
#'   did not. Memory and time grow with the square of the vocabulary.
#' - `outcomes = "all"` treats every object as an outcome on every trial,
#'   observing 1 if it is present and 0 if absent, as in the canonical
#'   filter, rather than updating only the objects present. Associations can
#'   then become negative, which Luce choice treats as 0; `phi` gives a
#'   choice rule defined for them.
#' - `phi` replaces Luce choice with a softmax choice rule with inverse
#'   temperature `phi` (see [softmax_weights()]).
#'
#' Note that the three noise parameters share a scale redundancy:
#' multiplying `tau2`, `sigma2_obs` and `sigma2_0` together by any constant
#' leaves every Kalman gain -- and hence the learned matrix -- unchanged. Only
#' two of the three are identifiable; when fitting, hold one fixed
#' (conventionally `sigma2_obs`).
#'
#' @param tau2 Process (diffusion) noise: how much uncertainty about every
#'   association grows per trial, whether or not it was observed
#' @param sigma2_obs Observation noise: how uninformative a single
#'   co-occurrence observation is (larger = slower learning per trial)
#' @param sigma2_0 Initial (prior) uncertainty about every association,
#'   before any training
#' @param covariance `"independent"` (the default): one filter per
#'   word-object pair. `"full"`: one filter per object, over all words, with
#'   a full covariance.
#' @param outcomes `"present"` (the default) updates only the objects present
#'   on a trial; `"all"` also updates absent objects toward 0.
#' @param phi Optional softmax inverse temperature for the choice rule.
#'   `NULL` (the default) uses Luce choice.
#'
#' @references
#' Dayan, P., & Kakade, S. (2001). Explaining away in weight space. In
#' *Advances in Neural Information Processing Systems 13* (pp. 451-457).
#'
#' Gershman, S. J. (2015). A unifying probabilistic view of associative
#' learning. *PLoS Computational Biology, 11*(11), e1004567.
#'
#' Kruschke, J. K. (2008). Bayesian approaches to associative learning: From
#' passive to active learning. *Learning & Behavior, 36*(3), 210-226.
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- kalman_filter(tau2 = .01, sigma2_obs = .5, sigma2_0 = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
#' # the full-covariance filter, with absent objects as outcomes
#' mod <- kalman_filter(tau2 = .001, sigma2_obs = 1, sigma2_0 = .5,
#'                      covariance = "full", outcomes = "all")
#' xsl_run(mod, get_example_ambiguous_condition())
kalman_filter <- function(tau2, sigma2_obs, sigma2_0, covariance = c("independent", "full"),
                          outcomes = c("present", "all"), phi = NULL) {
  covariance <- match.arg(covariance)
  outcomes <- match.arg(outcomes)
  params <- list(tau2 = tau2, sigma2_obs = sigma2_obs, sigma2_0 = sigma2_0,
                 covariance = covariance, outcomes = outcomes)
  if (!is.null(phi)) params$phi <- phi
  xslMod(
    name = "kalman_filter",
    description = paste(
      "Kalman filter associative model: tracks each word-object association",
      "as a Gaussian belief (mean and variance) and updates it via a",
      "Kalman-gain-scaled prediction error, giving an adaptive learning",
      "rate that starts high under uncertainty and shrinks with confidence",
      "(Dayan & Kakade 2001; Kruschke 2008)"
    ),
    model = kalman_filter_model,
    params = params,
    stochastic = FALSE
  )
}
