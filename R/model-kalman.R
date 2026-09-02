kalman_filter_model <- function(params, data, control) {
  tau2 <- params[["tau2"]] # process (diffusion) noise: uncertainty growth per trial
  sigma2_obs <- params[["sigma2_obs"]] # observation noise: how uninformative a single co-occurrence is
  sigma2_0 <- params[["sigma2_0"]] # initial (prior) uncertainty about every association

  reps <- control[["reps"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects

  mu <- matrix(0, voc_sz, ref_sz) # association strength estimate
  sigma2 <- matrix(sigma2_0, voc_sz, ref_sz) # uncertainty (variance) about each estimate
  colnames(mu) <- ref; rownames(mu) <- voc
  colnames(sigma2) <- ref; rownames(sigma2) <- voc
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

      # diffusion: every association's uncertainty grows with the passage of
      # time (a trial), whether or not it was observed on it -- this is what
      # keeps the model able to keep learning/revise rather than converging
      # to a fixed point, the Kalman-filter analog of "decay"
      sigma2 <- sigma2 + tau2

      # every word-object combination presented together this trial is
      # treated as a (noisy) observation that they co-occurred (target = 1),
      # exactly like every other associative model in this package tallies
      # the full cross-product of a trial's words and objects
      kalman_gain <- sigma2[tr_w, tr_o] / (sigma2[tr_w, tr_o] + sigma2_obs)
      pred_error <- 1 - mu[tr_w, tr_o]
      mu[tr_w, tr_o] <- mu[tr_w, tr_o] + kalman_gain * pred_error
      sigma2[tr_w, tr_o] <- (1 - kalman_gain) * sigma2[tr_w, tr_o]

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      if (keep_traj) traj[[index]] <- mu
    }
    perf[rep, ] <- get_perf(mu)
  }
  xslFit(perf = perf, matrix = mu, traj = traj)
}

#' Kalman filter associative model
#'
#' A Kalman-filter generalization of [rescorla_wagner()]'s error-driven
#' update rule (Dayan & Kakade, 2000; Kruschke, 2008; Gershman, 2015): each
#' word-object association is tracked as a Gaussian belief (mean `mu` and
#' variance `sigma2`), and each trial's update is scaled by that pair's own
#' Kalman gain, `sigma2 / (sigma2 + sigma2_obs)`. This makes the effective
#' learning rate adaptive rather than fixed -- large (fast learning) while
#' an association is still uncertain, and automatically shrinking as
#' confidence accumulates -- and it emerges from three interpretable
#' parameters rather than being hand-set. Between observations, every
#' association's variance grows by `tau2` (the model assumes true
#' associations drift slowly over time), which keeps the model able to
#' revise a belief rather than converging to a fixed point.
#'
#' @param tau2 Process (diffusion) noise: how much uncertainty about every
#'   association grows per trial, whether or not it was observed
#' @param sigma2_obs Observation noise: how uninformative a single
#'   co-occurrence observation is (larger = slower learning per trial)
#' @param sigma2_0 Initial (prior) uncertainty about every association,
#'   before any training
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- kalman_filter(tau2 = .01, sigma2_obs = .5, sigma2_0 = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
kalman_filter <- function(tau2, sigma2_obs, sigma2_0) {
  xslMod(
    name = "kalman_filter",
    description = paste(
      "Kalman filter associative model: tracks each word-object association",
      "as a Gaussian belief (mean and variance) and updates it via a",
      "Kalman-gain-scaled prediction error, giving an adaptive learning",
      "rate that starts high under uncertainty and shrinks with confidence",
      "(Dayan & Kakade 2000; Kruschke 2008)"
    ),
    model = kalman_filter_model,
    params = list(tau2 = tau2, sigma2_obs = sigma2_obs, sigma2_0 = sigma2_0),
    stochastic = FALSE
  )
}
