softmax_rl_model <- function(params, data, control) {
  alpha <- params[["alpha"]] # learning rate
  beta <- params[["beta"]] # inverse temperature (choice decisiveness)

  reps <- control[["reps"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects

  q <- matrix(0, voc_sz, ref_sz) # Q[w,o]: value of guessing object o for word w
  colnames(q) <- ref; rownames(q) <- voc
  keep_traj <- isTRUE(control[["keep_traj"]])
  traj <- list()
  perf <- matrix(0, reps, voc_sz) # a row for each block

  # numerically stable softmax over a vector of Q-values
  softmax <- function(qrow) {
    z <- beta * (qrow - max(qrow))
    p <- exp(z)
    p / sum(p)
  }

  for (rep in 1:reps) { # for trajectory experiments, train multiple times
    for (t in seq_along(data$words)) {
      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]

      # for each word heard this trial: propose a referent by sampling from
      # the agent's own current policy over ALL known objects (not just
      # this trial's candidates) -- this is the model's current best guess
      # at that word's meaning, exactly like propose_but_verify/pursuit
      # propose a single hypothesis before checking it against the scene
      for (w in tr_w) {
        proposal <- sample.int(ref_sz, 1, prob = softmax(q[w, ]))
        # reward: was the proposed referent actually present in the scene?
        # (1 = confirmed, 0 = disconfirmed) -- the only feedback available
        # in a cross-situational (no trial-by-trial correct-answer) task
        reward <- as.numeric(proposal %in% tr_o)
        # standard Q-learning / delta-rule update, applied only to the
        # sampled action -- unlike every other model here, which updates
        # every word-object pair presented together on the trial
        q[w, proposal] <- q[w, proposal] + alpha * (reward - q[w, proposal])
      }

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      if (keep_traj) traj[[index]] <- q
    }
    # test-time choice uses the same softmax policy the model learned
    # with, rather than a different (e.g. Luce/get_perf) decision rule
    perf[rep, ] <- vapply(seq_len(voc_sz), \(w) softmax(q[w, ])[w], numeric(1))
  }
  xslFit(perf = perf, matrix = q, traj = traj)
}

#' Softmax reinforcement learning model
#'
#' A Q-learning-style reinforcement learning model of cross-situational word
#' learning. Unlike every other model in this package, which updates every
#' word-object pair presented together on a trial, this model makes a
#' discrete choice: for each word heard, it samples a single candidate
#' referent from a softmax policy over its current values for *all* known
#' objects (its current best guess at that word's meaning), then updates
#' only that guess's value via a standard TD/delta-rule update, using
#' whether the guess was actually present in the scene as the reward (1 =
#' "confirmed", 0 = "disconfirmed"). This is the only feedback available in
#' a task with no trial-by-trial correct answer, and mirrors the
#' propose-and-verify logic of [propose_but_verify()]/[pursuit()], but
#' replaces their threshold-based keep/discard rules with graded value
#' learning and an explicit (softmax) exploration/exploitation tradeoff.
#'
#' @param alpha Learning rate
#' @param beta Inverse temperature: how deterministically the model favors
#'   its highest-valued guess (0 = uniformly random guessing, large = always
#'   picks the current best guess)
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- softmax_rl(alpha = .3, beta = 3)
#' xsl_run(mod, get_example_ambiguous_condition())
softmax_rl <- function(alpha, beta) {
  xslMod(
    name = "softmax_rl",
    description = paste(
      "Q-learning-style reinforcement learning model: samples a candidate",
      "referent per word from a softmax policy over its values, and",
      "updates only that guess via a TD/delta-rule reward (1 if the guess",
      "was present in the scene, 0 otherwise)"
    ),
    model = softmax_rl_model,
    params = list(alpha = alpha, beta = beta),
    stochastic = TRUE
  )
}
