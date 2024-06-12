# Define a simple likelihood function that only updates appropriate rows and columns
bd_likelihood <- function(words, objects, alpha, delta) {
  ## With alpha=0, this enforces a mutual exclusivity constraint...
  mat <- outer(words, objects) + outer(!words, !objects)
  ## Can relax mutual exclusivity constraint by increasing alpha or allowing for one of the following
  ## 1) Objects can have multiple words applied to them
  # mat <- outer(words,objects) + outer(!words,ones)
  ## 2) Words can refer to multiple objects
  # mat <- outer(words,objects) + outer(ones,!objects)
  alpha ^ (1 - mat) + (delta - 1) * outer(words, objects)
}

bayesian_decay_model <- function(params, data, control) {
  # Define noise probability (when alpha = 0, Bayesian model is a deterministic 'ideal observer')
  alpha <- params[["alpha"]] # 0.1, 0.5, 0.9 decay for word/object non-co-occurrence
  delta <- params[["delta"]] # Multiplier for word/object co-occurrence (set to 1 for no increase)
  reps <- control[["reps"]]

  voc <- unique(unlist(data$words))
  ref <- unique(unlist(data$objects[!is.na(data$objects)]))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects

  perf <- matrix(0, reps, voc_sz) # a row for each block
  traj <- list()

  ones <- rep(1, voc_sz)
  p_wgo <- matrix(1 / ref_sz, voc_sz, ref_sz) # prob(word|object) matrix
  pw_o <- matrix(1 / (ref_sz * voc_sz), voc_sz, ref_sz)
  colnames(p_wgo) <- ref
  rownames(p_wgo) <- voc
  colnames(pw_o) <- ref
  rownames(pw_o) <- voc
  # training
  for (rep in 1:reps) { # for trajectory experiments, train multiple times
    for (t in seq_along(data$words)) {
      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]

      words_tr <- rep(0, voc_sz)
      objects_tr <- rep(0, ref_sz)
      names(words_tr) <- voc
      names(objects_tr) <- ref
      words_tr[tr_w] <- 1
      objects_tr[tr_o] <- 1

      likelihood <- bd_likelihood(words_tr, objects_tr, alpha, delta)
      p_wgo <- likelihood * p_wgo
      p_wgo <- p_wgo / outer(ones, colSums(p_wgo)) # rowSums(p_wgo)
      pw_o <- likelihood * pw_o
      pw_o <- pw_o / sum(pw_o)

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      traj[[index]] <- p_wgo
    }
    # power choice rule
    perf[rep, ] <- get_perf(p_wgo)
  }

  xslFit(perf = perf, matrix = p_wgo, traj = traj)
}

#' Bayesian decay model
#'
#' @param alpha Decay for word/object non-co-occurrence (0.1, 0.5, 0.9)
#' @param delta Multiplier for word/object co-occurrence (1 for no increase)
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- bayesian_decay(alpha = 0.5, delta = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
bayesian_decay <- function(alpha, delta) {
  xslMod(
    name = "bayesian_decay",
    description = "Bayesian model of cross situational learning (originally conceived by Stephen Denton, Apr. 20, 2010)",
    model = bayesian_decay_model,
    params = list(alpha = alpha, delta = delta),
    stochastic = FALSE
  )
}
