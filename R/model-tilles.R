# Tilles and Fontanari's (2013) cross-situational word learning model
# George Kachergis  george.kachergis@gmail.com  September 16, 2013

# tilles_model() stops with tilles_stop() rather than returning NaN or
# negative "probabilities" in two cases:
# - "negative": eqs 2-13 are convex mixtures, and so keep associations
#   nonnegative, only while x, b and alpha (eq 7) are in [0, 1]. Eq 7's
#   entropy is normalized by its maximum, log2(n_o), so alpha is in
#   [alpha_0, 1], and associations can only go negative when a parameter is
#   outside [0, 1]. (Through XSLmodels 0.4.0, eq 7 divided entropy in bits by
#   log(n_o) in nats, as the original 2013 implementation did; alpha then fell
#   below 0 when alpha_0 < ~.31, which was the usual cause.) A negative row's
#   eq 7 entropy is NA, which would spread as NaN, and negative final
#   associations are invalid probabilities.
# - "zero": a word with no associations left, whose eq 7 entropy and
#   performance would be 0/0. Every word's associations sum to 1 (eqs 10 and
#   14) for x, b and alpha_0 in [0, 1], so this is a safety net; through
#   XSLmodels 0.4.0 it happened at b = 1 and at x = alpha_0 = 1, where mass
#   was lost when eq 2 had no new object to point to or when eqs 5 and 11 had
#   no prior association to be proportional to.
tilles_stop <- function(params, t, word, problem = c("negative", "zero")) {
  problem <- match.arg(problem)
  pars <- paste0("(x = ", params[["x"]], ", b = ", params[["b"]],
                 ", alpha_0 = ", params[["alpha_0"]], ")")
  stop(switch(problem,
    negative = paste0("tilles(): associations became negative by trial ", t,
                      " ", pars, ", so these parameters give invalid probabilities ",
                      "(x, b and alpha_0 must be in [0, 1])"),
    zero = paste0("tilles(): word ", word, " has no associations left by ",
                  "trial ", t, " ", pars, ", so its probabilities are 0/0 ",
                  "(x, b and alpha_0 must be in [0, 1])")), call. = FALSE)
}

tilles_model <- function(params, data, control) {
  x <- params[["x"]]
  b <- params[["b"]]
  alpha_0 <- params[["alpha_0"]]

  # reps <- control[["reps"]]
  # test_noise <- control[["test_noise"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects

  novel_w <- rep(TRUE, voc_sz) # set novel[i]=0 once i has appeared
  novel_o <- rep(TRUE, ref_sz)
  m <- matrix(0, voc_sz, ref_sz, dimnames = list(voc, ref)) # association probabilities

  # this model indexes m / novel_* by position, so map each trial's word and
  # object labels to their position in voc / ref (an identity map when the
  # labels are already 1..N, as in xsl_datasets, but not for a corpus)
  w_pos <- function(x) match(unlist(x), voc)
  o_pos <- function(x) match(unlist(x), ref)
  # compScore <- rep(0, nrow(data$words))
  # alpha_{t-1}: recomputed by eq 7 on every trial for every word seen so far
  # (see below)
  alpha <- rep(alpha_0, voc_sz)
  # training
  tr_w <- w_pos(data$words[[1]]); tr_w <- tr_w[!is.na(tr_w)]
  tr_o <- o_pos(data$objects[[1]]); tr_o <- tr_o[!is.na(tr_o)]
  # eq 1: 1/C. The paper's contexts have C words and C objects; with more
  # objects than words (e.g. 3x4 conditions), C is the number of objects, so
  # that each word's associations sum to 1 (eq 10)
  m[tr_w, tr_o] <- 1 / length(tr_o)
  novel_w[tr_w] <- FALSE
  novel_o[tr_o] <- FALSE

  for (t in 2:length(data$words)) {
    tr_w <- w_pos(data$words[[t]])
    tr_w <- tr_w[!is.na(tr_w)]
    tr_o <- o_pos(data$objects[[t]])
    tr_o <- tr_o[!is.na(tr_o)]

    # n_prev_w <- length(which(!novel_w)) # N_{t-1} for alpha_{t-1}
    # n_prev_o <- length(which(!novel_o))

    # update rule for context omega_t depends on membership in
    # 1) words and referents appearing for first time on trial t, or
    # 2) words and referents not appearing on t, but that have appeared before

    # objects
    cur_novel_o <- tr_o[which(novel_o[tr_o])] # ~ = current novel objects
    cur_old_o <- tr_o[which(!novel_o[tr_o])] # current trial's old objects
    other_old_o <- setdiff(which(!novel_o), cur_old_o) # stimuli appearing before, but not currently
    novel_o[tr_o] <- FALSE
    # words
    cur_novel_w <- tr_w[which(novel_w[tr_w])] # ~ = current novel stimuli
    cur_old_w <- tr_w[which(!novel_w[tr_w])] # current trial's old words
    other_old_w <- setdiff(which(!novel_w), cur_old_w) # stimuli appearing before, but not currently
    novel_w[tr_w] <- FALSE

    n_o <- length(which(!novel_o)) # number of distinct objects appearing up to and including N_t

    # preprocessing of novel words (eqs 2-4). Eq 2's C~_t is the number of new
    # pairs; the paper's new words and new objects are equally many, and here
    # it is the number of new objects, so that each new word's associations
    # sum to 1 (eq 10). If a new word appears with no new objects (its
    # referent was shown earlier as a foil), mutual exclusivity has no new
    # object to point to, and b goes to all of the trial's objects instead.
    if (length(cur_novel_w) != 0) {
      me_o <- if (length(cur_novel_o) != 0) cur_novel_o else tr_o
      m[cur_novel_w, cur_old_o] <- (1 - b) / n_o # Eqn 3
      m[cur_novel_w, other_old_o] <- (1 - b) / n_o # Eqn 4 (obj not appearing on current t, but earlier)
      m[cur_novel_w, me_o] <- b / length(me_o) + (1 - b) / n_o # Eqn 2
    }

    # eq 7: alpha_{t-1} for every word seen so far, this trial's and absent
    # ones alike, from the (preprocessed) associations before this trial's
    # update, as the paper's procedure specifies (p. 4)
    for (w in which(!novel_w)) {
      # eq 7's entropy is NA for a negative or all-zero row; stop instead of
      # letting NaN spread (see tilles_stop())
      if (any(m[w, ] < 0)) tilles_stop(params, t, voc[w], "negative")
      if (sum(m[w, ]) == 0) tilles_stop(params, t, voc[w], "zero")
      # eq 7: entropy normalized to [0, 1] by its maximum, log2(n_o)
      # (shannon_entropy() is in bits, and w's associations are all on the
      # n_o objects seen so far); with only one object seen there is no
      # uncertainty, rather than 0/0
      h <- if (n_o > 1) shannon_entropy(m[w, ]) / log2(n_o) else 0
      alpha[w] <- alpha_0 + (1 - alpha_0) * (1 - h)
    }

    # now for all stimuli on the current trial...
    # net gain of confidence for association w_i and o_j:

    flux <- rep(NA, voc_sz)
    r <- matrix(0, voc_sz, ref_sz) # sized to match m; only equals voc_sz x voc_sz when voc_sz == ref_sz

    # slow iterative method; rowSums(m) = 1
    for (w in tr_w) {
      # eq 5 shares the flux out of the absent objects, x * sum(m[w,
      # other_old_o]), among this trial's objects in proportion to w's prior
      # association with each. A word can be "old" yet have no association
      # with any of this trial's objects (e.g. always paired with different
      # objects until now); there is then nothing to be proportional to, and
      # the flux is distributed evenly instead, as the paper first describes
      # it (p. 3), so that w's associations still sum to 1 (eq 10)
      denom <- sum(m[w, tr_o])
      if (denom == 0) {
        r[w, tr_o] <- x * sum(m[w, other_old_o]) / length(tr_o)
      } else {
        flux[w] <- sum(m[w, other_old_o]) / denom # eq 5.1
        r[w, tr_o] <- x * m[w, tr_o] * flux[w] # eq 5.2
      }
      m[w, tr_o] <- m[w, tr_o] + alpha[w] * r[w, tr_o] + (1 - alpha[w]) * (1 / n_o - m[w, tr_o]) # eq 8
      m[w, other_old_o] <- m[w, other_old_o] - alpha[w] * x * m[w, other_old_o] + (1 - alpha[w]) * (1 / n_o - m[w, other_old_o]) # eq 9
    }

    # x=1 transfers confidences for absent objects to the currently ones
    # model also assumes that the more certain an association is, the more
    # efficiently that information should be used for reinforcement
    mt <- m
    # now update the other_old words
    flux <- rep(0, voc_sz)
    r <- matrix(0, voc_sz, ref_sz)
    # as in eq 5, an other_old word with no association with any absent
    # object gets eq 11's flux distributed evenly over them; and if no
    # object is absent, there is nowhere for eq 13's outflow to go, so b's
    # transfer is skipped (keeping eq 14's normalization in both cases)
    other_old_w_denom <- rowSums(m[other_old_w, other_old_o, drop = FALSE])
    other_old_w_num <- rowSums(m[other_old_w, tr_o, drop = FALSE])
    flux[other_old_w] <- ifelse(other_old_w_denom == 0, 0, other_old_w_num / other_old_w_denom) # eq 11.1
    r[other_old_w, other_old_o] <- b * mt[other_old_w, other_old_o] * flux[other_old_w] # eq 11.2
    even_w <- other_old_w_denom == 0
    if (any(even_w) && length(other_old_o) != 0) {
      r[other_old_w[even_w], other_old_o] <- b * other_old_w_num[even_w] / length(other_old_o)
    }
    b_out <- if (length(other_old_o) != 0) b else 0
    m[other_old_w, other_old_o] <- mt[other_old_w, other_old_o] + alpha[other_old_w] * r[other_old_w, other_old_o] + (1 - alpha[other_old_w]) * (1 / n_o - mt[other_old_w, other_old_o]) # eq 12
    m[other_old_w, tr_o] <- mt[other_old_w, tr_o] - alpha[other_old_w] * b_out * mt[other_old_w, tr_o] + (1 - alpha[other_old_w]) * (1 / n_o - mt[other_old_w, tr_o]) # eq 13

    # compScore[t] <- sum(diag(m))
  }
  t <- length(data$words)
  if (any(m < 0)) tilles_stop(params, t, NULL, "negative")
  perf <- diag(m) / rowSums(m)
  if (any(rowSums(m) == 0)) tilles_stop(params, t, voc[rowSums(m) == 0][1], "zero")

  # TODO: not using reps/traj?
  xslFit(perf = perf, matrix = m, traj = list())
}

#' Tilles and Fontanari (2013) reinforcement model
#'
#' @param x Reinforcement parameter for stimuli in current context
#' @param b Inference parameter, regulating ME and prior info integration
#'   (applies either to known words that do not appear in the current context or
#'   to new words in the current context)
#' @param alpha_0 Baseline efficiency corresponding to maximal uncertainty about
#'   referent of target word
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- tilles(x = 0.6, b = 0.8, alpha_0 = 0.85)
#' xsl_run(mod, get_example_ambiguous_condition())
tilles <- function(x, b, alpha_0) {
  xslMod(
    name = "tilles",
    description = "Tilles and Fontanari (2013) reinforcement model",
    model = tilles_model,
    params = list(x = x, b = b, alpha_0 = alpha_0),
    stochastic = FALSE
  )
}

# TODO: x = chi, b = beta
