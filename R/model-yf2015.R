# Trial-by-trial generalization of Yurovsky & Frank's (2015) Integrated model,
# runnable on any xslData (see model-yurovsky_frank2015.R for the paper's
# closed-form version of one paradigm).
#
# Each time a word is heard it picks a hypothesized referent among the objects
# present, then lays down a memory trace for every present object: gamma *
# sigma for the hypothesis, gamma * (1 - sigma) / (n - 1) for each other one.
# Traces decay as a power function of time since they were laid down, and
# strength is their sum. Learners choose (hypotheses during training, and
# referents at test) by the remember-or-guess rule (remember_guess()).
#
# George Kachergis + Claude, November 2026

# A learner's memory: timestamped traces per word, with power-law decay.
yf_learner <- function(sigma, lambda, gamma, mu, voc_sz, ref_sz) {
  times <- vector("list", voc_sz) # per word: when each trace was laid down
  amts <- vector("list", voc_sz)  # per word: traces x objects
  for (w in seq_len(voc_sz)) amts[[w]] <- matrix(0, 0, ref_sz)

  # Strengths of words wi with every object at time `now` (> every trace).
  strength <- function(wi, now) {
    out <- matrix(0, length(wi), ref_sz)
    for (j in seq_along(wi)) {
      tk <- times[[wi[j]]]
      if (length(tk)) out[j, ] <- colSums(amts[[wi[j]]] * (now - tk)^(-lambda))
    }
    out
  }

  # Guessing weights over objects oi for words wi: exp(-mu * competition),
  # where an object's competition is its strength with every other word.
  guess_weights <- function(wi, oi, s, now) {
    if (mu == 0) return(matrix(1, length(wi), length(oi)))
    tot <- colSums(strength(seq_len(voc_sz), now)[, oi, drop = FALSE])
    comp <- matrix(tot, length(wi), length(oi), byrow = TRUE) - s
    exp(-mu * (comp - apply(comp, 1, min)))
  }

  # Lay down word w's traces with objects oi, given the probability that each
  # one is its hypothesis (one-hot when the hypothesis is sampled).
  encode <- function(w, oi, now, p_hyp) {
    n <- length(oi)
    a <- if (n == 1) gamma else gamma * (sigma * p_hyp + (1 - sigma) / (n - 1) * (1 - p_hyp))
    trace <- numeric(ref_sz)
    trace[oi] <- a
    times[[w]] <<- c(times[[w]], now)
    amts[[w]] <<- rbind(amts[[w]], trace)
  }

  # One training trial at time `now`. Sampling: the words, in random order,
  # each sample a hypothesis among the objects not yet claimed on the trial
  # (all of them again once every object is claimed). Mean field: each word
  # encodes its expected traces, over its own hypothesis distribution.
  trial <- function(wi, oi, now, sampling) {
    s <- strength(wi, now)[, oi, drop = FALSE]
    g <- guess_weights(wi, oi, s, now)
    n <- length(oi)
    if (sampling) {
      free <- rep(TRUE, n)
      for (j in sample.int(length(wi))) {
        if (!any(free)) free[] <- TRUE
        idx <- which(free)
        p <- rg_probs(s[j, idx], g[j, idx])
        h <- idx[sample.int(length(idx), 1, prob = p)]
        free[h] <- FALSE
        encode(wi[j], oi, now, as.numeric(seq_len(n) == h))
      }
    } else {
      for (j in seq_along(wi)) encode(wi[j], oi, now, rg_probs(s[j, ], g[j, ]))
    }
  }

  list(strength = strength, encode = encode, trial = trial,
       matrix = function(now) strength(seq_len(voc_sz), now))
}

yf2015_model <- function(params, data, control) {
  mu <- params[["mu"]] %||% 0
  sampling <- isTRUE(params[["sampling"]] %||% TRUE)
  reps <- control[["reps"]]
  keep_traj <- isTRUE(control[["keep_traj"]])

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects)))
  voc_sz <- length(voc)
  ref_sz <- length(ref)
  lrn <- yf_learner(params[["sigma"]], params[["lambda"]], params[["gamma"]], mu, voc_sz, ref_sz)

  n_tr <- length(data$words)
  traj <- list()
  perf <- matrix(0, reps, voc_sz)
  for (rep in seq_len(reps)) {
    for (t in seq_len(n_tr)) {
      now <- (rep - 1) * n_tr + t
      wi <- unique(stats::na.omit(match(unlist(data$words[[t]]), voc)))
      oi <- unique(stats::na.omit(match(unlist(data$objects[[t]]), ref)))
      if (length(wi) > 0 && length(oi) > 0) lrn$trial(as.integer(wi), as.integer(oi), now, sampling)
      if (keep_traj) traj[[now]] <- lrn$matrix(now + 1)
    }
    # tested one time step after the last training trial
    m <- lrn$matrix(rep * n_tr + 1)
    perf[rep, ] <- get_perf(m, rule = "remember_guess", mu = mu)
  }
  dimnames(m) <- list(voc, ref)
  xslFit(perf = perf, matrix = m, traj = traj)
}

#' Yurovsky & Frank (2015) Integrated model, trial by trial
#'
#' A trial-by-trial version of Yurovsky & Frank's (2015) Integrated model of
#' cross-situational word learning, which runs on any [xslData] like the
#' package's other models. On the paper's own paradigm it gives exactly the
#' closed-form predictions of [yf2015_predict()] (see [yf2015_paradigm()]).
#'
#' **Encoding.** Each time a word is heard, the learner picks a hypothesized
#' referent among the \eqn{n} objects present, then lays down a memory trace
#' with each of them: \eqn{\gamma\sigma} for the hypothesis and
#' \eqn{\gamma(1 - \sigma)/(n - 1)} for each other object (\eqn{\gamma} for a
#' lone object). \eqn{\sigma = 1} is single-referent tracking (as in
#' [propose_but_verify()] or [pursuit()], but without verification), and
#' \eqn{\sigma = 1/n} statistical accumulation.
#'
#' **Memory.** Traces decay as a power function of the number of trials since
#' they were laid down, and a word-object strength is the sum of its traces:
#' \deqn{M_{wo}(t) = \sum_k a_k (t - t_k)^{-\lambda}.}
#' Training trials are one time step apart, and the test comes one step after
#' the last training trial (with `reps > 1`, after each repetition).
#'
#' **Choice.** Hypotheses during training and referents at test are both
#' chosen by the remember-or-guess rule ([remember_guess()]): recall a
#' referent with probability equal to its strength, or else guess. A word's
#' first hypothesis is therefore a uniform guess, and a referent it remembers
#' is likelier to be hypothesized again, and so to attract the \eqn{\sigma}
#' share of later traces. When a trial has several words, each samples its
#' hypothesis in turn (in random order) among the objects the others have not
#' already claimed.
#'
#' **Mutual exclusivity.** With `mu > 0`, guesses avoid objects associated with
#' other words (see [remember_guess()]). The paper modelled its New Label
#' trials with an ad hoc rule instead ([yf2015_predict()]'s
#' `new_label_scale`); `mu = 0` (the default) is its model of Same and Switch
#' trials.
#'
#' **Sampling or mean field.** With `sampling = TRUE` each simulated learner
#' samples its hypotheses, so the model is stochastic. With `sampling = FALSE`
#' each word instead lays down its expected traces, averaged over its
#' hypothesis distribution: deterministic, much faster, and smooth in the
#' parameters, so better for fitting. The two agree exactly (in expectation)
#' when every trial has one word, `mu = 0` and no word's strengths sum to more
#' than 1, where the choice rule is linear. On a word's first occurrence the
#' mean field lays down \eqn{\gamma/n} for every object whatever
#' \eqn{\sigma}, so \eqn{\sigma} only matters through repeated occurrences.
#'
#' **Identifiability.** Most datasets test every word once, after training, so
#' memory decay (\eqn{\lambda}) and encoding strength (\eqn{\gamma}) trade off
#' strongly; the paper's interleaved tests at varied intervals are what
#' separate them.
#'
#' [xsl_run()] scores the model with the remember-or-guess rule, which its
#' parameters name (`choice_rule = "remember_guess"`).
#'
#' @param sigma Attention to the hypothesized referent, in \[0, 1\].
#' @param lambda Power-law decay rate, `lambda >= 0`.
#' @param gamma Encoding strength, `gamma >= 0`. Strengths are recall
#'   probabilities, so it can exceed 1 (the paper's estimate is about 1.6).
#' @param mu Mutual-exclusivity strength, `mu >= 0`.
#' @param sampling Sample hypotheses (`TRUE`, stochastic) or encode their
#'   expectation (`FALSE`, deterministic).
#'
#' @return An object of class xslMod.
#' @references Yurovsky, D., & Frank, M. C. (2015). An integrative account of
#'   constraints on cross-situational learning. *Cognition*, 145, 53-62.
#'   \doi{10.1016/j.cognition.2015.07.013}
#' @seealso [yf2015_paradigm()], [yf2015_predict()], [remember_guess()]
#' @export
#'
#' @examples
#' mod <- yf2015(sigma = .57, lambda = .17, gamma = 1.62, sampling = FALSE)
#' xsl_run(mod, xsl_datasets[[1]])$fits[[1]]$perf
yf2015 <- function(sigma, lambda, gamma, mu = 0, sampling = TRUE) {
  stopifnot(is.numeric(sigma), length(sigma) == 1, sigma >= 0, sigma <= 1,
            is.numeric(lambda), length(lambda) == 1, lambda >= 0,
            is.numeric(gamma), length(gamma) == 1, gamma >= 0,
            is.numeric(mu), length(mu) == 1, mu >= 0,
            is.logical(sampling), length(sampling) == 1, !is.na(sampling))
  xslMod(
    name = "yf2015",
    description = "Yurovsky & Frank (2015) Integrated model, trial by trial",
    model = yf2015_model,
    params = list(sigma = sigma, lambda = lambda, gamma = gamma, mu = mu,
                  sampling = sampling, choice_rule = "remember_guess"),
    stochastic = sampling
  )
}

# The three Experiment 3 trial types' target and competitor ("Same vs. Switch"
# is a Same target against a Switch competitor).
yf3_types <- c("Same vs. Same", "Same vs. Switch", "Switch vs. Same", "Switch vs. Switch")

#' The trial-level model on Yurovsky & Frank's (2015) paradigm
#'
#' Runs a [yf2015()] model through the test trials of Yurovsky & Frank's
#' (2015) experiments (see [yurovsky_frank2015]), conditioning on the
#' learner's guess on each exposure trial as the experiments do. Because
#' Same, Switch and the Experiment 3 trial types are defined relative to that
#' guess, the predictions are exact, not simulated.
#'
#' - **Experiments 1 and 2.** Word A is heard with `n_referents` objects and
#'   the learner hypothesizes one; `interval` trials later, A (Same, Switch) or
#'   a novel word (New Label) is heard with one of those objects (the
#'   hypothesis on Same trials, another on Switch and New Label trials) among
#'   `n_referents - 1` novel ones.
#' - **Experiment 3.** Word A, then word B, are each heard with `n_referents`
#'   objects; on the next trial A is heard with one of A's objects (the
#'   target) and one of B's (the competitor), each of which was or was not
#'   the learner's hypothesis, plus `n_referents - 2` novel objects.
#'
#' With `mu = 0` the Same and Switch predictions equal [yf2015_predict()]'s
#' Integrated model (while every strength is at most 1), New Label trials are
#' at chance, and Experiment 3's competitor has no effect. With `mu > 0`
#' learners avoid a familiar object for a new word (New Label trials) and an
#' object they associate with another word (Experiment 3's competitors), and
#' more so for objects they hypothesized.
#'
#' The model's `sampling` setting doesn't matter here: the exposure guess is
#' conditioned on.
#'
#' @param model A [yf2015()] model.
#' @param conditions A data frame with columns `n_referents` and `trial_type`,
#'   plus `interval` (trials from exposure to test) for Experiment 1-2 trial
#'   types. `trial_type` is `"Same"`, `"Switch"` or `"New Label"`
#'   (Experiments 1-2), or `"Same vs. Same"`, `"Same vs. Switch"`, `"Switch
#'   vs. Same"` or `"Switch vs. Switch"` (Experiment 3: target vs.
#'   competitor).
#'
#' @return `conditions` with added columns `p_target`, the probability of
#'   choosing the object from the tested word's exposure trial (the repeated
#'   referent in Experiments 1-2, the correct target in Experiment 3), and
#'   `p_competitor`, the probability of choosing Experiment 3's competitor
#'   (`NA` for Experiments 1-2).
#' @seealso [yf2015()], [yf2015_predict()]
#' @export
#'
#' @examples
#' mod <- yf2015(sigma = .57, lambda = .17, gamma = 1.62, mu = 1)
#' yf2015_paradigm(mod, data.frame(n_referents = 4, interval = 2,
#'                                 trial_type = c("Same", "Switch", "New Label")))
#' yf2015_paradigm(mod, data.frame(n_referents = 2,
#'                                 trial_type = c("Switch vs. Same", "Switch vs. Switch")))
yf2015_paradigm <- function(model, conditions) {
  if (!inherits(model, "xslMod") || !identical(model$name, "yf2015")) {
    stop("`model` must be a yf2015() model", call. = FALSE)
  }
  p <- model$params
  tt <- as.character(conditions$trial_type)
  bad <- setdiff(unique(tt), c("Same", "Switch", "New Label", yf3_types))
  if (length(bad)) stop("unknown trial_type(s): ", paste(bad, collapse = ", "), call. = FALSE)
  e12 <- tt %in% c("Same", "Switch", "New Label")
  if (any(e12) && is.null(conditions$interval)) {
    stop("`conditions` needs an `interval` column for Same, Switch and New Label trials", call. = FALSE)
  }
  n <- conditions$n_referents
  if (any(n < 2)) stop("`n_referents` must be at least 2", call. = FALSE)

  one <- function(n, interval, type) {
    learner <- function(voc_sz, ref_sz) yf_learner(p$sigma, p$lambda, p$gamma, p$mu, voc_sz, ref_sz)
    hyp <- function(k) as.numeric(seq_len(n) == k)
    if (type %in% c("Same", "Switch", "New Label")) {
      # words A = 1, novel C = 2; A's objects 1..n (hypothesis 1), novel n+1..
      lrn <- learner(2, 2 * n - 1)
      lrn$encode(1, seq_len(n), 1, hyp(1))
      shown <- if (type == "Same") 1 else 2
      word <- if (type == "New Label") 2 else 1
      probs <- remember_guess(lrn$matrix(1 + interval), word, c(shown, n + seq_len(n - 1)), mu = p$mu)
      return(c(probs[1], NA))
    }
    # words A = 1, B = 2; A's objects 1..n, B's n+1..2n (hypotheses 1 and
    # n+1), novel 2n+1..; exposures at times 1 and 2, test at 3
    parts <- strsplit(type, " vs. ", fixed = TRUE)[[1]]
    lrn <- learner(2, 3 * n - 2)
    lrn$encode(1, seq_len(n), 1, hyp(1))
    lrn$encode(2, n + seq_len(n), 2, hyp(1))
    target <- if (parts[1] == "Same") 1 else 2
    competitor <- n + (if (parts[2] == "Same") 1 else 2)
    probs <- remember_guess(lrn$matrix(3), 1, c(target, competitor, 2 * n + seq_len(n - 2)), mu = p$mu)
    probs[1:2]
  }
  interval <- if (is.null(conditions$interval)) rep(NA, nrow(conditions)) else conditions$interval
  out <- vapply(seq_len(nrow(conditions)), \(i) one(n[i], interval[i], tt[i]), numeric(2))
  conditions$p_target <- out[1, ]
  conditions$p_competitor <- out[2, ]
  conditions
}
