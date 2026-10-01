# Ported (in substance, not in code) from a standalone prototype (REM.R)
# that never actually implemented REM: it declared a geometric-distribution
# feature generator (generate_geometric_vectors(), matching Steyvers &
# Shiffrin's environmental base-rate distribution) but then its `model()`
# and `minerva_test()` were verbatim copies of MINERVA2.R's continuous-
# Gaussian dot-product-cubed code, which don't reference w/g/u/c/nu at all
# and call helpers (sim(), echo_content(), generate_vectors()) that aren't
# even defined in that file. There was no REM-specific storage or matching
# logic to port.
#
# What follows is a fresh adaptation of Shiffrin & Steyvers (1997) to this
# package's cross-situational associative task, keeping REM's actual
# defining mechanics:
#  - word/object identity is a length-`w` vector of features drawn from a
#    geometric base-rate distribution (parameter `g`; shifted to support
#    {1, 2, 3, ...} so 0 can unambiguously mean "not stored")
#  - each trial creates one episodic trace per candidate (word, object) pair
#    present (the cross-product, as baseline_model() and others in this
#    package do for ambiguous trials), by imperfectly *copying* the word's
#    and object's true feature vectors: each feature is stored independently
#    with probability `u`; if stored, it is copied correctly with
#    probability `c`, else replaced by a fresh random draw from the base-rate
#    distribution (a "confusable" feature). `nu` (probability of strengthening
#    an old trace instead of creating a new one on repeated study) is not
#    implemented -- every study event creates its own trace, i.e. nu = 0
#  - at test, a (word, object) pair's match strength is the sum, across all
#    stored traces, of that trace's likelihood ratio given the fully-known
#    probe (the true word+object feature vector): each stored feature
#    contributes `(c + (1-c) p(v)) / p(v)` if it matches the probe, `(1-c)`
#    if it mismatches, and traces are combined multiplicatively across
#    features (in log space) and additively across traces -- REM's eq. 5-6

rem_encode <- function(true_vec, g, u, copy_acc) {
  nfeat <- length(true_vec)
  stored <- runif(nfeat) < u
  trace <- rep(0, nfeat)
  if (any(stored)) {
    vals <- true_vec[stored]
    wrong <- runif(length(vals)) >= copy_acc
    if (any(wrong)) vals[wrong] <- 1 + rgeom(sum(wrong), 1 - g)
    trace[stored] <- vals
  }
  trace
}

# log-likelihood-ratio match of every stored trace (rows) against one probe,
# summed on the natural (not log) scale -- Shiffrin & Steyvers eq. 5-6
rem_score <- function(trace_mat, probe, g, copy_acc) {
  n <- nrow(trace_mat)
  if (n == 0) return(0)
  nfeat <- length(probe)
  probe_mat <- matrix(probe, n, nfeat, byrow = TRUE)
  stored <- trace_mat != 0
  matched <- stored & (trace_mat == probe_mat)
  mismatched <- stored & !matched

  pv <- (1 - g) * g ^ (probe - 1) # base-rate probability of each probe feature value
  pv_mat <- matrix(pv, n, nfeat, byrow = TRUE)

  logfac <- matrix(0, n, nfeat)
  logfac[matched] <- log((copy_acc + (1 - copy_acc) * pv_mat[matched]) / pv_mat[matched])
  logfac[mismatched] <- log(1 - copy_acc)
  sum(exp(rowSums(logfac)))
}

rem_choice <- function(trace_mat, n_traces, word_true, obj_true, g, copy_acc) {
  voc_sz <- nrow(word_true)
  ref_sz <- nrow(obj_true)
  m <- matrix(0, voc_sz, ref_sz,
              dimnames = list(rownames(word_true), rownames(obj_true)))
  if (n_traces == 0) return(m)
  traces <- trace_mat[seq_len(n_traces), , drop = FALSE]
  for (wi in seq_len(voc_sz)) {
    for (oi in seq_len(ref_sz)) {
      probe <- c(word_true[wi, ], obj_true[oi, ])
      m[wi, oi] <- rem_score(traces, probe, g, copy_acc)
    }
  }
  # REM's summed likelihood-ratio odds are heavy-tailed: a lucky match on a
  # rare (low base-rate) feature value can inflate one trace's odds by
  # several orders of magnitude (odds ~ 1/p(v) for a matched feature), so raw
  # per-simulation magnitudes vary far more than e.g. minerva2()'s bounded
  # cosine-based scores. xsl_run() aggregates n_sim simulations by summing
  # their matrices, which would let whichever simulation got lucky dominate
  # the average; row-normalizing each simulation's choice weights here (so
  # every simulated "subject" contributes a genuine probability distribution
  # over objects, not a raw, wildly-scaled magnitude) keeps that aggregation
  # a fair average instead.
  rs <- rowSums(m)
  nz <- rs > 0
  m[nz, ] <- m[nz, , drop = FALSE] / rs[nz]
  m
}

rem_model <- function(params, data, control) {
  g <- params[["g"]] # base-rate feature-value distribution parameter
  u <- params[["u"]] # per-feature storage probability
  copy_acc <- params[["c"]] # probability a stored feature is copied correctly
  nfeat <- params[["w"]] # number of features per item
  reps <- control[["reps"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc)
  ref_sz <- length(ref)
  keep_traj <- isTRUE(control[["keep_traj"]])
  traj <- list()

  word_true <- matrix(1 + rgeom(voc_sz * nfeat, 1 - g), voc_sz, nfeat,
                      dimnames = list(voc, NULL))
  obj_true <- matrix(1 + rgeom(ref_sz * nfeat, 1 - g), ref_sz, nfeat,
                     dimnames = list(ref, NULL))

  # one trace per candidate (word, object) pair per trial: strip NAs once up
  # front so trace count -- and hence M's size -- is known exactly, rather
  # than growing M with rbind() (an O(n^2) cost REM is especially prone to,
  # since it creates trial_sz_w * trial_sz_o traces per trial, not one)
  trial_w <- lapply(data$words, function(x) {
    x <- x[!is.na(x)]
    x[x != ""]
  })
  trial_o <- lapply(data$objects, function(x) x[!is.na(x)])
  trace_counts <- mapply(function(wl, ol) length(wl) * length(ol), trial_w, trial_o)
  M <- matrix(0, reps * sum(trace_counts), 2 * nfeat)
  n_traces <- 0

  m <- matrix(0, voc_sz, ref_sz, dimnames = list(voc, ref))
  perf <- matrix(0, reps, voc_sz)

  for (rep in seq_len(reps)) {
    for (t in seq_along(trial_w)) {
      tr_w <- trial_w[[t]]
      tr_o <- trial_o[[t]]
      for (word in tr_w) {
        w_trace <- rem_encode(word_true[as.character(word), ], g, u, copy_acc)
        for (obj in tr_o) {
          o_trace <- rem_encode(obj_true[as.character(obj), ], g, u, copy_acc)
          n_traces <- n_traces + 1
          M[n_traces, ] <- c(w_trace, o_trace)
        }
      }

      index <- (rep - 1) * length(trial_w) + t
      if (keep_traj) {
        traj[[index]] <- rem_choice(M, n_traces, word_true, obj_true, g, copy_acc)
      }
    }
    m <- rem_choice(M, n_traces, word_true, obj_true, g, copy_acc)
    perf[rep, ] <- get_perf(m + 1e-12) # a boundary c=1 can make m exactly 0
  }
  xslFit(perf = perf, matrix = m + 1e-12, traj = traj)
}

#' REM (Retrieving Effectively from Memory) model
#'
#' Shiffrin & Steyvers' (1997) REM adapted to cross-situational word
#' learning. Word/object identity is a length-`w` feature vector drawn from a
#' geometric base-rate distribution (`g`); each trial stores one episodic
#' trace per candidate word-object pairing present, built by imperfectly
#' copying the pair's true features (each feature independently stored with
#' probability `u`, and if stored, correct with probability `c`, else a fresh
#' random draw). At test, a candidate pairing's strength is the summed
#' likelihood ratio of every trace given the true (fully-known) probe
#' features, REM's odds-based alternative to [minerva2()]'s dot-product
#' activation.
#'
#' Performance note: unlike [minerva2()] (one trace per trial) this model
#' stores one trace per candidate (word, object) pair per trial -- on a
#' highly ambiguous corpus (many words/objects per utterance) memory size
#' grows combinatorially rather than linearly with trial count, and both
#' encoding and the `voc_sz * ref_sz` test pass scale with that trace count,
#' making `rem()` the most expensive of this package's memory models on
#' cluttered/naturalistic data. `w` is a fixed representational
#' hyperparameter (not usually treated as a free cognitive parameter), not
#' included by default in this package's DEoptim registry bounds -- see
#' `xsl_model_registry()`.
#'
#' @param g Base-rate feature-value distribution parameter (larger = more
#'   variable/distinctive feature values, in (0, 1))
#' @param u Per-feature storage probability at study, in (0, 1)
#' @param c Probability a stored feature is copied correctly (vs. replaced by
#'   a random, possibly-confusable value), in (0, 1)
#' @param w Number of features per item (fixed hyperparameter, not a free
#'   cognitive parameter)
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- rem(g = 0.4, u = 0.3, c = 0.7, w = 12)
#' xsl_run(mod, get_example_ambiguous_condition())
rem <- function(g, u, c, w = 12) {
  xslMod(
    name = "rem",
    description = paste(
      "REM (Shiffrin & Steyvers 1997): stores one imperfectly-copied",
      "geometric feature-vector trace per candidate word-object pairing",
      "per trial, and retrieves via summed likelihood-ratio odds"
    ),
    model = rem_model,
    params = list(g = g, u = u, c = c, w = w),
    stochastic = TRUE
  )
}
