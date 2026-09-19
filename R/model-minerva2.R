# Ported from a standalone prototype (MINERVA2.R) that was never wired into
# the package. Hintzman's MINERVA2 (1984, 1986): each trial leaves one
# episodic trace (here, a word-part + object-part vector); a test probe
# (a word, padded with zeros where the object part would be) activates every
# trace by its similarity cubed, and the weighted sum of traces ("echo")
# is compared against each candidate object to give a graded choice.
#
# Fixed one bug from the prototype along the way: it compared the full 2D
# echo content vector against each D-dim object vector (`sim(content,
# objects[o,])`, mismatched lengths -- R recycles silently instead of
# erroring), which folded the word-half of the echo into that comparison.
# Only the echo's object-half is used below.

minerva2_choice <- function(M, n_traces, word_vecs, obj_vecs, D, X) {
  voc_sz <- nrow(word_vecs)
  ref_sz <- nrow(obj_vecs)
  m <- matrix(0, voc_sz, ref_sz,
              dimnames = list(rownames(word_vecs), rownames(obj_vecs)))
  if (n_traces == 0) return(m)

  trace_w <- M[seq_len(n_traces), seq_len(D), drop = FALSE]
  trace_o <- M[seq_len(n_traces), (D + 1):(2 * D), drop = FALSE]
  obj_norms <- sqrt(rowSums(obj_vecs ^ 2))

  for (w in seq_len(voc_sz)) {
    # MINERVA2's normalized-dot-product similarity, probed with the word half
    # only (object half of the probe is all zeros, so it drops out of the
    # numerator but not the length(probe) = 2D normalizer)
    sims <- as.numeric(trace_w %*% word_vecs[w, ]) / (2 * D)
    activation <- sims ^ 3 # sign-preserving cubing, per Hintzman
    content_o <- as.numeric(activation %*% trace_o) # echo's object half
    cn <- sqrt(sum(content_o ^ 2))
    if (cn == 0) next
    raw <- as.numeric(obj_vecs %*% content_o) / (obj_norms * cn)
    raw[!is.finite(raw)] <- 0
    m[w, ] <- exp(X * raw) # graded (Luce-choice-ready) in place of hard argmax
  }
  m
}

minerva2_model <- function(params, data, control) {
  X <- params[["X"]] # choice-rule temperature on echo-object cosine similarity
  D <- params[["D"]] # dimensionality of word/object feature vectors
  reps <- control[["reps"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc)
  ref_sz <- length(ref)
  keep_traj <- isTRUE(control[["keep_traj"]])
  traj <- list()

  word_vecs <- matrix(rnorm(voc_sz * D, sd = 1 / sqrt(D)), voc_sz, D,
                      dimnames = list(voc, NULL))
  obj_vecs <- matrix(rnorm(ref_sz * D, sd = 1 / sqrt(D)), ref_sz, D,
                     dimnames = list(ref, NULL))

  n_trials <- length(data$words)
  M <- matrix(0, reps * n_trials, 2 * D) # one episodic trace per trial (per rep)
  n_traces <- 0

  m <- matrix(0, voc_sz, ref_sz, dimnames = list(voc, ref))
  perf <- matrix(0, reps, voc_sz)

  for (rep in seq_len(reps)) {
    for (t in seq_along(data$words)) {
      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]

      # each word/object present is encoded into the trial's trace with an
      # independent random strength in (0,1), simulating imperfect encoding
      # (per the prototype's own comment; Hintzman's model itself assumes
      # all-or-none encoding with probability L=1)
      wpart <- rep(0, D)
      for (w in tr_w) wpart <- wpart + word_vecs[as.character(w), ] * runif(1)
      opart <- rep(0, D)
      for (o in tr_o) opart <- opart + obj_vecs[as.character(o), ] * runif(1)

      n_traces <- n_traces + 1
      M[n_traces, ] <- c(wpart, opart)

      index <- (rep - 1) * length(data$words) + t
      if (keep_traj) {
        traj[[index]] <- minerva2_choice(M, n_traces, word_vecs, obj_vecs, D, X)
      }
    }
    m <- minerva2_choice(M, n_traces, word_vecs, obj_vecs, D, X)
    perf[rep, ] <- get_perf(m)
  }
  xslFit(perf = perf, matrix = m, traj = traj)
}

#' MINERVA2 episodic memory model
#'
#' Hintzman's (1984, 1986) MINERVA2 adapted to cross-situational word
#' learning: each trial is stored as a single episodic trace formed by
#' concatenating that trial's word-vector sum and object-vector sum (words
#' and objects are random Gaussian vectors, one per vocabulary item). At
#' test, a word probe (word-half only, object-half zeroed) activates every
#' trace by its similarity cubed; the activation-weighted sum of traces (the
#' "echo") is compared, on its object half, against every candidate object to
#' give a graded association -- exponentiated by `X` into a Luce-choice-ready
#' matrix rather than MINERVA2's usual hard best-match.
#'
#' Performance note: memory is exact (no forgetting/decay), so the model
#' keeps one `2*D`-length trace per trial and a test pass costs
#' `O(voc_sz * n_traces * D)`. `D` is a fixed representational hyperparameter
#' (not usually treated as a free cognitive parameter), not included by
#' default in this package's DEoptim registry bounds -- see
#' `xsl_model_registry()`.
#'
#' @param X Choice-rule temperature: how sharply the echo/object cosine
#'   similarity is exponentiated into a choice weight (larger = more
#'   winner-take-all)
#' @param D Dimensionality of the random word/object feature vectors (fixed
#'   hyperparameter, not a free cognitive parameter)
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- minerva2(X = 5, D = 50)
#' xsl_run(mod, get_example_ambiguous_condition())
minerva2 <- function(X, D = 100) {
  xslMod(
    name = "minerva2",
    description = paste(
      "MINERVA2 episodic memory model (Hintzman 1984, 1986): stores one",
      "trace per trial (concatenated word + object vectors) and retrieves",
      "an object via activation-weighted (similarity-cubed) echo content"
    ),
    model = minerva2_model,
    params = list(X = X, D = D),
    stochastic = TRUE
  )
}
