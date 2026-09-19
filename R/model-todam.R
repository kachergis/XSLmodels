# Ported from two standalone prototypes (TODAM_max.R, "TODAM_v2 ROLL BACK.R")
# that were never wired into the package. TODAM (Murdock, 1982): word and
# object vectors are bound via circular convolution into a single holographic
# memory vector M; a candidate pairing is tested by circularly correlating a
# candidate object out of M and comparing it (cosine) against the word.
#
# The two prototypes differ in two ways, and this port keeps the more
# developed one throughout:
#  - encoding: TODAM_max just adds w + o + convo(w, o) per pair (no
#    weighting); the v2 file additionally attention-weights each trial's
#    words/objects by their current familiarity (cosine similarity to M)
#    before convolving. Kept: the attention-weighted v2 version.
#  - choice rule: TODAM_max takes a hard argmax over cosine similarities;
#    v2 shifts/normalizes the similarity row and exponentiates it by a
#    temperature X into a graded choice. Kept: v2's graded rule, since it
#    both fits gradient human accuracy better and is what this package's
#    Luce-choice get_perf() expects from a matrix.
#  - both prototypes called the removed base-R function `as.real()` (dropped
#    well over a decade ago); replaced with `Re()` below.
#
# Also fixed: v2 computed each trial's attention weights as a raw cosine
# similarity to M, `cos_sim(item, M)`. Before anything has been stored, M is
# the zero vector, so this is 0/0 = NaN on trial 1 -- and every subsequent
# trial too, since NaN propagates through M once added. Here attention is
# `1 + cos_sim(item, M)` (falls back to a neutral 1, i.e. uniform weighting,
# whenever M is all-zero, and stays in [0, 2] otherwise) before being
# renormalized within the trial.

convo <- function(a, b) {
  Re(fft(fft(a) * fft(b), inverse = TRUE) / length(a))
}

correlate <- function(a, b) {
  a <- rev(a)
  convo(c(a[length(a)], a[1:(length(a) - 1)]), b)
}

todam_attn <- function(item_vecs, M) {
  Mn <- sqrt(sum(M ^ 2))
  if (Mn == 0) return(rep(1, nrow(item_vecs)))
  vn <- sqrt(rowSums(item_vecs ^ 2))
  raw <- 1 + as.numeric(item_vecs %*% M) / (vn * Mn)
  raw[!is.finite(raw)] <- 1
  raw
}

todam_choice <- function(M, word_vecs, obj_vecs, X) {
  voc_sz <- nrow(word_vecs)
  ref_sz <- nrow(obj_vecs)
  m <- matrix(0, voc_sz, ref_sz,
              dimnames = list(rownames(word_vecs), rownames(obj_vecs)))
  for (w in seq_len(voc_sz)) {
    sims <- vapply(seq_len(ref_sz), function(o) {
      cos_sim(word_vecs[w, ], correlate(obj_vecs[o, ], M))
    }, numeric(1))
    sims[!is.finite(sims)] <- 0
    if (min(sims) < 0) sims <- sims - min(sims)
    s <- sum(sims)
    if (s > 0) sims <- sims / s
    m[w, ] <- exp(X * sims) # graded (Luce-choice-ready) in place of hard argmax
  }
  m
}

todam_model <- function(params, data, control) {
  X <- params[["X"]] # choice-rule temperature on the retrieved-vs-word cosine similarity
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

  M <- rep(0, D) # single holographic memory vector, built up across trials
  m <- matrix(0, voc_sz, ref_sz, dimnames = list(voc, ref))
  perf <- matrix(0, reps, voc_sz)

  for (rep in seq_len(reps)) {
    for (t in seq_along(data$words)) {
      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]

      if (length(tr_w) > 0 && length(tr_o) > 0) {
        w_idx <- match(as.character(tr_w), rownames(word_vecs))
        o_idx <- match(as.character(tr_o), rownames(obj_vecs))
        w_attn <- todam_attn(word_vecs[w_idx, , drop = FALSE], M)
        w_attn <- w_attn / sum(w_attn)
        o_attn <- todam_attn(obj_vecs[o_idx, , drop = FALSE], M)
        o_attn <- o_attn / sum(o_attn)

        for (wi in seq_along(tr_w)) {
          w_rep <- word_vecs[w_idx[wi], ]
          for (pi in seq_along(tr_o)) {
            o_rep <- obj_vecs[o_idx[pi], ]
            M <- M + w_attn[wi] * o_attn[pi] * convo(w_rep, o_rep)
          }
        }
      }

      index <- (rep - 1) * length(data$words) + t
      if (keep_traj) traj[[index]] <- todam_choice(M, word_vecs, obj_vecs, X)
    }
    m <- todam_choice(M, word_vecs, obj_vecs, X)
    perf[rep, ] <- get_perf(m)
  }
  xslFit(perf = perf, matrix = m, traj = traj)
}

#' TODAM holographic reduced representation model
#'
#' Murdock's (1982) TODAM adapted to cross-situational word learning: word
#' and object vectors (random Gaussian, one per vocabulary item) are bound
#' via circular convolution into a single memory vector `M`, attention-
#' weighted each trial by how familiar (cosine-similar to the current `M`)
#' each word/object already is. At test, a candidate object is circularly
#' correlated back out of `M` and compared (cosine) against each word;
#' similarities are shifted non-negative, row-normalized, and exponentiated
#' by `X` into a graded (Luce-choice-ready) matrix rather than a hard
#' best-match.
#'
#' Performance note: every encoding step and every test comparison requires
#' an FFT-based circular convolution/correlation over a `D`-length vector, so
#' cost scales with `D log(D)`; unlike [minerva2()]'s per-trial trace,
#' `todam()` holds a single fixed-size memory vector regardless of corpus
#' length. `D` is a fixed representational hyperparameter (not usually
#' treated as a free cognitive parameter), not included by default in this
#' package's DEoptim registry bounds -- see `xsl_model_registry()`.
#'
#' @param X Choice-rule temperature: how sharply the retrieved-object/word
#'   cosine similarity is exponentiated into a choice weight (larger = more
#'   winner-take-all)
#' @param D Dimensionality of the random word/object feature vectors (fixed
#'   hyperparameter, not a free cognitive parameter)
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- todam(X = 5, D = 200)
#' xsl_run(mod, get_example_ambiguous_condition())
todam <- function(X, D = 500) {
  xslMod(
    name = "todam",
    description = paste(
      "TODAM holographic reduced representation model (Murdock 1982):",
      "binds word/object vectors via circular convolution into a single",
      "attention-weighted memory vector, and retrieves via circular",
      "correlation"
    ),
    model = todam_model,
    params = list(X = X, D = D),
    stochastic = TRUE
  )
}
