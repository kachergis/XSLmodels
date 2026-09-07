# capturing hypothesis-testing vs. associative learning
# with one parameter determining amount of weight distributed
# per word-object association on each trial
# (and thus determining how many associations are made per word)
# George Kachergis May 15, 2023

multi_sampling_model <- function(params, data, control) {
  C <- params[["C"]]
  X <- params[["X"]]
  B <- params[["B"]]
  K <- params[["K"]]

  reps <- control[["reps"]]
  # start_matrix <- control[["start_matrix"]]
  # verbose <- control[["verbose"]]

  voc <- sort(unique(unlist(data$words)))
  ref <- sort(unique(unlist(data$objects[!is.na(data$objects)])))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  keep_traj <- isTRUE(control[["keep_traj"]])
  traj <- list()
  m <- matrix(0, voc_sz, ref_sz) # association matrix
  colnames(m) <- ref
  rownames(m) <- voc
  perf <- matrix(0, reps, voc_sz) # a row for each block

  # mean_ent <- c()

  # want an item x occurrence matrix, to be filled in during training
  freq <- rep(0, voc_sz) # number of occurrences per word, so far (to index the resps matrix)
  names(freq) <- voc
  # training
  for (rep in 1:reps) { # for trajectory experiments, train multiple times
    for (t in seq_along(data$words)) {

      #print(format(m, digits=3))
      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]

      freq[tr_w] <- freq[tr_w] + 1
      m <- update_known(m, tr_w, tr_o) # what's been seen so far?

      # Only the words/objects on *this* trial ever get a nonzero sampling
      # weight, and only entries in u_tr_w x u_tr_o ever get updated -- so
      # entropy, the weighting, and the update are all computed on that small
      # submatrix instead of the full voc_sz x ref_sz matrix (this used to be
      # the dominant cost for large vocabularies, e.g. naturalistic corpora).
      # The one exception is the probability vector handed to sample(): it
      # must stay ref_sz long (zero outside tr_o) so the draw is identical to
      # sampling from the full row, i.e. this is a pure speed optimization,
      # not a behavior change.
      u_tr_w <- unique(tr_w)
      u_tr_o <- unique(tr_o)
      tr_o_idx <- match(u_tr_o, ref)

      ent_w_tr <- exp(B * vapply(u_tr_w, function(w) shannon_entropy(m[w, ]), numeric(1)))
      names(ent_w_tr) <- u_tr_w
      ent_o_tr <- exp(B * vapply(u_tr_o, function(o) shannon_entropy(m[, o]), numeric(1)))
      names(ent_o_tr) <- u_tr_o

      chosen_small <- matrix(0, length(u_tr_w), length(u_tr_o),
                              dimnames = list(u_tr_w, u_tr_o))
      for (w in tr_w) {
        # a word with no positive sampling weight -- e.g. on a
        # non-referential utterance with no objects present -- has nothing
        # to sample; skip it (otherwise sample() errors on an all-zero prob)
        row_probs <- numeric(ref_sz)
        row_probs[tr_o_idx] <- m[w, u_tr_o] * ent_w_tr[[as.character(w)]] * ent_o_tr
        if (sum(row_probs) == 0) next
        x <- sample(1:ref_sz, K, replace = TRUE, prob = row_probs)
        x_lab <- ref[x]
        chosen_small[as.character(w), as.character(x_lab)] <- m[w, x_lab] # PK for chosen
      }
      nent_small <- outer(ent_w_tr, ent_o_tr)
      denom <- sum(chosen_small * nent_small)
      m <- m * C # decay everything
      # if nothing was sampled this trial (denom == 0) the update is a no-op,
      # like uncfam() on an objectless trial -- decay still applies
      if (denom > 0) {
        m[u_tr_w, u_tr_o] <- m[u_tr_w, u_tr_o] +
          (X * chosen_small * nent_small) / denom
      }

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      if (keep_traj) traj[[index]] <- m
    }
    perf[rep, ] <- get_perf(m + 1e-9)
  }

  resp_prob <- get_perf(m)
  xslFit(perf = resp_prob, matrix = m, traj = traj)
}

#' Multi-sampling associative model
#'
#' @param C Decay
#' @param X Total associative weight to distribute per trial
#' @param B Associative weight to distribute each sample
#' @param K Number of associations to update per word (constraint: B <= X)
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- multi_sampling(C = 1, X = .2, B = .3, K = 1)
#' xsl_run(mod, get_example_ambiguous_condition())
multi_sampling <- function(C, X, B, K = 1) {
  xslMod(
    name = "multi_sampling",
    description = "Multi-sampling associative model",
    model = multi_sampling_model,
    params = list(C = C, X = X, B = B, K = K),
    stochastic = TRUE
  )
}

# TODO: X = chi, B = lambda, C = alpha, K = k
