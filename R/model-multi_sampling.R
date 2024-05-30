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

  voc <- unique(unlist(data$words))
  ref <- unique(unlist(data$objects[!is.na(data$objects)]))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  traj <- list()
  m <- matrix(0, voc_sz, ref_sz) # association matrix
  colnames(m) <- ref
  rownames(m) <- voc
  perf <- matrix(0, reps, voc_sz) # a row for each block

  mean_ent <- c()

  # want an item x occurrence matrix, to be filled in during training
  freq <- rep(0, voc_sz) # number of occurrences per word, so far (to index the resps matrix)
  names(freq) <- voc
  # training
  for (rep in 1:reps) { # for trajectory experiments, train multiple times
    for (t in 1:length(data$words)) {

      #print(format(m, digits=3))
      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]

      freq[tr_w] <- freq[tr_w] + 1
      m <- update_known(m, tr_w, tr_o) # what's been seen so far?

      ent_w <- rep(0, voc_sz)
      names(ent_w) <- voc

      for (w in tr_w) {
        ent_w[w] <- shannon_entropy(m[w, ])
      }
      ent_w <- exp(B * ent_w)

      ent_o <- rep(0, ref_sz)
      names(ent_o) <- ref

      for (o in tr_o) {
        ent_o[o] <- shannon_entropy(m[, o])
      }
      ent_o <- exp(B * ent_o)

      temp_wts <- matrix(0, voc_sz, ref_sz)
      colnames(temp_wts) <- ref
      rownames(temp_wts) <- voc
      temp_wts[tr_w,tr_o] <- m[tr_w,tr_o] # use these weights to calculate entropy
      nent <- ent_w %*% t(ent_o)
      temp_wts <- temp_wts * as.matrix(nent)

      chosen_assocs <- matrix(0, voc_sz, ref_sz)
      colnames(chosen_assocs) <- ref
      rownames(chosen_assocs) <- voc
      for (w in tr_w) {
        # if (sum(temp_wts[w,]) == 0) {next}
        x <- sample(1:ref_sz, K, replace = TRUE, prob = temp_wts[w, ])
        chosen_assocs[w, x] <- m[w, x] # PK for chosen
      }
      denom <- sum(chosen_assocs * nent)
      chosen_assocs <- (X * chosen_assocs * nent) / denom
      m <- m * C # decay everything
      m <- m + chosen_assocs

      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      traj[[index]] <- m
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
    params = c(C = C, X = X, B = B, K = K),
    stochastic = TRUE
  )
}
