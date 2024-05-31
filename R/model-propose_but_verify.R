# based on Trueswell et al 2013 propose-but-verify model:
# 1. guess at chance, 2. next time a word occurs, remember previous guess w prob alpha
# 3. if the remembered guess is present, increase alpha; otherwise choose a new random guess

# params = c(0.2376261, 0.3546257) # from response XSL paradigm
# params = c(0.2077282, 0.1916493)

propose_but_verify_model <- function(params, data, control) {
  # verbose <- control[["verbose"]]
  # if (verbose) print(params)

  alpha <- params[["alpha"]] # prob to remember first guess
  alpha_increase <- params[["alpha_increase"]] # Trueswell 2013 empirically estimates this...
  #sa <- params[2] # prob of storage (slow learning down)
  reps <- control[["reps"]]
  start_matrix <- control[["start_matrix"]]

  voc <- unique(unlist(data$words))
  ref <- unique(unlist(data$objects[!is.na(data$objects)]))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects

  if (!is.null(start_matrix)) {
    m <- start_matrix
  } else {
    m <- matrix(0, voc_sz, ref_sz) # hypothesis matrix
  }

  colnames(m) <- ref
  rownames(m) <- voc

  traj <- list()
  perf <- matrix(0, reps, voc_sz) # a row for each block
  freq <- rep(0, voc_sz) # number of occurrences per pair, so far (to index the resps matrix)
  names(freq) <- voc

  for (rep in 1:reps) {
    for (t in seq_along(data$words)) {
      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]
      if (length(tr_o) == 0) {
        index <- t
        traj[[index]] <- m
        next
      }
      freq[tr_w] <- freq[tr_w] + 1

      # for each word, 1) check if there is a hypothesized ref
      # if so, is it on this trial? yes -> strengthen

      # forget if runif > stored hyp strength (should be 1 non-zero entry per row)
      # identify words that have hypothesized refs
      if (length(tr_w) > 1) {
        forget <- tr_w[which(runif(length(tr_w)) > rowSums(m[tr_w, ]))]
        m[forget, ] <- 0
        have_hypoths <- tr_w[which(rowSums(m[tr_w, ]) != 0)]
      } else { # 1 word/trial
        if (runif(1) > sum(m[tr_w, ])) {
          m[tr_w, ] <- 0 # forgotten
        }
        have_hypoths <- tr_w[which(sum(m[tr_w, ]) != 0)]
      }

      # throw out inconsistent hyps
      for (w in have_hypoths) {
        hypo <- which(m[w, ] > 0)
        consistent <- intersect(hypo, tr_o) # hyp on trial, strengthen
        m[w, consistent] <- m[w, consistent] + alpha_increase
        inconsistent <- setdiff(hypo, tr_o) # hyp not on trial, disconfirmed
        m[w, inconsistent] <- 0
      }
      if (length(tr_w) > 1) {
        need_hypoths <- tr_w[which(rowSums(m[tr_w, ]) == 0)] # any words that don't have a hyp
      } else if (sum(m[tr_w, ] == 0)) { # no hyp exists for this word
        need_hypoths <- tr_w
      }
      store <- need_hypoths
      new_hyps <- sample(tr_o, length(store), replace = TRUE) # select new random refs from trial
      for (w in seq_along(store)) {
        if (length(store) == 0) next
        m[need_hypoths[w], new_hyps[w]] <- alpha
      }
      index <- (rep - 1) * length(data$words) + t # index for learning trajectory
      traj[[index]] <- m
    }
    perf[rep, ] <- get_perf(m + 1e-12) # just in case of zeros
  }
  # if(verbose) print(perf)
  xslFit(perf = perf, matrix = m, traj = traj)
}


#' Trueswell et al. 2013 propose-but-verify model
#'
#' @param alpha initial association strength
#' @param alpha_increase learning rate
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' mod <- propose_but_verify(alpha = .1, alpha_increase = .5)
#' xsl_run(mod, get_example_ambiguous_condition())
propose_but_verify <- function(alpha, alpha_increase) {
  xslMod(
    name = "propose_but_verify",
    description = "Trueswell et al. 2013 propose-but-verify (PBV) model",
    model = propose_but_verify_model,
    params = c(alpha = alpha, alpha_increase = alpha_increase),
    stochastic = TRUE
  )
}
