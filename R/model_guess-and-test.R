# hypothesis-testing model based on Medina, Snedeker,
# Trueswell, & Gleitman, 2011's verbal description:
# one-trial / "fast mapping" hypothesis:
#  i) learners hypothesize a single meaning based on their first encounter with a word
# ii) learners neither weight nor even store back-up alternative meanings
# iii) on later encounters, learners attempt to retrieve this hypothesis from memory and test it against a new context, updating it only if it is disconfirmed
# Thus, they do not accrue a "best" final hypothesis by comparing multiple episodic memories of prior contexts or multiple semantic hypotheses.

# for ICDL 2012 was: hypoth_model.R
# similar to Blythe, Smith, and Smith's guess-and-test model

guess_and_test_model <- function(params, data, control) {
  f <- params[["f"]] # forget at retrieval
  sa <- params[["sa"]] # prob of storage (slow learning down)
  reps <- control[["reps"]]

  voc <- unique(unlist(data$words))
  ref <- unique(unlist(data$objects[!is.na(data$objects)]))
  voc_sz <- length(voc) # vocabulary size
  ref_sz <- length(ref) # number of objects
  ppt <- length(data$words[[1]]) # pairs per trial ASSUMES num words = num objects per trial
  m <- matrix(0, voc_sz, ref_sz) # hypothesis matrix
  colnames(m) <- ref
  rownames(m) <- voc
  traj <- list()
  perf <- matrix(0, nrow=reps, ncol=voc_sz) # a row for each block
  freq <- rep(0,voc_sz) # number of occurrences per pair, so far
  names(freq) <- voc
  for(rep in 1:reps) {
    for(t in 1:length(data$words)) {
      tr_w <- unlist(data$words[t])
      tr_w <- tr_w[!is.na(tr_w)]
      tr_w <- tr_w[tr_w != ""]
      tr_o <- unlist(data$objects[t])
      tr_o <- tr_o[!is.na(tr_o)]
      if(length(tr_o) == 0) {
        index <- (rep-1)*length(data$words) + t
        traj[[index]] <- m
        next
      }
      freq[tr_w] <- freq[tr_w] + 1
      # forget randomly-selected hypotheses
      forget <- tr_w[which(runif(ppt) < f)]
      forget <- forget[!is.na(forget)]
      m[forget,] <- m[forget,]*0
      if(length(tr_w)==1) {
        have_hypoths <- tr_w[which(sum(m[tr_w,])!=0)]
      } else {
        have_hypoths <- tr_w[which(rowSums(m[tr_w,])!=0)] # throw out inconsistent ones
      }
      # issue is that there are duplicates in have_hypoths
      for(w in have_hypoths) {
        if(length(which(m[w,]==1))) { next }
        if(!is.element(which(m[w,]==1), tr_o)) { m[w,] <- m[w,]*0 } # disconfirmed
      }

      # make new hypotheses
      if(length(tr_w)==1) {
        need_hypoths <- tr_w[which(sum(m[tr_w,])==0)]
      } else {
        need_hypoths <- tr_w[which(rowSums(m[tr_w,])==0)]
      }
      store <- need_hypoths[which(runif(length(need_hypoths)) < sa)]
      new_hyps <- sample(tr_o, length(store), replace=TRUE)
      for(w in 1:length(store)) {
        if(length(store) == 0) {next}
        m[need_hypoths[w], new_hyps[w]] <- 1 # was m[need_hypoths[w], store[w]]
      }
      index <- (rep-1)*length(data$words) + t  # index for learning trajectory
      traj[[index]] <- m
    }
    perf[rep,] <- get_perf(m+1e-12)
  }
  xslFit(perf = perf, matrix = m+1e-12, traj = traj)
}

#' Guess and test model
#'
#' @param f forget at retrieval
#' @param sa prob of storage (slow learning down)
#'
#' @return An object of class xslMod
#' @export
guess_and_test <- function(f, sa) {
  xslMod(
    name = "guess_and_test",
    description = "Trueswell & Gleitman 2011 guess-and-test model",
    model = guess_and_test_model,
    params = c(f = f, sa = sa),
    stochastic = TRUE
  )
}
