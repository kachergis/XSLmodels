

#' Creates Co-occurrence Matrix from Training Trials
#'
#' Given a training order (list of words and objects per trial), this function returns a matrix
#' tallying word-object co-occurrences across the trials. This matrix can be used to analyze
#' the frequency with which certain words and objects appear together during the training phase.
#'
#' @param train A list representing the training data, where each element is a trial that includes
#' both words and objects. The structure is expected to have sub-elements `words` and `objs`
#' for each trial.
#'
#' @return A matrix where each element represents the count of co-occurrences between a word
#' (rows) and an object (columns). The row and column names correspond to the unique words and
#' objects found in the training data, respectively.
#' @export
create_cooc_matrix <- function(train) {
  Nwords = length(unique(unlist(train$words)))
  Nobjs = length(unique(unlist(train$objs)))
  M = matrix(0, nrow=Nwords, ncol=Nobjs)
  rownames(M) = sort(unique(unlist(train$words)))
  colnames(M) = sort(unique(unlist(train$objs)))
  # iterate over training trials
  for(i in 1:length(train$words)) {
    M[train$words[[i]], train$objs[[i]]] = M[train$words[[i]], train$objs[[i]]] + 1
  }
  return(M)
}


#' Calculate Luce Choice (Proportion Correct) for Each Item in a Model Knowledge Matrix
#'
#' This function computes the Luce choice, or the proportion of correct selections,
#' for each item in a given model knowledge matrix. It assesses the probability of
#' correctly identifying each referent based on the knowledge matrix, providing a
#' measure of model performance per item.
#'
#' @param m A square matrix representing the model's knowledge, where rows
#' correspond to words and columns correspond to referents. The diagonal
#' elements represent correct associations, and off-diagonal elements represent
#' incorrect associations.
#'
#' @return A named numeric vector where each element corresponds to an
#' item in the matrix. The value of each element represents the proportion of
#' correct selections (Luce choice) for that item, calculated as the ratio of the
#' correct association (diagonal element) to the total associations for that item.
#' @export
get_perf <- function(m) {
  perf <- rep(0, nrow(m))
  names(perf) <- rownames(m)
  for (ref in colnames(m)) {
    if (!(ref %in% rownames(m))) {
      next
    }
    correct <- m[ref, ref]
    total <- sum(m[ref,])
    if (total == 0) {
      next
    }
    perf[ref] <- correct / total
  }
  return(perf)
}


get_test_study_ord = function(simple=T) {
  if(simple) {
    design = matrix(c(1,2, 1,3), nrow=2, ncol=2, byrow=T)
  } else {
    design = matrix(c(1,2,3, 1,4,5, 2,3,4, 5,6,1), nrow=4, ncol=3, byrow=T)
  }
  ord = list(words=design, objs=design)
  return(ord)
}


#' Calculates Shannon entropy of a supplied vector, after normalizing it
#'
shannon.entropy <- function(p) {
  if (min(p) < 0 || sum(p) <= 0)
    return(NA)
  p.norm <- p[p>0]/sum(p)
  -sum(log2(p.norm)*p.norm)
}


#' Utility function for several models that fills in a given co-occurrence matrix
#' m with startval, for cells of m that were 0 before
#'
update_known = function(m, tr_w, tr_o, startval = .01) {
  tr_assocs = m[tr_w, tr_o]
  tr_assocs[which(tr_assocs==0)] = startval
  m[tr_w, tr_o] = tr_assocs

  # for any other experienced word (not on this trial), fill in startval

  fam_objs = which(colSums(m)>0)
  fam_words = which(rowSums(m)>0)

  for(w in tr_w) {
    zeros = which(m[w,fam_objs]==0)
    m[w,zeros] = startval
  }

  for(o in tr_o) {
    zeros = which(m[fam_words,o]==0)
    m[zeros,o] = startval
  }

  return(m)
}
