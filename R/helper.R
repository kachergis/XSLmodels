#' Creates co-occurrence matrix from training trials
#'
#' Given a training order (list of words and objects per trial), this function
#' returns a matrix tallying word-object co-occurrences across the trials. This
#' matrix can be used to analyze the frequency with which certain words and
#' objects appear together during the training phase.
#'
#' @param train A list representing the training data, where each element is a
#'   trial that includes both words and objects. The structure is expected to
#'   have sub-elements `words` and `objects` for each trial.
#'
#' @return A matrix where each element represents the count of co-occurrences
#'   between a word (rows) and an object (columns). The row and column names
#'   correspond to the unique words and objects found in the training data,
#'   respectively.
#' @export
#'
#' @examples
#' create_cooc_matrix(xsl_datasets[[1]]$train)
create_cooc_matrix <- function(train) {
  words <- sort(unique(unlist(train$words)))
  objects <- sort(unique(unlist(train$objects)))
  m <- matrix(0, nrow = length(words), ncol = length(objects),
              dimnames = list(words, objects))

  # iterate over training trials
  for (i in seq_along(words)) {
    m[train$words[[i]], train$objects[[i]]] <- m[train$words[[i]], train$objects[[i]]] + 1
  }
  return(m)
}

#' Calculates Shannon entropy of a supplied vector, after normalizing it
#'
#' @param p Numeric vector of probabilities
shannon_entropy <- function(p) {
  if (min(p) < 0 || sum(p) <= 0) return(NA)
  p_norm <- p[p > 0] / sum(p)
  -sum(log2(p_norm) * p_norm)
}


#' Update known
#'
#' Utility function for several models that fills in a given co-occurrence
#' matrix m with startval, for cells of m that were 0 before
#'
#' @param m Co-occurrence matrix
#' @param tr_w Words
#' @param tr_o Objects
#' @param startval Starting value
update_known <- function(m, tr_w, tr_o, startval = .01) {
  tr_assocs <- m[tr_w, tr_o]
  tr_assocs[which(tr_assocs == 0)] <- startval
  m[tr_w, tr_o] <- tr_assocs
  # for any other experienced word (not on this trial), fill in startval

  fam_objects <- which(colSums(m) > 0)
  fam_words <- which(rowSums(m) > 0)

  for (w in tr_w) {
    zeros <- which(m[w, fam_objects] == 0)
    m[w, zeros] <- startval
  }

  for (o in tr_o) {
    zeros <- which(m[fam_words, o] == 0)
    m[zeros, o] <- startval
  }

  return(m)
}
