#' Calculate Luce choice (proportion correct) for each item in a model knowledge
#' matrix
#'
#' This function computes the Luce choice, or the proportion of correct
#' selections, for each item in a given model knowledge matrix. It assesses the
#' probability of correctly identifying each referent based on the knowledge
#' matrix, providing a measure of model performance per item.
#'
#' @param m A square matrix representing the model's knowledge, where rows
#'   correspond to words and columns correspond to referents. The diagonal
#'   elements represent correct associations, and off-diagonal elements
#'   represent incorrect associations.
#' @param d Exponent for exponentiated choice rule
#'
#' @return A named numeric vector where each element corresponds to an item in
#'   the matrix. The value of each element represents the proportion of correct
#'   selections (Luce choice) for that item, calculated as the ratio of the
#'   correct association (diagonal element) to the total associations for that
#'   item.
#' @export
#'
#' @examples
#' x <- xsl_run(baseline(), get_example_ambiguous_condition())
#' mat <- x$fits[[1]]$matrix
#' get_perf(mat)
get_perf <- function(m, d = NULL) {
  # if (is.null(d)) return(diag(m) / rowSums(m))
  # ones <- rep(1, ncol(m))
  # md <- m ^ d / outer(ones, colSums(m ^ d))
  # diag(md)
  if (is.null(d)) d <- 1
  diag(m) ^ d / rowSums(m ^ d)
}

# power choice rule
# diag(m ^ d / outer(ones, colSums(m ^ d)))
# diag(m) ^ d / colSums(m ^ d)

# get_perf <- function(m) {
#   perf <- rep(0, nrow(m))
#   names(perf) <- rownames(m)
#   for (ref in colnames(m)) {
#     if (!(ref %in% rownames(m))) {
#       next
#     }
#     correct <- m[ref, ref]
#     total <- sum(m[ref,])
#     if (total == 0) {
#       next
#     }
#     perf[ref] <- correct / total
#   }
#   return(perf)
# }


#' Evaluate m-alternative forced choice test
#'
#' This function evaluates a given set of test trials using the provided model
#' memory matrix (word x referent). Each test trial is assumed to present one
#' word and a set of referents of size less than the width of the model memory
#' matrix.
#'
#' @param m A matrix representing model memory with words as rows and
#'   referents as columns.
#' @param test A list representing the test trials, each containing a word and
#'   its associated referents.
#'
#' @return A vector with the probability of choosing the correct object, given
#'   each word.
#' @export
#'
#' @examples
#' dat <- xsl_datasets[[10]]
#' x <- xsl_run(baseline(), dat)
#' mat <- x$fits[[1]]$matrix
#' mafc_test(mat, dat$test)
mafc_test <- function(m, test) {
  trials <- length(test$words)
  perf <- rep(0, trials)
  for (i in 1:trials) {
    w <- test$words[[i]]
    denom <- sum(m[w, test$objects[[i]]])
    perf[i] <- m[w, w] / denom
  }
  return(perf)
}


#' Get true positives (TP), given a knowledge matrix and a gold-standard lexicon
#'
#' This function iterates over words in a given gold lexicon and accumulates the
#' associative strength (can be integral e.g. 1, or real-valued) in the
#' knowledge matrix for the intended referents (present in the gold lexicon).
#' Returns the number of expected true positives (TP) for this gold lexicon and
#' knowledge matrix.
#'
#' @param m A matrix representing the knowledge matrix with words as rows and
#'   referents as columns.
#' @param gold_lexicon A data frame or list where each row/element represents a
#'   word-object pair in the gold lexicon.
#'
#' @return A single value with the expected number of true positives.
#' @export
#'
#' @examples
#' dat <- xsl_datasets[[10]]
#' x <- xsl_run(baseline(), dat)
#' mat <- x$fits[[1]]$matrix
#' lex <- list(words = rep(1:18), objects = rep(1:18))
#' get_tp(mat, lex)
get_tp <- function(m, gold_lexicon) {
  count <- 0
  if (length(gold_lexicon) > 0) {
    for (i in seq_along(gold_lexicon[["words"]])) {
      word <- gold_lexicon[["words"]][i]
      ref <- gold_lexicon[["objects"]][i]
      if (!(word %in% rownames(m)) || !(ref %in% colnames(m))) {
        next
      }
      count <- count + m[word, ref]
    }
  } else {
    for (ref in colnames(m)) {
      if (!(ref %in% rownames(m))) {
        next
      }
      count <- count + m[ref, ref]
    }
  }
  return(count)
}

#' Calculate F-score, precision, recall, and specificity for a knowledge matrix
#' at a given threshold
#'
#' This function calculates the F-score, precision, recall, and specificity for
#' a given knowledge matrix at a specified threshold. It uses the concept of
#' true positives, false positives, and false negatives, determined from the
#' knowledge matrix and an optional gold lexicon. The function is useful for
#' evaluating the performance of a model in terms of its ability to correctly
#' identify associations between words and referents.
#'
#' @param m A matrix representing the knowledge matrix with words as rows and
#'   referents as columns.
#' @param threshold A numeric value representing the threshold for considering
#'   an association between a word and a referent as positive.
#' @param gold_lexicon Optional; a data frame or list where each row/element
#'   represents a word-object pair in the gold lexicon. If provided, it is used
#'   to calculate true positives, false positives, and false negatives.
#'
#' @return A tibble with columns for threshold, precision, recall, specificity,
#'   and F-score.
#' @export
#'
#' @examples
#' dat <- xsl_datasets[[1]]
#' x <- xsl_run(baseline(), dat)
#' mat <- x$fits[[1]]$matrix
#' lex <- list(words = rep(1:18), objects = rep(1:18))
#' get_fscore(mat, 0.5, lex)
get_fscore <- function(m, threshold, gold_lexicon = NULL) {
  tmat <- m >= threshold
  tp <- get_tp(tmat, gold_lexicon) # correct referents selected
  words <- gold_lexicon[["words"]]
  words <- words[words %in% rownames(tmat)]
  objects <- gold_lexicon[["objects"]]
  objects <- objects[objects %in% colnames(tmat)]
  if (!is.null(gold_lexicon)) {
    fp <- sum(tmat[words, objects]) - tp
    fn <- length(objects) - tp
  } else {
    fp <- sum(tmat) - tp # incorrect referents selected: all selected referents - TPs
    fn <- ncol(tmat) - tp # correct referents missed: num of words - TPs
  }
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn) # aka sensitivity / true positive rate
  tn <- sum(!tmat) - fn # all the 0s that should be 0s
  specificity <- tn / (tn + fp) # TN = 0 where should be 0
  fscore <- 2 * precision * recall / (precision + recall)
  if (is.nan(fscore)) fscore <- 0 # if tp+fn=0 or tp+fp=0
  tibble::tibble(threshold = threshold, precision = precision, recall = recall,
                 fscore = fscore, specificity = specificity)
}


#' Calculate receiver operating characteristic (ROC) scores for a model
#' association Matrix
#'
#' This function computes receiver operating characteristic (ROC) scores for a
#' given model association matrix. It evaluates the performance of the model at
#' various thresholds, providing metrics like f-scores, precision, and recall.
#' The function can operate with a range of thresholds and optionally consider a
#' gold lexicon for calculating true and false positives and negatives. The
#' result is a comprehensive assessment of model performance over a continuum of
#' classification thresholds.
#'
#' @inheritParams get_fscore
#' @param thresholds Vector of thresholds to use.
#'
#' @return A tibble with columns for the threshold, f-score,
#'   precision, and recall.
#' @export
#'
#' @examples
#' dat <- xsl_datasets[[1]]
#' x <- xsl_run(baseline(), dat)
#' mat <- x$fits[[1]]$matrix
#' lex <- list(words = rep(1:18), objects = rep(1:18))
#' get_roc(mat, gold_lexicon = lex)
#' plot_roc(mat, gold_lexicon = lex)
#' get_roc_max(mat, gold_lexicon = lex)
get_roc <- function(m, thresholds = seq(0, 1, .01), gold_lexicon = NULL) {
  #mat <- mdat / max(unlist(mdat)) # normalize so max value(s) in entire matrix are 1
  mn <- m / rowSums(m) # row-normalize matrix (better for all models?)
  map(thresholds, \(t) get_fscore(mn, t, gold_lexicon)) |> list_rbind()
}

#' Plot receiver operating characteristic (ROC) scores for a model
#' association matrix
#'
#' @rdname get_roc
#' @export
plot_roc <- function(m, thresholds = seq(0, 1, .01), gold_lexicon = NULL) {
  roc <- get_roc(m, thresholds, gold_lexicon)
  ggplot(roc, aes(x = 1 - .data$specificity, y = .data$recall)) +
    geom_line() +
    xlim(0, 1) + ylim(0, 1)
}

#' Get maximum F-score from ROC scores
#'
#' This function computes the maximum F-score from the receiver operating
#' characteristic (ROC) scores of a model. It leverages the `get_roc()` function
#' to calculate the ROC scores and then extracts the highest F-score, providing
#' a concise metric for the best classification performance of the model.
#'
#' @rdname get_roc
#'
#' @return A single numeric value representing the maximum F-score obtained from
#'   the ROC scores of the model.
#' @export
get_roc_max <- function(m, thresholds = seq(0, 1, .01), gold_lexicon = NULL) {
  fscores <- get_roc(m, thresholds = thresholds, gold_lexicon = gold_lexicon)$fscore
  max(fscores[!is.na(fscores)])
}
