#' Calculate Luce Choice (Proportion Correct) for Each Item in a Model Knowledge
#' Matrix
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
#'
#' @return A named numeric vector where each element corresponds to an item in
#'   the matrix. The value of each element represents the proportion of correct
#'   selections (Luce choice) for that item, calculated as the ratio of the
#'   correct association (diagonal element) to the total associations for that
#'   item.
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


#' Evaluate m-alternative forced choice test
#'
#' This function evaluates a given set of test trials using the provided model
#' memory matrix (word x referent). Each test trial is assumed to present one
#' word and a set of referents of size less than the width of the model memory
#' matrix.
#'
#' @param mperf A matrix representing model memory with words as rows and
#'   referents as columns.
#' @param test A list representing the test trials, each containing a word and
#'   its associated referents.
#'
#' @return A vector with the probability of choosing the correct object, given
#'   each word.
#' @export
mafc_test <- function(mperf, test) {
  trials <- length(test$words)
  perf = rep(0, trials)
  for(i in 1:trials) {
    w = test$words[[i]]
    denom = sum(mperf[w, test$objects[[i]]])
    perf[i] = mperf[w,w] / denom
  }
  return(perf)
}


#' Get True Positives (TP), Given a Knowledge Matrix and a Gold Lexicon
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
get_tp <- function(m, gold_lexicon) {
  count = 0
  if (length(gold_lexicon) > 0) {
    for (i in 1:length(gold_lexicon[["word"]])) {
      word = gold_lexicon[["word"]][i]
      ref = gold_lexicon[["object"]][i]
      if (!(word %in% rownames(m)) | !(ref %in% colnames(m))) {
        next
      }
      count = count + m[word, ref]
    }
    return(count)
  } else {
    for (ref in colnames(m)) {
      if (!(ref %in% rownames(m))) {
        next
      }
      count = count + m[ref, ref]
    }
    return(count)
  }
}

#' Calculate F-score, Precision, Recall, and Specificity for a Knowledge Matrix
#' at a Given Threshold
#'
#' This function calculates the F-score, precision, recall, and specificity for
#' a given knowledge matrix at a specified threshold. It uses the concept of
#' true positives, false positives, and false negatives, determined from the
#' knowledge matrix and an optional gold lexicon. The function is useful for
#' evaluating the performance of a model in terms of its ability to correctly
#' identify associations between words and referents.
#'
#' @param thresh A numeric value representing the threshold for considering an
#'   association between a word and a referent as positive.
#' @param mat A matrix representing the knowledge matrix with words as rows and
#'   referents as columns.
#' @param fscore_only Logical; if TRUE, only the F-score is returned. If FALSE,
#'   a data frame with precision, recall, specificity, and F-score for each
#'   threshold is returned.
#' @param gold_lexicon Optional; a data frame or list where each row/element
#'   represents a word-object pair in the gold lexicon. If provided, it is used
#'   to calculate true positives, false positives, and false negatives.
#' @param verbose Logical; if TRUE, additional details about true positives,
#'   false positives, and false negatives are printed.
#'
#' @return If `fscore_only` is TRUE, returns a single numeric value representing
#'   the F-score. If `fscore_only` is FALSE, returns a tibble (data frame) with
#'   columns for threshold, precision, recall, specificity, and F-score at each
#'   threshold.
#' @export
get_fscore <- function(thresh, mat, fscore_only=T, gold_lexicon = c(), verbose=F) {
  tmat <- mat >= thresh
  tp = get_tp(tmat, gold_lexicon) # correct referents selected
  words = gold_lexicon[["word"]]
  words = words[words %in% rownames(tmat)]
  objects = gold_lexicon[["object"]]
  objects = objects[objects %in% colnames(tmat)]
  if (length(gold_lexicon) > 0) {
    fp = sum(tmat[words, objects]) - tp
    fn = length(objects) - tp
  } else {
    fp = sum(tmat) - tp # incorrect referents selected: all selected referents - TPs
    fn = ncol(tmat) - tp # correct referents missed: num of words - TPs
  }
  if(verbose) print(c(tp, fp, fn))
  precision = tp / (tp + fp)
  recall = tp / (tp + fn) # aka sensitivity / true positive rate
  tn = sum(!tmat) - fn # all the 0s that should be 0s
  specificity = tn / (tn + fp) # TN = 0 where should be 0
  fscore = 2*precision*recall / (precision + recall)
  if(is.nan(fscore)) fscore = 0 # if tp+fn=0 or tp+fp=0
  if(fscore_only) {
    return(fscore)
  } else {
    return(tidyr::tibble(thresh=thresh, precision=precision, recall=recall,
                         fscore=fscore, specificity=specificity))
  }
}


#' Calculate Receiver Operating Characteristic (ROC) Scores for a Model
#' Association Matrix
#'
#' This function computes Receiver Operating Characteristic (ROC) scores for a
#' given model association matrix. It evaluates the performance of the model at
#' various thresholds, providing metrics like f-scores, precision, and recall.
#' The function can operate with a range of thresholds and optionally consider a
#' gold lexicon for calculating true and false positives and negatives. The
#' result is a comprehensive assessment of model performance over a continuum of
#' classification thresholds.
#'
#' @param mdat A matrix representing the model association matrix with words as
#'   rows and referents as columns.
#' @param fscores_only Logical; if TRUE, only the f-scores are returned for each
#'   threshold. If FALSE, a dataframe including precision, recall, and f-scores
#'   for each threshold is returned.
#' @param plot Logical; if TRUE, a plot of the ROC curve is generated and
#'   displayed.
#' @param gold_lexicon Optional; a data frame or list where each row/element
#'   represents a word-object pair in the gold lexicon. If provided, it is used
#'   for more accurate calculation of true positives, false positives, and false
#'   negatives.
#'
#' @return A dataframe or tibble with columns for the threshold, f-score,
#'   precision, and recall. If `fscores_only` is TRUE, only f-scores are
#'   returned for each threshold from 0 to 1 in increments of 0.01. If
#'   `fscores_only` is FALSE, the dataframe includes precision and recall along
#'   with f-scores for each threshold.
#' @export
get_roc <- function(mdat, fscores_only=T, plot=F, gold_lexicon = c()) {}
#   #mat <- mdat / max(unlist(mdat)) # normalize so max value(s) in entire matrix are 1
#   mat <- mdat / rowSums(mdat) # row-normalize matrix (better for all models?)
#   threshes <- seq(0,1,.01)
#   #fscores <- unlist(lapply(threshes, get_fscore, mat))
#   prf <- dplyr::bind_rows(lapply(threshes, get_fscore, mat, fscore_only=F, gold_lexicon = gold_lexicon))
#   if(plot) {
#     g <- ggplot2::ggplot(data=prf, aes(x=1-specificity, y=recall)) + geom_line() +
#       theme_classic() + xlim(0,1) + ylim(0,1)
#     print(g)
#   }
#   if(fscores_only) {
#     return(prf$fscore)
#   } else {
#     return(prf)
#   }
# }


#' Get Maximum F-score from ROC Scores
#'
#' This function computes the maximum F-score from the Receiver Operating
#' Characteristic (ROC) scores of a model. It leverages the `get_roc` function
#' to calculate the ROC scores and then extracts the highest F-score, providing
#' a concise metric for the best classification performance of the model.
#'
#' @param mdat A matrix representing the model association matrix with words as
#'   rows and referents as columns, used to calculate the ROC scores.
#' @param gold_lexicon Optional; a data frame or list where each row/element
#'   represents a word-object pair in the gold lexicon. If provided, it enhances
#'   the accuracy of the ROC score calculations.
#'
#' @return A single numeric value representing the maximum F-score obtained from
#'   the ROC scores of the model.
#' @export
get_roc_max <- function(mdat, gold_lexicon = c()) {
  fscores <- get_roc(mdat, gold_lexicon = gold_lexicon)
  return(max(fscores[!is.na(fscores)]))
}
