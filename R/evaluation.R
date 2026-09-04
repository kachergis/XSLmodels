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


#' Predict referent selection for a single trial from an association matrix
#'
#' Given a model's word-object association matrix, returns the probability
#' distribution over a trial's candidate objects for one heard word -- the
#' quantity to compare against a participant's choice on a referent-selection
#' (m-alternative forced choice) trial. This is model-agnostic: it works on the
#' `matrix` of any `xslFit` (or any word-by-object matrix), reading each entry
#' as an association strength / meaning weight.
#'
#' The literal rule is Bayes' rule on the word's row,
#' `P(object | word) proportional to m[word, object] * prior(object)`,
#' normalized over the objects present. With `pragmatic = TRUE` the objects are
#' instead resolved by a one-step Rational Speech Act pragmatic listener
#' ([rsa_listener()]), which additionally reasons that the speaker could have
#' used a different word -- this is what yields *strong* mutual exclusivity when
#' a heard word is lexically ambiguous and a competitor word names one of the
#' candidates. A truly novel (unseen) word carries no lexical evidence and
#' returns the prior in either mode.
#'
#' @param m A word-by-object association matrix (e.g.
#'   `xsl_run(mod, data)$fits[[1]]$matrix`). Assumed non-negative; negative
#'   entries are clamped to 0. `dimnames` are used to resolve character
#'   `word`/`objects`.
#' @param word The heard word: a row name of `m`, or a positive integer row
#'   index. A value not present in `m` is treated as a novel word.
#' @param objects The candidate objects present on the trial: column names of
#'   `m`, or positive integer column indices. An entry not present in `m` is
#'   treated as an unnamed (novel) object.
#' @param prior Optional prior over `objects` (length `length(objects)`,
#'   non-negative). Defaults to uniform. Set an entry to 0 to remove that
#'   object from consideration entirely (e.g. an object the speaker has no
#'   epistemic access to).
#' @param pragmatic If `TRUE`, use the RSA pragmatic listener instead of the
#'   literal one.
#' @param threshold Optional; binarize `m` at this value (`m >= threshold`)
#'   before predicting. The RSA layer is sharpest on a binary lexicon.
#' @param rsa_alpha,depth Speaker rationality and recursion depth for the RSA
#'   listener (only used when `pragmatic = TRUE`).
#'
#' @return A numeric vector of probabilities over `objects`, in the given
#'   order, summing to 1.
#' @export
#'
#' @examples
#' m <- xsl_run(fgt2009(alpha = 1), get_example_unambiguous_condition())$fits[[1]]$matrix
#' predict_referent(m, 1, c(1, 2, 3))
#' predict_referent(m, 1, c(1, 2, 3), prior = c(1, 0, 1)) # object 2 unavailable
predict_referent <- function(m, word, objects, prior = NULL,
                             pragmatic = FALSE, threshold = NULL,
                             rsa_alpha = 3, depth = 1) {
  M <- pmax(m, 0)
  if (!is.null(threshold)) M <- (M >= threshold) * 1
  wn <- rownames(M)
  on <- colnames(M)
  W <- nrow(M)
  O <- ncol(M)

  resolve <- function(x, nms, n) {
    if (is.numeric(x)) {
      idx <- as.integer(round(x))
      idx[idx < 1 | idx > n] <- NA_integer_
      return(idx)
    }
    match(as.character(x), nms)
  }

  P <- length(objects)
  if (P == 0) stop("`objects` must be non-empty")
  pr <- if (is.null(prior)) rep(1, P) else as.numeric(prior)
  if (length(pr) != P) stop("`prior` must have length ", P)
  if (any(pr < 0) || sum(pr) <= 0) {
    stop("`prior` must be non-negative with positive total mass")
  }

  oidx <- resolve(objects, on, O)
  Msub <- matrix(0, W, P)
  for (k in seq_len(P)) if (!is.na(oidx[k])) Msub[, k] <- M[, oidx[k]]

  widx <- resolve(word, wn, W)
  if (length(widx) != 1 || is.na(widx)) return(pr / sum(pr))  # novel word

  if (!pragmatic) {
    row <- Msub[widx, ] * pr
    return(if (sum(row) > 0) row / sum(row) else pr / sum(pr))
  }

  cand <- which(rowSums(Msub) > 0)
  if (!(widx %in% cand)) cand <- c(cand, widx)
  listener <- rsa_listener(Msub[cand, , drop = FALSE], pr,
                           alpha = rsa_alpha, depth = depth)
  row <- listener[match(widx, cand), ]
  if (sum(row) > 0 && all(is.finite(row))) row / sum(row) else pr / sum(pr)
}

#' Evaluate m-alternative forced choice test
#'
#' Scores a set of test trials against a model's word-object matrix, returning
#' the probability of choosing the correct object on each trial. Each trial
#' presents one word and a set of candidate referents; the correct object is
#' the one whose id matches the word's id (the package's diagonal convention).
#' A thin wrapper over [predict_referent()].
#'
#' @param m A matrix representing model memory with words as rows and
#'   referents as columns.
#' @param test A list representing the test trials, each containing a word and
#'   its associated referents.
#' @param ... Further arguments passed to [predict_referent()] (e.g.
#'   `pragmatic`, `threshold`).
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
mafc_test <- function(m, test, ...) {
  vapply(seq_along(test$words), function(i) {
    w <- test$words[[i]]
    os <- test$objects[[i]]
    probs <- predict_referent(m, w, os, ...)
    correct <- match(w, os)
    if (is.na(correct)) NA_real_ else probs[correct]
  }, numeric(1))
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
