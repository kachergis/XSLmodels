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
#' @param rule The choice rule: `"luce"` (the default), or `"remember_guess"`,
#'   under which a learner recalls a word's referent from memory or else
#'   guesses (see [remember_guess()]).
#' @param mu Mutual-exclusivity strength for `rule = "remember_guess"`: guesses
#'   avoid objects associated with other words (see [remember_guess()]). 0,
#'   the default, guesses uniformly.
#'
#' @return A named numeric vector where each element corresponds to an item in
#'   the matrix. The value of each element represents the proportion of correct
#'   selections (Luce choice) for that item, calculated as the ratio of the
#'   correct association (diagonal element) to the total associations for that
#'   item.
#'
#' @details Negative associations (possible in error-driven models such as
#'   [rescorla_wagner()] and [kalman_filter()]) are treated as 0 before the
#'   choice rule is applied -- Luce choice is undefined for negative values,
#'   and would otherwise yield "probabilities" outside \[0, 1\] -- matching
#'   [predict_referent()]. A word whose associations are then all 0 is
#'   answered at chance (`1 / ncol(m)`).
#' @export
#'
#' @examples
#' x <- xsl_run(baseline(), get_example_ambiguous_condition())
#' mat <- x$fits[[1]]$matrix
#' get_perf(mat)
get_perf <- function(m, d = NULL, rule = c("luce", "remember_guess"), mu = 0) {
  rule <- match.arg(rule)
  if (is.null(d)) d <- 1
  m <- pmax(m, 0) ^ d
  if (rule == "remember_guess") {
    out <- vapply(seq_len(nrow(m)), \(i) {
      if (i > ncol(m)) return(0)
      remember_guess(m, i, seq_len(ncol(m)), mu = mu)[i]
    }, numeric(1))
    return(stats::setNames(out, rownames(m)))
  }
  tot <- rowSums(m)
  ifelse(tot > 0, diag(m) / tot, 1 / ncol(m))
}

#' Softmax choice weights
#'
#' Transforms a word-by-object association matrix so that Luce choice over it
#' (as in [get_perf()], [mafc_test()] and [choice_probabilities()]) is a
#' softmax choice rule with inverse temperature `phi`: a word's probability of
#' choosing object `o` among a set of alternatives is
#' `exp(phi * m[w, o]) / sum(exp(phi * m[w, alternatives]))`. Unlike Luce
#' choice, softmax is defined for negative associations (which error-driven
#' models such as [rescorla_wagner()] and [kalman_filter()] can learn) and
#' gives objects a word has never been paired with nonzero probability.
#'
#' [xsl_run()] applies it automatically to a model with a `phi` parameter
#' (e.g. `rescorla_wagner(..., phi = 5)`), to the mean of the simulations'
#' matrices.
#'
#' @param m A word-by-object association matrix.
#' @param phi Inverse temperature (choice sensitivity), `phi >= 0`: 0 is
#'   uniform guessing, and larger values choose the strongest association
#'   more deterministically.
#'
#' @return A matrix of positive choice weights with `m`'s dimensions and
#'   names. Each row is shifted by its maximum before exponentiating, which
#'   leaves the choice probabilities unchanged and avoids overflow.
#' @export
#'
#' @examples
#' m <- matrix(c(1, -.5, 0, .2), 2, dimnames = list(1:2, 1:2))
#' get_perf(softmax_weights(m, phi = 3))
softmax_weights <- function(m, phi) {
  stopifnot(is.numeric(phi), length(phi) == 1, phi >= 0)
  out <- exp(phi * (m - apply(m, 1, max)))
  dimnames(out) <- dimnames(m)
  out
}

# The matrix xsl_run() scores: a model's association matrix summed over its
# simulations, as softmax weights of the mean if the model has an inverse
# temperature `phi`. Luce choice is scale-free, so the sum serves; the
# remember-or-guess rule reads strengths as probabilities, so it gets the mean.
choice_matrix <- function(mat, n_sim, params) {
  phi <- params[["phi"]]
  if (!is.null(phi)) return(softmax_weights(mat / n_sim, phi))
  if (choice_rule(params) == "remember_guess") mat / n_sim else mat
}

# A model's choice rule and its arguments, as xsl_run() scores it. `mu` is
# read only under the remember-or-guess rule (other models use the name).
choice_rule <- function(params) params[["choice_rule"]] %||% "luce"
choice_args <- function(params) {
  rule <- choice_rule(params)
  list(d = params[["ch_dec"]], rule = rule,
       mu = if (rule == "remember_guess") params[["mu"]] %||% 0 else 0)
}

#' Remember-or-guess choice rule
#'
#' The choice rule of Yurovsky & Frank (2015): a learner hearing word `w`
#' recalls one of its remembered referents, or else guesses among the objects
#' on offer. Reading the association strengths `r_o = m[w, o]` of the
#' alternatives as the probabilities of recalling each one, the probability of
#' choosing object `o` among `k` alternatives is
#' \deqn{P(o) = r_o + (1 - \textstyle\sum r) \, g_o}
#' when \eqn{\sum r \le 1}, where \eqn{g} is the guessing distribution, and
#' \eqn{P(o) = r_o / \sum r} (Luce choice) when the strengths sum to more than
#' 1. The two agree at \eqn{\sum r = 1}. With one remembered object and
#' uniform guessing this is \eqn{M + (1 - M) / k}, the paper's Eq. 5.
#'
#' Unlike Luce choice, the rule depends on how many alternatives there are, so
#' it is not a transformation of the matrix (as [softmax_weights()] is), and
#' it is not scale-free: strengths are probabilities.
#'
#' **Mutual exclusivity.** Guesses are uniform by default. With `mu > 0` they
#' avoid objects associated with other words: \eqn{g_o \propto \exp(-\mu
#' c_o)}, where \eqn{c_o} is object `o`'s summed strength with every word but
#' `w`. A novel word (not a row of `m`) has nothing to recall, so it is a pure
#' guess: chance with `mu = 0`, and below chance for objects already named
#' with `mu > 0`.
#'
#' A model uses this rule in [xsl_run()], [get_perf()], [mafc_test()] and
#' [choice_probabilities()] when its parameters include `choice_rule =
#' "remember_guess"` (and optionally `mu`), as [yf2015()]'s do. For a
#' stochastic model `xsl_run()` applies it to the mean of the simulations'
#' matrices; that is the mean of the simulated learners' choice probabilities
#' as long as no learner's strengths sum to more than 1 and `mu = 0`, where
#' the rule is linear.
#'
#' @param m A word-by-object matrix of association strengths (negative
#'   entries are treated as 0).
#' @param word The word: a row name of `m`, or a row index. A value not in `m`
#'   is a novel word.
#' @param objects The alternatives: column names of `m`, or column indices.
#'   An entry not in `m` is a novel object (strength 0 with every word).
#' @param mu Mutual-exclusivity strength, `mu >= 0`.
#' @param prior Optional weights over `objects` scaling the guessing
#'   distribution; an object with weight 0 is never chosen.
#'
#' @return A numeric vector of choice probabilities over `objects`, summing to
#'   1.
#' @references Yurovsky, D., & Frank, M. C. (2015). An integrative account of
#'   constraints on cross-situational learning. *Cognition*, 145, 53-62.
#'   \doi{10.1016/j.cognition.2015.07.013}
#' @seealso [predict_referent()], whose `rule` argument uses it.
#' @export
#'
#' @examples
#' m <- matrix(c(.6, .1, 0, 0, 0, .5), 2, byrow = TRUE,
#'             dimnames = list(c("A", "B"), c("a1", "a2", "b1")))
#' remember_guess(m, "A", c("a1", "a2"))          # .6 + .3 / 2 for a1
#' remember_guess(m, "A", c("a2", "x", "y"))      # a2 vs. two novel objects
#' remember_guess(m, "C", c("b1", "x"), mu = 2)   # a novel word avoids b1
remember_guess <- function(m, word, objects, mu = 0, prior = NULL) {
  stopifnot(is.numeric(mu), length(mu) == 1, mu >= 0)
  M <- pmax(m, 0)
  k <- length(objects)
  if (k == 0) stop("`objects` must be non-empty", call. = FALSE)
  pr <- if (is.null(prior)) rep(1, k) else as.numeric(prior)
  if (length(pr) != k || any(pr < 0) || sum(pr) <= 0) {
    stop("`prior` must be ", k, " non-negative weights with a positive sum", call. = FALSE)
  }
  oi <- resolve_index(objects, colnames(M), ncol(M))
  sub <- matrix(0, nrow(M), k)
  sub[, !is.na(oi)] <- M[, oi[!is.na(oi)], drop = FALSE]
  wi <- resolve_index(word, rownames(M), nrow(M))
  if (length(wi) != 1) stop("`word` must be a single word", call. = FALSE)
  r <- if (is.na(wi)) numeric(k) else sub[wi, ] * (pr > 0)
  g <- pr
  if (mu > 0) {
    comp <- colSums(sub) - (if (is.na(wi)) 0 else sub[wi, ])
    g <- g * exp(-mu * (comp - min(comp)))  # shifted: same proportions, no underflow to all 0
  }
  rg_probs(r, g)
}

# The remember-or-guess rule on strengths r and guessing weights g.
rg_probs <- function(r, g) {
  s <- sum(r)
  if (s >= 1) return(r / s)
  r + (1 - s) * g / sum(g)
}

# Row or column positions of `x` (labels, or integer positions) in a matrix
# dimension with names `nms` and size `n`; NA where absent.
resolve_index <- function(x, nms, n) {
  if (is.numeric(x)) {
    idx <- as.integer(round(x))
    idx[idx < 1 | idx > n] <- NA_integer_
    return(idx)
  }
  match(as.character(x), nms)
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
#' @param d Optional exponent for an exponentiated (power) choice rule: the
#'   (clamped, possibly thresholded) associations are raised to `d` before
#'   choice, as in [get_perf()]. `NULL` (the default) leaves them unchanged.
#'   [xsl_run()] passes a model's `ch_dec` parameter here, so that
#'   [bayesian_decay()] scores test trials with the same decision rule it uses
#'   elsewhere.
#' @param rsa_alpha,depth Speaker rationality and recursion depth for the RSA
#'   listener (only used when `pragmatic = TRUE`).
#' @param rule The literal listener's choice rule: `"luce"` (Bayes' rule on
#'   the word's row, the default), or `"remember_guess"` (see
#'   [remember_guess()]). Not available with `pragmatic = TRUE`.
#' @param mu Mutual-exclusivity strength for `rule = "remember_guess"`.
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
                             rsa_alpha = 3, depth = 1, d = NULL,
                             rule = c("luce", "remember_guess"), mu = 0) {
  rule <- match.arg(rule)
  M <- pmax(m, 0)
  if (!is.null(threshold)) M <- (M >= threshold) * 1
  if (!is.null(d)) M <- M ^ d
  if (rule == "remember_guess") {
    if (pragmatic) stop("`rule = \"remember_guess\"` has no pragmatic listener", call. = FALSE)
    return(remember_guess(M, word, objects, mu = mu, prior = prior))
  }
  wn <- rownames(M)
  on <- colnames(M)
  W <- nrow(M)
  O <- ncol(M)

  resolve <- resolve_index

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
#' the one whose id matches the word's id (the package's diagonal convention),
#' unless the test gives an explicit `correct` object for that trial (e.g. to
#' test a word's association with a second, co-occurring referent, with its
#' own referent absent). A thin wrapper over [predict_referent()].
#'
#' @param m A matrix representing model memory with words as rows and
#'   referents as columns.
#' @param test A list representing the test trials: `words` (one word per
#'   trial), `objects` (the candidate referents on each trial), and optionally
#'   `correct` (the correct referent on each trial; defaults to the word's own
#'   id).
#' @param ... Further arguments passed to [predict_referent()] (e.g.
#'   `pragmatic`, `threshold`, `d`, the exponent of a power choice rule, or
#'   `rule = "remember_guess"`).
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
    target <- if (is.null(test$correct)) w else test$correct[[i]]
    correct <- match(target, os)
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
      # index by label, not position: lexicon ids are often numeric
      word <- as.character(gold_lexicon[["words"]][i])
      ref <- as.character(gold_lexicon[["objects"]][i])
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
  words <- as.character(gold_lexicon[["words"]])
  words <- words[words %in% rownames(tmat)]
  objects <- as.character(gold_lexicon[["objects"]])
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
  mn[rowSums(m) == 0, ] <- 0 # a word with no associations selects nothing
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


# The alternatives and correct object for each test item, as xsl_run() scores
# accuracy: a condition with a test offers each trial's listed objects;
# otherwise every object is an alternative, and word i's correct referent is
# the i-th object (get_perf()'s diagonal convention).
item_alternatives <- function(m, data) {
  if (length(data$test) == 0) {
    return(list(alternatives = rep(list(colnames(m)), nrow(m)),
                correct = colnames(m)[seq_len(nrow(m))]))
  }
  test <- labelled_test(data$test)
  target <- if (is.null(test$correct)) test$words else test$correct
  list(alternatives = test$objects, correct = vapply(target, `[[`, character(1), 1))
}

# ... plus the model's choice distribution over them, by its choice rule (Luce
# choice with exponent d by default), exactly as xsl_run() scores accuracy.
item_choices <- function(m, data, d = NULL, rule = "luce", mu = 0) {
  ch <- item_alternatives(m, data)
  ch$probs <- if (length(data$test) > 0) {
    test <- labelled_test(data$test)
    lapply(seq_along(ch$alternatives), \(i) {
      predict_referent(m, test$words[[i]], ch$alternatives[[i]], d = d, rule = rule, mu = mu)
    })
  } else if (rule != "luce") {
    lapply(seq_len(nrow(m)), \(i) predict_referent(m, i, seq_len(ncol(m)), d = d, rule = rule, mu = mu))
  } else {
    M <- pmax(m, 0) ^ (if (is.null(d)) 1 else d)
    lapply(seq_len(nrow(m)), \(i) {
      tot <- sum(M[i, ])
      if (tot > 0) unname(M[i, ]) / tot else rep(1 / ncol(M), ncol(M))
    })
  }
  ch
}

# Human response counts over each item's alternatives, from the data's
# response_matrix (NULL if it has none).
item_counts <- function(data, alternatives) {
  rm <- data$response_matrix
  if (is.null(rm)) return(NULL)
  if (is.null(colnames(rm))) colnames(rm) <- as.character(sort(unique(unlist(data$train$objects))))
  lapply(seq_along(alternatives), \(i) {
    idx <- match(alternatives[[i]], colnames(rm))
    if (anyNA(idx) || sum(rm[i, ]) != sum(rm[i, idx])) {
      stop("condition '", data$label, "' item ", i, ": response_matrix has responses to objects ",
           "that aren't among the item's test alternatives", call. = FALSE)
    }
    unname(rm[i, idx])
  })
}

# Number correct and number of responses per test item: from the
# response_matrix if there is one, otherwise from accuracy and n_subj.
correct_counts <- function(data, alternatives, correct) {
  counts <- item_counts(data, alternatives)
  if (!is.null(counts)) {
    return(list(n = vapply(counts, sum, numeric(1)),
                correct = vapply(seq_along(counts), \(i) counts[[i]][match(correct[i], alternatives[[i]])], numeric(1))))
  }
  if (length(data$n_subj) == 0 || is.na(data$n_subj)) {
    stop("condition '", data$label, "' has neither a response_matrix nor n_subj, ",
         "so it has no likelihood", call. = FALSE)
  }
  list(n = rep(data$n_subj, length(data$accuracy)), correct = round(data$accuracy * data$n_subj))
}

# Lapse mixture: with probability `lapse` the learner guesses uniformly among
# the item's alternatives.
with_lapse <- function(p, lapse, k = length(p)) (1 - lapse) * p + lapse / k

# Log-likelihood of one condition's human test responses under a model's
# learned matrix m (whose accuracy predictions are `perf`).
condition_loglik <- function(m, perf, data, objective, lapse, d = NULL, rule = "luce", mu = 0) {
  ch <- item_alternatives(m, data)
  if (objective == "binomial") {
    cnt <- correct_counts(data, ch$alternatives, ch$correct)
    q <- with_lapse(perf, lapse, lengths(ch$alternatives))
    return(sum(stats::dbinom(cnt$correct, cnt$n, q, log = TRUE)))
  }
  if (is.null(data$response_matrix)) {
    stop("condition '", data$label, "' has no response_matrix, which the multinomial ",
         "objective needs; use objective = \"binomial\"", call. = FALSE)
  }
  probs <- item_choices(m, data, d, rule, mu)$probs
  counts <- item_counts(data, ch$alternatives)
  sum(vapply(seq_along(probs), \(i) {
    stats::dmultinom(counts[[i]], prob = with_lapse(probs[[i]], lapse), log = TRUE)
  }, numeric(1)))
}

# Number of human responses a likelihood objective scores.
n_responses <- function(data) {
  if ("xslData" %in% class(data)) data <- list(data)
  sum(vapply(data, \(d) {
    if (!is.null(d$response_matrix)) sum(unlist(d$response_matrix)) else d$n_subj * length(d$accuracy)
  }, numeric(1)))
}

#' Choice probabilities for each test item, with human response counts
#'
#' For each test item, returns the alternatives offered, the correct object,
#' the model's probability of choosing each alternative, and how many
#' participants chose each one. This is the full choice distribution behind
#' the accuracy that [xsl_run()] scores: the probability of the correct
#' alternative is exactly that item's entry in `perf`. It is what a
#' multinomial likelihood (`xslControl(objective = "multinomial")`) compares
#' with the data, and it shows which wrong objects a model expects learners to
#' choose.
#'
#' Items are scored as in [xsl_run()]: on a condition with a `test`, by Luce
#' choice over each test trial's listed objects (see [mafc_test()]);
#' otherwise among all objects, with word i's correct referent the i-th
#' object (see [get_perf()]).
#'
#' @param m A word-by-object matrix, e.g. `xsl_run(mod, data)$fits[[1]]$matrix`.
#' @param data The `xslData` condition `m` was learned from.
#' @param d Optional choice exponent (a model's `ch_dec`; see [get_perf()]).
#' @param phi Optional softmax inverse temperature (a model's `phi`; see
#'   [softmax_weights()]). For a stochastic model, pass the mean matrix,
#'   `matrix / n_sim`, as [xsl_run()] scores it.
#' @param rule,mu The choice rule and its mutual-exclusivity strength (a
#'   model's `choice_rule` and `mu`; see [remember_guess()]). For a
#'   stochastic model with `rule = "remember_guess"`, pass the mean matrix.
#' @param lapse Lapse rate: the probability of guessing uniformly among the
#'   alternatives, mixed into `p` (0, the default, leaves `p` as the model's
#'   choice rule gives it).
#' @param block For a multi-block condition, which block's responses to
#'   return (`m` should then be that block's matrix, e.g.
#'   `xsl_run(mod, data)$fits[[1]]$blocks[[block]]$matrix`).
#'
#' @return A tibble with one row per test item: `item`, `correct` (the
#'   correct object's label), and list columns `alternatives` (object
#'   labels), `p` (choice probabilities over `alternatives`) and `counts`
#'   (participants choosing each alternative; `NULL` entries if `data` has no
#'   `response_matrix`).
#' @export
#'
#' @examples
#' dat <- xsl_datasets[[1]]
#' fit <- xsl_run(uncfam(X = .1, B = 3, C = .97), dat)$fits[[1]]
#' ch <- choice_probabilities(fit$matrix, dat)
#' ch$p[[1]]       # the model's choice distribution for item 1
#' ch$counts[[1]]  # what participants chose
choice_probabilities <- function(m, data, d = NULL, lapse = 0, block = NULL, phi = NULL,
                                 rule = c("luce", "remember_guess"), mu = 0) {
  rule <- match.arg(rule)
  stopifnot("xslData" %in% class(data), lapse >= 0, lapse <= 1)
  if (!is.null(phi)) m <- softmax_weights(m, phi)
  if (n_blocks(data) > 1) {
    if (is.null(block)) stop("`data` has ", n_blocks(data), " blocks; say which with `block`", call. = FALSE)
    data <- data_block(data, block)
  }
  ch <- item_choices(m, data, d, rule, mu)
  counts <- item_counts(data, ch$alternatives)
  tibble::tibble(item = seq_along(ch$correct), correct = ch$correct,
                 alternatives = ch$alternatives,
                 p = lapply(ch$probs, with_lapse, lapse = lapse),
                 counts = if (is.null(counts)) vector("list", length(ch$correct)) else counts)
}
