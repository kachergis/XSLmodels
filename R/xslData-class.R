#' xslData S3 class
#'
#' @name xslData-class
#' @rdname xslData-class
#'
#' @param train Training data
#' @param test Test data
#' @param accuracy Proportion correct per test item: per test trial (in
#'   `test` order) if `test` is given, otherwise per word. For a design with
#'   several training blocks, each followed by the same test, a matrix with
#'   one row per block (see Details)
#' @param n_subj Number of subjects
#' @param label Label
#' @param condition Condition
#' @param description Description
#' @param response_matrix Optional test-response (confusion) matrix: for each
#'   test item (rows, in `accuracy` order) the number of participants choosing
#'   each object (columns, one per object in `train`, in sorted label order).
#'   For a multi-block design, a list with one such matrix per block
#'
#' @details
#' **Multi-block designs.** Some experiments repeat the same training sequence
#' several times, testing after each repetition (a learning trajectory). Give
#' `train` as one block and `accuracy` as a matrix with one row per block; the
#' number of rows is the number of blocks. [xsl_run()] then scores block `b`
#' on a model trained on `b` repetitions of `train`.
#'
#' @return An object of class xslData
#' @export
xslData <- function(train = list(), test = list(), accuracy = numeric(),
                    n_subj = numeric(), label = "", condition = "",
                    description = "", response_matrix = NULL) {
  validate_xslData(
    new_xslData(list(train = train, test = test, accuracy = accuracy,
                     n_subj = n_subj, label = label, condition = condition,
                     description = description, response_matrix = response_matrix))
  )

}

validate_xslData <- function(x) {
  stopifnot(!is.null(names(x)))
  stopifnot(all(names(x) %in% c("train", "test", "accuracy", "n_subj", "label",
                                "condition", "description", "response_matrix")))
  stopifnot(!is.null(names(x$train)))
  stopifnot(all(names(x$train) %in% c("words", "objects")))
  stopifnot(length(x$test) == 0 |
              (!is.null(names(x$test)) & all(names(x$test) %in% c("words", "objects", "correct"))))

  stopifnot(length(x$train$words) == length(x$train$objects))
  stopifnot(length(x$test) == 0 | length(x$test$words) == length(x$test$objects))
  stopifnot(is.null(x$test$correct) || length(x$test$correct) == length(x$test$words))
  # testing a single word per trial
  #for(i in x$test$words) stopifnot(length(i) == 1)
  # one accuracy (and one response_matrix row) per test item: per test trial
  # when a test is specified, otherwise per word
  n_items <- if (length(x$test) > 0) length(x$test$words) else length(unique(unlist(x$train$words)))
  # multi-block: one accuracy row (and one response_matrix) per block
  blocks <- n_blocks(x)
  if (is.matrix(x$accuracy)) {
    stopifnot(ncol(x$accuracy) == n_items)
  } else {
    stopifnot(length(x$accuracy) == 0 | length(x$accuracy) == n_items)
  }
  rms <- if (is.matrix(x$response_matrix)) list(x$response_matrix) else x$response_matrix
  stopifnot(is.null(rms) || length(rms) == blocks)
  for (rm in rms) {
    stopifnot(nrow(rm) == n_items,
              ncol(rm) == length(unique(unlist(x$train$objects))),
              all(rm >= 0))
  }
  x
}

# Number of training/test blocks: the rows of a multi-block accuracy matrix.
n_blocks <- function(data) if (is.matrix(data$accuracy)) nrow(data$accuracy) else 1L

# Block b of a multi-block condition, as an ordinary single-block one:
# training repeated b times, and that block's accuracy and responses.
data_block <- function(data, b) {
  data$train <- list(words = rep(data$train$words, b), objects = rep(data$train$objects, b))
  data$accuracy <- data$accuracy[b, ]
  if (is.list(data$response_matrix)) data$response_matrix <- data$response_matrix[[b]]
  data
}

#' Constructor for xslData S3 class
#'
#' @rdname xslData-class
#' @param x List with elements train, test, accuracy, n_subj, label, condition, description, response_matrix
#'
#' @export
new_xslData <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("xslData", "list"))
}

#' @export
print.xslData <- function(x, ...) {
  cat(sprintf('xslData object with label "%s" and condition "%s"\n',
              x$label, x$condition))
  cat(sprintf("  training trials: %s\n", length(x$train$words)))
  cat(sprintf("      test trials: %s\n", length(x$test$words)))
  cat(sprintf("            words: %s\n", length(unique(unlist(x$train$words)))))
  cat(sprintf("          objects: %s\n", length(unique(unlist(x$train$objects)))))
  if (n_blocks(x) > 1) cat(sprintf("           blocks: %s (training repeated, tested after each)\n", n_blocks(x)))
  cat(sprintf("       accuracies: %s\n", length(x$accuracy)))
  cat(sprintf("         subjects: %s\n", x$n_subj))
}
