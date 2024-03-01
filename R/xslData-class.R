#' xslData S3 class
#'
#' @name xslData-class
#' @rdname xslData-class
#'
#' @param train
#' @param test
#' @param accuracy
#' @param n_subj
#' @param label
#'
#' @return An object of class xslMod
#' @export
xslData <- function(train = list(), test = list(), accuracy = numeric(),
                    n_subj = numeric(), label = character(),
                    condition = character()) {
  validate_xslData(
    new_xslData(list(train = train, test = test, accuracy = accuracy,
                     n_subj = n_subj, label = label, condition = condition))
  )

}

validate_xslData <- function(x) {
  stopifnot(all(names(x) %in% c("train", "test", "accuracy", "n_subj", "label", "condition")))
  stopifnot(all(names(x$train) %in% c("words", "objects")))
  stopifnot(is.null(x$test) || all(names(x$test) %in% c("words", "objects")))

  stopifnot(length(x$train$words) == length(x$train$objects))
  stopifnot(is.null(x$test) || length(x$test$words) == length(x$test$objects))
  stopifnot(length(unique(unlist(x$train$words))) == length(x$accuracy))

  x
}

#' Constructor for xsl_model S3 class
#'
#' @rdname xslMod-class
#'
#' @export
new_xslData <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("xslData", "list"))
}

#' @export
print.xslData <- function(x, ...) {
  cat(sprintf("xslData object with label %s and condition %s\n",
              x$label, x$condition))
  cat(sprintf("\t training trials: %s\n", length(x$train$words)))
  cat(sprintf("\t test trials: %s\n", length(x$test$words)))
  cat(sprintf("\t words: %s\n", length(unique(unlist(x$train$words)))))
  cat(sprintf("\t objects: %s\n", length(unique(unlist(x$train$objects)))))
  cat(sprintf("\t accuracies: %s\n", length(x$accuracy)))
  cat(sprintf("\t subjects: %s\n", x$n_subj))
}
