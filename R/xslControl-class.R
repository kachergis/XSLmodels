#' xslControl S3 class
#'
#' Control arguments for `xsl_run()`
#'
#' @name xslControl-class
#' @rdname xslControl-class
#'
#' @param reps Number of times to repeat training
#' @param start_matrix Starting matrix
#' @param test_noise Test noise
#' @param n_sim Number of simulations for stochastic models
#' @param verbose Verbosity
#'
#' @return An object of class xslControl
#' @export
xslControl <- function(reps = 1, start_matrix = NULL, test_noise = 0,
                       n_sim = 500, verbose = FALSE) {
  validate_xslControl(
    new_xslControl(list(reps = reps, start_matrix = start_matrix,
                        test_noise = test_noise, n_sim = n_sim,
                        verbose = verbose))
  )

}


validate_xslControl <- function(x) {
  stopifnot(all(names(x) %in%
                  c("reps", "start_matrix", "test_noise", "n_sim", "verbose")))

  stopifnot(typeof(x$reps) %in% c("double", "integer"))
  stopifnot(round(x$reps) == x$reps)
  stopifnot(x$reps > 0)

  stopifnot(is.null(x$start_matrix) || "matrix" %in% class(x$start_matrix))

  stopifnot(typeof(x$test_noise) %in% c("double", "integer"))
  stopifnot(x$test_noise >= 0 && x$test_noise <= 1)

  stopifnot(typeof(x$n_sim) %in% c("double", "integer"))
  stopifnot(round(x$n_sim) == x$n_sim)
  stopifnot(x$n_sim > 0)

  stopifnot(typeof(x$verbose) == "logical")

  x
}

#' Constructor for xslControl S3 class
#'
#' @rdname xslControl-class
#' @param x List with elements train, test, accuracy, n_subj, label, condition
#'
#' @export
new_xslControl <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("xslControl", "list"))
}
