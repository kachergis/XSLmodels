#' xslFit S3 class
#'
#' @name xslFit-class
#' @rdname xslFit-class
#'
#' @param perf
#' @param matrix
#' @param traj
#'
#' @return An object of class xslFit
#' @export
xslFit <- function(perf = matrix(), matrix = matrix(), traj = list()) {
  new_xslFit(list(perf = perf, matrix = matrix, traj = traj))
}

#' Constructor for xsl_model S3 class
#'
#' @rdname xslMod-class
#'
#' @export
new_xslFit <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("xslFit", "list"))
}
