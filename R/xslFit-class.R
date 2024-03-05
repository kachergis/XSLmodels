#' xslFit S3 class
#'
#' @name xslFit-class
#' @rdname xslFit-class
#'
#' @param perf Perf
#' @param matrix Matrix
#' @param traj Traj
#' @param sse SSE
#'
#' @return An object of class xslFit
#' @export
xslFit <- function(perf = matrix(), matrix = matrix(), traj = list(), sse = double()) {
  new_xslFit(list(perf = perf, matrix = matrix, traj = traj, sse = sse))
}

#' Constructor for xsl_model S3 class
#'
#' @rdname xslMod-class
#' @param x List with elements perf, matrix, traj, sse
#'
#' @export
new_xslFit <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("xslFit", "list"))
}
