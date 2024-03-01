#' xslMod S3 class
#'
#' @name xslMod-class
#' @rdname xslMod-class
#'
#' @param name Name
#' @param description Description
#' @param model Model fitting function
#' @param params List of parameters
#'
#' @return An object of class xslMod
#' @export
xslMod <- function(name = character(), description = character(),
                   model, params = numeric(), stochastic = logical()) {
  new_xslMod(list(name = name, description = description,
                  model = model, params = params, stochastic = stochastic))
}

#' Constructor for xsl_model S3 class
#'
#' @rdname xslMod-class
#'
#' @export
new_xslMod <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("xslMod", "list"))
}
