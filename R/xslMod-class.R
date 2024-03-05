#' xslMod S3 class
#'
#' @name xslMod-class
#' @rdname xslMod-class
#'
#' @param name Name
#' @param description Description
#' @param model Model fitting function
#' @param params List of parameters
#' @param stochastic Logical indicating whether model is stochastic
#'
#' @return An object of class xslMod
#' @export
xslMod <- function(name = character(), description = character(),
                   model, params = numeric(), stochastic = logical()) {
  new_xslMod(list(name = name, description = description,
                  model = model, params = params, stochastic = stochastic))
}

#' Constructor for xslMod S3 class
#'
#' @rdname xslMod-class
#'
#' @export
new_xslMod <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("xslMod", "list"))
}

#' Update parameters of xslMod
#'
#' @param xsl_mod Object of class xslMod
#' @param params Named list of parameters
#'
#' @return An object of class xslMod
#' @export
update_params <- function(xsl_mod, params) {
  stopifnot(length(xsl_mod$params) == length(params))
  stopifnot(all(names(xsl_mod$params) == names(params)))

  names(params) <- names(xsl_mod$params)
  xsl_mod$params <- params
  xsl_mod
}
