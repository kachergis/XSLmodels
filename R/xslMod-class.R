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
#' Replaces `xsl_mod`'s parameter values with `params`, matched by position
#' (not by name) and of the same length -- e.g. a raw numeric vector such as
#' `DEoptim`'s `bestmem`. The result keeps `xsl_mod`'s original parameter
#' names.
#'
#' @param xsl_mod Object of class xslMod
#' @param params Vector or list of parameter values, in the same order as
#'   `xsl_mod$params`
#'
#' @return An object of class xslMod
#' @export
update_params <- function(xsl_mod, params) {
  stopifnot(length(xsl_mod$params) == length(params))

  names(params) <- names(xsl_mod$params)
  xsl_mod$params <- as.list(params)
  xsl_mod
}
