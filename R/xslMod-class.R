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
#' Replaces `xsl_mod`'s leading parameter values with `params`, matched by
#' position (not by name) -- e.g. a raw numeric vector such as `DEoptim`'s
#' `bestmem`. The result keeps `xsl_mod`'s original parameter names.
#'
#' `params` may be shorter than `xsl_mod$params`, in which case only the
#' first `length(params)` parameters are replaced and the rest keep their
#' existing values. This is what lets `xsl_fit()` optimize a leading subset
#' of a model's parameters (e.g. `uncfam()`'s numeric `X`/`B`/`C` but not its
#' categorical `variant`) by giving DEoptim `lower`/`upper` bounds shorter
#' than the model's full parameter list.
#'
#' @param xsl_mod Object of class xslMod
#' @param params Vector or list of parameter values, in the same order as
#'   the leading elements of `xsl_mod$params`
#'
#' @return An object of class xslMod
#' @export
update_params <- function(xsl_mod, params) {
  stopifnot(length(params) <= length(xsl_mod$params))

  n <- length(params)
  if (n > 0) {
    values <- as.list(params)
    names(values) <- names(xsl_mod$params)[seq_len(n)]
    xsl_mod$params[seq_len(n)] <- values
  }
  xsl_mod
}
