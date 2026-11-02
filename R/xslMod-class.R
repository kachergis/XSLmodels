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
#' @param supports_start_matrix Logical: does `model` initialize from
#'   `control$start_matrix`? `xsl_run()` errors if a start matrix is supplied
#'   to a model that doesn't, rather than letting it be silently ignored.
#'
#' @return An object of class xslMod
#' @export
xslMod <- function(name = character(), description = character(),
                   model, params = numeric(), stochastic = logical(),
                   supports_start_matrix = FALSE) {
  new_xslMod(list(name = name, description = description,
                  model = model, params = params, stochastic = stochastic,
                  supports_start_matrix = supports_start_matrix))
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
#' Replaces some of `xsl_mod`'s parameter values with `params` -- e.g. a raw
#' numeric vector such as `DEoptim`'s `bestmem`. The result keeps `xsl_mod`'s
#' original parameter names.
#'
#' By default `params` replaces the *leading* parameters, matched by position
#' (not by name): `params` may be shorter than `xsl_mod$params`, in which case
#' only the first `length(params)` parameters are replaced. This is what lets
#' `xsl_fit()` optimize a leading subset of a model's parameters (e.g.
#' `uncfam()`'s numeric `X`/`B`/`C` but not its categorical `variant`).
#'
#' With `free`, `params` instead replaces the named parameters, in the order
#' given -- so any subset can be fit while the rest stay fixed (e.g.
#' `rescorla_wagner()`'s `beta`, `C` and `lambda` with `alpha` fixed at 1).
#'
#' @param xsl_mod Object of class xslMod
#' @param params Vector or list of parameter values, in the same order as
#'   the leading elements of `xsl_mod$params`, or as `free`
#' @param free Optional character vector of parameter names that `params`
#'   replaces, in order
#'
#' @return An object of class xslMod
#' @export
#'
#' @examples
#' rw <- rescorla_wagner(C = .98, alpha = 1, beta = .5, lambda = 1)
#' update_params(rw, c(.2, .95, .8), free = c("beta", "C", "lambda"))$params
update_params <- function(xsl_mod, params, free = NULL) {
  n <- length(params)
  if (is.null(free)) {
    stopifnot(n <= length(xsl_mod$params))
    free <- names(xsl_mod$params)[seq_len(n)]
  } else {
    if (length(free) != n) stop("`free` names ", length(free), " parameters but `params` has ", n, call. = FALSE)
    unknown <- setdiff(free, names(xsl_mod$params))
    if (length(unknown)) stop("unknown parameter(s) in `free`: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (n > 0) xsl_mod$params[free] <- unname(as.list(params))
  xsl_mod
}
