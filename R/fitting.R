# xsl_run <- function(model, data, ...) {
#   UseMethod("xsl_run")
# }

#' Run XSL model
#'
#' @param model An object of class xslMod.
#' @param data An object of class xslData.
#' @param reps Number of times to repeat training (defaults to 1).
#' @param sse_only Logical; if TRUE, only the SSE will be returned.
#' @param print_perf Logical; if TRUE, model performance and SSE will be
#'   printed.
#'
#' @return TODO
#' @export

#' @export
xsl_run <- function(model, data, reps = 1, verbose = FALSE) {
  stopifnot("xslMod" %in% class(model))
  if ("xslData" %in% class(data)) data <- list(data)
  stopifnot(all(map_lgl(data, \(d) "xslData" %in% class(d))))

  model_fun <- model$model
  model_params <- model$params

  fits <- map(data, \(d) model_fun(model_params, d$train, reps = reps)) # TODO train vs test
  sse_terms <- map2_dbl(data, fits, \(d, f) sum(f$perf - d$accuracy) ^ 2)
  unweighted_sse <- sum(sse_terms)
  subj <- map_int(data, \(d) d$n_subj)
  sse <- sum(sse_terms * subj) / sum(subj)

  list(fits = fits, sse = sse, unweighted_sse = unweighted_sse)
}

# xsl_fit <- function(model, data, ...) {
#   UseMethod("xsl_fit")
# }

#' Fit Model to Conditions Using Differential Evolution
#'
#' This function fits a model to provided conditions using the Differential
#' Evolution optimization algorithm. It optimizes the model parameters to
#' minimize the sum of squared errors (SSE) between the model's predictions and
#' human accuracy data.
#'
#' @inheritParams xsl_run
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#'
#' @return An object of class `DEoptim` representing the fitting result, which
#'   includes the best set of parameters found and the corresponding SSE value.
#' @export
xsl_fit <- function(model, data, lower, upper) {
  stopifnot("xslMod" %in% class(model))
  stopifnot("xslData" %in% class(data))

  run_wrapper <- \(params) xsl_run(update_params(model, params), data)$sse
  DEoptim::DEoptim(run_wrapper, lower = lower, upper = upper,
                   DEoptim::DEoptim.control(reltol = .001, NP = 100, itermax = 100))
}
