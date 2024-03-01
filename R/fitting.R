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
xsl_run <- function(model, data, reps = 1, sse_only = FALSE, verbose = FALSE) {
  stopifnot("xslMod" %in% class(model))
  stopifnot("xslData" %in% class(data))

  model_fun <- model$model
  model_params <- model$params

  if (!is.null(data$train)) {
    # mod = list(perf = model(parameters, ord=data$train)$perf)
    mod <- list(perf = model_fun(model_params, data = data$train, reps = reps)$perf)
    SSE <- sum( (mod$perf - data$accuracy)^2 )
  } else {
    mod <- list()
    SSE <- 0
    unweighted_SSE <- 0
    totSs <- 0
    for (i in 1:length(data)) {
      # mp = model(parameters, data[[i]]$train)
      mp <- model_fun(model_params, data[[i]]$train)
      if (!is.null(data[[i]]$test)) {
        mperf <- mafc_test(mp$matrix, data[[i]]$test)
        mod[[names(data)[i]]] <- mperf
      } else {
        mperf <- mp$perf
        mod[[names(data)[i]]] <- mperf
      }
      SSE <- SSE + data[[i]]$n_subj * sum( (mperf - data[[i]]$accuracy)^2 )
      unweighted_SSE <- unweighted_SSE + sum( (mperf - data[[i]]$accuracy)^2 )
      totSs <- totSs + data[[i]]$n_subj
    }
    SSE <- SSE / totSs
  }
  if (verbose) {
    message(mod)
    message(paste("SSE:", SSE))
  }
  mod$SSE <- SSE

  if (sse_only) return(SSE)
  return(mod)
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

  run_wrapper <- function(params) {
    xsl_run(update_params(model, params), data, sse_only = TRUE)
  }
  DEoptim::DEoptim(run_wrapper, lower = lower, upper = upper,
                   DEoptim::DEoptim.control(reltol = .001, NP = 100, itermax = 100))
}
