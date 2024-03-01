#' Fit XSL model
#'
#' @param model An object of class xslMod.
#' @param data An object of class xslData.
#' @param sse_only Logical; if TRUE, only the SSE will be returned.
#' @param print_perf Logical; if TRUE, model performance and SSE will be
#'   printed.
#'
#' @return TODO
#' @export
xsl_run <- function(model, data, ...) {
  UseMethod("xsl_run")
}

#' @rdname xsl_run
#' @export
xsl_run.xslMod <- function(model, data, reps = 1, sse_only = FALSE, verbose = FALSE) {
  message(paste("Running model", model$name))

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
