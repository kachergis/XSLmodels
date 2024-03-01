#' Fit XSL model
#'
#' @param model_name An object of class xslMod.
#' @param conditions A list representing the conditions under which the model is
#'   run. Each condition should include training data (`train`) and human item
#'   accuracy (`HumanItemAcc`).
#' @param sse_only Logical; if TRUE, only the SSE will be returned.
#' @param print_perf Logical; if TRUE, model performance and SSE will be
#'   printed.
#'
#' @return TODO
#' @export
xsl_run <- function(model, conds, ...) {
  UseMethod("xsl_run")
}

#' @rdname xsl_run
#' @export
xsl_run.xslMod <- function(model, conds, stochastic = FALSE, sse_only = FALSE, verbose = FALSE) {
  message(paste("Running model", model$name))

  # model <- models[[model_name]]$model # if stochastic_model model_name = paste0("stochastic/",model_name)
  model_fun <- model$model
  model_params <- model$params
  # model$stochastic

  if (!is.null(conds$train)) {
    # mod = list(perf = model(parameters, ord=conds$train)$perf)
    mod <- list(perf = model_fun(model_params, ord=conds$train)$perf)
    SSE <- sum( (mod$perf - conds$HumanItemAcc)^2 )
  } else {
    mod <- list()
    SSE <- 0
    unweighted_SSE <- 0
    totSs <- 0
    for (i in 1:length(conds)) {
      # mp = model(parameters, conds[[i]]$train)
      mp <- model_fun(model_params, conds[[i]]$train)
      if (!is.null(conds[[i]]$test)) {
        mperf <- mafc_test(mp$matrix, conds[[i]]$test)
        mod[[names(conds)[i]]] <- mperf
      } else {
        mperf <- mp$perf
        mod[[names(conds)[i]]] <- mperf
      }
      SSE <- SSE + conds[[i]]$Nsubj * sum( (mperf - conds[[i]]$HumanItemAcc)^2 )
      unweighted_SSE <- unweighted_SSE + sum( (mperf - conds[[i]]$HumanItemAcc)^2 )
      totSs <- totSs + conds[[i]]$Nsubj
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
