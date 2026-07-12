#' Run XSL model
#'
#' @param model An object of class xslMod.
#' @param data An object (or list of objects) of class xslData.
#' @param control Control arguments returned by `xsl_control()`.
#'
#' @return List of fits, sse, unweighted_sse
#' @export
xsl_run <- function(model, data, control = xslControl()) {
  stopifnot("xslMod" %in% class(model))
  if ("xslData" %in% class(data)) data <- list(data)
  stopifnot(all(map_lgl(data, \(d) "xslData" %in% class(d))))

  model_fun <- model$model
  model_params <- model$params

  n_sim <- control$n_sim
  if (!model$stochastic) n_sim <- 1
  fits <- map(data, function(dat) {
    sims <- map(1:n_sim,
                \(i) model_fun(params = model_params, data = dat$train, control = control))
    mat <- reduce(transpose(sims)$matrix, `+`)
    perf <- if (!is.null(dat$test)) mafc_test(mat, dat$test) else get_perf(mat, d = model_params[["ch_dec"]])
    sse <- sum((perf - dat$accuracy) ^ 2)
    list(sims = sims, perf = perf, matrix = mat, sse = sse, data = dat)
  })

  sse_terms <- unlist(transpose(fits)$sse)
  # unweighted_sse <- sum(sse_terms)
  unweighted_sse <- mean(sse_terms)
  # n_subj may be unset (e.g. for get_example_ambiguous_condition() and
  # other datasets without human sample sizes); fall back to the unweighted
  # SSE rather than dividing by zero, and keep subj aligned with sse_terms
  # (a plain unlist() would silently drop and misalign missing n_subj)
  subj <- vapply(data, \(d) if (length(d$n_subj) == 0) NA_real_ else d$n_subj,
                 numeric(1))
  sse <- if (all(is.na(subj)) || sum(subj, na.rm = TRUE) == 0) {
    unweighted_sse
  } else {
    sum(sse_terms * subj, na.rm = TRUE) / sum(subj, na.rm = TRUE)
  }

  list(fits = fits, sse = sse, unweighted_sse = unweighted_sse)
}


#' Fit XSL model using differential evolution
#'
#' Fits a model to provided data using the Differential Evolution optimization
#' algorithm. It optimizes the model parameters to minimize the sum of squared
#' errors (SSE) between the model's predictions and human accuracy data.
#'
#' @inheritParams xsl_run
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#' @param by_data Logical indicating whether to fit to each entry in data
#'   separately.
#' @param control Control parameters passed to `xsl_run()`.
#' @param deoptim_control Control parameters passed to `DEoptim()`.
#'
#' @return An object of class `DEoptim` representing the fitting result, which
#'   includes the best set of parameters found and the corresponding SSE value.
#' @export
xsl_fit <- function(model, data, lower, upper, by_data = FALSE,
                    control = xslControl(),
                    deoptim_control = DEoptim::DEoptim.control(reltol = .001,
                                                               NP = 100,
                                                               itermax = 100)) {
  stopifnot("xslMod" %in% class(model))
  if ("xslData" %in% class(data)) data <- list(data)
  stopifnot(all(map_lgl(data, \(d) "xslData" %in% class(d))))

  if (by_data) data_wrap <- data else data_wrap <- list(data)
  map(data_wrap, function(dat) {
    run_wrapper <- \(params) {
      # some models are numerically unstable for certain parameter draws
      # (e.g. producing NA associations); treat those as an infinitely bad
      # fit rather than letting one bad draw abort the whole optimization
      sse <- tryCatch(
        xsl_run(model = update_params(model, params), data = dat, control = control)$sse,
        error = function(e) NA_real_
      )
      if (is.na(sse)) Inf else sse
    }
    if (length(lower) == 0) {
      # DEoptim segfaults when given zero-length bounds; a model with no
      # free parameters (e.g. baseline()) has nothing to optimize, so just
      # evaluate it once instead
      list(optim = list(bestmem = numeric(0), bestval = run_wrapper(numeric(0))))
    } else {
      DEoptim::DEoptim(run_wrapper, lower = lower, upper = upper, deoptim_control)
    }
  })
}
