#' Run XSL model
#'
#' @param model An object of class xslMod.
#' @param data An object (or list of objects) of class xslData.
#' @param control Control arguments returned by `xsl_control()`.
#'
#' @return List of fits, sse, unweighted_sse
#' @export
xsl_run <- function(model, data, control = xsl_control()) {
  stopifnot("xslMod" %in% class(model))
  if ("xslData" %in% class(data)) data <- list(data)
  stopifnot(all(map_lgl(data, \(d) "xslData" %in% class(d))))

  model_fun <- model$model
  model_params <- model$params

  if (!model$stochastic) n_sim <- 1
  fits <- map(data, function(dat) {
    sims <- map(1:n_sim, \(i) model_fun(params = model_params,
                                        data = dat$train,
                                        control = control))
    mat <- reduce(transpose(sims)$matrix, `+`)
    perf <- if (!is.null(dat$test)) mafc_test(mat, dat$test) else get_perf(mat)
    sse <- sum((perf - dat$accuracy) ^ 2)
    list(sims = sims, perf = perf, matrix = mat, sse = sse)
  })

  sse_terms <- unlist(transpose(fits)$sse)
  unweighted_sse <- sum(sse_terms)
  subj <- unlist(transpose(data)$n_subj)
  sse <- sum(sse_terms * subj) / sum(subj)

  list(fits = fits, sse = sse, unweighted_sse = unweighted_sse)
}

#' Control arguments for `xsl_run()`
#'
#' @param reps Number of times to repeat training
#' @param start_matrix Starting matrix
#' @param test_noise Test noise
#' @param n_sim Number of simulations for stochastic models
#'
#' @return List of arguments
#' @export
xsl_control <- function(reps = 1, start_matrix = c(),
                        test_noise = 0, n_sim = 500) {
  list(reps = reps, start_matrix = start_matrix,
       test_noise = test_noise, n_sim = n_sim)
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
                    control = xsl_control(),
                    deoptim_control = DEoptim::DEoptim.control(reltol = .001,
                                                               NP = 100,
                                                               itermax = 100)) {
  stopifnot("xslMod" %in% class(model))
  if ("xslData" %in% class(data)) data <- list(data)
  stopifnot(all(map_lgl(data, \(d) "xslData" %in% class(d))))

  if (by_data) data_wrap <- data else data_wrap <- list(data)
  map(data_wrap, function(dat) {
    run_wrapper <- \(params) xsl_run(model = update_params(model, params),
                                     data = dat, control = control)$sse
    DEoptim::DEoptim(run_wrapper, lower = lower, upper = upper, deoptim_control)
  })
}
