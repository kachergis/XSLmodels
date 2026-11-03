#' xslControl S3 class
#'
#' Control arguments for `xsl_run()`
#'
#' @name xslControl-class
#' @rdname xslControl-class
#'
#' @param reps Number of times to repeat training
#' @param start_matrix Starting matrix
#' @param test_noise Test noise
#' @param n_sim Number of simulations for stochastic models
#' @param verbose Verbosity
#' @param keep_traj Whether each model run should record its per-trial
#'   association matrix (`xslFit$traj`). Off by default: the trajectory is not
#'   used by any function in the package, and for a long corpus it is a serious
#'   memory cost (one word-by-object matrix per trial, per simulation). Set
#'   `TRUE` only when you want to inspect the learning trajectory yourself.
#' @param keep_sims Whether `xsl_run()` should retain the full list of
#'   per-simulation `xslFit` objects (`fits[[i]]$sims`). Off by default;
#'   `fits[[i]]$responses` (an `n_sim` x n-words matrix of each simulated
#'   participant's final per-word accuracy) is always returned instead, at a
#'   fraction of the memory.
#' @param objective What [xsl_fit()] minimizes, and what [xsl_run()] scores:
#'   - `"sse"` (the default): the `n_subj`-weighted sum of squared errors
#'     between model and human accuracy per test item.
#'   - `"binomial"`: the negative log-likelihood of how many participants got
#'     each test item right, given the model's probability of a correct
#'     choice. Counts come from the condition's `response_matrix`, or
#'     otherwise from `accuracy` and `n_subj`.
#'   - `"multinomial"`: the negative log-likelihood of which object each
#'     participant chose on each test item, given the model's full choice
#'     distribution over the item's alternatives (see
#'     [choice_probabilities()]). Needs a `response_matrix`.
#'
#'   The likelihoods treat every response as an independent draw from the
#'   model's prediction, ignoring individual differences, so they overstate
#'   the evidence for one model over another; rankings are more trustworthy
#'   than the absolute size of AIC or likelihood-ratio differences.
#' @param lapse For the likelihood objectives: the probability that a
#'   response is a uniform guess among the item's alternatives rather than the
#'   model's choice. Keeps the likelihood finite when a model is certain and a
#'   participant chooses otherwise. Fit it by naming `"lapse"` in
#'   [xsl_fit()]'s `free`. Ignored by `"sse"`.
#'
#' @return An object of class xslControl
#' @export
xslControl <- function(reps = 1, start_matrix = NULL, test_noise = 0,
                       n_sim = 500, verbose = FALSE,
                       keep_traj = FALSE, keep_sims = FALSE,
                       objective = c("sse", "binomial", "multinomial"),
                       lapse = .01) {
  objective <- match.arg(objective)
  validate_xslControl(
    new_xslControl(list(reps = reps, start_matrix = start_matrix,
                        test_noise = test_noise, n_sim = n_sim,
                        verbose = verbose, keep_traj = keep_traj,
                        keep_sims = keep_sims, objective = objective,
                        lapse = lapse))
  )

}


validate_xslControl <- function(x) {
  stopifnot(all(names(x) %in%
                  c("reps", "start_matrix", "test_noise", "n_sim", "verbose",
                    "keep_traj", "keep_sims", "objective", "lapse")))

  stopifnot(typeof(x$reps) %in% c("double", "integer"))
  stopifnot(round(x$reps) == x$reps)
  stopifnot(x$reps > 0)

  stopifnot(is.null(x$start_matrix) || "matrix" %in% class(x$start_matrix))

  stopifnot(typeof(x$test_noise) %in% c("double", "integer"))
  stopifnot(x$test_noise >= 0 && x$test_noise <= 1)

  stopifnot(typeof(x$n_sim) %in% c("double", "integer"))
  stopifnot(round(x$n_sim) == x$n_sim)
  stopifnot(x$n_sim > 0)

  stopifnot(typeof(x$verbose) == "logical")
  stopifnot(typeof(x$keep_traj) == "logical")
  stopifnot(typeof(x$keep_sims) == "logical")

  # absent in an xslControl built by an older version
  stopifnot(is.null(x$objective) || x$objective %in% c("sse", "binomial", "multinomial"))
  stopifnot(is.null(x$lapse) || (is.numeric(x$lapse) && x$lapse >= 0 && x$lapse <= 1))

  x
}

#' Constructor for xslControl S3 class
#'
#' @rdname xslControl-class
#' @param x List with elements train, test, accuracy, n_subj, label, condition
#'
#' @export
new_xslControl <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("xslControl", "list"))
}
