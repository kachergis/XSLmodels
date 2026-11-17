# Yurovsky & Frank (2015) integrated model of cross-situational learning, and
# its two special cases (statistical accumulation and single-referent tracking).
#
# Port of github.com/dyurovsky/XSIT-MIN (models/*.stan.R and
# models/model.helpers.R). The original fit the models in Stan and took
# posterior means; here they are fit by maximum likelihood, which gives
# essentially the same estimates (flat priors, ~2400 binomial observations).
#
# Unlike the package's other models, this one is not an `xslMod`: it is a
# closed-form model of one paradigm, whose test trials depend on each
# participant's own guess on the exposure trial (Same / Switch) and come at a
# fixed interval after it, which `xslData` can't express. It predicts
# condition-level choice probabilities for the `yurovsky_frank2015` data.
#
# George Kachergis + Claude, November 2026

yf2015_model_names <- c("integrated", "accumulation", "single_referent")

yf2015_param_names <- function(model) {
  if (model == "integrated") c("sigma", "lambda", "gamma") else c("lambda", "gamma")
}

# Named numeric vector of the model's parameters, from a list or vector.
yf2015_params <- function(params, model) {
  nm <- yf2015_param_names(model)
  missing <- setdiff(nm, names(params))
  if (length(missing)) {
    stop("`params` needs ", paste(nm, collapse = ", "), " for the ", model,
         " model; missing ", paste(missing, collapse = ", "), call. = FALSE)
  }
  unlist(params[nm])
}

# Memory strength M of the test trial's repeated referent (Eq. 4 of the paper),
# and the probability of choosing it.
yf2015_strength <- function(p, model, n_referents, interval, trial_type, new_label_scale) {
  sigma <- switch(model,
                  integrated = rep(p[["sigma"]], length(n_referents)),
                  accumulation = 1 / n_referents,
                  single_referent = rep(1, length(n_referents)))
  decay <- p[["gamma"]] * interval^(-p[["lambda"]])
  other <- ifelse(n_referents > 1, (1 - sigma) / (n_referents - 1), 0) * decay
  trial_type <- as.character(trial_type)
  ifelse(trial_type == "Same", sigma * decay,
         ifelse(trial_type == "Switch", other, new_label_scale * other))
}

yf2015_choice <- function(strength, n_referents, trial_type) {
  ifelse(as.character(trial_type) == "New Label",
         (1 - strength) / n_referents - strength,
         strength + (1 - strength) / n_referents)
}

check_trial_types <- function(trial_type) {
  bad <- setdiff(unique(as.character(trial_type)), c("Same", "Switch", "New Label"))
  if (length(bad)) {
    stop("unknown trial_type(s): ", paste(bad, collapse = ", "),
         " (expected Same, Switch or New Label)", call. = FALSE)
  }
}

#' Predictions of Yurovsky & Frank's (2015) models
#'
#' The probability that a learner chooses the repeated referent on a test
#' trial of Yurovsky & Frank's (2015) paradigm (see [yurovsky_frank2015]),
#' under the paper's Integrated model or either of its special cases.
#'
#' On a word's exposure trial the learner attends to its hypothesized referent
#' (the object it chose) with weight \eqn{\sigma} and to each of the other
#' \eqn{N - 1} objects with weight \eqn{(1 - \sigma) / (N - 1)}, where \eqn{N}
#' is the number of referents. Memory for a word-object pairing decays as a
#' power function of the interval \eqn{t} to the test trial, so the repeated
#' referent's memory strength is
#' \deqn{M = \gamma \, w \, t^{-\lambda},}
#' where \eqn{w} is its attention weight: \eqn{\sigma} on Same trials,
#' \eqn{(1 - \sigma) / (N - 1)} on Switch trials. The learner retrieves the
#' remembered referent with probability \eqn{M} and otherwise guesses among
#' the \eqn{N} objects on the test trial, so
#' \deqn{P(\mathrm{repeated}) = M + (1 - M) / N.}
#'
#' - `"integrated"` has free \eqn{\sigma}, \eqn{\lambda} and \eqn{\gamma}.
#' - `"accumulation"` (statistical accumulation) attends to all objects
#'   equally, \eqn{\sigma = 1 / N}.
#' - `"single_referent"` (single-referent tracking) attends only to its
#'   hypothesis, \eqn{\sigma = 1}, so it is at chance on Switch trials.
#'
#' **New Label trials** (Experiment 2) are not described in the paper. The
#' original code models them by scaling the Switch-trial strength by 1/4
#' (`new_label_scale`) and subtracting it from chance:
#' \eqn{P = (1 - M) / N - M}, so that learners avoid an object they
#' associate with another word. Experiment 2 is only predicted, not fit, in the
#' paper, so this choice affects its reported E1+2 \eqn{r^2} but no parameter
#' estimate.
#'
#' `M` must stay within \[0, 1\] (and so must the New Label probability); the
#' original Stan models enforced this as a constraint. `yf2015_predict()`
#' returns whatever the formulas give; [yf2015_fit()] keeps to the valid region.
#'
#' @param params A named list or vector: `sigma`, `lambda` and `gamma` for the
#'   integrated model, `lambda` and `gamma` for the others (extra elements are
#'   ignored).
#' @param conditions A data frame with columns `n_referents`, `interval` (trials
#'   from exposure to test, 1 = back-to-back) and `trial_type` (`"Same"`,
#'   `"Switch"` or `"New Label"`), e.g. `yurovsky_frank2015$exp12`.
#' @param model `"integrated"`, `"accumulation"` or `"single_referent"`.
#' @param new_label_scale The factor scaling Switch-trial memory strength on New
#'   Label trials (1/4 in the original code).
#'
#' @return `conditions` with added columns `strength` (\eqn{M}) and `p_repeated`
#'   (the probability of choosing the repeated referent).
#' @references Yurovsky, D., & Frank, M. C. (2015). An integrative account of
#'   constraints on cross-situational learning. *Cognition*, 145, 53-62.
#'   \doi{10.1016/j.cognition.2015.07.013}
#' @seealso [yf2015_fit()], [yurovsky_frank2015]
#' @export
#'
#' @examples
#' grid <- expand.grid(n_referents = c(2, 3, 4, 8), interval = c(1, 2, 4, 8),
#'                     trial_type = c("Same", "Switch", "New Label"))
#' pred <- yf2015_predict(list(sigma = .57, lambda = .17, gamma = 1.62), grid)
#' head(pred)
#'
#' # The special cases are the integrated model at sigma = 1/N and sigma = 1
#' all.equal(
#'   yf2015_predict(list(lambda = .2, gamma = 1), grid, "single_referent")$p_repeated,
#'   yf2015_predict(list(sigma = 1, lambda = .2, gamma = 1), grid)$p_repeated
#' )
yf2015_predict <- function(params, conditions, model = c("integrated", "accumulation", "single_referent"),
                           new_label_scale = 1 / 4) {
  model <- match.arg(model)
  p <- yf2015_params(params, model)
  stopifnot(all(c("n_referents", "interval", "trial_type") %in% names(conditions)))
  check_trial_types(conditions$trial_type)
  m <- yf2015_strength(p, model, conditions$n_referents, conditions$interval,
                       conditions$trial_type, new_label_scale)
  conditions$strength <- m
  conditions$p_repeated <- yf2015_choice(m, conditions$n_referents, conditions$trial_type)
  conditions
}

# Per participant and trial type: how many of the test trials chose the
# repeated referent (k of n). Experiments 1 and 2 have 4 of each per
# participant; these are the binomial observations the original Stan models
# were fit to, and the n in its BIC.
yf2015_counts <- function(data) {
  data |>
    dplyr::group_by(.data$experiment, .data$subject, .data$n_referents, .data$interval,
                    .data$trial_type) |>
    dplyr::summarise(k = sum(.data$chose_repeated), n = dplyr::n(), .groups = "drop")
}

# Negative log-likelihood; Inf outside the region where every strength and
# choice probability is a probability.
yf2015_nll <- function(p, model, counts, new_label_scale) {
  m <- yf2015_strength(p, model, counts$n_referents, counts$interval, counts$trial_type,
                       new_label_scale)
  pr <- yf2015_choice(m, counts$n_referents, counts$trial_type)
  if (any(!is.finite(pr)) || any(m < 0 | m > 1) || any(pr <= 0 | pr >= 1)) return(Inf)
  -sum(stats::dbinom(counts$k, counts$n, pr, log = TRUE))
}

# Unconstrained <-> natural parameters: sigma on the logit scale, lambda and
# gamma on the log scale.
yf2015_to_natural <- function(x, model) {
  nm <- yf2015_param_names(model)
  out <- exp(x)
  if (model == "integrated") out[1] <- stats::plogis(x[1])
  stats::setNames(out, nm)
}

#' Fit Yurovsky & Frank's (2015) models by maximum likelihood
#'
#' Fits one of the models of [yf2015_predict()] to Experiments 1 and/or 2 of
#' [yurovsky_frank2015], and scores its predictions for every condition in
#' `data`, fitted or not. The paper fit Experiment 1 and predicted Experiment
#' 2 from those parameters (the default here).
#'
#' The observations are binomial counts: for each participant and trial type,
#' how many of their test trials (4) chose the repeated referent. The
#' log-likelihood includes the binomial coefficients, and BIC uses the number
#' of these counts as its \eqn{n}, as in the paper (2392 for Experiment 1).
#' Parameters are optimized on unbounded scales (logit \eqn{\sigma}, log
#' \eqn{\lambda} and \eqn{\gamma}) by Nelder-Mead from `n_starts` random
#' starts, then polished by BFGS, keeping memory strengths and choice
#' probabilities within \[0, 1\] on the fitted trials.
#'
#' **Comparison with the paper.** The paper's Table 3 reports the mean Stan
#' `lp__` (log posterior up to a constant, which omits the binomial
#' coefficients and adds Jacobian terms) as "Log Likelihood", and its BIC is
#' computed from it, so neither is on this function's scale. In addition, the
#' original fitting code passed Stan the participants' condition indices
#' transposed (the data of the condition with \eqn{i}-th interval and
#' \eqn{j}-th number of referents were scored as the condition with the
#' \eqn{i}-th number of referents and \eqn{j}-th interval), so its parameter
#' estimates and likelihoods are for mislabelled data. Its \eqn{r^2}s, which
#' were computed against correctly labelled data, are nonetheless about the
#' same as those from fitting the correct data (integrated: E1 .95, E1+2 .97
#' vs. .95 and .98). See `data-raw/YurovskyFrank2015/PROVENANCE.md`.
#'
#' Condition means pool Experiments 1 and 2 (their Same trials are the same
#' condition), as in the paper.
#'
#' @param data Test trials in the format of `yurovsky_frank2015$exp12`.
#' @param model `"integrated"`, `"accumulation"` or `"single_referent"`.
#' @param experiments Which experiments in `data` to fit (default 1).
#' @param new_label_scale Passed to [yf2015_predict()].
#' @param n_starts Number of random starting points.
#' @param seed Optional random seed for the starting points.
#'
#' @return A list with
#'   - `model`, `params` (named estimates), `loglik`, `k` (number of
#'     parameters), `n_obs` (number of binomial counts), `aic`, `bic`;
#'   - `conditions`: every Referents x Interval x Trial type condition in
#'     `data`, with `n` (participants), `observed` (proportion choosing the
#'     repeated referent), `predicted`, and `fitted` (whether the condition
#'     contains fitted data);
#'   - `r2`: squared correlation of observed and predicted condition means over
#'     the fitted conditions (`fitted`) and over all of them (`all`).
#' @seealso [yf2015_predict()], [yurovsky_frank2015]
#' @export
#'
#' @examples
#' \donttest{
#' fits <- lapply(c("accumulation", "single_referent", "integrated"),
#'                function(m) yf2015_fit(model = m, seed = 1))
#' sapply(fits, function(f) c(loglik = f$loglik, bic = f$bic, f$r2))
#' fits[[3]]$params
#' }
yf2015_fit <- function(data = XSLmodels::yurovsky_frank2015$exp12,
                       model = c("integrated", "accumulation", "single_referent"),
                       experiments = 1, new_label_scale = 1 / 4, n_starts = 10, seed = NULL) {
  model <- match.arg(model)
  check_trial_types(data$trial_type)
  counts <- yf2015_counts(data)
  fit_counts <- counts[counts$experiment %in% experiments, ]
  if (nrow(fit_counts) == 0) stop("no data from experiment(s) ", paste(experiments, collapse = ", "), call. = FALSE)

  nll <- function(x) yf2015_nll(yf2015_to_natural(x, model), model, fit_counts, new_label_scale)
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
    on.exit(if (is.null(old_seed)) rm(".Random.seed", envir = globalenv()) else
      assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
    set.seed(seed)
  }
  # Random starts with small strengths (always feasible): sigma in (.3, .9),
  # lambda in (.05, 1), gamma in (.1, 1).
  best <- NULL
  for (s in seq_len(n_starts)) {
    x0 <- c(sigma = stats::qlogis(stats::runif(1, .3, .9)),
            lambda = log(stats::runif(1, .05, 1)), gamma = log(stats::runif(1, .1, 1)))
    if (model != "integrated") x0 <- x0[-1]
    if (!is.finite(nll(x0))) next
    o <- stats::optim(x0, nll, method = "Nelder-Mead", control = list(maxit = 5000, reltol = 1e-12))
    o2 <- tryCatch(suppressWarnings(stats::optim(o$par, nll, method = "BFGS",
                                                 control = list(maxit = 1000, reltol = 1e-14))),
                   error = function(e) o)
    if (is.finite(o2$value) && o2$value <= o$value) o <- o2
    if (is.null(best) || o$value < best$value) best <- o
  }
  if (is.null(best)) stop("no feasible starting point found", call. = FALSE)

  params <- yf2015_to_natural(best$par, model)
  loglik <- -best$value
  k <- length(params)
  n_obs <- nrow(fit_counts)

  # Condition means (pooling experiments): mean of participants' proportions.
  conds <- counts |>
    dplyr::group_by(.data$n_referents, .data$interval, .data$trial_type) |>
    dplyr::summarise(observed = mean(.data$k / .data$n), n = dplyr::n(),
                     fitted = any(.data$experiment %in% experiments), .groups = "drop")
  conds$predicted <- yf2015_predict(params, conds, model, new_label_scale)$p_repeated
  fitted_conds <- yf2015_counts(data[data$experiment %in% experiments, ]) |>
    dplyr::group_by(.data$n_referents, .data$interval, .data$trial_type) |>
    dplyr::summarise(observed = mean(.data$k / .data$n), .groups = "drop")
  fitted_conds$predicted <- yf2015_predict(params, fitted_conds, model, new_label_scale)$p_repeated

  list(
    model = model,
    params = params,
    loglik = loglik,
    k = k,
    n_obs = n_obs,
    aic = 2 * k - 2 * loglik,
    bic = k * log(n_obs) - 2 * loglik,
    conditions = conds[, c("n_referents", "interval", "trial_type", "n", "observed", "predicted", "fitted")],
    r2 = c(fitted = stats::cor(fitted_conds$observed, fitted_conds$predicted)^2,
           all = stats::cor(conds$observed, conds$predicted)^2)
  )
}
