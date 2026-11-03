#' Creates co-occurrence matrix from training trials
#'
#' Given a training order (list of words and objects per trial), this function
#' returns a matrix tallying word-object co-occurrences across the trials. This
#' matrix can be used to analyze the frequency with which certain words and
#' objects appear together during the training phase.
#'
#' @param train A list representing the training data, where each element is a
#'   trial that includes both words and objects. The structure is expected to
#'   have sub-elements `words` and `objects` for each trial.
#'
#' @return A matrix where each element represents the count of co-occurrences
#'   between a word (rows) and an object (columns). The row and column names
#'   correspond to the unique words and objects found in the training data,
#'   respectively.
#' @export
#'
#' @examples
#' create_cooc_matrix(xsl_datasets[[1]]$train)
create_cooc_matrix <- function(train) {
  words <- sort(unique(unlist(train$words)))
  objects <- sort(unique(unlist(train$objects)))
  m <- matrix(0, nrow = length(words), ncol = length(objects),
              dimnames = list(words, objects))

  for (i in seq_along(train$words)) {
    w <- as.character(stats::na.omit(train$words[[i]]))
    o <- as.character(stats::na.omit(train$objects[[i]]))
    m[w, o] <- m[w, o] + 1
  }
  m
}

#' Calculates Shannon entropy of a supplied vector, after normalizing it
#'
#' @param p Numeric vector of probabilities
#' @keywords internal
shannon_entropy <- function(p) {
  if (min(p) < 0) return(NA)
  s <- sum(p)
  if (s <= 0) return(NA)
  p_norm <- p[p > 0] / s
  -sum(log2(p_norm) * p_norm)
}

#' Cosine similarity of two vectors
#'
#' Used by the distributed-memory models ([minerva2()], [todam()]): `a . b /
#' (||a|| ||b||)`. Returns 0 (rather than `NaN`) when either vector has zero
#' norm, since that just means "no signal yet" for these models (e.g. an
#' empty memory trace before any training).
#'
#' @param a,b Numeric vectors of the same length
#' @keywords internal
cos_sim <- function(a, b) {
  denom <- sqrt(sum(a ^ 2)) * sqrt(sum(b ^ 2))
  if (denom == 0) return(0)
  sum(a * b) / denom
}


#' Update known
#'
#' Utility function for several models that fills in a given co-occurrence
#' matrix m with startval, for cells of m that were 0 before
#'
#' @param m Co-occurrence matrix
#' @param tr_w Words
#' @param tr_o Objects
#' @param startval Starting value
#' @keywords internal
update_known <- function(m, tr_w, tr_o, startval = .01) {
  tr_assocs <- m[tr_w, tr_o]
  tr_assocs[tr_assocs == 0] <- startval
  m[tr_w, tr_o] <- tr_assocs
  # for any other experienced word (not on this trial), fill in startval

  fam_objects <- colSums(m) > 0
  fam_words <- rowSums(m) > 0

  for (w in tr_w) {
    zero_fam <- fam_objects & (m[w, ] == 0)
    m[w, zero_fam] <- startval
  }

  for (o in tr_o) {
    zero_fam <- fam_words & (m[, o] == 0)
    m[zero_fam, o] <- startval
  }

  return(m)
}

#' Show available models in the package
#'
#' Returns the names of all available cross-situational word learning
#' models, as accepted by `get_group_model_fit()` and
#' `get_crossvalidated_model_fit()`. Most are the package's model functions;
#' a few are variants of them used in the model comparison paper
#' (`uncfam_novelty`, `uncfam_uncertainty_only`, `uncfam_strength`,
#' `fazly_threshold`).
#'
#' @return A character vector of model names
#' @export
#'
#' @examples
#' show_models()
show_models <- function() {
  names(xsl_model_registry())
}

#' Show available datasets in the package
#'
#' Returns information about the available cross-situational word learning
#' datasets included in the package.
#'
#' @return A data frame with dataset information
#' @export
#'
#' @examples
#' show_datasets()
show_datasets <- function() {
  if (!exists("xsl_datasets")) {
    stop("xsl_datasets not found. Please load the package data.")
  }
  
  dataset_info <- lapply(seq_along(xsl_datasets), function(i) {
    dataset <- xsl_datasets[[i]]
    list(
      index = i,
      label = dataset$label,
      condition = dataset$condition,
      n_trials = length(dataset$train$words),
      n_words = length(unique(unlist(dataset$train$words))),
      n_objects = length(unique(unlist(dataset$train$objects))),
      n_subjects = dataset$n_subj,
      has_test = !is.null(dataset$test)
    )
  })
  
  do.call(rbind, lapply(dataset_info, as.data.frame))
}

#' Registry mapping model names to constructors and DEoptim parameter bounds
#'
#' Single source of truth for `show_models()`, `get_group_model_fit()` and
#' `get_crossvalidated_model_fit()`.
#'
#' Bounds follow the journal paper's v05 fits (xsl-bakeoff `fits/v05/specs.R`)
#' wherever a model appears there, so the package's own group and
#' cross-validated fits search the same space. In particular every
#' uncfam-family model gets X in \[.001, 5\], B in \[.1, 15\] and C in
#' \[.5, 1\]: the older registry's B in \[.8, 1\] excluded the original
#' model's own optimum (B ~ 14 on xsl_datasets).
#'
#' @return A named list, one entry per model, each with
#'   - `constructor`: a zero-argument function returning an `xslMod` with
#'     starting (and any fixed) parameter values;
#'   - `free`: the names of the fitted parameters, in bounds order;
#'   - `lower`, `upper`: DEoptim bounds for `free`;
#'   - optionally `n_restarts`: the default number of DEoptim runs to keep the
#'     best of, for models with unreliable single runs.
#'   Entries other than the package's model functions are paper variants with
#'   a fixed parameter or a non-default option: `uncfam_novelty`,
#'   `uncfam_uncertainty_only`, `uncfam_strength` (familiarity only, B = 0),
#'   and `fazly_threshold`.
#' @noRd
xsl_model_registry <- function() {
  # uncfam family: the paper's bounds for X, B, C
  fu_free  <- c("X", "B", "C")
  fu_lower <- c(.001, .1, .5)
  fu_upper <- c(5, 15, 1)
  list(
    baseline = list(
      constructor = function() baseline(),
      free = character(0), lower = numeric(0), upper = numeric(0)
    ),
    decay = list(
      constructor = function() decay(C = 0.98),
      free = "C", lower = 0.8, upper = 1.0
    ),
    uncfam = list(
      constructor = function() uncfam(X = 0.1, B = 0.98, C = 1),
      free = fu_free, lower = fu_lower, upper = fu_upper
    ),
    uncfam_novelty = list(
      constructor = function() uncfam(X = 0.1, B = 0.98, C = 1, variant = "novelty"),
      free = fu_free, lower = fu_lower, upper = fu_upper
    ),
    uncfam_uncertainty_only = list(
      constructor = function() uncfam(X = 0.1, B = 0.98, C = 1, variant = "uncertainty-only"),
      free = fu_free, lower = fu_lower, upper = fu_upper
    ),
    uncfam_strength = list(
      # familiarity only: uncertainty weighting B fixed at 0
      constructor = function() uncfam(X = 0.1, B = 0, C = 1),
      free = c("X", "C"), lower = c(.001, .1), upper = c(5, 1)
    ),
    uncfam_gamma = list(
      constructor = function() uncfam_gamma(X = 0.1, B = 0.98, C = 1, gamma = 1),
      # gamma's best-fit value is context-dependent (see ?uncfam_gamma), so its
      # bounds are wide in both directions
      free = c(fu_free, "gamma"), lower = c(fu_lower, 0.1), upper = c(fu_upper, 6)
    ),
    uncfam_general = list(
      # fits X, B, C, gamma, eps, kappa with K = Inf (deterministic) and
      # entropy uncertainty; fix any of them to fit a nested special case
      constructor = function() uncfam_general(X = 0.1, B = 0.98, C = 1),
      free = c(fu_free, "gamma", "eps", "kappa"),
      lower = c(fu_lower, 0, 0, 0), upper = c(fu_upper, 6, 20, 2)
    ),
    uncfam_elimination = list(
      constructor = function() uncfam_elimination(X = 0.1, B = 0.98, C = 1, eps = 0),
      free = c(fu_free, "eps"), lower = c(fu_lower, 0), upper = c(fu_upper, 20)
    ),
    uncfam_attention = list(
      constructor = function() uncfam_attention(X = 0.1, B = 0.98, C = 1),
      free = fu_free, lower = fu_lower, upper = fu_upper
    ),
    uncfam_predictive = list(
      constructor = function() uncfam_predictive(X = 0.1, B = 0.98, C = 1),
      # about a third of single DEoptim runs end in a poor basin (SSE ~2.5
      # vs. ~.66 on xsl_datasets), so keep the best of several
      free = fu_free, lower = fu_lower, upper = fu_upper, n_restarts = 5
    ),
    uncfam_sampling = list(
      constructor = function() uncfam_sampling(X = 0.1, B = 0.98, C = 1, K = 10),
      free = c(fu_free, "K"), lower = c(fu_lower, 1), upper = c(fu_upper, 20)
    ),
    multi_sampling = list(
      # parameters are ordered (C, X, B, K) in multi_sampling(); naming them
      # keeps the bounds attached to the right ones
      constructor = function() multi_sampling(C = 1, X = 0.1, B = 0.98, K = 10),
      free = c(fu_free, "K"), lower = c(fu_lower, 1), upper = c(fu_upper, 20)
    ),
    propose_but_verify = list(
      constructor = function() propose_but_verify(alpha = 0.1, alpha_increase = 0.5),
      free = c("alpha", "alpha_increase"), lower = c(1e-4, 1e-4), upper = c(1, 1)
    ),
    pursuit = list(
      constructor = function() pursuit(gamma = 0.2, threshold = 0.3, lambda = 0.05),
      free = c("gamma", "threshold", "lambda"), lower = c(1e-5, 1e-5, 1e-5), upper = c(1, 1, 1)
    ),
    fazly = list(
      constructor = function() fazly(lambda = 1e-5, beta = 8500),
      free = c("lambda", "beta"), lower = c(1e-10, 2), upper = c(2, 20000)
    ),
    fazly_threshold = list(
      constructor = function() fazly(lambda = 0.1, beta = 100, theta = 0.5),
      free = c("lambda", "beta", "theta"), lower = c(.01, 2, .01), upper = c(2, 1000, 1)
    ),
    guess_and_test = list(
      constructor = function() guess_and_test(f = 0.1, sa = 0.5),
      free = c("f", "sa"), lower = c(1e-4, 1e-4), upper = c(1, 1)
    ),
    rescorla_wagner = list(
      # salience alpha fixed at 1: alpha and beta only enter as a product, so
      # fitting both leaves one direction unidentified
      constructor = function() rescorla_wagner(C = 0.98, alpha = 1, beta = 0.1, lambda = 1),
      free = c("beta", "C", "lambda"), lower = c(1e-5, 1e-5, 1e-5), upper = c(1, 1, 1)
    ),
    tilles = list(
      constructor = function() tilles(x = 0.5, b = 0.8, alpha_0 = 0.85),
      free = c("x", "b", "alpha_0"), lower = c(0, 0, 0), upper = c(1, 1, 1), n_restarts = 3
    ),
    bayesian_decay = list(
      constructor = function() bayesian_decay(alpha = 0.5, delta = 1, ch_dec = 1),
      free = c("alpha", "delta", "ch_dec"), lower = c(1e-5, 1e-5, 1e-5), upper = c(10, 10, 10)
    ),
    kalman_filter = list(
      constructor = function() kalman_filter(tau2 = 0.01, sigma2_obs = 1, sigma2_0 = 1),
      # The three parameters share a scale redundancy -- multiplying tau2,
      # sigma2_obs and sigma2_0 together leaves every Kalman gain, and so the
      # learned matrix, unchanged -- and the SSE landscape plateaus in
      # sigma2_obs above a few hundred, so a fit walks sigma2_obs to whatever
      # upper bound it is given without changing the fit quality (the paper's
      # bound is 500; fits at 500, 2000 or 20000 are equally good). Treat a
      # fitted sigma2_obs as "large", not as an estimate.
      free = c("tau2", "sigma2_obs", "sigma2_0"), lower = c(1e-6, 0.01, 0.001), upper = c(1, 2000, 10)
    ),
    softmax_rl = list(
      constructor = function() softmax_rl(alpha = 0.3, beta = 3),
      # a beta upper bound of 20 was railed against; 100 leaves room
      free = c("alpha", "beta"), lower = c(0.01, 0.1), upper = c(1, 100)
    ),
    # fgt2009()/fgt2009_rsa() are BATCH MCMC models: one run is many seconds,
    # so a full DEoptim search over them is impractical, and the lexicon-size
    # prior `alpha` scales with corpus size -- a single global value fit across
    # all of xsl_datasets is not meaningful. These entries fit `alpha` only so
    # get_group_model_fit()/get_crossvalidated_model_fit() don't error on the
    # name, but prefer fgt2009_sweep_alpha() on one dataset.
    fgt2009 = list(
      constructor = function() fgt2009(alpha = 1),
      free = "alpha", lower = 0.1, upper = 20
    ),
    fgt2009_rsa = list(
      constructor = function() fgt2009_rsa(alpha = 1),
      free = "alpha", lower = 0.1, upper = 20
    ),
    # minerva2()/todam()'s `D` (feature-vector dimensionality) and rem()'s `w`
    # (feature count) are fixed representational hyperparameters, not free
    # cognitive parameters, so they aren't fit
    minerva2 = list(
      constructor = function() minerva2(X = 5, D = 100),
      free = "X", lower = 0.1, upper = 50
    ),
    todam = list(
      constructor = function() todam(X = 5, D = 500),
      free = "X", lower = 0.1, upper = 50
    ),
    rem = list(
      constructor = function() rem(g = 0.4, u = 0.3, c = 0.7, w = 12),
      free = c("g", "u", "c"), lower = c(0.05, 0.01, 0.5), upper = c(0.95, 1.0, 0.999)
    )
  )
}

#' Look up a model's constructor and fitting bounds by name, or error with
#' the list of valid names.
#' @noRd
get_model_registry_entry <- function(model_name) {
  registry <- xsl_model_registry()
  if (!model_name %in% names(registry)) {
    stop("Model '", model_name, "' not found. Available models: ",
         paste(names(registry), collapse = ", "))
  }
  registry[[model_name]]
}

#' A registry entry's free parameters and bounds, with the lapse rate added
#' (in \[0, 1\]) when `fit_lapse`.
#' @noRd
registry_free <- function(entry, control, fit_lapse) {
  if (!fit_lapse) return(entry[c("free", "lower", "upper")])
  if ((control$objective %||% "sse") == "sse") {
    stop("`fit_lapse = TRUE` needs a likelihood objective, e.g. ",
         "control = xslControl(objective = \"binomial\")", call. = FALSE)
  }
  list(free = c(entry$free, "lapse"), lower = c(entry$lower, 0), upper = c(entry$upper, 1))
}

#' Get group model fit for a specific model
#'
#' Fits a model to all available datasets and returns the group-level fit.
#' This function provides a convenient way to get pre-computed model fits
#' for common models.
#'
#' @param model_name Name of the model to fit (see `show_models()`)
#' @param datasets Optional list of datasets to fit to (defaults to all available)
#' @param control Control arguments passed to `xsl_run()` (via `xsl_fit()`);
#'   notably `n_sim`, the number of simulations averaged per evaluation of a
#'   stochastic model (default 500 -- lowering this can speed up fitting of
#'   stochastic models substantially).
#' @param deoptim_control Control parameters passed to `DEoptim()` (via
#'   `xsl_fit()`).
#' @param n_restarts Number of DEoptim runs to keep the best of; defaults to
#'   the model's registry default (5 for `uncfam_predictive`, 3 for `tilles`,
#'   otherwise 1).
#' @param seed Optional random seed for reproducible fits.
#' @param fit_lapse With a likelihood objective (`control =
#'   xslControl(objective = "binomial")` or `"multinomial"`), also fit the
#'   lapse rate, in \[0, 1\].
#'
#' @return A list containing the fitted model results
#' @export
#'
#' @examples
#' # Get group fit for the uncfam model. A small dataset subset and a
#' # reduced DEoptim search keep this example fast; drop those arguments
#' # for a real, thorough fit.
#' group_fit <- get_group_model_fit(
#'   "uncfam", datasets = xsl_datasets[1:3],
#'   deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 5))
get_group_model_fit <- function(model_name, datasets = NULL,
                                control = xslControl(),
                                deoptim_control = DEoptim::DEoptim.control(
                                  reltol = .001, NP = 100, itermax = 100),
                                n_restarts = NULL, seed = NULL, fit_lapse = FALSE) {
  if (is.null(datasets)) {
    datasets <- xsl_datasets
  }

  entry <- get_model_registry_entry(model_name)
  model <- entry$constructor()
  fr <- registry_free(entry, control, fit_lapse)

  result <- xsl_fit(model, datasets, lower = fr$lower, upper = fr$upper,
                    control = control, deoptim_control = deoptim_control,
                    free = fr$free, n_restarts = n_restarts %||% entry$n_restarts %||% 1,
                    seed = seed)

  list(
    model_name = model_name,
    fit_result = result,
    datasets_used = length(datasets)
  )
}

#' Get cross-validated model fit
#'
#' Performs k-fold cross-validation on a model using the available datasets:
#' the datasets are split into `n_folds` groups, and for each fold the model
#' is fit (via `xsl_fit()`) to the other folds and evaluated (via
#' `xsl_run()`) on the held-out fold.
#'
#' @param model_name Name of the model to cross-validate (see `show_models()`)
#' @param n_folds Number of folds for cross-validation (default: 5)
#' @param datasets Optional list of datasets to use (defaults to all available)
#' @param control Control arguments passed to `xsl_run()`/`xsl_fit()`;
#'   notably `n_sim`, the number of simulations averaged per evaluation of a
#'   stochastic model (default 500 -- lowering this can speed up
#'   cross-validation of stochastic models substantially).
#' @param deoptim_control Control parameters passed to `DEoptim()` (via
#'   `cross_validated_group_fits()`).
#' @param n_restarts Number of DEoptim runs to keep the best of, per fold;
#'   defaults to the model's registry default (see `get_group_model_fit()`).
#' @param seed Optional random seed for reproducible folds and fits.
#' @inheritParams get_group_model_fit
#'
#' @return A list containing cross-validation results (see
#'   `cross_validated_group_fits()`)
#' @export
#'
#' @examples
#' # Get cross-validated fit for the decay model. A reduced DEoptim search
#' # keeps this example fast; drop deoptim_control for a real, thorough fit.
#' cv_fit <- get_crossvalidated_model_fit(
#'   "decay", n_folds = 2, datasets = xsl_datasets[1:6],
#'   deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 5))
get_crossvalidated_model_fit <- function(model_name, n_folds = 5, datasets = NULL,
                                         control = xslControl(),
                                         deoptim_control = DEoptim::DEoptim.control(
                                           reltol = .001, NP = 100, itermax = 100),
                                         n_restarts = NULL, seed = NULL, fit_lapse = FALSE) {
  if (is.null(datasets)) {
    datasets <- xsl_datasets
  }

  entry <- get_model_registry_entry(model_name)
  model <- entry$constructor()
  fr <- registry_free(entry, control, fit_lapse)

  result <- cross_validated_group_fits(model, datasets, lower = fr$lower,
                                       upper = fr$upper, n_folds = n_folds,
                                       control = control,
                                       deoptim_control = deoptim_control,
                                       seed = seed, free = fr$free,
                                       n_restarts = n_restarts %||% entry$n_restarts %||% 1)

  list(
    model_name = model_name,
    n_folds = n_folds,
    datasets_used = length(datasets),
    cv_result = result
  )
}
