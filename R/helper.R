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

  # iterate over training trials
  for (i in seq_along(words)) {
    m[train$words[[i]], train$objects[[i]]] <- m[train$words[[i]], train$objects[[i]]] + 1
  }
  return(m)
}

#' Calculates Shannon entropy of a supplied vector, after normalizing it
#'
#' @param p Numeric vector of probabilities
#' @keywords internal
shannon_entropy <- function(p) {
  if (min(p) < 0 || sum(p) <= 0) return(NA)
  p_norm <- p[p > 0] / sum(p)
  -sum(log2(p_norm) * p_norm)
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
  tr_assocs[which(tr_assocs == 0)] <- startval
  m[tr_w, tr_o] <- tr_assocs
  # for any other experienced word (not on this trial), fill in startval

  fam_objects <- which(colSums(m) > 0)
  fam_words <- which(rowSums(m) > 0)

  for (w in tr_w) {
    zeros <- which(m[w, fam_objects] == 0)
    m[w, zeros] <- startval
  }

  for (o in tr_o) {
    zeros <- which(m[fam_words, o] == 0)
    m[zeros, o] <- startval
  }

  return(m)
}

#' Show available models in the package
#'
#' Returns a list of all available cross-situational word learning models
#' that can be used with the package.
#'
#' @return A character vector of model names
#' @export
#'
#' @examples
#' show_models()
show_models <- function() {
  c("baseline", "decay", "uncfam", "uncfam_sampling", "multi_sampling", 
    "propose_but_verify", "pursuit", "fazly", "guess_and_test", 
    "rescorla_wagner", "tilles", "bayesian_decay")
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
#' Single source of truth for `get_group_model_fit()` and
#' `get_crossvalidated_model_fit()`, so the two never drift out of sync with
#' each model's actual parameter names.
#'
#' @return A named list, one entry per model in `show_models()`, each with
#'   elements `constructor` (a zero-argument function returning an `xslMod`
#'   with starting parameter values), `lower`, and `upper` (DEoptim bounds,
#'   in the same order as the model's parameters).
#' @noRd
xsl_model_registry <- function() {
  list(
    baseline = list(
      constructor = function() baseline(),
      lower = numeric(0), upper = numeric(0)
    ),
    decay = list(
      constructor = function() decay(C = 0.98),
      lower = 0.8, upper = 1.0
    ),
    uncfam = list(
      constructor = function() uncfam(X = 0.1, C = 1, B = 0.98),
      lower = c(0.01, 0.8, 0.8), upper = c(0.5, 1.0, 1.0)
    ),
    uncfam_sampling = list(
      constructor = function() uncfam_sampling(X = 0.1, C = 1, B = 0.98, K = 10),
      lower = c(0.01, 0.8, 0.8, 1), upper = c(0.5, 1.0, 1.0, 20)
    ),
    multi_sampling = list(
      constructor = function() multi_sampling(C = 1, X = 0.1, B = 0.98, K = 10),
      lower = c(0.01, 0.8, 0.8, 1), upper = c(0.5, 1.0, 1.0, 20)
    ),
    propose_but_verify = list(
      constructor = function() propose_but_verify(alpha = 0.1, alpha_increase = 0.5),
      lower = c(0, 0), upper = c(1, 1)
    ),
    pursuit = list(
      constructor = function() pursuit(gamma = 0.2, threshold = 0.3, lambda = 0.05),
      lower = c(0, 0, 0), upper = c(1, 1, 1)
    ),
    fazly = list(
      constructor = function() fazly(lambda = 1e-5, beta = 8500),
      lower = c(1e-6, 1000), upper = c(1e-3, 10000)
    ),
    guess_and_test = list(
      constructor = function() guess_and_test(f = 0.1, sa = 0.5),
      lower = c(0, 0), upper = c(1, 1)
    ),
    rescorla_wagner = list(
      constructor = function() rescorla_wagner(C = 0.98, alpha = 0.1, lambda = 1, beta = 1),
      lower = c(0.8, 0.01, 0.5, 0.5), upper = c(1.0, 0.5, 2.0, 2.0)
    ),
    tilles = list(
      constructor = function() tilles(x = 0.5, b = 0.8, alpha_0 = 0.85),
      lower = c(0, 0, 0), upper = c(1, 1, 1)
    ),
    bayesian_decay = list(
      constructor = function() bayesian_decay(alpha = 0.5, delta = 1, ch_dec = 1),
      lower = c(0.1, 0.5, 0.5), upper = c(0.9, 2.0, 2.0)
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
                                  reltol = .001, NP = 100, itermax = 100)) {
  if (is.null(datasets)) {
    datasets <- xsl_datasets
  }

  entry <- get_model_registry_entry(model_name)
  model <- entry$constructor()

  result <- xsl_fit(model, datasets, lower = entry$lower, upper = entry$upper,
                    control = control, deoptim_control = deoptim_control)

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
                                           reltol = .001, NP = 100, itermax = 100)) {
  if (is.null(datasets)) {
    datasets <- xsl_datasets
  }

  entry <- get_model_registry_entry(model_name)
  model <- entry$constructor()

  result <- cross_validated_group_fits(model, datasets, lower = entry$lower,
                                       upper = entry$upper, n_folds = n_folds,
                                       control = control,
                                       deoptim_control = deoptim_control)

  list(
    model_name = model_name,
    n_folds = n_folds,
    datasets_used = length(datasets),
    cv_result = result
  )
}
