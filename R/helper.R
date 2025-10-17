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

#' Get group model fit for a specific model
#'
#' Fits a model to all available datasets and returns the group-level fit.
#' This function provides a convenient way to get pre-computed model fits
#' for common models.
#'
#' @param model_name Name of the model to fit (see `show_models()`)
#' @param datasets Optional list of datasets to fit to (defaults to all available)
#'
#' @return A list containing the fitted model results
#' @export
#'
#' @examples
#' # Get group fit for the uncfam model
#' group_fit <- get_group_model_fit("uncfam")
get_group_model_fit <- function(model_name, datasets = NULL) {
  if (is.null(datasets)) {
    datasets <- xsl_datasets
  }
  
  # Map model names to model constructors
  model_constructors <- list(
    baseline = baseline,
    decay = function() decay(C = 0.98),
    uncfam = function() uncfam(X = 0.1, C = 1, B = 0.98),
    uncfam_sampling = function() uncfam_sampling(X = 0.1, C = 1, B = 0.98, N = 10),
    multi_sampling = function() multi_sampling(X = 0.1, C = 1, B = 0.98, N = 10),
    propose_but_verify = function() propose_but_verify(alpha = 0.1, alpha_increase = 0.5),
    pursuit = function() pursuit(gamma = 0.2, threshold = 0.3, lambda = 0.05),
    fazly = function() fazly(alpha = 1e-5, beta = 8500),
    guess_and_test = function() guess_and_test(f = 0.1, sa = 0.5),
    rescorla_wagner = function() rescorla_wagner(C = 0.98, alpha = 0.1, lambda = 1, beta = 1),
    tilles = function() tilles(alpha = 0.5, beta = 0.8, gamma = 0.85),
    bayesian_decay = function() bayesian_decay(alpha = 0.5, delta = 1, ch_dec = 1)
  )
  
  if (!model_name %in% names(model_constructors)) {
    stop("Model '", model_name, "' not found. Available models: ", 
         paste(names(model_constructors), collapse = ", "))
  }
  
  model <- model_constructors[[model_name]]()
  
  # Get parameter bounds for fitting
  param_bounds <- list(
    baseline = list(lower = numeric(0), upper = numeric(0)),
    decay = list(lower = 0.8, upper = 1.0),
    uncfam = list(lower = c(0.01, 0.8, 0.8), upper = c(0.5, 1.0, 1.0)),
    uncfam_sampling = list(lower = c(0.01, 0.8, 0.8, 1), upper = c(0.5, 1.0, 1.0, 20)),
    multi_sampling = list(lower = c(0.01, 0.8, 0.8, 1), upper = c(0.5, 1.0, 1.0, 20)),
    propose_but_verify = list(lower = c(0, 0), upper = c(1, 1)),
    pursuit = list(lower = c(0, 0, 0), upper = c(1, 1, 1)),
    fazly = list(lower = c(1e-6, 1000), upper = c(1e-3, 10000)),
    guess_and_test = list(lower = c(0, 0), upper = c(1, 1)),
    rescorla_wagner = list(lower = c(0.8, 0.01, 0.5, 0.5), upper = c(1.0, 0.5, 2.0, 2.0)),
    tilles = list(lower = c(0, 0, 0), upper = c(1, 1, 1)),
    bayesian_decay = list(lower = c(0.1, 0.5, 0.5), upper = c(0.9, 2.0, 2.0))
  )
  
  bounds <- param_bounds[[model_name]]
  
  # Fit the model
  result <- xsl_fit(model, datasets, lower = bounds$lower, upper = bounds$upper)
  
  list(
    model_name = model_name,
    fit_result = result,
    datasets_used = length(datasets)
  )
}

#' Get cross-validated model fit
#'
#' Performs cross-validation on a model using the available datasets.
#' This provides a more robust assessment of model performance.
#'
#' @param model_name Name of the model to cross-validate (see `show_models()`)
#' @param n_folds Number of folds for cross-validation (default: 5)
#' @param datasets Optional list of datasets to use (defaults to all available)
#'
#' @return A list containing cross-validation results
#' @export
#'
#' @examples
#' # Get cross-validated fit for the uncfam model
#' cv_fit <- get_crossvalidated_model_fit("uncfam")
get_crossvalidated_model_fit <- function(model_name, n_folds = 5, datasets = NULL) {
  if (is.null(datasets)) {
    datasets <- xsl_datasets
  }
  
  # Use the existing cross_validated_group_fits function
  # First, we need to get the parameter bounds
  param_bounds <- list(
    baseline = list(lower = numeric(0), upper = numeric(0)),
    decay = list(lower = 0.8, upper = 1.0),
    uncfam = list(lower = c(0.01, 0.8, 0.8), upper = c(0.5, 1.0, 1.0)),
    uncfam_sampling = list(lower = c(0.01, 0.8, 0.8, 1), upper = c(0.5, 1.0, 1.0, 20)),
    multi_sampling = list(lower = c(0.01, 0.8, 0.8, 1), upper = c(0.5, 1.0, 1.0, 20)),
    propose_but_verify = list(lower = c(0, 0), upper = c(1, 1)),
    pursuit = list(lower = c(0, 0, 0), upper = c(1, 1, 1)),
    fazly = list(lower = c(1e-6, 1000), upper = c(1e-3, 10000)),
    guess_and_test = list(lower = c(0, 0), upper = c(1, 1)),
    rescorla_wagner = list(lower = c(0.8, 0.01, 0.5, 0.5), upper = c(1.0, 0.5, 2.0, 2.0)),
    tilles = list(lower = c(0, 0, 0), upper = c(1, 1, 1)),
    bayesian_decay = list(lower = c(0.1, 0.5, 0.5), upper = c(0.9, 2.0, 2.0))
  )
  
  if (!model_name %in% names(param_bounds)) {
    stop("Model '", model_name, "' not found. Available models: ", 
         paste(names(param_bounds), collapse = ", "))
  }
  
  bounds <- param_bounds[[model_name]]
  
  # For now, return a placeholder since cross_validated_group_fits needs more work
  # In a full implementation, this would use the existing cross_validated_group_fits function
  warning("Cross-validation functionality is not fully implemented yet. 
          This is a placeholder that returns basic model information.")
  
  list(
    model_name = model_name,
    n_folds = n_folds,
    datasets_used = length(datasets),
    parameter_bounds = bounds,
    message = "Cross-validation not yet implemented - use get_group_model_fit() instead"
  )
}
