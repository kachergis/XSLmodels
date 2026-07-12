#' Split Data into Training and Testing Sets Based on Indices
#'
#' This function divides a given set of conditions into training and testing
#' sets. The division is based on a provided vector of indices, which specify
#' the conditions to be used for testing.
#'
#' @param test_inds A numeric vector of indices indicating which elements in
#'   `conds` should be used for the test set.
#' @param conds A list of conditions (e.g. `xslData` objects), where each
#'   condition is represented as a separate element.
#'
#' @return A list with two elements: `train` and `test`. Each element is a list
#'   of conditions, where `train` includes the conditions not indexed by
#'   `test_inds`, and `test` includes the conditions indexed by `test_inds`.
#' @export
#'
#' @examples
#' split <- get_train_test_split(c(1, 3), xsl_datasets[1:5])
#' length(split$train)
#' length(split$test)
get_train_test_split <- function(test_inds, conds) {
  list(train = conds[-test_inds], test = conds[test_inds])
}


#' Perform Cross-Validation on Group Fits of a Model
#'
#' This function runs k-fold cross-validation for group fits of a specified
#' model using a set of conditions. On each fold, the model is fit (via
#' `xsl_fit()`) to the training conditions, and the fitted model is then
#' evaluated (via `xsl_run()`) on the held-out test conditions to obtain an
#' out-of-sample SSE.
#'
#' @param model An object of class `xslMod` giving the model to cross-validate
#'   (its starting parameter values are only used to determine how many
#'   parameters `xsl_fit()` should optimize).
#' @param combined_data A list of `xslData` conditions used for
#'   cross-validation.
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#' @param n_folds Number of folds (default: 5).
#' @param control Control arguments passed to `xsl_fit()`/`xsl_run()`.
#' @param seed Optional random seed, for reproducible fold assignment.
#'
#' @return A list containing cross-validation results:
#' - `folds`: a list (one per fold) with the fitted `params`, `train_sse`
#'   (in-sample), and `test_sse` (out-of-sample).
#' - `params`: a matrix of fitted parameters, one row per fold.
#' - `train_sse` / `test_sse`: numeric vectors of per-fold SSE.
#' @export
#'
#' @examples
#' cross_validated_group_fits(decay(C = 0.98), xsl_datasets[1:6],
#'                            lower = 0.8, upper = 1.0, n_folds = 2)
cross_validated_group_fits <- function(model, combined_data, lower, upper,
                                       n_folds = 5, control = xslControl(),
                                       seed = NULL) {
  stopifnot("xslMod" %in% class(model))
  n <- length(combined_data)
  stopifnot(n_folds > 1, n_folds <= n)

  if (!is.null(seed)) set.seed(seed)
  fold_id <- sample(rep(seq_len(n_folds), length.out = n))
  folds <- split(seq_len(n), fold_id)

  fold_results <- map(folds, function(test_inds) {
    split_data <- get_train_test_split(test_inds, combined_data)
    fit <- xsl_fit(model, split_data$train, lower = lower, upper = upper,
                   control = control)[[1]]
    fitted_model <- update_params(model, fit$optim$bestmem)
    test_run <- xsl_run(fitted_model, split_data$test, control = control)
    list(params = fit$optim$bestmem, train_sse = fit$optim$bestval,
         test_sse = test_run$sse)
  })

  list(
    folds = fold_results,
    params = do.call(rbind, map(fold_results, "params")),
    train_sse = unlist(map(fold_results, "train_sse")),
    test_sse = unlist(map(fold_results, "test_sse"))
  )
}
