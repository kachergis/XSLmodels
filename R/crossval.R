#' Split Data into Training and Testing Sets Based on Indices
#'
#' This function divides a given set of conditions into training and testing
#' sets. The division is based on a provided vector of indices, which specify
#' the conditions to be used for testing.
#'
#' @param test_inds A numeric vector of indices indicating which elements in
#'   `conds` should be used for the test set.
#' @param conds A list of conditions, where each condition is represented as a
#'   separate element.
#'
#' @return A list with two elements: `train` and `test`. Each element is a list
#'   of conditions, where `train` includes the conditions not indexed by
#'   `test_inds`, and `test` includes the conditions indexed by `test_inds`.
#' @export
get_train_test_split <- function(test_inds, conds) {}
#   dat = list(train = conds, test = list())
#   for(i in test_inds) {
#     dat$train[[i]] = NULL
#     dat$test[[names(conds)[i]]] = conds[[i]]
#   }
#   return(dat)
# }


#' Perform Cross-Validation on Group Fits of a Model
#'
#' This function runs cross-validation for group fits of a specified model using
#' a set of combined data conditions. It supports both standard and stochastic
#' models and performs fitting for each fold in the cross-validation process.
#' The function records both the training accuracy and the testing results for
#' each fold.
#'
#' @param model_name A string specifying the name of the model to be
#'   cross-validated.
#' @param combined_data A list of combined data conditions used for
#'   cross-validation.
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#'
#' @return A list containing cross-validation results, including fitted
#'   parameters, training accuracy, and a data frame of test results for each
#'   fold.
#' @export
cross_validated_group_fits <- function(model_name, combined_data, lower, upper) {}
#   #model_name = "kachergis"
#   dat = list()
#   test = list()
#   testdf = tidyr::tibble()
#   set.seed(123)
#   folds <- caret::createFolds(names(combined_data), k = 5, list = T)
#   cv_group_fits = list()
#   for(i in 1:length(folds)) {
#     conds = get_train_test_split(folds[[i]], combined_data)
#     if(is.element(model_name, stochastic_models)) {
#       opt = fit_stochastic_model(model_name, conds$train, lower, upper)
#       test[[i]] = run_stochastic_model(conds$test, model_name, opt$optim$bestmem)
#     } else {
#       opt = fit_model(model_name, conds$train, lower, upper)
#       test[[i]] = run_model(conds$test, model_name, opt$optim$bestmem)
#     }
#     dat[["pars"]] = rbind(dat[["pars"]], opt$optim$bestmem)
#     dat[["train_acc"]] = c(dat[["train_acc"]], opt$optim$bestval)
#
#     # add the data frame of test data? much more convenient..
#     tmp = list()
#     tmp[[model_name]] = opt
#     testdf = rbind(testdf, get_model_dataframe(tmp, conds$test, cvdat=test[[i]]))
#     print(paste("fold",i,"train SSE:",round(opt$optim$bestval,3),"test SSE:",round(test[[i]]$SSE,3)))
#   }
#   # add the folds ??
#   dat[["test"]] = test
#   dat[["testdf"]] = testdf # get_cv_test_df(test)
#   return(dat)
# }
