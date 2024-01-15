order_dir = "orders/"
model_dir = "models/"
data_dir = "data/"


#load(here::here("data/combined_data.RData"))

# imported from fitting_functions.R

#' Run specified model on provided condition(s)
#'
#' This function runs a specified word learning model on a set of provided conditions. It calculates the model's performance, the sum of squared errors (SSE), and optionally, the conditional probability of selecting the correct referent for each word.
#'
#' @param conds A list representing the conditions under which the model is run. Each condition should include training data (`train`) and human item accuracy (`HumanItemAcc`).
#' @param model_name A string specifying the name of the model to run.
#' @param parameters A list of parameters to be used by the model.
#' @param SSE_only Logical; if TRUE, only the SSE will be returned.
#' @param print_perf Logical; if TRUE, model performance and SSE will be printed.
#'
#' @return A list containing the model's association matrix after training, conditional probability of selecting the correct referent for each word, and the sum of squared error (SSE). If `SSE_only` is TRUE, only the SSE is returned.
#' @export
run_model <- function(conds, model_name, parameters, SSE_only=F, print_perf=F) {
  source(here::here(paste0(model_dir,model_name,".R"))) # sourcing not allowed in packages
  if(!is.null(conds$train)) {
    mod = list(perf = model(parameters, ord=conds$train)$perf)
    SSE = sum( (mod$perf - conds$HumanItemAcc)^2 )
  } else {
    mod = list()
    SSE = 0
    unweighted_SSE = 0
    totSs = 0
    for(i in 1:length(conds)) {
      #print(conds[[i]]$Condition)
      #print(parameters)
      mp = model(parameters, conds[[i]]$train)
      if(!is.null(conds[[i]]$test)) {
        mperf = mafc_test(mp$matrix, conds[[i]]$test)
        mod[[names(conds)[i]]] = mperf
      } else {
        mperf = mp$perf
        mod[[names(conds)[i]]] = mperf
      }
      SSE = SSE + conds[[i]]$Nsubj * sum( (mperf - conds[[i]]$HumanItemAcc)^2 )
      unweighted_SSE = unweighted_SSE + sum( (mperf - conds[[i]]$HumanItemAcc)^2 )
      totSs = totSs + conds[[i]]$Nsubj
    }
    SSE = SSE / totSs
  }
  if(print_perf) {
    print(mod)
    print(paste0("SSE: ",SSE))
    #print(paste0("unweighted SSE: ", unweighted_SSE)) # baseline: 31.84
  }
  mod$SSE = SSE

  if(SSE_only) return(SSE)
  return(mod)
}


#' Fit Model to Conditions Using Differential Evolution
#'
#' This function fits a model to provided conditions using the Differential Evolution optimization algorithm. It optimizes the model parameters to minimize the sum of squared errors (SSE) between the model's predictions and human accuracy data.
#'
#' @param model_name A string specifying the name of the model to be fitted.
#' @param conds A list representing the conditions under which the model is fit.
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#'
#' @return An object of class `DEoptim` representing the fitting result, which includes the best set of parameters found and the corresponding SSE value.
#' @export
fit_model <- function(model_name, conds, lower, upper) {
  fit = DEoptim::DEoptim(run_model, lower=lower, upper=upper,
                         DEoptim::DEoptim.control(reltol=.001, NP=100, itermax=100),
                model_name=model_name, conds=conds, SSE_only=T)
  return(fit)
}


#' Dummy Function for Stochastic Simulation
#'
#' This function serves as a placeholder for running stochastic simulations. It calls the `model` function with provided parameters and a specific order, returning the model's performance.
#'
#' @param n Integer, representing the number of simulations to run.
#' @param parameters A list of parameters to be used by the model.
#' @param ord The order in which the model operates.
#'
#' @return A numeric value representing the model's performance.
#' @export
stochastic_dummy <- function(n, parameters, ord) {
  return(model(parameters, ord)$perf)
}

stochastic_matrix_dummy <- function(n, parameters, ord) {
  return(model(parameters, ord)$matrix)
}

stochastic_traj_dummy <- function(n, parameters, ord) {
  return(model(parameters, ord)$traj)
}


#' Run Stochastic Model on Provided Conditions
#'
#' This function runs a stochastic version of the specified word learning model on a set of provided conditions. It simulates the model multiple times and aggregates the results. The function can return either a response matrix, model performance, or sum of squared errors (SSE).
#'
#' @param conds A list representing the conditions under which the model is run. This could include training and testing data, along with human item accuracy (`HumanItemAcc`).
#' @param model_name A string specifying the name of the stochastic model to run.
#' @param parameters A list of parameters to be used by the stochastic model.
#' @param SSE_only Logical; if TRUE, only the SSE will be returned.
#' @param print_perf Logical; if TRUE, model performance and SSE will be printed.
#' @param get_resp_matrix Logical; if TRUE, the function returns the response matrix instead of the standard performance metrics.
#' @param Nsim Integer; the number of simulations to run for the stochastic model.
#'
#' @return Depending on the parameters, the function can return different types of outputs:
#' - If `get_resp_matrix` is TRUE, it returns a response matrix aggregated over all simulations.
#' - If `SSE_only` is TRUE, it returns only the SSE.
#' - Otherwise, it returns a list containing model performance across conditions and the SSE.
#' @export
run_stochastic_model <- function(conds, model_name, parameters, SSE_only=F, print_perf=F, get_resp_matrix=F, Nsim=500) {
  source(here::here(paste0(model_dir,"stochastic/",model_name,".R")))
  # fitting a single condition
  if(!is.null(conds$train)) {
    if(get_resp_matrix) {
      mp = lapply(1:Nsim, stochastic_matrix_dummy, parameters=parameters,
                  ord=conds$train)
      mod = Reduce('+', mp)
    } else {
      mp = sapply(1:Nsim, stochastic_dummy, parameters=parameters, ord=conds$train)
      mod = get_perf(mp)
      SSE = sum( (mod - conds$HumanItemAcc)^2 )
    }
  } else {
    mod = list()
    SSE = 0
    totSs = 0
    for(i in 1:length(conds)) {
      # just want response matrix, not AFC performance / SSE
      if(get_resp_matrix) {
        mp = lapply(1:Nsim, stochastic_matrix_dummy, parameters=parameters,
                    ord=conds[[i]]$train)
        mod[[names(conds)[i]]] = Reduce('+', mp)
      } else {
        #print(conds[[i]]$Condition)
        # 4AFC conditions use different testing
        if(!is.null(conds[[i]]$test)) {
          mp = lapply(1:Nsim, stochastic_matrix_dummy, parameters=parameters,
                      ord=conds[[i]]$train)
          mperf = Reduce('+', mp)
          mperf = mafc_test(mperf, conds[[i]]$test)
        } else {
          mp = sapply(1:Nsim, stochastic_dummy, parameters=parameters, ord=conds[[i]]$train)
          mperf = get_perf(mp)
        }
        mod[[names(conds)[i]]] = mperf
        SSE = SSE + conds[[i]]$Nsubj * sum( (mperf - conds[[i]]$HumanItemAcc)^2 )
        totSs = totSs + conds[[i]]$Nsubj
        SSE = SSE / totSs
      }

    }
    if(print_perf) {
      print(mod)
      print(paste0("SSE: ",SSE))
    }
    mod$SSE = SSE

    if(SSE_only) return(SSE)
    return(mod)
  }
}


#' Fit Stochastic Model to Conditions Using Differential Evolution
#'
#' This function fits a stochastic model to provided conditions using the Differential Evolution optimization algorithm. It aims to optimize the model parameters to minimize the sum of squared errors (SSE) between the model's predictions and human accuracy data.
#'
#' @param model_name A string specifying the name of the stochastic model to be fitted.
#' @param conds A list representing the conditions under which the model is fit. This typically includes training data and expected outcomes.
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#'
#' @return An object of class `DEoptim` representing the fitting result. This includes the best set of parameters found and the corresponding SSE value.
#' @export
fit_stochastic_model <- function(model_name, conds, lower, upper) {
  fit = DEoptim::DEoptim(run_stochastic_model, lower=lower, upper=upper,
                         DEoptim::DEoptim.control(reltol=.001, NP=100, itermax=30),
                model_name=model_name, conds=conds, SSE_only=T)
  return(fit)
}


#' Create a Data Frame from Model Fits
#'
#' This function generates a data frame summarizing the fits of models to various conditions. It compiles information such as model performance, human performance, and subject count for each condition, creating a comprehensive overview suitable for analysis and visualization.
#'
#' @param fits A list of model fitting results, where each element corresponds to a different model. The elements are objects resulting from the optimization process (e.g., using `DEoptim`).
#' @param conds A list representing the conditions for which the models were fitted. Each element in the list should correspond to a condition and include relevant data such as human item accuracy (`HumanItemAcc`).
#' @param cvdat An optional list of cross-validation data. If provided, this data is used instead of the data in `conds`. Default is an empty vector, indicating that `conds` will be used.
#'
#' @return A `tibble` (data frame) where each row represents a model-condition pair and includes columns for the model name, condition number, condition details, model performance, human performance, and subject count.
#' @export
get_model_dataframe <- function(fits, conds, cvdat=c()) {
  mdf = tidyr::tibble()
  for(model_name in names(fits)) {
    if(length(cvdat)!=0) {
      mdat = cvdat
    } else {
      if(is.element(model_name, stochastic_models)) {
        pars = fits[[model_name]]$optim$bestmem
        mdat = run_stochastic_model(conds, model_name, pars)
      } else {
        pars = fits[[model_name]]$optim$bestmem
        mdat = run_model(conds, model_name, pars)
      }
    }
    for(c in names(mdat)) {
      if(c!="SSE") {
        Nitems = length(mdat[[c]])
        HumanPerf = conds[[c]]$HumanItemAcc
        if(length(HumanPerf)==0) HumanPerf = rep(NA, Nitems)
        tmp = tidyr::tibble(Model=rep(model_name, Nitems),
                     condnum=rep(c, Nitems),
                     Condition=rep(conds[[c]]$Condition, Nitems),
                     ModelPerf=as.vector(mdat[[c]]),
                     HumanPerf=HumanPerf,
                     Nsubj=rep(conds[[c]]$Nsubj, Nitems))
        mdf = rbind(mdf, tmp)
      }
    }
  }
  return(mdf)
}


#' Generate Data Frame for Model Fits by Condition
#'
#' This function creates a data frame summarizing the fits of models for each specific condition. It consolidates data such as model performance, human performance, and number of subjects for each condition, enabling detailed analysis and comparison of model fits across different conditions.
#'
#' @param fits A list of model fitting results by condition, where each top-level element corresponds to a different model and each nested element represents a specific condition.
#' @param conds A list representing the conditions for which the models were fitted, including data such as human item accuracy (`HumanItemAcc`).
#'
#' @return A `tibble` (data frame) where each row represents a specific model-condition pair, including columns for the model name, condition number, detailed condition information, model performance, human performance, and number of subjects.
#' @export
get_model_dataframe_cond_fits <- function(fits, conds) {
  mdf = tidyr::tibble()
  SSE = 0
  for(model_name in names(fits)) {
    print(model_name)
    for(c in names(fits[[model_name]])) {
      print(c)
      pars = fits[[model_name]][[c]]$optim$bestmem
      SSE = fits[[model_name]][[c]]$optim$bestval

      if(is.element(model_name, stochastic_models)) {
        mdat = run_stochastic_model(conds, model_name, pars)
      } else {
        mdat = run_model(conds[[c]], model_name, pars)
      }
      Nitems = length(mdat$perf)
      HumanPerf = conds[[c]]$HumanItemAcc
      if(length(HumanPerf)==0) HumanPerf = rep(NA, Nitems)
      tmp = tidyr::tibble(Model=rep(model_name, Nitems),
                   condnum=rep(c, Nitems),
                   Condition=rep(conds[[c]]$Condition, Nitems),
                   ModelPerf=as.vector(mdat$perf),
                   HumanPerf=HumanPerf,
                   Nsubj=rep(conds[[c]]$Nsubj, Nitems))
      mdf = rbind(mdf, tmp)
    }
  }
  return(mdf)
}



#' Fit Model to Each Condition Individually
#'
#' This function applies the `fit_model` function to each specified condition individually. It iteratively fits a given model to each condition and compiles the fitting results.
#'
#' @param mname A string specifying the name of the model to be fitted to each condition.
#' @param conds A list where each element represents a different condition with its respective data.
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#'
#' @return A list of fitting results, where each element corresponds to a different condition. The fitting results are objects returned from the `fit_model` function.
#' @export
fit_by_cond <- function(mname, conds, lower, upper) {
  mod_fits = list()
  for(cname in names(conds)) {
    print(cname)
    mod_fits[[cname]] = fit_model(mname, conds[[cname]], lower, upper)
  }
  return(mod_fits)
}


#' Fit Stochastic Model to Each Condition Individually
#'
#' This function applies the `fit_stochastic_model` function to each specified condition individually. It iteratively fits a stochastic version of the given model to each condition and aggregates the fitting results.
#'
#' @param mname A string specifying the name of the stochastic model to be fitted to each condition.
#' @param conds A list where each element represents a different condition with its respective data.
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#'
#' @return A list of fitting results, where each element corresponds to a different condition. The fitting results are objects returned from the `fit_stochastic_model` function.
#' @export
fit_stochastic_by_cond <- function(mname, conds, lower, upper) {
  mod_fits = list()
  for(cname in names(conds)) {
    print(cname)
    mod_fits[[cname]] = fit_stochastic_model(mname, conds[[cname]], lower, upper)
  }
  return(mod_fits)
}



#' Split Data into Training and Testing Sets Based on Indices
#'
#' This function divides a given set of conditions into training and testing sets. The division is based on a provided vector of indices, which specify the conditions to be used for testing.
#'
#' @param test_inds A numeric vector of indices indicating which elements in `conds` should be used for the test set.
#' @param conds A list of conditions, where each condition is represented as a separate element.
#'
#' @return A list with two elements: `train` and `test`. Each element is a list of conditions, where `train` includes the conditions not indexed by `test_inds`, and `test` includes the conditions indexed by `test_inds`.
#' @export
get_train_test_split <- function(test_inds, conds) {
  dat = list(train = conds, test = list())
  for(i in test_inds) {
    dat$train[[i]] = NULL
    dat$test[[names(conds)[i]]] = conds[[i]]
  }
  return(dat)
}


#' Perform Cross-Validation on Group Fits of a Model
#'
#' This function runs cross-validation for group fits of a specified model using a set of combined data conditions. It supports both standard and stochastic models and performs fitting for each fold in the cross-validation process. The function records both the training accuracy and the testing results for each fold.
#'
#' @param model_name A string specifying the name of the model to be cross-validated.
#' @param combined_data A list of combined data conditions used for cross-validation.
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#'
#' @return A list containing cross-validation results, including fitted parameters, training accuracy, and a data frame of test results for each fold.
#' @export
cross_validated_group_fits <- function(model_name, combined_data, lower, upper) {
  #model_name = "kachergis"
  dat = list()
  test = list()
  testdf = tidyr::ibble()
  set.seed(123)
  folds <- caret::createFolds(names(combined_data), k = 5, list = T)
  cv_group_fits = list()
  for(i in 1:length(folds)) {
    conds = get_train_test_split(folds[[i]], combined_data)
    if(is.element(model_name, stochastic_models)) {
      opt = fit_stochastic_model(model_name, conds$train, lower, upper)
      test[[i]] = run_stochastic_model(conds$test, model_name, opt$optim$bestmem)
    } else {
      opt = fit_model(model_name, conds$train, lower, upper)
      test[[i]] = run_model(conds$test, model_name, opt$optim$bestmem)
    }
    dat[["pars"]] = rbind(dat[["pars"]], opt$optim$bestmem)
    dat[["train_acc"]] = c(dat[["train_acc"]], opt$optim$bestval)

    # add the data frame of test data? much more convenient..
    tmp = list()
    tmp[[model_name]] = opt
    testdf = rbind(testdf, get_model_dataframe(tmp, conds$test, cvdat=test[[i]]))
    print(paste("fold",i,"train SSE:",round(opt$optim$bestval,3),"test SSE:",round(test[[i]]$SSE,3)))
  }
  # add the folds ??
  dat[["test"]] = test
  dat[["testdf"]] = testdf # get_cv_test_df(test)
  return(dat)
}




# below mostly imported from ROC.R

#' Evaluate m-alternative forced choice test
#'
#' This function evaluates a given set of test trials using the provided model memory matrix
#' (word x referent). Each test trial is assumed to present one word and a set of referents
#' of size less than the width of the model memory matrix.
#'
#' @param mperf A matrix representing model memory with words as rows and referents as columns.
#' @param test A list representing the test trials, each containing a word and its associated referents.
#'
#' @return A vector with the probability of choosing the correct object, given each word.
#' @export
mafc_test <- function(mperf, test) {
  perf = rep(0, length(test$trials))
  for(i in 1:length(test$trials)) {
    w = test$trials[[i]]$word
    denom = sum(mperf[w, test$trials[[i]]$objs])
    perf[i] = mperf[w,w] / denom
  }
  return(perf)
}


#' Get True Positives (TP), Given a Knowledge Matrix and a Gold Lexicon
#'
#' This function iterates over words in a given gold lexicon and accumulates the associative
#' strength (can be integral e.g. 1, or real-valued) in the knowledge matrix for the intended
#' referents (present in the gold lexicon). Returns the number of expected true positives (TP)
#' for this gold lexicon and knowledge matrix.
#'
#' @param m A matrix representing the knowledge matrix with words as rows and referents as columns.
#' @param gold_lexicon A data frame or list where each row/element represents a word-object pair in the gold lexicon.
#'
#' @return A single value with the expected number of true positives.
#' @export
get_tp <- function(m, gold_lexicon) {
  count = 0
  if (length(gold_lexicon) > 0) {
    for (i in 1:length(gold_lexicon[["word"]])) {
      word = gold_lexicon[["word"]][i]
      ref = gold_lexicon[["object"]][i]
      if (!(word %in% rownames(m)) | !(ref %in% colnames(m))) {
        next
      }
      count = count + m[word, ref]
    }
    return(count)
  } else {
    for (ref in colnames(m)) {
      if (!(ref %in% rownames(m))) {
        next
      }
      count = count + m[ref, ref]
    }
    return(count)
  }
}

#' Calculate F-score, Precision, Recall, and Specificity for a Knowledge Matrix at a Given Threshold
#'
#' This function calculates the F-score, precision, recall, and specificity for a given knowledge matrix at a specified threshold. It uses the concept of true positives, false positives, and false negatives, determined from the knowledge matrix and an optional gold lexicon. The function is useful for evaluating the performance of a model in terms of its ability to correctly identify associations between words and referents.
#'
#' @param thresh A numeric value representing the threshold for considering an association between a word and a referent as positive.
#' @param mat A matrix representing the knowledge matrix with words as rows and referents as columns.
#' @param fscore_only Logical; if TRUE, only the F-score is returned. If FALSE, a data frame with precision, recall, specificity, and F-score for each threshold is returned.
#' @param gold_lexicon Optional; a data frame or list where each row/element represents a word-object pair in the gold lexicon. If provided, it is used to calculate true positives, false positives, and false negatives.
#' @param verbose Logical; if TRUE, additional details about true positives, false positives, and false negatives are printed.
#'
#' @return If `fscore_only` is TRUE, returns a single numeric value representing the F-score. If `fscore_only` is FALSE, returns a tibble (data frame) with columns for threshold, precision, recall, specificity, and F-score at each threshold.
#' @export
get_fscore <- function(thresh, mat, fscore_only=T, gold_lexicon = c(), verbose=F) {
  tmat <- mat >= thresh
  tp = get_tp(tmat, gold_lexicon) # correct referents selected
  words = gold_lexicon[["word"]]
  words = words[words %in% rownames(tmat)]
  objects = gold_lexicon[["object"]]
  objects = objects[objects %in% colnames(tmat)]
  if (length(gold_lexicon) > 0) {
    fp = sum(tmat[words, objects]) - tp
    fn = length(objects) - tp
  } else {
    fp = sum(tmat) - tp # incorrect referents selected: all selected referents - TPs
    fn = ncol(tmat) - tp # correct referents missed: num of words - TPs
  }
  if(verbose) print(c(tp, fp, fn))
  precision = tp / (tp + fp)
  recall = tp / (tp + fn) # aka sensitivity / true positive rate
  tn = sum(!tmat) - fn # all the 0s that should be 0s
  specificity = tn / (tn + fp) # TN = 0 where should be 0
  fscore = 2*precision*recall / (precision + recall)
  if(is.nan(fscore)) fscore = 0 # if tp+fn=0 or tp+fp=0
  if(fscore_only) {
    return(fscore)
  } else {
    return(tidyr::tibble(thresh=thresh, precision=precision, recall=recall,
                  fscore=fscore, specificity=specificity))
  }
}


#' Calculate Receiver Operating Characteristic (ROC) Scores for a Model Association Matrix
#'
#' This function computes Receiver Operating Characteristic (ROC) scores for a given model association matrix. It evaluates the performance of the model at various thresholds, providing metrics like f-scores, precision, and recall. The function can operate with a range of thresholds and optionally consider a gold lexicon for calculating true and false positives and negatives. The result is a comprehensive assessment of model performance over a continuum of classification thresholds.
#'
#' @param mdat A matrix representing the model association matrix with words as rows and referents as columns.
#' @param fscores_only Logical; if TRUE, only the f-scores are returned for each threshold. If FALSE, a dataframe including precision, recall, and f-scores for each threshold is returned.
#' @param plot Logical; if TRUE, a plot of the ROC curve is generated and displayed.
#' @param gold_lexicon Optional; a data frame or list where each row/element represents a word-object pair in the gold lexicon. If provided, it is used for more accurate calculation of true positives, false positives, and false negatives.
#'
#' @return A dataframe or tibble with columns for the threshold, f-score, precision, and recall. If `fscores_only` is TRUE, only f-scores are returned for each threshold from 0 to 1 in increments of 0.01. If `fscores_only` is FALSE, the dataframe includes precision and recall along with f-scores for each threshold.
#' @export
get_roc <- function(mdat, fscores_only=T, plot=F, gold_lexicon = c()) {
  #mat <- mdat / max(unlist(mdat)) # normalize so max value(s) in entire matrix are 1
  mat <- mdat / rowSums(mdat) # row-normalize matrix (better for all models?)
  threshes <- seq(0,1,.01)
  #fscores <- unlist(lapply(threshes, get_fscore, mat))
  prf <- dplyr::bind_rows(lapply(threshes, get_fscore, mat, fscore_only=F, gold_lexicon = gold_lexicon))
  if(plot) {
    g <- ggplot2::ggplot(data=prf, aes(x=1-specificity, y=recall)) + geom_line() +
      theme_classic() + xlim(0,1) + ylim(0,1)
    print(g)
  }
  if(fscores_only) {
    return(prf$fscore)
  } else {
    return(prf)
  }
}


#' Get Maximum F-score from ROC Scores
#'
#' This function computes the maximum F-score from the Receiver Operating Characteristic (ROC) scores of a model. It leverages the `get_roc` function to calculate the ROC scores and then extracts the highest F-score, providing a concise metric for the best classification performance of the model.
#'
#' @param mdat A matrix representing the model association matrix with words as rows and referents as columns, used to calculate the ROC scores.
#' @param gold_lexicon Optional; a data frame or list where each row/element represents a word-object pair in the gold lexicon. If provided, it enhances the accuracy of the ROC score calculations.
#'
#' @return A single numeric value representing the maximum F-score obtained from the ROC scores of the model.
#' @export
get_roc_max <- function(mdat, gold_lexicon = c()) {
  fscores <- get_roc(mdat, gold_lexicon = gold_lexicon)
  return(max(fscores[!is.na(fscores)]))
}
