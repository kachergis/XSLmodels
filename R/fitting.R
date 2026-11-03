#' Run XSL model
#'
#' @param model An object of class xslMod.
#' @param data An object (or list of objects) of class xslData.
#' @param control Control arguments returned by `xsl_control()`.
#'
#' @return A list with `sse`, `unweighted_sse`, and `fits` (one entry per
#'   dataset). Each fit has `matrix` (the word-by-object matrix, summed over
#'   simulations for a stochastic model), `perf`, `sse`, `data`, `responses`
#'   (an `n_sim` x n-words matrix of each simulated participant's final
#'   per-word accuracy), and `sims` (the full per-simulation `xslFit` list,
#'   `NULL` unless `control` had `keep_sims = TRUE`).
#'
#'   With a likelihood objective (`xslControl(objective = "binomial")` or
#'   `"multinomial"`), each fit also has `loglik`, the log-likelihood of that
#'   condition's human test responses, and the result has `loglik` (summed
#'   over conditions) and `nll` (its negative, which [xsl_fit()] minimizes).
#'   Unlike `sse`, which averages conditions weighted by `n_subj`, the
#'   likelihood sums over every response, so larger conditions count for more
#'   in the same way.
#' @export
xsl_run <- function(model, data, control = xslControl()) {
  stopifnot("xslMod" %in% class(model))
  if ("xslData" %in% class(data)) data <- list(data)
  stopifnot(all(map_lgl(data, \(d) "xslData" %in% class(d))))

  if (!is.null(control$start_matrix) && !isTRUE(model$supports_start_matrix)) {
    stop("model '", model$name, "' does not use control$start_matrix, so it ",
         "would be silently ignored; pass the prior learning as training ",
         "trials instead", call. = FALSE)
  }

  model_fun <- model$model
  model_params <- model$params

  n_sim <- control$n_sim
  if (!model$stochastic) n_sim <- 1
  keep_sims <- isTRUE(control$keep_sims)
  objective <- control$objective %||% "sse"

  fits <- map(data, function(dat) {
    # Models index their matrices by the raw word/object values (m[tr_w,
    # tr_o]), which is only correct when those values are exactly
    # 1..voc_sz / 1..ref_sz. Hand every model positions instead, and put the
    # original labels back on what it returns (issue #14).
    pos <- positional_train(dat$train)
    ctrl <- align_start_matrix(control, pos$voc, pos$ref)

    # Accumulate the summed word-by-object matrix one simulation at a time so
    # the n_sim per-simulation results never coexist in memory -- for a long
    # corpus each carries a matrix per trial, which otherwise blows up.
    mat <- NULL
    responses <- NULL      # n_sim x n_words: each sim's final per-word accuracy
    sims <- if (keep_sims) vector("list", n_sim) else NULL
    for (i in seq_len(n_sim)) {
      s <- model_fun(params = model_params, data = pos$train, control = ctrl)
      s <- relabel_fit(s, pos$voc, pos$ref)
      mat <- if (is.null(mat)) s$matrix else mat + s$matrix
      pr <- s$perf
      if (is.matrix(pr)) pr <- pr[nrow(pr), ]   # last block = final state
      if (is.null(responses)) {
        responses <- matrix(NA_real_, n_sim, length(pr),
                            dimnames = list(NULL, names(pr)))
      }
      responses[i, ] <- pr
      if (keep_sims) sims[[i]] <- s
    }
    # dat$test defaults to list() (not NULL) when unset -- length() check (not
    # is.null()) is required so an xslData built without test isn't silently
    # routed through mafc_test(mat, list()), which returns numeric(0) (and
    # thus sse = 0, a false "perfect fit") rather than erroring
    # score test trials by label (character), not position: a word's correct
    # referent is the object with the same label, which is only the same
    # *position* when the word and object label sets line up
    # both scoring paths apply the model's choice exponent (ch_dec, if any)
    perf <- if (length(dat$test) > 0) {
      mafc_test(mat, labelled_test(dat$test), d = model_params[["ch_dec"]])
    } else {
      get_perf(mat, d = model_params[["ch_dec"]])
    }
    sse <- sum((perf - dat$accuracy) ^ 2)
    out <- list(sims = sims, responses = responses, perf = perf, matrix = mat,
                sse = sse, data = dat)
    if (objective != "sse") {
      out$loglik <- condition_loglik(mat, perf, dat, objective, control$lapse %||% 0,
                                     d = model_params[["ch_dec"]])
    }
    out
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

  out <- list(fits = fits, sse = sse, unweighted_sse = unweighted_sse)
  if (objective != "sse") {
    out$loglik <- sum(vapply(fits, `[[`, numeric(1), "loglik"))
    out$nll <- -out$loglik
  }
  out
}

# The value xsl_fit() minimizes: SSE, or the negative log-likelihood.
objective_value <- function(run, control) {
  if ((control$objective %||% "sse") == "sse") run$sse else run$nll
}

# Set the fitted values `params` (named by `free`) on the model, except a
# free "lapse", which the model doesn't have, goes to the control.
apply_free <- function(model, control, params, free) {
  is_lapse <- free == "lapse" & !"lapse" %in% names(model$params)
  if (any(is_lapse)) {
    control$lapse <- params[[which(is_lapse)]]
    params <- params[!is_lapse]
    free <- free[!is_lapse]
  }
  list(model = update_params(model, params, free), control = control)
}

# Relabel a training set's words/objects as their positions in the sorted
# word/object vocabularies. Order-preserving, so for labels that are already
# 1..N this is the identity. Empty-string and NA labels map to NA (the
# models drop NA words/objects per trial).
positional_train <- function(train) {
  is_label <- function(x) !is.na(x) & x != ""
  w <- unlist(train$words)
  o <- unlist(train$objects)
  voc <- sort(unique(w[is_label(w)]))
  ref <- sort(unique(o[is_label(o)]))
  list(train = list(words = lapply(train$words, match, table = voc),
                    objects = lapply(train$objects, match, table = ref)),
       voc = voc, ref = ref)
}

# Put the original word/object labels back on a model's returned xslFit.
relabel_fit <- function(s, voc, ref) {
  wl <- as.character(voc)
  ol <- as.character(ref)
  relabel_matrix <- function(m) {
    if (is.matrix(m) && nrow(m) == length(wl) && ncol(m) == length(ol)) {
      dimnames(m) <- list(wl, ol)
    }
    m
  }
  s$matrix <- relabel_matrix(s$matrix)
  if (length(s$traj) > 0) s$traj <- lapply(s$traj, relabel_matrix)
  if (is.matrix(s$perf)) {
    if (!is.null(colnames(s$perf)) && ncol(s$perf) == length(wl)) colnames(s$perf) <- wl
  } else if (!is.null(names(s$perf)) && length(s$perf) == length(wl)) {
    names(s$perf) <- wl
  }
  s
}

# A start matrix with dimnames is matched to the training data by label;
# one without is taken to already be in sorted word/object order.
align_start_matrix <- function(control, voc, ref) {
  sm <- control$start_matrix
  if (is.null(sm)) return(control)
  if (is.null(rownames(sm)) || is.null(colnames(sm))) {
    if (nrow(sm) != length(voc) || ncol(sm) != length(ref)) {
      stop("control$start_matrix is ", nrow(sm), " x ", ncol(sm), " but the ",
           "training data has ", length(voc), " words x ", length(ref),
           " objects", call. = FALSE)
    }
    return(control)
  }
  ri <- match(as.character(voc), rownames(sm))
  ci <- match(as.character(ref), colnames(sm))
  if (anyNA(ri) || anyNA(ci)) {
    stop("control$start_matrix's dimnames must include every word (rows) and ",
         "object (columns) in the training data", call. = FALSE)
  }
  sm <- sm[ri, ci, drop = FALSE]
  dimnames(sm) <- list(as.character(seq_along(voc)), as.character(seq_along(ref)))
  control$start_matrix <- sm
  control
}

labelled_test <- function(test) {
  out <- list(words = lapply(test$words, as.character),
              objects = lapply(test$objects, as.character))
  if (!is.null(test$correct)) out$correct <- lapply(test$correct, as.character)
  out
}


#' Fit XSL model using differential evolution
#'
#' Fits a model to provided data using the Differential Evolution optimization
#' algorithm. By default it optimizes the model parameters to minimize the sum
#' of squared errors (SSE) between the model's predictions and human accuracy
#' data; with `control = xslControl(objective = "binomial")` (or
#' `"multinomial"`) it instead maximizes the likelihood of the human test
#' responses (see [xslControl()]).
#'
#' @inheritParams xsl_run
#' @param lower Numeric vector of lower bounds for the model's parameters.
#' @param upper Numeric vector of upper bounds for the model's parameters.
#' @param by_data Logical indicating whether to fit to each entry in data
#'   separately.
#' @param control Control parameters passed to `xsl_run()`.
#' @param deoptim_control Control parameters passed to `DEoptim()`.
#' @param free Optional names of the parameters that `lower`/`upper` bound,
#'   in order (see `update_params()`); by default, the model's leading
#'   parameters. With a likelihood objective, `"lapse"` fits the control's
#'   lapse rate along with the model's parameters (e.g. bounds 0 to 1).
#' @param n_restarts Number of independent DEoptim runs per fit; the best
#'   (lowest SSE) is kept. Some models' objectives have poor local basins that
#'   a single run lands in a good fraction of the time (e.g.
#'   `uncfam_predictive()`), so a few restarts make fits far more reliable.
#' @param seed Optional random seed, set once before fitting, so DEoptim's
#'   random initial populations (and a stochastic model's simulations) are
#'   reproducible.
#'
#' @return A list with one element per fit (one in total, or one per dataset
#'   with `by_data = TRUE`), each the `DEoptim` result of the best run, which
#'   includes the best parameters found (`optim$bestmem`, named) and their SSE
#'   (`optim$bestval`; the negative log-likelihood with a likelihood
#'   objective). With `n_restarts > 1`, `restarts` holds every run's best
#'   value. With a likelihood objective, each fit also has `loglik`, `n_obs`
#'   (the number of human responses), `k` (the number of fitted parameters,
#'   including a fitted lapse), and `aic` and `bic`.
#' @export
#'
#' @examples
#' fit <- xsl_fit(decay(C = .98), xsl_datasets[1:2], lower = .8, upper = 1,
#'                n_restarts = 2, seed = 1,
#'                deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 5, trace = FALSE))
#' fit[[1]]$optim$bestmem
#' fit[[1]]$restarts
#'
#' # fit by binomial likelihood, with the lapse rate as a free parameter
#' fit <- xsl_fit(decay(C = .98), xsl_datasets[1:2], lower = c(.8, 0), upper = c(1, .5),
#'                free = c("C", "lapse"), control = xslControl(objective = "binomial"),
#'                seed = 1,
#'                deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 5, trace = FALSE))
#' fit[[1]]$optim$bestmem
#' fit[[1]]$aic
xsl_fit <- function(model, data, lower, upper, by_data = FALSE,
                    control = xslControl(),
                    deoptim_control = DEoptim::DEoptim.control(reltol = .001,
                                                               NP = 100,
                                                               itermax = 100),
                    free = NULL, n_restarts = 1, seed = NULL) {
  stopifnot("xslMod" %in% class(model))
  if ("xslData" %in% class(data)) data <- list(data)
  stopifnot(all(map_lgl(data, \(d) "xslData" %in% class(d))))
  # with a parallel DEoptim backend the objective's closure is shipped to
  # worker processes; an unforced argument would be evaluated there, where
  # the caller's variables don't exist, so every evaluation would fail
  force(control)
  stopifnot(length(lower) == length(upper), n_restarts >= 1)
  free <- if (is.null(free)) names(model$params)[seq_along(lower)] else free
  if (length(free) != length(lower)) {
    stop("`free` names ", length(free), " parameters but the bounds have ", length(lower), call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  if (by_data) data_wrap <- data else data_wrap <- list(data)
  fits <- map(data_wrap, function(dat) {
    run_wrapper <- \(params) {
      # some models are numerically unstable for certain parameter draws
      # (e.g. producing NA associations); treat those as an infinitely bad
      # fit rather than letting one bad draw abort the whole optimization
      val <- tryCatch({
        set <- apply_free(model, control, params, free)
        objective_value(xsl_run(model = set$model, data = dat, control = set$control), set$control)
      }, error = function(e) NA_real_)
      if (is.na(val)) Inf else val
    }
    if (length(lower) == 0) {
      # DEoptim segfaults when given zero-length bounds; a model with no
      # free parameters (e.g. baseline()) has nothing to optimize, so just
      # evaluate it once instead
      best <- list(optim = list(bestmem = numeric(0), bestval = run_wrapper(numeric(0))))
    } else {
      runs <- lapply(seq_len(n_restarts), \(i) DEoptim::DEoptim(run_wrapper, lower = lower, upper = upper, deoptim_control))
      vals <- vapply(runs, \(r) r$optim$bestval, numeric(1))
      best <- runs[[which.min(vals)]]
      names(best$optim$bestmem) <- free
      if (n_restarts > 1) best$restarts <- vals
    }
    if ((control$objective %||% "sse") != "sse") {
      best$loglik <- -best$optim$bestval
      best$n_obs <- n_responses(dat)
      best$k <- length(free)
      best$aic <- 2 * best$k - 2 * best$loglik
      best$bic <- log(best$n_obs) * best$k - 2 * best$loglik
    }
    best
  })
  if (any(!map_lgl(fits, \(f) is.finite(f$optim$bestval)))) {
    warning("every parameter evaluation failed for at least one fit (bestval = Inf); ",
            "check that xsl_run(model, data, control) works at parameters within the bounds",
            call. = FALSE)
  }
  fits
}
