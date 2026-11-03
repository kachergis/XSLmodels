# Choice-level scoring: choice_probabilities() and the binomial and
# multinomial likelihood objectives (xslControl(objective =)).

small_de <- DEoptim::DEoptim.control(NP = 20, itermax = 3, trace = FALSE)
labels <- vapply(xsl_datasets, `[[`, "", "label")
# one condition scored among all objects, one with a 4AFC test, one with
# filtering tests (correct = a co-occurring partner)
conds <- xsl_datasets[labels %in% c("201", "212", "filt3E_6L")]
p_correct <- function(ch) mapply(\(p, a, c) p[match(c, a)], ch$p, ch$alternatives, ch$correct)

test_that("choice_probabilities() is the distribution behind xsl_run()'s accuracy", {
  for (m in list(uncfam(X = .3, B = 3, C = .95), bayesian_decay(alpha = .5, delta = 1, ch_dec = 2.5))) {
    for (f in xsl_run(m, conds)$fits) {
      ch <- choice_probabilities(f$matrix, f$data, d = m$params[["ch_dec"]])
      expect_equal(nrow(ch), length(f$data$accuracy))
      expect_equal(unname(p_correct(ch)), unname(f$perf), tolerance = 1e-12, info = f$data$label)
      expect_true(all(abs(vapply(ch$p, sum, 1) - 1) < 1e-12))
      # human counts reproduce the stored accuracy
      n_correct <- mapply(\(h, a, c) h[match(c, a)], ch$counts, ch$alternatives, ch$correct)
      expect_equal(unname(n_correct / vapply(ch$counts, sum, 1)), f$data$accuracy, tolerance = 1e-12)
    }
  }
  ch <- choice_probabilities(xsl_run(baseline(), conds[[2]])$fits[[1]]$matrix, conds[[2]], lapse = 1)
  expect_true(all(vapply(ch$p, \(p) all(p == 1 / length(p)), TRUE)))
})

test_that("the binomial likelihood is the probability of each item's number correct", {
  m <- uncfam(X = .3, B = 3, C = .95)
  r <- xsl_run(m, conds, xslControl(objective = "binomial", lapse = .1))
  manual <- sum(vapply(r$fits, \(f) {
    k <- if (length(f$data$test)) lengths(f$data$test$objects) else rep(ncol(f$matrix), length(f$perf))
    q <- .9 * f$perf + .1 / k
    sum(dbinom(round(f$data$accuracy * f$data$n_subj), f$data$n_subj, q, log = TRUE))
  }, 1))
  expect_equal(r$loglik, manual)
  expect_equal(r$nll, -r$loglik)
  expect_equal(sum(vapply(r$fits, `[[`, 1, "loglik")), r$loglik)
  # without a response_matrix, counts come from accuracy and n_subj
  no_rm <- lapply(conds, \(d) { d$response_matrix <- NULL; d })
  expect_equal(xsl_run(m, no_rm, xslControl(objective = "binomial", lapse = .1))$loglik, r$loglik)
  # the SSE is unaffected by the objective
  expect_equal(r$sse, xsl_run(m, conds)$sse)
  expect_null(xsl_run(m, conds)$loglik)
})

test_that("the multinomial likelihood scores which object was chosen", {
  m <- uncfam(X = .3, B = 3, C = .95)
  r <- xsl_run(m, conds, xslControl(objective = "multinomial", lapse = .05))
  manual <- sum(vapply(r$fits, \(f) {
    ch <- choice_probabilities(f$matrix, f$data, lapse = .05)
    sum(mapply(\(h, p) dmultinom(h, prob = p, log = TRUE), ch$counts, ch$p))
  }, 1))
  expect_equal(r$loglik, manual)
  # it scores the foils too, so it's lower than the binomial on the same data
  expect_lt(r$loglik, xsl_run(m, conds, xslControl(objective = "binomial", lapse = .05))$loglik)
  # with every response a guess, both likelihoods are the model-free chance level
  chance <- \(obj) xsl_run(m, conds, xslControl(objective = obj, lapse = 1))$loglik
  expect_equal(chance("multinomial"), xsl_run(baseline(), conds, xslControl(objective = "multinomial", lapse = 1))$loglik)
  expect_equal(chance("binomial"), xsl_run(baseline(), conds, xslControl(objective = "binomial", lapse = 1))$loglik)
})

test_that("with two alternatives, the multinomial and binomial likelihoods agree", {
  d <- xslData(train = list(words = list(c(1, 2), c(1, 3), c(2, 3)), objects = list(c(1, 2), c(1, 3), c(2, 3))),
               test = list(words = list(1, 2, 3), objects = list(c(1, 2), c(2, 3), c(3, 1))),
               accuracy = c(.7, .6, .5), n_subj = 10,
               response_matrix = rbind(c(7, 3, 0), c(0, 6, 4), c(5, 0, 5)))
  ll <- \(obj) xsl_run(decay(C = .9), d, xslControl(objective = obj, lapse = .02))$loglik
  expect_equal(ll("multinomial"), ll("binomial"))
})

test_that("likelihoods error clearly without the counts they need", {
  d <- conds[[1]]
  d$response_matrix <- NULL
  expect_error(xsl_run(decay(C = .9), d, xslControl(objective = "multinomial")), "no response_matrix")
  d$n_subj <- numeric()
  expect_error(xsl_run(decay(C = .9), d, xslControl(objective = "binomial")), "neither a response_matrix nor n_subj")
  expect_error(xslControl(objective = "poisson"))
  expect_error(xslControl(lapse = 2))
})

test_that("xsl_fit() minimizes the negative log-likelihood and reports AIC/BIC", {
  ctrl <- xslControl(objective = "binomial", lapse = .02)
  fit <- xsl_fit(decay(C = .98), conds, lower = .8, upper = 1, control = ctrl,
                 seed = 1, deoptim_control = small_de)[[1]]
  at_best <- xsl_run(update_params(decay(C = .98), fit$optim$bestmem, "C"), conds, ctrl)
  expect_equal(fit$optim$bestval, at_best$nll)
  expect_equal(fit$loglik, at_best$loglik)
  expect_equal(fit$n_obs, sum(vapply(conds, \(d) sum(d$response_matrix), 1)))
  expect_equal(fit$k, 1)
  expect_equal(fit$aic, 2 - 2 * fit$loglik)
  expect_equal(fit$bic, log(fit$n_obs) - 2 * fit$loglik)
})

test_that("naming \"lapse\" in `free` fits the lapse rate", {
  ctrl <- xslControl(objective = "multinomial")
  fit <- xsl_fit(decay(C = .98), conds, lower = c(.8, 0), upper = c(1, 1),
                 free = c("C", "lapse"), control = ctrl, seed = 2, deoptim_control = small_de)[[1]]
  expect_named(fit$optim$bestmem, c("C", "lapse"))
  ctrl$lapse <- fit$optim$bestmem[["lapse"]]
  at_best <- xsl_run(update_params(decay(C = .98), fit$optim$bestmem[["C"]], "C"), conds, ctrl)
  expect_equal(fit$optim$bestval, at_best$nll)
  expect_equal(fit$k, 2)
})

test_that("cross-validation reports in- and out-of-sample likelihoods", {
  cv <- cross_validated_group_fits(decay(C = .98), xsl_datasets[1:4], lower = c(.8, 0), upper = c(1, 1),
                                   free = c("C", "lapse"), n_folds = 2, seed = 3,
                                   control = xslControl(objective = "binomial"),
                                   deoptim_control = small_de)
  expect_length(cv$test_nll, 2)
  expect_true(all(is.finite(cv$test_nll) & is.finite(cv$train_nll) & is.finite(cv$train_sse)))
  # each fold's test NLL is the held-out conditions' likelihood at that fold's parameters
  set.seed(3)
  held_out <- which(sample(rep(1:2, length.out = 4)) == 1)   # fold 1, as assigned in the function
  f1 <- cv$folds[[1]]
  ctrl <- xslControl(objective = "binomial", lapse = f1$params[["lapse"]])
  fitted <- update_params(decay(C = .98), f1$params[["C"]], "C")
  expect_equal(f1$test_nll, xsl_run(fitted, xsl_datasets[held_out], ctrl)$nll)
  expect_null(cross_validated_group_fits(decay(C = .98), xsl_datasets[1:4], lower = .8, upper = 1,
                                         n_folds = 2, seed = 3, deoptim_control = small_de)$test_nll)
})

test_that("the registry helpers can add a fitted lapse", {
  fit <- get_group_model_fit("decay", datasets = conds, control = xslControl(objective = "binomial"),
                             deoptim_control = small_de, seed = 4, fit_lapse = TRUE)
  expect_named(fit$fit_result[[1]]$optim$bestmem, c("C", "lapse"))
  expect_error(get_group_model_fit("decay", datasets = conds, fit_lapse = TRUE), "likelihood objective")
})

test_that("an xslControl from before the objective field still scores by SSE", {
  old <- new_xslControl(list(reps = 1, start_matrix = NULL, test_noise = 0, n_sim = 1,
                             verbose = FALSE, keep_traj = FALSE, keep_sims = FALSE))
  r <- xsl_run(decay(C = .9), conds, old)
  expect_equal(r$sse, xsl_run(decay(C = .9), conds)$sse)
  expect_null(r$nll)
})
