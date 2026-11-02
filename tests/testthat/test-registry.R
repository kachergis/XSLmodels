# The model registry (xsl_model_registry()) and the fitting options it drives:
# named free parameters, restarts, and seeds.

small_de <- DEoptim::DEoptim.control(NP = 30, itermax = 2, trace = FALSE)

test_that("update_params() sets named parameters with `free`, in the given order", {
  rw <- rescorla_wagner(C = .98, alpha = 1, beta = .5, lambda = 1)
  upd <- update_params(rw, c(.2, .95, .8), free = c("beta", "C", "lambda"))
  expect_equal(upd$params, list(C = .95, alpha = 1, beta = .2, lambda = .8))
  # positional by default, as before
  expect_equal(update_params(rw, c(.9, .5))$params, list(C = .9, alpha = .5, beta = .5, lambda = 1))
  expect_error(update_params(rw, 1, free = "nope"), "unknown parameter")
  expect_error(update_params(rw, c(1, 2), free = "beta"), "names 1 parameters")
})

test_that("every registry entry is self-consistent", {
  reg <- xsl_model_registry()
  expect_setequal(show_models(), names(reg))
  for (nm in names(reg)) {
    e <- reg[[nm]]
    mod <- e$constructor()
    expect_s3_class(mod, "xslMod")
    expect_true(all(e$free %in% names(mod$params)), info = nm)
    expect_equal(length(e$free), length(e$lower), info = nm)
    expect_equal(length(e$lower), length(e$upper), info = nm)
    expect_true(all(e$lower <= e$upper), info = nm)
    start <- unlist(mod$params[e$free])
    expect_true(all(start >= e$lower & start <= e$upper), info = nm)
  }
})

test_that("multi_sampling()'s bounds are attached to the right parameters", {
  # regression test: multi_sampling() orders its parameters (C, X, B, K), but
  # its registry bounds were written for (X, B, C, K) and assigned by position,
  # giving the decay C the bounds [.01, .5]
  e <- xsl_model_registry()$multi_sampling
  bounds <- setNames(e$lower, e$free)
  expect_equal(unname(bounds["C"]), .5)
  expect_equal(unname(setNames(e$upper, e$free)["C"]), 1)
})

test_that("uncfam-family bounds include the original model's optimum (B ~ 14)", {
  reg <- xsl_model_registry()
  for (nm in c("uncfam", "uncfam_attention", "uncfam_predictive", "uncfam_gamma", "uncfam_elimination")) {
    b <- setNames(reg[[nm]]$upper, reg[[nm]]$free)
    expect_gte(unname(b["B"]), 14, label = nm)
  }
})

test_that("xsl_fit() keeps the best of n_restarts runs and reports them all", {
  fit <- xsl_fit(decay(C = .98), xsl_datasets[1:2], lower = .8, upper = 1,
                 n_restarts = 3, seed = 7, deoptim_control = small_de)[[1]]
  expect_length(fit$restarts, 3)
  expect_equal(fit$optim$bestval, min(fit$restarts))
  expect_named(fit$optim$bestmem, "C")
})

test_that("xsl_fit() with a seed is reproducible", {
  f <- function() xsl_fit(decay(C = .98), xsl_datasets[1:2], lower = .8, upper = 1,
                          seed = 11, deoptim_control = small_de)[[1]]$optim
  expect_identical(f()$bestmem, f()$bestmem)
})

test_that("xsl_fit() fits a named subset of parameters, leaving the rest fixed", {
  rw <- rescorla_wagner(C = .98, alpha = 1, beta = .1, lambda = 1)
  fit <- xsl_fit(rw, xsl_datasets[1:2], lower = c(1e-5, .5), upper = c(1, 1),
                 free = c("beta", "C"), seed = 3, deoptim_control = small_de)[[1]]
  expect_named(fit$optim$bestmem, c("beta", "C"))
  fitted <- update_params(rw, fit$optim$bestmem, c("beta", "C"))
  expect_equal(fitted$params$alpha, 1)
  expect_equal(fitted$params$lambda, 1)
  expect_equal(xsl_run(fitted, xsl_datasets[1:2])$sse, fit$optim$bestval)
  expect_error(xsl_fit(rw, xsl_datasets[1], lower = 0, upper = 1, free = c("beta", "C")), "bounds have 1")
})

test_that("get_group_model_fit() uses the registry's free parameters and default restarts", {
  fit <- get_group_model_fit("uncfam_strength", datasets = xsl_datasets[1:2],
                             deoptim_control = small_de, seed = 5)
  expect_named(fit$fit_result[[1]]$optim$bestmem, c("X", "C"))
  fit <- get_group_model_fit("tilles", datasets = xsl_datasets[1:2],
                             deoptim_control = small_de, seed = 5)
  expect_length(fit$fit_result[[1]]$restarts, 3)
  fit <- get_group_model_fit("tilles", datasets = xsl_datasets[1:2],
                             deoptim_control = small_de, seed = 5, n_restarts = 1)
  expect_null(fit$fit_result[[1]]$restarts)
})

test_that("cross-validation passes free parameters and restarts through to each fold", {
  cv <- get_crossvalidated_model_fit("rescorla_wagner", n_folds = 2, datasets = xsl_datasets[1:4],
                                     deoptim_control = small_de, seed = 2, n_restarts = 2)
  expect_equal(colnames(cv$cv_result$params), c("beta", "C", "lambda"))
  expect_true(all(is.finite(cv$cv_result$test_sse)))
})
