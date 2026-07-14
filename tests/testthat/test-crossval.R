# small, fast DEoptim settings so these tests don't take the default
# NP = 100 / itermax = 100 (which can take 10-20s per fit x n_folds), and a
# small n_sim so stochastic models don't average over the default 500 sims
# per evaluation
fast_control <- DEoptim::DEoptim.control(NP = 10, itermax = 2, trace = FALSE)
fast_run_control <- xslControl(n_sim = 5)

test_that("get_train_test_split partitions by index without overlap", {
  conds <- xsl_datasets[1:6]
  split <- get_train_test_split(c(2, 4), conds)
  expect_length(split$train, 4)
  expect_length(split$test, 2)
  expect_identical(split$test[[1]], conds[[2]])
  expect_identical(split$test[[2]], conds[[4]])
  expect_false(any(vapply(split$train, identical, logical(1), conds[[2]])))
  expect_false(any(vapply(split$train, identical, logical(1), conds[[4]])))
})

test_that("cross_validated_group_fits returns one result per fold", {
  res <- cross_validated_group_fits(decay(C = 0.98), xsl_datasets[1:6],
                                     lower = 0.8, upper = 1.0, n_folds = 2,
                                     deoptim_control = fast_control, seed = 1)
  expect_length(res$folds, 2)
  expect_length(res$train_sse, 2)
  expect_length(res$test_sse, 2)
  expect_equal(nrow(res$params), 2)
  expect_true(all(is.finite(res$train_sse)))
  expect_true(all(is.finite(res$test_sse)))
})

test_that("cross_validated_group_fits is reproducible with a fixed seed", {
  res1 <- cross_validated_group_fits(decay(C = 0.98), xsl_datasets[1:6],
                                      lower = 0.8, upper = 1.0, n_folds = 2,
                                      deoptim_control = fast_control, seed = 42)
  res2 <- cross_validated_group_fits(decay(C = 0.98), xsl_datasets[1:6],
                                      lower = 0.8, upper = 1.0, n_folds = 2,
                                      deoptim_control = fast_control, seed = 42)
  expect_equal(res1$test_sse, res2$test_sse)
})

test_that("cross_validated_group_fits rejects n_folds outside [2, length(data)]", {
  expect_error(
    cross_validated_group_fits(decay(C = 0.98), xsl_datasets[1:3],
                               lower = 0.8, upper = 1.0, n_folds = 1)
  )
  expect_error(
    cross_validated_group_fits(decay(C = 0.98), xsl_datasets[1:3],
                               lower = 0.8, upper = 1.0, n_folds = 10)
  )
})

test_that("cross_validated_group_fits handles a model with zero free parameters", {
  res <- cross_validated_group_fits(baseline(), xsl_datasets[1:6],
                                     lower = numeric(0), upper = numeric(0),
                                     n_folds = 2, deoptim_control = fast_control, seed = 1)
  expect_equal(ncol(res$params), 0)
  expect_true(all(is.finite(res$test_sse)))
})

test_that("get_group_model_fit errors on an unknown model name", {
  expect_error(get_group_model_fit("not_a_real_model"), "not found")
})

test_that("get_group_model_fit works for models whose parameter names were previously mismatched", {
  # regression test: the model dispatch table used to call uncfam_sampling()/
  # multi_sampling() with a nonexistent N= argument (should be K=), tilles()
  # with alpha/beta/gamma (should be x/b/alpha_0), and fazly() without its
  # required lambda argument -- all three errored immediately
  for (m in c("uncfam_sampling", "multi_sampling", "fazly")) {
    result <- suppressWarnings(get_group_model_fit(m, datasets = xsl_datasets[1:3],
                                                    control = fast_run_control,
                                                    deoptim_control = fast_control))
    expect_true(is.finite(result$fit_result[[1]]$optim$bestval), info = m)
  }
})

test_that("get_group_model_fit works for baseline (zero free parameters)", {
  # regression test: DEoptim segfaults on zero-length lower/upper bounds
  result <- get_group_model_fit("baseline", datasets = xsl_datasets[1:3],
                                deoptim_control = fast_control)
  expect_true(is.finite(result$fit_result[[1]]$optim$bestval))
})

test_that("get_crossvalidated_model_fit returns cross-validation results", {
  result <- get_crossvalidated_model_fit("decay", n_folds = 2, datasets = xsl_datasets[1:6],
                                         deoptim_control = fast_control)
  expect_equal(result$n_folds, 2)
  expect_length(result$cv_result$test_sse, 2)
  expect_true(all(is.finite(result$cv_result$test_sse)))
})
