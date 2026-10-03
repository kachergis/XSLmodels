d <- benitez2020[["Interleaved, kids"]]   # 8 words x 8 objects
run_seeded <- function(mod, ctrl) {
  set.seed(1)
  xsl_run(mod, d, control = ctrl)$fits[[1]]
}

test_that("models that support start_matrix use it, and a zero one changes nothing", {
  reg <- xsl_model_registry()
  supported <- names(reg)[vapply(reg, \(e) isTRUE(e$constructor()$supports_start_matrix), logical(1))]
  expect_setequal(supported, c(
    "baseline", "decay", "rescorla_wagner", "softmax_rl", "uncfam_sampling", "multi_sampling",
    "uncfam", "uncfam_attention", "uncfam_predictive", "uncfam_gamma", "uncfam_elimination",
    "propose_but_verify", "pursuit"
  ))
  zero <- matrix(0, 8, 8)
  prior <- matrix(0, 8, 8); diag(prior) <- .5
  for (nm in supported) {
    mod <- reg[[nm]]$constructor()
    default <- run_seeded(mod, xslControl(n_sim = 3))
    from_zero <- run_seeded(mod, xslControl(n_sim = 3, start_matrix = zero))
    from_prior <- run_seeded(mod, xslControl(n_sim = 3, start_matrix = prior))
    expect_equal(from_zero$matrix, default$matrix, info = nm)
    expect_false(isTRUE(all.equal(from_prior$matrix, default$matrix)), info = nm)
  }
})

test_that("models that don't support start_matrix error instead of ignoring it", {
  reg <- xsl_model_registry()
  unsupported <- names(reg)[!vapply(reg, \(e) isTRUE(e$constructor()$supports_start_matrix), logical(1))]
  expect_gt(length(unsupported), 0)
  for (nm in unsupported) {
    expect_error(
      xsl_run(reg[[nm]]$constructor(), d, control = xslControl(start_matrix = matrix(0, 8, 8))),
      "does not use control\\$start_matrix", info = nm
    )
  }
})

test_that("a start_matrix of the wrong size errors", {
  expect_error(
    xsl_run(uncfam(X = .1, B = .98, C = 1), d, control = xslControl(start_matrix = matrix(0, 7, 8))),
    "7 x 8 but the training data has 8 words x 8 objects"
  )
})
