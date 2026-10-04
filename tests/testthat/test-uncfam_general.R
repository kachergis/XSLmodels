# uncfam_general() must reproduce each nested uncfam-family model exactly.

dat <- get_example_ambiguous_condition()
run <- function(mod, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  mod$model(mod$params, dat$train, xslControl())
}
same <- function(a, b) {
  expect_equal(a$matrix, b$matrix, tolerance = 1e-12)
  expect_equal(unname(as.vector(a$perf)), unname(as.vector(b$perf)), tolerance = 1e-12)
}

test_that("defaults reproduce uncfam()", {
  same(run(uncfam_general(X = .1, B = .98, C = .97)), run(uncfam(X = .1, B = .98, C = .97)))
})

test_that("gamma = 0 reproduces uncfam(variant = 'uncertainty-only')", {
  same(run(uncfam_general(X = .1, B = .98, C = .97, gamma = 0)),
       run(uncfam(X = .1, B = .98, C = .97, variant = "uncertainty-only")))
})

test_that("uncertainty = 'novelty' reproduces uncfam(variant = 'novelty')", {
  same(run(uncfam_general(X = .1, B = 2, C = .97, uncertainty = "novelty")),
       run(uncfam(X = .1, B = 2, C = .97, variant = "novelty")))
})

test_that("B = 0 reproduces uncfam(B = 0) (familiarity only)", {
  same(run(uncfam_general(X = .1, B = 0, C = .97)), run(uncfam(X = .1, B = 0, C = .97)))
})

test_that("free gamma reproduces uncfam_gamma()", {
  for (g in c(.3, 2.5)) {
    same(run(uncfam_general(X = .1, B = .98, C = .97, gamma = g)),
         run(uncfam_gamma(X = .1, B = .98, C = .97, gamma = g)))
  }
})

test_that("free eps reproduces uncfam_elimination()", {
  for (e in c(.5, 5)) {
    same(run(uncfam_general(X = .1, B = .98, C = .97, eps = e)),
         run(uncfam_elimination(X = .1, B = .98, C = .97, eps = e)))
  }
})

test_that("kappa = 1 reproduces uncfam_attention()", {
  same(run(uncfam_general(X = .1, B = .98, C = .97, kappa = 1)),
       run(uncfam_attention(X = .1, B = .98, C = .97)))
})

test_that("finite K reproduces uncfam_sampling() draw for draw", {
  for (k in c(1, 3)) {
    same(run(uncfam_general(X = .1, B = .98, C = .97, K = k), seed = 11),
         run(uncfam_sampling(X = .1, B = .98, C = .97, K = k), seed = 11))
  }
  expect_true(uncfam_general(X = .1, B = .98, C = 1, K = 1)$stochastic)
  expect_false(uncfam_general(X = .1, B = .98, C = 1)$stochastic)
})

test_that("large K approaches deterministic allocation", {
  m_inf <- run(uncfam_general(X = .1, B = .98, C = .97))$matrix
  m_big <- run(uncfam_general(X = .1, B = .98, C = .97, K = 500), seed = 3)$matrix
  expect_equal(m_big, m_inf, tolerance = 1e-10)
})

test_that("extensions combine without error and stay finite", {
  fit <- run(uncfam_general(X = .2, B = 3, C = .95, gamma = 2, eps = 2, kappa = 1, K = 2), seed = 1)
  expect_true(all(is.finite(fit$matrix)))
  expect_true(all(fit$perf >= 0 & fit$perf <= 1))
  expect_error(uncfam_general(X = .1, B = 1, C = 1, K = 0), "positive integer")
})

test_that("sampled attention matches uncfam_sampling() even with tied sampling probabilities", {
  # regression test: in xsl_datasets[[45]] a new word's sampling
  # probabilities are tied, and R's weighted sample() sorts them, so a 1-ulp
  # difference in the previous trial's update (from a different order of
  # floating-point operations) changed which object was drawn
  d <- xsl_datasets[[45]]
  set.seed(5)
  a <- uncfam_general(X = .1, B = 3, C = .97, K = 1)
  a <- a$model(a$params, d$train, xslControl())
  set.seed(5)
  b <- uncfam_sampling(X = .1, B = 3, C = .97, K = 1)
  b <- b$model(b$params, d$train, xslControl())
  expect_identical(a$matrix, b$matrix)
})
