# uncfam_general() must reproduce each nested uncfam-family model exactly.
# Since uncfam(), uncfam_gamma(), uncfam_elimination(), uncfam_attention()
# and uncfam_sampling() became wrappers around uncfam_general_model(), the
# equivalence tests below check the wrappers' parameter mapping; the
# reference test checks all of them against the matrices their original,
# standalone implementations produced.

test_that("the uncfam family reproduces its original standalone implementations", {
  # fixtures/uncfam_family_reference.rds: matrices from the pre-wrapper
  # implementations (XSLmodels 5af3a89), two training blocks, set.seed(9)
  ref <- readRDS(test_path("fixtures", "uncfam_family_reference.rds"))
  conds <- Filter(\(d) d$label %in% attr(ref, "labels"), xsl_datasets)
  mods <- list(
    uncfam = list(uncfam(X = .1, B = 3, C = .97), uncfam(X = 2, B = 14, C = .9)),
    novelty = list(uncfam(X = .3, B = 2, C = .95, variant = "novelty")),
    unc_only = list(uncfam(X = .3, B = 2, C = .95, variant = "uncertainty-only")),
    strength = list(uncfam(X = .3, B = 0, C = .95)),
    gamma = list(uncfam_gamma(X = .2, B = 3, C = .96, gamma = .4), uncfam_gamma(X = .2, B = 3, C = .96, gamma = 3)),
    elim = list(uncfam_elimination(X = .2, B = 3, C = .96, eps = 2)),
    attn = list(uncfam_attention(X = .2, B = 3, C = .96)),
    samp = list(uncfam_sampling(X = .2, B = 3, C = .96, K = 1), uncfam_sampling(X = .2, B = 3, C = .96, K = 4)))
  for (nm in names(mods)) for (j in seq_along(mods[[nm]])) for (k in seq_along(conds)) {
    m <- mods[[nm]][[j]]
    set.seed(9)
    f <- suppressWarnings(m$model(m$params, positional_train(conds[[k]]$train)$train, xslControl(reps = 2)))
    expect_equal(unname(f$matrix), ref[[nm]][[j]][[k]], tolerance = 1e-12,
                 info = paste(nm, j, conds[[k]]$label))
  }
})

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
  # regression test: in condition 1_max_temp_spat_cont_orig a new word's sampling
  # probabilities are tied, and R's weighted sample() sorts them, so a 1-ulp
  # difference in the previous trial's update (from a different order of
  # floating-point operations) changed which object was drawn
  d <- Filter(\(x) x$label == "1_max_temp_spat_cont_orig", xsl_datasets)[[1]]
  set.seed(5)
  a <- uncfam_general(X = .1, B = 3, C = .97, K = 1)
  a <- a$model(a$params, d$train, xslControl())
  set.seed(5)
  b <- uncfam_sampling(X = .1, B = 3, C = .97, K = 1)
  b <- b$model(b$params, d$train, xslControl())
  expect_identical(a$matrix, b$matrix)
})

test_that("rho = 0 ignores the short-term trace entirely (any Cs)", {
  same(run(uncfam_general(X = .1, B = .98, C = .97, rho = 0, Cs = .1)),
       run(uncfam_general(X = .1, B = .98, C = .97)))
  same(run(uncfam_general(X = .1, B = .98, C = .97, K = 2, rho = 0, Cs = .9), seed = 4),
       run(uncfam_general(X = .1, B = .98, C = .97, K = 2), seed = 4))
})

test_that("a short-term trace favours a pairing repeated on the previous trial", {
  # word 1 meets object 1 on trials 1-2 (massed) and object 2 on trials 3-4
  # (massed again); on trial 5 it appears with both. With equal long-term
  # strength, the trace favours the more recent pairing (1-2).
  d <- xslData(train = list(words = list(1, 1, 1, 1, 1), objects = list(1, 1, 2, 2, c(1, 2))))
  share_12 <- function(rho) {
    mod <- uncfam_general(X = .5, B = 0, C = 1, rho = rho, Cs = .5)
    # single word/object trials trigger R's "recycling array of length 1"
    # deprecation warning in the shared uncfam() arithmetic (also in uncfam())
    f <- suppressWarnings(mod$model(mod$params, d$train, xslControl(keep_traj = TRUE)))
    gain <- f$traj[[5]] - f$traj[[4]]
    gain[1, 2] / sum(gain[1, ])
  }
  expect_gt(share_12(5), share_12(0))
})

test_that("short-term trace combines with other extensions and stays valid", {
  fit <- run(uncfam_general(X = .2, B = 3, C = .95, gamma = 2, eps = 1, rho = 3, Cs = .3, K = 1), seed = 2)
  expect_true(all(is.finite(fit$matrix)))
  expect_true(all(fit$perf >= 0 & fit$perf <= 1))
})
