# small, fast DEoptim settings so these tests don't take the default
# NP = 100 / itermax = 100 (which can take 10-20s per fit)
fast_control <- DEoptim::DEoptim.control(NP = 10, itermax = 2, trace = FALSE)

test_that("update_params replaces values positionally and keeps the original names", {
  mod <- decay(C = 0.98)
  updated <- update_params(mod, c(0.85))
  expect_type(updated$params, "list")
  expect_equal(names(updated$params), "C")
  expect_equal(updated$params$C, 0.85)
})

test_that("update_params ignores any names on the incoming vector", {
  mod <- rescorla_wagner(C = 0.98, alpha = 0.1, lambda = 1, beta = 1)
  incoming <- c(par1 = 0.9, par2 = 0.2, par3 = 0.5, par4 = 1.5)
  updated <- update_params(mod, incoming)
  expect_equal(names(updated$params), names(mod$params))
  expect_equal(unname(unlist(updated$params)), unname(incoming))
})

test_that("update_params errors on a length mismatch", {
  mod <- decay(C = 0.98)
  expect_error(update_params(mod, c(0.9, 0.8)))
})

test_that("a model updated via update_params can still be run with xsl_run", {
  # regression test: update_params used to store params as a bare numeric
  # vector instead of a list, which made xsl_run()'s
  # model_params[["ch_dec"]] lookup error (subscript out of bounds) for
  # every model except bayesian_decay whenever it was called post-fitting
  mod <- update_params(decay(C = 0.98), c(0.9))
  result <- xsl_run(mod, xsl_datasets[[1]])
  expect_type(result$sse, "double")
})

test_that("xsl_run falls back to get_perf() when test is unset, rather than silently returning sse = 0", {
  # regression test: xslData()'s test defaults to list() (not NULL), and
  # xsl_run() checked !is.null(dat$test) rather than length(dat$test) > 0 --
  # so any xslData built without an explicit test (e.g. the package's own
  # get_example_ambiguous_condition()) was silently routed through
  # mafc_test(mat, list()) instead of get_perf(). Because of an R quirk
  # (1:0 counts down instead of being empty, and vec[i] <- numeric(0)
  # truncates the vector rather than erroring), that returned perf =
  # numeric(0) -- and thus sse = 0, a false "perfect fit" -- with no error.
  dat <- xslData(
    train = list(words = list(c(1, 2), c(1, 2)), objects = list(c(1, 2), c(1, 2))),
    accuracy = c(0.1, 0.9),
    label = "asymmetric toy example"
  )
  result <- xsl_run(baseline(), dat)
  expect_length(result$fits[[1]]$perf, 2)
  expect_equal(unname(result$fits[[1]]$perf), c(0.5, 0.5))
  expect_equal(result$sse, sum((c(0.5, 0.5) - c(0.1, 0.9))^2))
})

test_that("xsl_run still uses mafc_test() when real test trials are provided", {
  dat <- xslData(
    train = list(words = list(c(1, 2), c(1, 2)), objects = list(c(1, 2), c(1, 2))),
    test = list(words = list(1, 2), objects = list(1:2, 1:2)),
    accuracy = c(0.5, 0.5),
    label = "toy example with test trials"
  )
  result <- xsl_run(baseline(), dat)
  expect_length(result$fits[[1]]$perf, 2)
})

test_that("xsl_fit scores an erroring model as an infinitely bad fit instead of crashing", {
  always_errors <- xslMod(
    name = "always_errors",
    description = "test model that always errors",
    model = function(params, data, control) stop("boom"),
    params = list(p = 0.5),
    stochastic = FALSE
  )
  fit <- xsl_fit(always_errors, get_example_ambiguous_condition(),
                 lower = 0, upper = 1, deoptim_control = fast_control)
  expect_true(is.infinite(fit[[1]]$optim$bestval))
})

test_that("xsl_fit fits a simple model without error", {
  fit <- xsl_fit(decay(C = 0.98), get_example_unambiguous_condition(),
                 lower = 0.8, upper = 1.0, deoptim_control = fast_control)
  expect_s3_class(fit[[1]], "DEoptim")
  expect_true(is.finite(fit[[1]]$optim$bestval))
  expect_true(fit[[1]]$optim$bestmem >= 0.8 && fit[[1]]$optim$bestmem <= 1.0)
})

test_that("xsl_fit handles a model with zero free parameters without segfaulting", {
  # regression test: DEoptim segfaults (uncatchable, crashes the R session)
  # when given zero-length lower/upper bounds, which baseline() -- the only
  # model with no free parameters -- always does
  fit <- xsl_fit(baseline(), xsl_datasets[1:3], lower = numeric(0), upper = numeric(0))
  expect_length(fit[[1]]$optim$bestmem, 0)
  expect_true(is.finite(fit[[1]]$optim$bestval))
})

test_that("xsl_fit with by_data = TRUE fits each dataset separately", {
  fits <- xsl_fit(decay(C = 0.98), xsl_datasets[1:3], lower = 0.8, upper = 1.0,
                  by_data = TRUE, deoptim_control = fast_control)
  expect_length(fits, 3)
  expect_true(all(vapply(fits, \(f) is.finite(f$optim$bestval), logical(1))))
})
