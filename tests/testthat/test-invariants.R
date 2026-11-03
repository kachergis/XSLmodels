# Invariants every registered model must satisfy on real data. These catch
# numerical failures (NaN/Inf associations, "probabilities" outside [0, 1])
# that the per-model regression tests only cover where they were once found.

registry <- xsl_model_registry()
# the batch-MCMC models take seconds per condition (the rest, milliseconds),
# so they're checked on a couple of conditions only; fgt2009_rsa shares
# fgt2009's sampler (adding a pragmatic readout), so only fgt2009 does the
# slower corner and reproducibility checks
SLOW <- c("fgt2009", "fgt2009_rsa")
labels <- vapply(xsl_datasets, `[[`, "", "label")
representative <- xsl_datasets[labels %in% c(
  "201",                       # 3x4, 18 words
  "204",                       # extra objects (3x4 +6o)
  "212",                       # 4AFC test
  "301",                       # 24 words, 24AFC
  "filt3E_6L",                 # filtering: AtoA + AtoB tests (correct = partner)
  "freq369-3x3hiCD",           # varied frequency
  "temp_cont_1olap2tr_contr"   # temporal contiguity
)]
stopifnot(length(representative) == 7)

expect_valid_fit <- function(f, info) {
  expect_true(all(is.finite(f$matrix)), info = paste(info, "matrix finite"))
  expect_true(all(is.finite(f$perf)), info = paste(info, "perf finite"))
  expect_true(all(f$perf >= 0 & f$perf <= 1), info = paste(info, "perf in [0, 1]"))
  expect_true(is.finite(f$sse), info = paste(info, "sse finite"))
  expect_equal(length(f$perf), length(f$data$accuracy), info = paste(info, "one prediction per test item"))
}

test_that("every registered model gives valid predictions on every xsl_datasets condition", {
  skip_on_cran()
  for (nm in names(registry)) {
    data <- if (nm %in% SLOW) representative[1:2] else xsl_datasets
    set.seed(1)
    res <- xsl_run(registry[[nm]]$constructor(), data, control = xslControl(n_sim = 2))
    for (f in res$fits) expect_valid_fit(f, paste(nm, f$data$label))
  }
})

test_that("every registered model gives valid predictions or a clear error at its bound corners", {
  skip_on_cran()
  for (nm in setdiff(names(registry), c("baseline", "fgt2009_rsa"))) {
    e <- registry[[nm]]
    corners <- list(lower = e$lower, upper = e$upper, mid = (e$lower + e$upper) / 2)
    data <- if (nm %in% SLOW) representative[1] else representative
    for (cn in names(corners)) {
      mod <- update_params(e$constructor(), corners[[cn]], e$free)
      set.seed(1)
      res <- tryCatch(xsl_run(mod, data, control = xslControl(n_sim = 2)), error = function(err) err)
      info <- paste(nm, "at", cn, "bounds")
      if (inherits(res, "error")) {
        # an error is acceptable (xsl_fit() scores it as an infinitely bad
        # fit) but must say what went wrong, not be an internal subscript or
        # missing-value failure
        expect_false(grepl("subscript out of bounds|missing value where|argument of length 0",
                           conditionMessage(res)), info = paste(info, ":", conditionMessage(res)))
      } else {
        for (f in res$fits) expect_valid_fit(f, paste(info, f$data$label))
      }
    }
  }
})

test_that("models are reproducible: deterministic always, stochastic under a seed", {
  skip_on_cran()
  for (nm in setdiff(names(registry), "fgt2009_rsa")) {
    data <- if (nm %in% SLOW) representative[1] else representative[1:2]
    mod <- registry[[nm]]$constructor()
    run <- function() { set.seed(42); xsl_run(mod, data, control = xslControl(n_sim = 2)) }
    a <- run(); b <- run()
    expect_identical(a$fits[[1]]$matrix, b$fits[[1]]$matrix, info = nm)
    expect_identical(a$sse, b$sse, info = nm)
  }
})
