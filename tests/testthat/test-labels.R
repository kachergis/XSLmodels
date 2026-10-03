# Issue #14: models index their matrices by raw word/object value, which is
# only correct for labels 1..N. xsl_run() now hands models positions and
# restores the original labels, so any order-preserving relabeling of a
# dataset -- including one that introduces gaps, or switches to character
# labels -- must leave every model's output unchanged.

relabel_data <- function(d, f) {
  map_trials <- function(tr) lapply(tr, f)
  d$train <- list(words = map_trials(d$train$words), objects = map_trials(d$train$objects))
  if (length(d$test) > 0) {
    d$test <- list(words = map_trials(d$test$words), objects = map_trials(d$test$objects))
  }
  d
}

run_seeded <- function(mod, d) {
  set.seed(1)
  suppressWarnings(xsl_run(mod, d, control = xslControl(n_sim = 2)))$fits[[1]]
}

test_that("the issue #14 repro (a vocabulary with a gap) runs for every model", {
  dat <- xslData(
    train = list(words = list(c(1, 3), c(4, 5), c(6, 3), c(1, 6)),
                 objects = list(c(1, 3), c(4, 5), c(6, 3), c(1, 6))),
    label = "gap test"
  )
  for (nm in names(xsl_model_registry())) {
    fit <- run_seeded(xsl_model_registry()[[nm]]$constructor(), dat)
    expect_equal(rownames(fit$matrix), c("1", "3", "4", "5", "6"), info = nm)
    expect_false(anyNA(fit$perf), info = nm)
  }
})

test_that("every model is invariant to order-preserving relabeling (gapped and character labels)", {
  d <- benitez2020[["Interleaved, kids"]]   # 8 words/objects, has 2AFC test trials
  gapped <- relabel_data(d, \(x) x * 10)
  chars  <- relabel_data(d, \(x) sprintf("w%02d", x))
  for (nm in names(xsl_model_registry())) {
    mod <- xsl_model_registry()[[nm]]$constructor()
    ref <- run_seeded(mod, d)
    for (alt in list(gapped = gapped, chars = chars)) {
      fit <- run_seeded(mod, alt)
      expect_equal(unname(fit$matrix), unname(ref$matrix), info = nm)
      expect_equal(unname(fit$perf), unname(ref$perf), info = nm)
      expect_equal(fit$sse, ref$sse, info = nm)
    }
  }
})

test_that("the returned matrix (and get_perf()-based perf) carry the original labels", {
  # no test trials, so perf comes from get_perf(), which names it by word
  d <- relabel_data(kachergis_initial_accuracy[["Low Initial Accuracy"]], \(x) x * 10)
  fit <- run_seeded(uncfam(X = .1, B = .98, C = 1), d)
  labs <- as.character(seq(10, 180, by = 10))
  expect_equal(rownames(fit$matrix), labs)
  expect_equal(colnames(fit$matrix), labs)
  expect_equal(names(fit$perf), labs)
})

test_that("a start_matrix with dimnames is aligned to the data by label", {
  d <- kachergis_initial_accuracy[["High Initial Accuracy"]]
  sm <- matrix(0, 18, 18, dimnames = list(1:18, 1:18))
  diag(sm) <- .05
  shuffled <- sm[18:1, c(2:18, 1)]   # same content, rows/cols out of order
  mod <- uncfam(X = .1, B = .98, C = 1)
  a <- xsl_run(mod, d, control = xslControl(start_matrix = sm))$fits[[1]]
  b <- xsl_run(mod, d, control = xslControl(start_matrix = shuffled))$fits[[1]]
  expect_equal(a$matrix, b$matrix)
  expect_error(
    xsl_run(mod, d, control = xslControl(start_matrix = sm[1:17, ])),
    "must include every word"
  )
})
