# rescorla_wagner() must apply each object's prediction error to that
# object's associations. Until 0.5.0 the per-object error vector was added
# to the trial's word x object submatrix directly, so R recycled it down the
# columns and word i got object i's error for every object on the trial.

# Rescorla-Wagner, written out cell by cell (words are cues, objects
# outcomes, as in rescorla_wagner(), whose learning rate is beta * lambda).
rw_reference <- function(train, C, beta, lambda) {
  voc <- sort(unique(unlist(train$words)))
  ref <- sort(unique(unlist(train$objects)))
  m <- matrix(0, length(voc), length(ref), dimnames = list(voc, ref))
  for (t in seq_along(train$words)) {
    w <- as.character(train$words[[t]])
    o <- as.character(train$objects[[t]])
    err <- vapply(o, \(oj) lambda - sum(m[w, oj]), numeric(1))
    for (oj in o) for (wi in w) m[wi, oj] <- m[wi, oj] + beta * lambda * err[[oj]]
    m <- m * C
  }
  m
}

run_rw <- function(train, C = 1, beta = .3, lambda = 1) {
  mod <- rescorla_wagner(C = C, alpha = 1, beta = beta, lambda = lambda)
  mod$model(mod$params, train, xslControl())
}

test_that("a word with no new information is blocked from the predicted object", {
  # A is trained with X to asymptote; then A and B appear with X and Y. X is
  # fully predicted by A, so B learns (almost) nothing about X but does learn
  # Y, the unpredicted object. The recycled update gave B every error on
  # the trial, and A none.
  train <- list(words = c(rep(list("A"), 30), list(c("A", "B"))),
                objects = c(rep(list("X"), 30), list(c("X", "Y"))))
  m <- run_rw(train)$matrix
  expect_lt(m["B", "X"], 1e-4)
  expect_equal(m["B", "Y"], .3)
  expect_equal(m["A", "Y"], .3)
})

test_that("rescorla_wagner() matches a cell-by-cell reference implementation", {
  for (d in xsl_datasets[c(1, 5, 20, 30)]) {
    pos <- positional_train(d$train)$train
    for (p in list(c(C = 1, beta = .1, lambda = 1), c(C = .95, beta = .4, lambda = 2))) {
      expect_equal(unname(run_rw(pos, p[["C"]], p[["beta"]], p[["lambda"]])$matrix),
                   unname(rw_reference(pos, p[["C"]], p[["beta"]], p[["lambda"]])), tolerance = 1e-12,
                   info = paste(d$label, toString(p)))
    }
  }
})

test_that("Rescorla-Wagner no longer reduces to co-occurrence counting", {
  # with the recycled update, Luce choice over a word's associations was
  # exactly its co-occurrence proportion in designs like these. Correct RW
  # departs from it most when pair frequencies differ (here 3, 6 and 9);
  # in a fully balanced design (every pair 6 times) it stays close
  d <- kachergis_priming[["4x4 3/6/9x"]]
  rw <- xsl_run(rescorla_wagner(C = 1, alpha = 1, beta = .1, lambda = 1), d)$fits[[1]]$perf
  bl <- xsl_run(baseline(), d)$fits[[1]]$perf
  expect_gt(max(abs(rw - bl)), .1)
})

test_that("single-word and single-object trials update as before", {
  train <- list(words = list(1, c(1, 2), 2), objects = list(c(1, 2), 1, 2))
  expect_equal(unname(suppressWarnings(run_rw(train, beta = .2))$matrix),
               unname(rw_reference(train, C = 1, beta = .2, lambda = 1)), tolerance = 1e-12)
})
