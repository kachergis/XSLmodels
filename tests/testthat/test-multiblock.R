# Multi-block data (training repeated, tested after each block) and the
# kachergis_priming trajectories.

d <- kachergis_priming[["4x4 3/6/9x"]]
labels <- vapply(xsl_datasets, `[[`, "", "label")
same_train <- function(a, b) {
  length(a$words) == length(b$words) &&
    all(mapply(setequal, a$words, b$words)) && all(mapply(setequal, a$objects, b$objects))
}

test_that("kachergis_priming is three complete 4-block conditions", {
  expect_named(kachergis_priming, c("4x4 6x", "4x4 3/6/9x", "3x3 3/6/9x"))
  expect_equal(unname(vapply(kachergis_priming, `[[`, 1, "n_subj")), c(54, 44, 66))
  for (x in kachergis_priming) {
    expect_equal(n_blocks(x), 4)
    expect_equal(dim(x$accuracy), c(4, 18))
    expect_length(x$response_matrix, 4)
    for (b in 1:4) {
      rm <- x$response_matrix[[b]]
      expect_true(all(rowSums(rm) == x$n_subj))
      expect_equal(unname(diag(rm) / rowSums(rm)), unname(x$accuracy[b, ]))
    }
    # people learn across blocks
    expect_true(all(diff(rowMeans(x$accuracy)) > 0))
  }
  # two sequences are xsl_datasets conditions'; the third is 223's but for
  # words 4 and 16 trading places between trials 11 and 24
  expect_true(same_train(kachergis_priming[["4x4 3/6/9x"]]$train, xsl_datasets[[which(labels == "3_x8_369_4x4")]]$train))
  expect_true(same_train(kachergis_priming[["3x3 3/6/9x"]]$train, xsl_datasets[[which(labels == "freq369-3x3loCD")]]$train))
  t223 <- xsl_datasets[[which(labels == "223")]]$train
  orig <- kachergis_priming[["4x4 6x"]]$train
  swap <- \(x) ifelse(x == 4, 16, ifelse(x == 16, 4, x))
  orig$words[c(11, 24)] <- lapply(orig$words[c(11, 24)], swap)
  orig$objects[c(11, 24)] <- lapply(orig$objects[c(11, 24)], swap)
  expect_true(same_train(orig, t223))
})

test_that("xslData validates multi-block accuracy and response matrices", {
  expect_error(xslData(train = d$train, accuracy = d$accuracy[, 1:17]))
  expect_error(xslData(train = d$train, accuracy = d$accuracy, response_matrix = d$response_matrix[1:3]))
  expect_s3_class(xslData(train = d$train, accuracy = d$accuracy, response_matrix = d$response_matrix), "xslData")
})

test_that("block b is scored on b repetitions of the training sequence", {
  mod <- uncfam(X = .2, B = 3, C = .97)
  f <- xsl_run(mod, d)$fits[[1]]
  expect_equal(dim(f$perf), c(4, 18))
  for (b in 1:4) {
    single <- xslData(train = list(words = rep(d$train$words, b), objects = rep(d$train$objects, b)),
                      accuracy = d$accuracy[b, ], n_subj = d$n_subj, response_matrix = d$response_matrix[[b]])
    expect_equal(f$perf[b, ], xsl_run(mod, single)$fits[[1]]$perf)
    expect_equal(f$blocks[[b]]$sse, xsl_run(mod, single)$sse)
  }
  expect_equal(f$sse, sum(vapply(f$blocks, `[[`, 1, "sse")))
  expect_equal(f$matrix, f$blocks[[4]]$matrix)
  # the same as training continuously: the model's own reps loop
  pos <- positional_train(d$train)
  r <- mod$model(mod$params, pos$train, xslControl(reps = 4))
  expect_equal(unname(r$perf), unname(f$perf), tolerance = 1e-12)
})

test_that("likelihoods sum over blocks", {
  for (obj in c("binomial", "multinomial")) {
    ctrl <- xslControl(objective = obj, lapse = .05)
    f <- xsl_run(decay(C = .9), d, ctrl)$fits[[1]]
    expect_equal(f$loglik, sum(vapply(f$blocks, `[[`, 1, "loglik")))
    expect_true(is.finite(f$loglik))
  }
  expect_equal(n_responses(d), 4 * 18 * 44)
})

test_that("multi-block data refuses control$reps, and choice_probabilities() needs a block", {
  expect_error(xsl_run(decay(C = .9), d, xslControl(reps = 2)), "leave control\\$reps at 1")
  f <- xsl_run(decay(C = .9), d)$fits[[1]]
  expect_error(choice_probabilities(f$matrix, d), "say which with `block`")
  ch <- choice_probabilities(f$blocks[[2]]$matrix, d, block = 2)
  expect_equal(ch$counts[[1]], unname(d$response_matrix[[2]][1, ]))
})

test_that("models can be fit to a learning trajectory", {
  fit <- xsl_fit(decay(C = .98), kachergis_priming, lower = .8, upper = 1, seed = 1,
                 control = xslControl(objective = "binomial"),
                 deoptim_control = DEoptim::DEoptim.control(NP = 10, itermax = 3, trace = FALSE))[[1]]
  expect_true(is.finite(fit$optim$bestval))
  expect_equal(fit$n_obs, sum(vapply(kachergis_priming, \(x) 72 * x$n_subj, 1)))
})

test_that("xslData objects print a summary, with the blocks when there are several", {
  single <- xsl_datasets[[1]]
  out <- capture.output(print(single))
  expect_match(out[1], single$label, fixed = TRUE)
  expect_match(out, paste("training trials:", length(single$train$words)), all = FALSE)
  expect_false(any(grepl("blocks", out)))
  expect_match(capture.output(print(d)), "blocks: 4 \\(training repeated", all = FALSE)
})
