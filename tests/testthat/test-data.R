test_that("rollins_corpus has the expected structure", {
  expect_s3_class(rollins_corpus$data, "xslData")
  expect_length(rollins_corpus$data$train$words, 619)
  expect_length(rollins_corpus$data$train$objects, 619)
  expect_length(rollins_corpus$data$accuracy, 0)          # no human data
  expect_equal(length(unique(unlist(rollins_corpus$data$train$words))), 416)
  expect_type(rollins_corpus$data$train$words[[1]], "character")

  expect_named(rollins_corpus$gold, c("words", "objects"))
  expect_equal(length(rollins_corpus$gold$words),
               length(rollins_corpus$gold$objects))
  expect_length(rollins_corpus$gold$words, 34)
})

test_that("fm_corpus has the expected structure", {
  expect_s3_class(fm_corpus$data, "xslData")
  expect_length(fm_corpus$data$train$words, 4763)
  expect_length(fm_corpus$intents, 4763)
  expect_type(fm_corpus$intents[[1]], "character")
  # roughly half the utterances are non-referential (empty coded intent)
  expect_gt(mean(lengths(fm_corpus$intents) == 0), 0.4)

  expect_length(fm_corpus$gold$words, 41)
  expect_length(fm_corpus$gold_variants$strict$words, 39)
  expect_length(fm_corpus$gold_variants$permissive$words, 116)
})

test_that("a model runs on the corpora and scores against the gold lexicon", {
  m <- suppressWarnings(
    xsl_run(baseline(), rollins_corpus$data)$fits[[1]]$matrix)
  expect_equal(dim(m), c(416, 22))
  expect_setequal(rownames(m),
                  as.character(sort(unique(unlist(rollins_corpus$data$train$words)))))

  f <- get_roc_max(m, gold_lexicon = rollins_corpus$gold)
  expect_true(is.finite(f) && f > 0 && f <= 1)

  # gold words / objects absent from the matrix must not break scoring
  expect_s3_class(get_fscore(m / rowSums(m), 0.1, rollins_corpus$gold),
                  "data.frame")
})
