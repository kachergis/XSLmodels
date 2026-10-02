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

test_that("vlach_debrock2017 has the expected structure", {
  expect_s3_class(vlach_debrock2017, "xslData")
  expect_length(vlach_debrock2017$train$words, 36)
  expect_equal(length(unique(unlist(vlach_debrock2017$train$words))), 12)
  expect_equal(length(unique(unlist(vlach_debrock2017$train$objects))), 12)
  expect_true(all(is.na(vlach_debrock2017$accuracy)))  # no per-word accuracy available
  expect_length(vlach_debrock2017$accuracy, 12)
  expect_equal(vlach_debrock2017$n_subj, 47)
  # each of the 12 pairs appears in exactly 6 of the 36 trials
  expect_true(all(table(unlist(vlach_debrock2017$train$words)) == 6))

  m <- xsl_run(baseline(), vlach_debrock2017)$fits[[1]]$matrix
  expect_equal(dim(m), c(12, 12))
})

test_that("benitez2020 is a standalone dataset with real per-word accuracy, not in xsl_datasets", {
  expect_false(any(grepl("^Benitez2020", sapply(xsl_datasets, function(d) d$label))))

  expect_named(benitez2020, c("Interleaved, kids", "Interleaved, adults",
                              "Massed, kids", "Massed, adults",
                              "Unstructured (order 1), kids",
                              "Unstructured (order 2/3), kids",
                              "Unstructured (order 2/3), adults"))

  d <- benitez2020[["Interleaved, kids"]]
  expect_s3_class(d, "xslData")
  expect_length(d$train$words, 12)
  expect_equal(length(unique(unlist(d$train$words))), 8)
  expect_length(d$accuracy, 8)
  expect_false(any(is.na(d$accuracy)))      # real per-word data, not an estimate
  expect_true(all(d$accuracy >= 0 & d$accuracy <= 1))
  expect_equal(dim(d$response_matrix), c(8, 8))
  expect_equal(d$n_subj, 46)

  m <- xsl_run(baseline(), d)$fits[[1]]$matrix
  mt <- mafc_test(m, d$test)
  expect_length(mt, 8)
  expect_true(all(mt >= 0 & mt <= 1))
})

test_that("xsl_holdout_datasets registers every standalone dataset", {
  expect_true(all(c("benitez2020", "kachergis_initial_accuracy", "vlach_debrock2017",
                    "kachergis2012_highlighting", "gangwani2011_category",
                    "rollins_corpus", "fm_corpus") %in% names(xsl_holdout_datasets)))
  for (entry in xsl_holdout_datasets) {
    expect_type(entry$scorable, "logical")
    expect_type(entry$scoring, "character")
  }
  # none of the registered names should also be in xsl_datasets
  expect_false(any(names(xsl_holdout_datasets) %in% sapply(xsl_datasets, function(d) d$label)))
})

test_that("the 3 temporal-contiguity overlap-contr conditions are in xsl_datasets", {
  labels <- sapply(xsl_datasets, function(d) d$label)
  tc_labels <- c("1olap3tr_contr", "temp_cont_1olap2tr_contr", "temp_cont_2olap2tr_contr")
  expect_true(all(tc_labels %in% labels))

  for (lab in tc_labels) {
    d <- xsl_datasets[[which(labels == lab)]]
    expect_length(d$train$words, 27)
    expect_equal(length(unique(unlist(d$train$words))), 18)
    expect_length(d$accuracy, 18)
    expect_false(any(is.na(d$accuracy)))
    expect_equal(d$n_subj, 31)
  }

  m <- xsl_run(baseline(), xsl_datasets[[which(labels == "1olap3tr_contr")]])$fits[[1]]$matrix
  expect_equal(dim(m), c(18, 18))
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
