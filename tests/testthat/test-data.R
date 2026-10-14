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

test_that("filtering conditions test both associations of each word (AtoA and AtoB)", {
  # Kachergis, Yu & Shiffrin (2012): word w co-occurs with its own object and
  # its partner's object; it was tested once with the partner object absent
  # (AtoA, own object correct) and once with its own object absent (AtoB,
  # partner object correct). See data-raw/fix_filtering_tests.R
  filt <- Filter(\(d) grepl("^filt", d$label), xsl_datasets)
  expect_length(filt, 12)
  partner <- c(7:12, 1:6)
  for (d in filt) {
    expect_length(d$accuracy, 24)
    expect_equal(unlist(d$test$words), c(1:12, 1:12))
    expect_equal(unlist(d$test$correct), c(1:12, partner))
    for (k in 1:24) {
      w <- d$test$words[[k]]
      os <- d$test$objects[[k]]
      expect_length(os, 11)
      expect_true(d$test$correct[[k]] %in% os)
      expect_false((if (k <= 12) partner[w] else w) %in% os)
    }
  }
  # the 6L conditions exclude the 2 participants with shifted test coding
  expect_true(all(sapply(Filter(\(d) grepl("_6L$", d$label), filt), `[[`, "n_subj") == 23))
  # AtoA scoring excludes the partner: higher than 12-alternative Luce choice
  d <- filt[[which(sapply(filt, `[[`, "label") == "filt3E_6L")]]
  fit <- xsl_run(uncfam(X = .1, B = 3, C = .97), d)$fits[[1]]
  expect_length(fit$perf, 24)
  expect_true(all(fit$perf[1:12] > get_perf(fit$matrix)))
})

test_that("response matrices reproduce each condition's accuracy (one response per participant per item)", {
  with_rm <- Filter(\(d) !is.null(d$response_matrix), xsl_datasets)
  expect_equal(length(with_rm), length(xsl_datasets))
  for (d in with_rm) {
    m <- d$response_matrix
    objs <- sort(unique(unlist(lapply(d$train$objects, \(x) unlist(x, use.names = FALSE)))))
    expect_equal(nrow(m), length(d$accuracy), info = d$label)
    expect_equal(as.numeric(colnames(m)), as.numeric(objs), info = d$label)
    expect_true(all(rowSums(m) == d$n_subj), info = d$label)
    correct <- if (length(d$test) > 0) {
      if (is.null(d$test$correct)) unlist(d$test$words) else unlist(d$test$correct)
    } else seq_along(d$accuracy)
    acc <- m[cbind(seq_len(nrow(m)), match(correct, objs))] / rowSums(m)
    expect_equal(unname(acc), unname(d$accuracy), info = d$label)
  }
})

test_that("no two conditions duplicate each other (same training and same accuracy)", {
  key <- vapply(xsl_datasets, function(d) {
    trials <- mapply(\(w, o) paste(paste(unlist(w), collapse = ","), paste(unlist(o), collapse = ","), sep = "|"),
                     d$train$words, d$train$objects)
    paste(paste(trials, collapse = ";"), paste(signif(d$accuracy, 8), collapse = ","))
  }, character(1))
  expect_false(any(duplicated(key)))
})

test_that("frequency / contextual-diversity conditions have their source sample sizes", {
  n <- setNames(vapply(xsl_datasets, \(d) as.numeric(d$n_subj), 1), vapply(xsl_datasets, `[[`, "", "label"))
  expect_equal(unname(n[c("2_x8_39_4x4", "3_x8_369_4x4", "freq369-3x3loCD", "freq369-3x3hiCD",
                          "freq369_36mx", "freq369_39mx", "cont_div6-12")]), c(30, 30, 34, 104, 66, 40, 62))
  expect_false(any(c("orig_3x3", "orig_4x4") %in% names(n)))
  expect_length(xsl_datasets, 47)
})

test_that("condition-mean-only datasets are held out of xsl_datasets", {
  labs <- vapply(xsl_datasets, `[[`, "", "label")
  expect_false(any(grepl("^Suanda2014|^Koehne2013", labs)))
  expect_length(suanda2014, 3)
  expect_length(koehne2013, 4)
  expect_true(all(c("suanda2014", "koehne2013") %in% names(xsl_holdout_datasets)))
  for (d in c(suanda2014, koehne2013)) expect_length(unique(d$accuracy), 1)
})

test_that("every human test response is to one of the item's listed alternatives", {
  # regression test: condition 214's listed 4AFC alternatives were not the
  # ones shown (175 of 648 responses fell outside them), and 211's word 17
  # listed object 17 twice in place of foil 2 (see data-raw/fix_4afc_tests.R)
  for (d in Filter(\(d) length(d$test) > 0, xsl_datasets)) {
    m <- d$response_matrix
    for (k in seq_along(d$test$objects)) {
      o <- as.character(d$test$objects[[k]])
      expect_false(anyDuplicated(o) > 0, info = paste(d$label, "item", k))
      expect_equal(sum(m[k, !colnames(m) %in% o]), 0, info = paste(d$label, "item", k))
    }
  }
})
