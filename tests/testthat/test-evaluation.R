test_that("rsa_listener() matches the reference RSA recursion", {
  # ported from wordlearn/rsa.py; these are its test_rsa.py assertions.
  amb <- matrix(c(1, 0, 1, 1), 2, byrow = TRUE)   # w2 names either object

  # depth 0: literal listener, a word true of both objects is 50/50
  L0 <- rsa_listener(amb, c(0.5, 0.5), depth = 0)
  expect_equal(L0[2, ], c(0.5, 0.5))

  # depth 1: pragmatic listener resolves the ambiguous word to the novel object
  L1 <- rsa_listener(amb, c(0.5, 0.5), alpha = 3, depth = 1)
  expect_gt(L1[2, 2], 0.7)

  # an object with prior 0 (no epistemic access) gets probability exactly 0
  epi <- matrix(c(1, 0, 0, 1, 1, 1), 2, byrow = TRUE)
  Le <- rsa_listener(epi, c(1, 1, 0), alpha = 3, depth = 1)
  expect_equal(Le[2, 3], 0)
  expect_equal(sum(Le[2, ]), 1)

  # the speaker distribution is normalized over words for each reachable object
  S <- rsa_speaker(amb, c(0.5, 0.5), alpha = 3, depth = 1)
  expect_equal(unname(colSums(S)), c(1, 1))
})

test_that("predict_referent() literal mode is Bayes' rule on the word's row", {
  m <- matrix(c(0.8, 0.1, 0.1,
                0.2, 0.6, 0.2,
                0.1, 0.1, 0.9), 3, byrow = TRUE,
              dimnames = list(c("1", "2", "3"), c("1", "2", "3")))

  p <- predict_referent(m, 1, c(1, 2, 3))
  expect_equal(p, (m[1, ] * 1) / sum(m[1, ]), ignore_attr = TRUE)
  expect_equal(sum(p), 1)

  # order of `objects` is respected
  p_rev <- predict_referent(m, 1, c(3, 2, 1))
  expect_equal(rev(p_rev), p, ignore_attr = TRUE)

  # non-uniform prior; a 0 entry removes that object entirely
  p_mask <- predict_referent(m, 2, c(1, 2, 3), prior = c(1, 0, 1))
  expect_equal(p_mask[2], 0)
  expect_equal(sum(p_mask), 1)
})

test_that("predict_referent() degrades gracefully on unknown words and objects", {
  m <- matrix(c(0.9, 0.1, 0.1, 0.9), 2, byrow = TRUE,
              dimnames = list(c("1", "2"), c("1", "2")))

  # a novel word carries no lexical evidence -> returns the prior
  expect_equal(predict_referent(m, 99, c(1, 2)), c(0.5, 0.5))
  expect_equal(predict_referent(m, "wug", c(1, 2), prior = c(3, 1)), c(0.75, 0.25))

  # a novel object is unnamed -> zero weight under the literal rule
  p <- predict_referent(m, 1, c(1, 99))
  expect_equal(unname(p), c(1, 0))
})

test_that("predict_referent() validates its inputs", {
  m <- matrix(1, 2, 2, dimnames = list(c("1", "2"), c("1", "2")))
  expect_error(predict_referent(m, 1, integer(0)))
  expect_error(predict_referent(m, 1, c(1, 2), prior = c(1, 2, 3)))
  expect_error(predict_referent(m, 1, c(1, 2), prior = c(0, 0)))
  expect_error(predict_referent(m, 1, c(1, 2), prior = c(-1, 2)))
})

test_that("predict_referent(pragmatic = TRUE) yields mutual exclusivity", {
  # "ball" names the familiar object; "wug" is lexically ambiguous (names the
  # familiar object AND a novel one). A pragmatic listener infers the speaker
  # would have said "ball" for the familiar object, so "wug" means the novel.
  m <- matrix(c(1, 0, 1, 1), 2, byrow = TRUE,
              dimnames = list(c("ball", "wug"), c("fam", "novel")))

  expect_equal(predict_referent(m, "wug", c("fam", "novel")), c(0.5, 0.5))
  prag <- predict_referent(m, "wug", c("fam", "novel"), pragmatic = TRUE)
  expect_gt(prag[2], 0.7)
  # the unambiguous word is unaffected
  expect_equal(
    predict_referent(m, "ball", c("fam", "novel"), pragmatic = TRUE),
    c(1, 0))
})

test_that("mafc_test() is unchanged and now delegates to predict_referent()", {
  dat <- xsl_datasets[[10]]
  m <- xsl_run(baseline(), dat)$fits[[1]]$matrix

  old <- vapply(seq_along(dat$test$words), function(i) {
    w <- dat$test$words[[i]]
    m[w, w] / sum(m[w, dat$test$objects[[i]]])
  }, numeric(1))
  expect_equal(mafc_test(m, dat$test), old, ignore_attr = TRUE)

  # extra args pass through to predict_referent()
  expect_length(mafc_test(m, dat$test, pragmatic = TRUE), length(dat$test$words))
})

test_that("predict_referent() works on any model's matrix, not just fgt2009", {
  dat <- get_example_unambiguous_condition()
  for (mod in list(baseline(), decay(C = 0.98),
                   rescorla_wagner(C = 1, alpha = 0.1, beta = 0.1, lambda = 1))) {
    m <- xsl_run(mod, dat)$fits[[1]]$matrix
    p <- predict_referent(m, 1, seq_len(ncol(m)))
    expect_equal(sum(p), 1)
    expect_true(all(p >= 0))
  }
})
