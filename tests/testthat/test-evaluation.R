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

test_that("get_perf() treats negative associations as 0 and answers empty rows at chance", {
  m <- matrix(c( 1, -1,  0,
                -2,  1,  1,
                -1, -1, -1), 3, byrow = TRUE)
  p <- get_perf(m)
  expect_equal(p, c(1, .5, 1 / 3))
  expect_true(all(p >= 0 & p <= 1))
})

test_that("mafc_test() scores an explicit correct referent when given", {
  m <- matrix(c(.6, .3, .1,
                .2, .5, .3,
                .1, .1, .8), 3, byrow = TRUE, dimnames = list(1:3, 1:3))
  test <- list(words = list(1, 1), objects = list(c(1, 3), c(2, 3)))
  expect_equal(mafc_test(m, test), c(.6 / .7, NA))
  test$correct <- list(1, 2)
  expect_equal(mafc_test(m, test), c(.6 / .7, .3 / .4))
  expect_error(validate_xslData(new_xslData(list(train = list(), test = list(words = list(1), objects = list(1), correct = list(1, 2)),
                                                  accuracy = numeric()))))
})

test_that("predict_referent() thresholds the lexicon before choice", {
  m <- matrix(c(.6, .4, .2, .8), 2, byrow = TRUE, dimnames = list(1:2, 1:2))
  expect_equal(predict_referent(m, 1, 1:2, threshold = .5), c(1, 0))
  # nothing above threshold: the word carries no evidence, so the prior
  expect_equal(predict_referent(m, 1, 1:2, threshold = .9, prior = c(1, 3)), c(.25, .75))
  # a power choice rule sharpens the literal listener
  expect_equal(predict_referent(m, 1, 1:2, d = 2), c(.36, .16) / .52)
})

test_that("the pragmatic listener handles a word with no associations", {
  # word 2's row is empty, so it isn't among the speaker's candidate words;
  # the listener falls back to the prior over the trial's objects
  m <- matrix(c(1, 0, 0, 0), 2, byrow = TRUE, dimnames = list(1:2, c("a", "b")))
  expect_equal(predict_referent(m, 2, c("a", "b"), pragmatic = TRUE), c(.5, .5))
  expect_equal(predict_referent(m, 2, c("a", "b"), prior = c(1, 3), pragmatic = TRUE), c(.25, .75))
})

# a 3 x 3 lexicon whose labels aren't in positional order
lex_m <- diag(3)
dimnames(lex_m) <- list(c("3", "1", "2"), c("3", "1", "2"))
lex_m["1", "2"] <- .5

test_that("get_tp() sums the gold pairs' associations, by label", {
  expect_equal(get_tp(lex_m, list(words = 1:3, objects = 1:3)), 3)
  # numeric ids are labels, not positions: m[1, 2] is the "3"-"1" cell (0)
  expect_equal(get_tp(lex_m, list(words = 1, objects = 2)), .5)
  expect_equal(get_tp(lex_m, list(words = "1", objects = "2")), .5)
  # pairs not in the matrix are skipped
  expect_equal(get_tp(lex_m, list(words = c(1, 9), objects = c(1, 1))), 1)
  # without a lexicon, every word whose label is also an object's is gold
  expect_equal(get_tp(lex_m, NULL), 3)
  expect_equal(get_tp(lex_m[, c("1", "2")], NULL), 2)
})

test_that("get_fscore() counts hits, false alarms and misses at a threshold", {
  gold <- list(words = 1:3, objects = 1:3)
  # at .4: the diagonal plus "1"-"2" -> 3 hits, 1 false alarm, no misses
  f <- get_fscore(lex_m, .4, gold)
  expect_equal(f$precision, 3 / 4)
  expect_equal(f$recall, 1)
  expect_equal(f$specificity, 5 / 6)
  expect_equal(f$fscore, 2 * .75 / 1.75)
  # at .6 the false alarm is gone
  expect_equal(get_fscore(lex_m, .6, gold)$precision, 1)
  # nothing selected: precision is undefined and the F-score is 0
  none <- get_fscore(lex_m, 2, gold)
  expect_equal(none$recall, 0)
  expect_equal(none$fscore, 0)
  # numeric ids as labels: a lexicon with only the "1"-"2" pair
  one <- get_fscore(lex_m, .4, list(words = 1, objects = 2))
  expect_equal(c(one$precision, one$recall), c(1, 1))
  # no lexicon: the same as the identity lexicon here
  expect_equal(get_fscore(lex_m, .4), f)
})

test_that("get_roc() and get_roc_max() sweep thresholds over the row-normalized matrix", {
  roc <- get_roc(lex_m, thresholds = c(0, .5, 1))
  expect_equal(roc$threshold, c(0, .5, 1))
  expect_equal(roc$fscore[2], get_fscore(lex_m / rowSums(lex_m), .5)$fscore)
  # at 0 every cell is selected: all hits, specificity 0
  expect_equal(c(roc$recall[1], roc$specificity[1]), c(1, 0))
  # a perfectly learned lexicon reaches an F-score of 1
  expect_equal(get_roc_max(diag(4) |> `dimnames<-`(list(1:4, 1:4))), 1)
  expect_lte(get_roc_max(lex_m), 1)
  expect_gt(get_roc_max(lex_m), .8)
})

test_that("get_roc() treats a word with no associations as selecting nothing", {
  # regression test: an all-zero row was NaN after row normalization, making
  # every threshold's scores NA and get_roc_max() -Inf
  z <- diag(3)
  z[3, 3] <- 0
  dimnames(z) <- list(1:3, 1:3)
  roc <- get_roc(z, thresholds = .5)
  expect_false(anyNA(roc))
  expect_equal(c(roc$precision, roc$recall), c(1, 2 / 3))
  expect_equal(get_roc_max(z), 2 * (2 / 3) / (1 + 2 / 3))
})

test_that("plot_roc() plots recall against the false-alarm rate", {
  p <- plot_roc(lex_m, thresholds = c(0, .5, 1))
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 3)
  b <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(sort(b$x), sort(1 - get_roc(lex_m, c(0, .5, 1))$specificity))
})

test_that("choice probabilities without a test give every object, at chance for an empty row", {
  m <- matrix(c(2, 1, 1, 0, 0, 0), 2, byrow = TRUE, dimnames = list(1:2, 1:3))
  d <- xslData(train = list(words = list(1:2), objects = list(1:3)), accuracy = c(.5, .5), n_subj = 4)
  ch <- item_choices(m, d)
  expect_equal(ch$correct, c("1", "2"))
  expect_equal(ch$probs, list(c(.5, .25, .25), rep(1 / 3, 3)))
  expect_equal(item_choices(m, d, d = 2)$probs[[1]], c(4, 1, 1) / 6)
})

test_that("response counts must fall among an item's test alternatives", {
  d <- xslData(train = list(words = list(1:2), objects = list(1:2)),
               test = list(words = list(1, 2), objects = list(1:2, 2)),
               accuracy = c(.5, 1), n_subj = 2,
               response_matrix = matrix(c(1, 1, 1, 1), 2, dimnames = list(1:2, 1:2)))
  # item 2 offers only object 2, but one response chose object 1
  m <- diag(2) |> `dimnames<-`(list(1:2, 1:2))
  expect_error(choice_probabilities(m, d), "item 2: response_matrix has responses to objects")
})

test_that("n_responses() falls back to n_subj x items without a response_matrix", {
  d1 <- xslData(train = list(words = list(1:2), objects = list(1:2)), accuracy = c(.5, .5), n_subj = 10)
  d2 <- xsl_datasets[[1]]
  expect_equal(n_responses(d1), 20)
  expect_equal(n_responses(list(d1, d2)), 20 + sum(d2$response_matrix))
})
