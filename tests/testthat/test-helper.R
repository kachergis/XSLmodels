test_that("update_known() fills startval into the correct familiar cell", {
  # regression test: update_known() used to compute which(m[w, fam_objects]
  # == 0) -- positions *within* the fam_objects-filtered subset -- and then
  # index m[w, zeros] as if those were absolute column positions, silently
  # filling in the wrong cell whenever fam_objects was a proper subset of
  # all columns (i.e. not every object was familiar yet). With 5 objects,
  # only 2 of which ("w", "x") are familiar before this trial, the old code
  # mapped subset-positions 1:2 back to absolute columns 1:2 ("v", "w")
  # instead of the actual familiar columns ("w", "x") -- incorrectly
  # touching the wholly-unfamiliar "v" while missing "x".
  m <- matrix(0, 3, 5, dimnames = list(c("bunny", "dog", "cat"),
                                        c("v", "w", "x", "y", "z")))
  m["dog", "w"] <- 0.5   # makes "w" a familiar object
  m["cat", "x"] <- 0.5   # makes "x" a familiar object
  # "bunny" is new; this trial pairs it with "y"
  m2 <- XSLmodels:::update_known(m, tr_w = "bunny", tr_o = "y")
  expect_equal(m2["bunny", "v"], 0)     # "v" is wholly unfamiliar -- must stay untouched
  expect_equal(m2["bunny", "w"], 0.01)  # familiar object, never paired with bunny
  expect_equal(m2["bunny", "x"], 0.01)  # familiar object, never paired with bunny
  expect_equal(m2["bunny", "y"], 0.01)  # the trial's own direct pairing
  expect_equal(m2["bunny", "z"], 0)     # "z" is wholly unfamiliar -- must stay untouched
})

test_that("update_known() only fills zero cells, never overwrites learned values", {
  m <- matrix(0, 3, 3, dimnames = list(c("a", "b", "c"), c("x", "y", "z")))
  m["a", "x"] <- 0.5
  m["b", "y"] <- 0.3
  m2 <- XSLmodels:::update_known(m, tr_w = "a", tr_o = "x")
  expect_equal(m2["a", "x"], 0.5)  # untouched, already nonzero
  expect_equal(m2["b", "y"], 0.3)  # untouched, not in this trial
})

test_that("shannon_entropy() matches its original (uncached) formula", {
  old_entropy <- function(p) {
    if (min(p) < 0 || sum(p) <= 0) return(NA)
    p_norm <- p[p > 0] / sum(p)
    -sum(log2(p_norm) * p_norm)
  }
  set.seed(1)
  for (i in 1:200) {
    n <- sample(1:15, 1)
    p <- runif(n, -0.5, 5)
    if (runif(1) < 0.2) p[sample(n, sample(1:n, 1))] <- 0
    a <- old_entropy(p)
    b <- XSLmodels:::shannon_entropy(p)
    if (is.na(a)) {
      expect_true(is.na(b))
    } else {
      expect_equal(a, b)
    }
  }
  expect_true(is.na(XSLmodels:::shannon_entropy(c(0, 0, 0))))
  expect_true(is.na(XSLmodels:::shannon_entropy(c(-1, 2, 3))))
  expect_equal(XSLmodels:::shannon_entropy(c(1, 0, 0)), 0)
})
