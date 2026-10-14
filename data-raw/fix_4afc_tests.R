# Correct the 4AFC test alternatives of conditions 211 and 214 in xsl_datasets.
#
# The four 4AFC conditions (211-214) carry a `test` listing, for each word,
# the 4 objects offered at test (the word's own object and 3 foils). Since a
# participant can only choose an object that was on screen, every response in
# the raw data (data-raw/data/asym_test_data.RData) must be one of the listed
# alternatives. Two conditions' stored tests violate this:
#
# - 211 (1x3 6/18 4AFC), word 17: listed as c(17, 16, 5, 17), with object 17
#   twice and so only 2 foils. Participants chose objects 2, 5, 16 and 17 --
#   all 4 alternatives -- so the missing foil is object 2 (4 of 43 responses).
#
# - 214 (3x4 6/8 4AFC): 175 of 648 responses, on all 18 words, are to objects
#   not among the listed alternatives (e.g. word 1 lists {1, 7, 9, 4}, but
#   participants chose 1, 4, 12 and 14). Every one of 214's responses is
#   instead among condition 213's listed alternatives for the same word, on
#   all 18 words (13 of which drew responses to all 4 of them), and no other
#   condition's test fits. So 214's participants were evidently tested with
#   213's test displays (the two conditions share 18 words / 18 objects and
#   both have 4AFC tests), and 214's stored test, whose source is unknown, was
#   never shown. 214's test is replaced with 213's. Correctness is coded as
#   response == word, so `accuracy` and `response_matrix` are unchanged.
#   214's training sequence is kept: its foil choices track co-occurrence in
#   214's training (r = .22) at least as well as in 213's (r = .17), as in
#   211-213 (r = .14-.22), so there is no sign that its training is mislabelled.
#
# The test spec is the same in every intermediate copy
# (data-raw/data/asymmetric_conditions.RData, data-raw/orders/...,
# data-raw/combined_data.rda), so those are left as they are and the
# correction is applied to xsl_datasets here, as in fix_filtering_tests.R.

devtools::load_all(".")

e <- new.env()
load("data-raw/data/asym_test_data.RData", e)
tdat <- e$tdat
resp_sets <- function(cond) {
  d <- tdat[tdat$condnum == as.numeric(cond), ]
  lapply(1:18, \(w) sort(unique(d$Response[as.integer(d$Word) == w])))
}
within <- function(resp, objs) all(mapply(\(r, o) all(r %in% o), resp, objs))

labels <- vapply(xsl_datasets, `[[`, character(1), "label")
i211 <- which(labels == "211")
i213 <- which(labels == "213")
i214 <- which(labels == "214")

# 211, word 17: replace the duplicated 17 with the missing foil, 2
obj <- xsl_datasets[[i211]]$test$objects
stopifnot(identical(as.numeric(obj[[17]]), c(17, 16, 5, 17)),
          identical(resp_sets("211")[[17]], c(2L, 5L, 16L, 17L)))
obj[[17]] <- c(17, 16, 5, 2)
xsl_datasets[[i211]]$test$objects <- obj
stopifnot(within(resp_sets("211"), xsl_datasets[[i211]]$test$objects))

# 214: its responses fit 213's test, and no other condition's (incl. its own)
r214 <- resp_sets("214")
fits <- vapply(xsl_datasets, \(d) length(d$test$objects) == 18 &&
                 within(r214, d$test$objects), logical(1))
stopifnot(identical(labels[fits], "213"))
xsl_datasets[[i214]]$test <- xsl_datasets[[i213]]$test

# every test-bearing condition's human responses now fall within its listed
# alternatives, and each item offers distinct objects
for (d in xsl_datasets) {
  if (length(d$test) == 0) next
  rm <- d$response_matrix
  for (k in seq_along(d$test$objects)) {
    o <- as.character(d$test$objects[[k]])
    stopifnot(!anyDuplicated(o), sum(rm[k, !colnames(rm) %in% o]) == 0)
  }
}

usethis::use_data(xsl_datasets, overwrite = TRUE)
