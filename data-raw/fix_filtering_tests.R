# Give the 12 filtering conditions (Kachergis, Yu, & Shiffrin, 2012; filt*E_*L)
# the test format participants actually had.
#
# In these designs every word has a partner: each late word always appears
# with one particular early word (or, in filt0E_*, with one other late word),
# so a word co-occurs with its own object and its partner's object equally
# often. The experiment tested two things separately: a word-to-object test
# ("AtoA", the accuracy in xsl_datasets) and a word-to-partner's-object test
# ("AtoB"). In the raw data (data-raw/data/filtering_data.RData), across all
# 4152 AtoA tests no participant ever chose the tested word's partner object,
# although every other object was chosen; in AtoB tests the partner object is
# the correct answer. So the AtoA test did not offer the partner object: it
# was an 11-alternative choice among the other objects.
#
# xsl_datasets previously had no test specification for these conditions, so
# they were scored with get_perf() over all 12 objects, counting a model's
# association with the partner object (unavailable to participants) against
# it, and capping filt0E_* -- where the two pairs are symmetric -- at .5. This
# adds a test specification (scored by mafc_test()) offering each word its own
# object plus the 10 objects that are neither it nor its partner.

devtools::load_all(".")

e <- new.env()
load("data-raw/data/filtering_data.RData", e)
raw <- e$raw

ix <- which(vapply(xsl_datasets, \(d) grepl("^filt", d$label), logical(1)))
stopifnot(length(ix) == 12)

for (i in ix) {
  d <- xsl_datasets[[i]]
  W <- lapply(d$train$words, \(x) unlist(x, use.names = FALSE))
  words <- sort(unique(unlist(W)))
  stopifnot(identical(as.numeric(words), as.numeric(1:12)), length(d$accuracy) == 12)
  # a word's partner: the only word a late word ever appears with; an early
  # word's partner is the late word that appears only with it
  cooc <- lapply(words, \(w) unique(unlist(lapply(W[vapply(W, \(x) w %in% x, logical(1))], \(x) setdiff(x, w)))))
  partner <- vapply(seq_along(words), function(k) {
    if (length(cooc[[k]]) == 1) return(as.numeric(cooc[[k]]))
    as.numeric(words[vapply(cooc, \(co) length(co) == 1 && co == words[k], logical(1))])
  }, numeric(1))
  stopifnot(identical(partner, as.numeric(c(7:12, 1:6))))
  d$test <- list(
    words = as.list(as.numeric(words)),
    objects = lapply(seq_along(words), \(k) as.numeric(setdiff(words, partner[k])))
  )
  xsl_datasets[[i]] <- d
}

# the raw data agree: the partner object is never chosen on AtoA tests, and
# is the correct answer on AtoB tests (except 48 filt0E AtoB rows coded with
# nonexistent objects 13-18)
partner_of <- function(w) ifelse(w <= 6, w + 6, w - 6)
atob <- raw[!raw$AtoA & raw$CorrectAns <= 12, ]
stopifnot(sum(raw$AtoA & raw$Response == partner_of(raw$Word)) == 0,
          sum(!raw$AtoA & raw$CorrectAns > 12) == 48,
          all(atob$CorrectAns == partner_of(atob$Word)))

usethis::use_data(xsl_datasets, overwrite = TRUE)
