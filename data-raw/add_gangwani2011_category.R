# Add Experiment 1 of Gangwani, Kachergis & Yu, "Simultaneous Cross-situational
# Learning of Category and Object Names" (CogSci submission; an undergraduate-
# journal version is Gangwani & Kachergis, IUJCS 5, 2010) as its own package
# dataset `gangwani2011_category`.
#
# ---------------------------------------------------------------------------
# WHY THIS SHIPS STANDALONE (not appended to xsl_datasets)
# ---------------------------------------------------------------------------
# This is a *hierarchical* cross-situational design. On every training trial
# the learner sees 2 objects and hears 3 words: the two objects' own 1-to-1
# names PLUS one 1-to-many "category" label that names both visible objects and
# two further objects seen on other trials. So each object is part of a 2-to-1
# word->referent mapping (a basic name and a category label), and the whole
# point of the paper is that learners violate mutual exclusivity -- they learn
# BOTH labels for the same object.
#
# That breaks two xsl_datasets conventions at once:
#   * the association matrix is 15 words x 12 objects (non-square): the 3
#     category labels (words 13-15) have no same-index object, so `get_perf()`
#     / `mafc_test()` (which score via the diagonal `m[w, w]`) cannot produce a
#     model number for them.
#   * `xsl_run()`'s built-in SSE (`sum((perf - accuracy)^2)`) therefore can't
#     consume this directly. Score it with `score_gangwani2011_category()` in
#     tests/bakeoff_comparison/gangwani2011_category_fit.R instead.
# Same reasoning, and same standalone treatment, as `kachergis2012_highlighting`.
#
# The two *natural*-category findings in the paper -- the block-2 feature-
# similarity effect and generalization to novel objects -- are out of scope for
# every (amodal, index-based) model in this package and are not represented
# here; only the co-occurrence structure, which is identical across all three
# blocks, is modelable.
#
# ---------------------------------------------------------------------------
# SOURCES (a sibling checkout of github.com/kachergis/category_xsl)
# ---------------------------------------------------------------------------
#   ../../category_xsl/category_xsl_order.txt
#       36 training trials, one per row: "obj_i  obj_j  cat", where cat is
#       13/14/15 for category a/b/c (objects 1-4 / 5-8 / 9-12). The two 1-to-1
#       names on a trial are implicit (the names of obj_i and obj_j). This
#       matches the paper's Figure 2 co-occurrence matrix: each object appears
#       6x, each within-category object pair 2x, each category label 6x with
#       each of its 4 members. (Two cells are off by one -- object 3 appears
#       7x and object 4 5x, from an apparent single-trial transcription slip in
#       the order file; left as-is, models are insensitive to it. The E-Prime
#       StudyList is Order=0, i.e. a fixed sequence, so this is taken to be the
#       schedule every participant saw; if the run-time program instead
#       randomised trial order per subject, this is one representative order --
#       which only matters for strongly order-sensitive sequential models.)
#   ../../category_xsl/analysis/data-tidy/category_xsl_trials.csv
#       tidy trial-level test data (see that repo's analysis/README.md). We use
#       the canonical-block-order participants (n = 34/block; the paper reports
#       33 -- it additionally drops one low-performing subject).
#
# ---------------------------------------------------------------------------
# REPRODUCTION CHECK (asserted below)
# ---------------------------------------------------------------------------
# Block-mean accuracy, this build vs. the paper:
#   1-to-1 (names):     .52 / .35 / .39   (paper: .52 / .35 / "3x chance")
#   1-to-many (labels): .49 / .60 / .58   (paper: .49 / .60 / .57)
#   generalization (block 2): .53         (paper: .53)  -- not imported, checked only
#
# Run with data-raw/ as the working directory.

devtools::load_all("..")
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

cat_dir <- "../../category_xsl"
stopifnot(dir.exists(cat_dir))

# ---- 1. Training order -> a single shared 36-trial schedule --------------
# words 1-12  = the 12 object names (name i <-> object i)
# words 13-15 = the 3 category labels (13->{1,2,3,4}, 14->{5,6,7,8}, 15->{9,10,11,12})
ord <- read.table(file.path(cat_dir, "category_xsl_order.txt"),
                  header = FALSE, fill = TRUE, strip.white = TRUE,
                  col.names = c("o1", "o2", "cat"))
ord <- ord[stats::complete.cases(ord), ]
stopifnot(nrow(ord) == 36, all(ord$cat %in% 13:15))

category_members <- list(`13` = 1:4, `14` = 5:8, `15` = 9:12)
# every trial's two objects must belong to the category named on that trial
stopifnot(all(mapply(function(a, b, c) all(c(a, b) %in% category_members[[as.character(c)]]),
                     ord$o1, ord$o2, ord$cat)))

train <- list(
  words   = lapply(seq_len(nrow(ord)), \(i) c(ord$o1[i], ord$o2[i], ord$cat[i])),
  objects = lapply(seq_len(nrow(ord)), \(i) c(ord$o1[i], ord$o2[i]))
)
stopifnot(length(unique(unlist(train$words)))   == 15,
          length(unique(unlist(train$objects))) == 12)

# ---- 2. Human test data --------------------------------------------------
trials <- readr::read_csv(file.path(cat_dir, "analysis/data-tidy/category_xsl_trials.csv"),
                          show_col_types = FALSE) |>
  filter(experiment == "exp1_3x2", canonical_order, cat_type != "greebles")

cat_label_of <- c(a = 13, b = 14, c = 15)   # member_of -> category-label word index

# per-block accuracy vector, length 15: names (P correct object | name),
# then category labels (P chose an in-category object | label)
block_accuracy <- function(block) {
  d <- filter(trials, block_pos == block)

  name_acc <- d |>
    filter(test_type == "1to1") |>
    summarise(acc = mean(correct), .by = correct_ans) |>
    arrange(correct_ans) |>
    pull(acc)
  stopifnot(length(name_acc) == 12)

  cat_acc <- d |>
    filter(test_type == "1tomany") |>
    summarise(acc = mean(correct), .by = member_of) |>
    mutate(w = cat_label_of[member_of]) |>
    arrange(w) |>
    pull(acc)
  stopifnot(length(cat_acc) == 3)

  c(name_acc, cat_acc)
}

# per-block human response matrix, 15 words x 12 objects, row-normalised:
# rows 1-12  P(chose object k | heard name i)      from 1-to-1 test trials
# rows 13-15 P(chose object k | heard category L)  from 1-to-many test trials
block_response_matrix <- function(block) {
  d <- filter(trials, block_pos == block)
  m <- matrix(0, 15, 12, dimnames = list(1:15, 1:12))

  n1 <- filter(d, test_type == "1to1")
  for (i in seq_len(nrow(n1))) m[n1$correct_ans[i], n1$response[i]] <- m[n1$correct_ans[i], n1$response[i]] + 1

  nm <- filter(d, test_type == "1tomany")
  wr <- cat_label_of[nm$member_of]
  for (i in seq_len(nrow(nm))) m[wr[i], nm$response[i]] <- m[wr[i], nm$response[i]] + 1

  sweep(m, 1, pmax(rowSums(m), 1), "/")
}

n_subj <- n_distinct(trials$participant)   # 34 (all blocks, within-subject)

# ---- 3. Reproduction check --------------------------------------------------
blk_mean <- trials |>
  filter(test_type %in% c("1to1", "1tomany")) |>
  summarise(acc = mean(correct), .by = c(block_pos, test_type)) |>
  arrange(test_type, block_pos)
stopifnot(
  all(abs(filter(blk_mean, test_type == "1to1")$acc    - c(.52, .35, .39)) < .01),
  all(abs(filter(blk_mean, test_type == "1tomany")$acc  - c(.49, .60, .58)) < .01)
)

# ---- 4. Build the three per-block xslData objects -----------------------
blocks <- c("block 1 (ad hoc)", "block 2 (natural)", "block 3 (ad hoc)")
common_desc <- paste(
  "Experiment 1 of Gangwani, Kachergis & Yu, 'Simultaneous Cross-situational",
  "Learning of Category and Object Names'. 3 words + 2 referents per training",
  "trial: the two visible objects' 1-to-1 names (words 1-12) plus one 1-to-many",
  "category label (words 13-15; label 13/14/15 -> objects 1-4/5-8/9-12).",
  "36 training trials (shared schedule from category_xsl_order.txt; identical",
  "co-occurrence structure across all three blocks -- only the stimuli differ).",
  "`accuracy` is per-word human P(correct referent | word): positions 1-12 are",
  "12-AFC name accuracy, positions 13-15 are 3-AFC category-membership accuracy",
  "(n = 34 canonical-order participants). Because the matrix is 15x12,",
  "get_perf()/mafc_test() only score words 1-12; score words 13-15 (and the",
  "ME-violation measure that is the paper's point) with",
  "score_gangwani2011_category() in",
  "tests/bakeoff_comparison/gangwani2011_category_fit.R. `test` holds the 12",
  "name trials (12-AFC) only. `response_matrix` (15x12, row-normalised) is the",
  "full human choice distribution incl. the category labels."
)

gangwani2011_category <- setNames(lapply(seq_along(blocks), function(b) {
  xslData(
    train = train,
    test = list(words = as.list(1:12), objects = rep(list(1:12), 12)),
    accuracy = block_accuracy(b),
    n_subj = n_subj,
    label = paste0("Gangwani2011-category-", c("adhoc1", "natural", "adhoc2")[b]),
    condition = blocks[b],
    description = paste(common_desc, "This condition:", blocks[b],
                        if (b == 2) paste(
                          "-- category members share a salient perceptual",
                          "feature (hook/arrow/...); the feature-similarity and",
                          "novel-object generalization effects are not",
                          "represented here."
                        ) else "-- category members share no perceptual feature."),
    response_matrix = block_response_matrix(b)
  )
}), blocks)

for (b in gangwani2011_category) print(b)

usethis::use_data(gangwani2011_category, overwrite = TRUE)
