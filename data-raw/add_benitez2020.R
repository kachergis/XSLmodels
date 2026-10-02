# Add Benitez, Yurovsky, et al. (2020) as a standalone dataset `benitez2020`:
# a named list of 7 xslData objects, one per condition x age-group
# combination:
#   Interleaved-kids / Interleaved-adults
#   Massed-kids / Massed-adults
#   Unstructured1-kids (no adult data for this order)
#   Unstructured23-kids / Unstructured23-adults
#
# ---------------------------------------------------------------------------
# WHY THIS SHIPS STANDALONE (unlike structural necessity for
# kachergis2012_highlighting/gangwani2011_category/vlach_debrock2017)
# ---------------------------------------------------------------------------
# Structurally this dataset has NO problem joining xsl_datasets: 8 words x 8
# objects, symmetric design (word i <-> object i, same convention as
# orig_4x4/orig_3x3/etc.), and real per-word 2AFC response counts are
# available broken out by condition AND age group, so a genuine per-word
# `accuracy` vector (diag(response_matrix) / rowSums(response_matrix)) is
# recoverable, not just an overall mean.
#
# It's kept standalone anyway, by choice: every `xsl_datasets` condition gets
# used for group/cross-validated fitting, so a model never sees held-out
# data to test genuine generalization. `benitez2020` (along with the other
# standalone datasets -- see `xsl_holdout_datasets`) is deliberately kept
# out of that loop for exactly this purpose.
#
# ---------------------------------------------------------------------------
# SOURCE
# ---------------------------------------------------------------------------
# data-raw/Benitez2020-rep/CrossSitRep_allData.csv, downloaded from
# https://osf.io/2hmxr/ by data-raw/Benitez2020-rep/Benitez2020_XSLdata_extraction.R
# (which stopped short of actually building xslData objects -- this script
# picks up exactly where its "ToDo: call XSL-data constructor" left off, by
# first re-running its extraction to regenerate the intermediate .txt files
# -- training order, 2AFC test candidates, and response-count matrices -- if
# they aren't already present, then reading those .txt files below).
# The paper itself: Benitez, V. L., et al. (2020). "The temporal structure
# of naming events differentially affects children's and adults'
# cross-situational word learning." (data/materials: https://osf.io/2hmxr/)
#
# Each condition has 12 training trials, 2 words (and their same-index
# objects) per trial; participants did a 2AFC test on all 8 words
# afterward. Unstructured has two training orders; GK verified (per the
# extraction script's comment) that orders 2 and 3 are identical, so they
# are pooled as "order2n3" -- order 1 has kids-only data, order2n3 has both.
#
# Run from the package root: Rscript data-raw/add_benitez2020.R

devtools::load_all(quiet = TRUE)

raw_dir <- "data-raw/Benitez2020-rep"
if (!file.exists(file.path(raw_dir, "Benitez2020_interleaved.txt"))) {
  source(file.path(raw_dir, "Benitez2020_XSLdata_extraction.R"), chdir = TRUE)
}

read_symmetric_train <- function(fname) {
  ord <- read.table(fname, header = FALSE, sep = "\t")
  stopifnot(ncol(ord) == 2)
  words <- lapply(seq_len(nrow(ord)), \(i) c(ord[i, 1], ord[i, 2]))
  list(words = words, objects = words)
}

read_2afc_test <- function(fname) {
  cand <- read.table(fname, header = FALSE, sep = "\t")
  stopifnot(nrow(cand) == 8, ncol(cand) == 2)  # one row per word, in word order
  list(words = as.list(1:8),
       objects = lapply(seq_len(nrow(cand)), \(i) c(cand[i, 1], cand[i, 2])))
}

read_response_matrix <- function(fname) {
  m <- as.matrix(read.table(fname, header = FALSE, sep = "\t"))
  dimnames(m) <- list(1:8, 1:8)
  m
}

build_condition <- function(label, condition_desc, train_file, test_file,
                            response_file, n_subj) {
  m <- read_response_matrix(file.path(raw_dir, response_file))
  xslData(
    train = read_symmetric_train(file.path(raw_dir, train_file)),
    test = read_2afc_test(file.path(raw_dir, test_file)),
    accuracy = diag(m) / rowSums(m),
    n_subj = n_subj,
    label = label,
    condition = condition_desc,
    description = paste(
      "Benitez, V. L., et al. (2020). The temporal structure of naming",
      "events differentially affects children's and adults' cross-",
      "situational word learning (data/materials: https://osf.io/2hmxr/).",
      "8 symmetric word-object pairs (word i <-> object i), 2 pairs per",
      "training trial, 12 trials, followed by an 8-item 2AFC test.",
      "`accuracy` and `response_matrix` are real per-word 2AFC accuracy",
      "and raw response counts for this condition/age-group combination",
      "(not an estimate). See data-raw/add_benitez2020.R."
    ),
    response_matrix = m
  )
}

conditions <- setNames(list(
  build_condition("Benitez2020-Interleaved-kids", "Interleaved, kids",
                  "Benitez2020_interleaved.txt", "Benitez2020_interleaved_test.txt",
                  "Benitez2020_interleaved_kids_responses.txt", 46),
  build_condition("Benitez2020-Interleaved-adults", "Interleaved, adults",
                  "Benitez2020_interleaved.txt", "Benitez2020_interleaved_test.txt",
                  "Benitez2020_interleaved_adults_responses.txt", 30),
  build_condition("Benitez2020-Massed-kids", "Massed, kids",
                  "Benitez2020_massed.txt", "Benitez2020_massed_test.txt",
                  "Benitez2020_massed_kids_responses.txt", 45),
  build_condition("Benitez2020-Massed-adults", "Massed, adults",
                  "Benitez2020_massed.txt", "Benitez2020_massed_test.txt",
                  "Benitez2020_massed_adults_responses.txt", 30),
  build_condition("Benitez2020-Unstructured1-kids", "Unstructured (order 1), kids",
                  "Benitez2020_unstructured_order1.txt", "Benitez2020_unstructured_order1_test.txt",
                  "Benitez2020_unstructured_order1_kids_responses.txt", 18),
  build_condition("Benitez2020-Unstructured23-kids", "Unstructured (order 2/3), kids",
                  "Benitez2020_unstructured_order2n3.txt", "Benitez2020_unstructured_order2n3_test.txt",
                  "Benitez2020_unstructured_order2n3_kids_responses.txt", 16),
  build_condition("Benitez2020-Unstructured23-adults", "Unstructured (order 2/3), adults",
                  "Benitez2020_unstructured_order2n3.txt", "Benitez2020_unstructured_order2n3_test.txt",
                  "Benitez2020_unstructured_order2n3_adults_responses.txt", 30)
), c("Interleaved, kids", "Interleaved, adults", "Massed, kids", "Massed, adults",
     "Unstructured (order 1), kids", "Unstructured (order 2/3), kids",
     "Unstructured (order 2/3), adults"))

for (cd in conditions) print(cd)

benitez2020 <- conditions
usethis::use_data(benitez2020, overwrite = TRUE)
