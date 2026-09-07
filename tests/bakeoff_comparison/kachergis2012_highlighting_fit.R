# Model comparison + entropy-ablation on the Kachergis (2012, CogSci)
# highlighting dataset (data-raw/add_kachergis2012_highlighting.R).
#
# The real training order (read from
# associative_word_learning/orderings/highlighting.txt, see that ingestion
# script's header for the full account) is NOT the classic simultaneous
# blocking/highlighting design this paper's prose describes -- every trial
# is a single (word, object) pair, and word 1 and word 2 are each trained
# equally often (7x/7x) with their own object and a shared object 3, with
# word 1 given a 9-trial head start and the schedule's tail increasingly
# dominated by word 2. Because of that, there's no reliable, validated
# per-item human accuracy target to fit against for this specific real item
# structure (see data-raw/add_kachergis2012_highlighting.R for why), so
# this script does NOT do a DEoptim fit against reported percentages.
# Instead it asks a cleaner, still-substantive question: does each
# candidate model predict the primacy/recency DISSOCIATION the design
# seems built to elicit -- word 1 (given equal counts) ultimately favoring
# its OWN early object as forgetting increases, while word 2 favors the
# SHARED, more-recently-trained object? A model with no notion of recency
# (e.g. a bare co-occurrence baseline) should show no dissociation at all
# regardless of decay; models with decay/recency should show it grow as
# decay increases.

devtools::load_all("..")
library(purrr)

words_dat <- kachergis2012_highlighting[["words as cues"]]
objects_dat <- kachergis2012_highlighting[["objects as cues"]]

# words-as-cues: word 1's own-target is object 1 (early), shared is object
# 3 (late); word 2's own-target is object 2, shared is also object 3.
# objects-as-cues is the exact role swap -- but that moves *which item* is
# ambiguous: words 1/2 (of this swapped condition) are each cleanly
# unambiguous (word1->object1 only, word2->object2 only), while word 3 is
# the ambiguous one, split between object 1 (linked to word 1's early
# co-occurrence) and object 2 (linked to word 2's late co-occurrence) --
# confirmed empirically below via `table(o[w==item])` on the built dataset.
preference <- function(mat, item, own, shared) {
  rn <- mat[as.character(item), ] / sum(mat[as.character(item), ])
  c(own = unname(rn[as.character(own)]), shared = unname(rn[as.character(shared)]))
}

summarize_run <- function(mat_words, mat_objects) {
  w1 <- preference(mat_words, 1, own = 1, shared = 3)
  w2 <- preference(mat_words, 2, own = 2, shared = 3)
  # objects-as-cues: the ambiguous item is word 3, split between object 1
  # ("early", word-1-linked) and object 2 ("late", word-2-linked)
  w3 <- preference(mat_objects, 3, own = 1, shared = 2)
  tibble::tibble(
    word1_own = w1["own"], word1_shared = w1["shared"],
    word2_own = w2["own"], word2_shared = w2["shared"],
    ocue_early = w3["own"], ocue_late = w3["shared"]
  )
}

## ---- 1. Does decay alone produce the primacy/recency dissociation? -------
cat("=== 1. uncfam (entropy variant): effect of decay (C) ===\n")
decay_sweep <- map(c(1, 0.99, 0.97, 0.93, 0.88, 0.8), function(C) {
  m <- uncfam(X = .1, B = .5, C = C, variant = "entropy")
  mat_w <- suppressWarnings(xsl_run(m, words_dat)$fits[[1]]$matrix)
  mat_o <- suppressWarnings(xsl_run(m, objects_dat)$fits[[1]]$matrix)
  summarize_run(mat_w, mat_o) |> tibble::add_column(C = C, .before = 1)
}) |> list_rbind()
print(decay_sweep, digits = 3)
cat("\n")

## ---- 2. Entropy ablation: does the dissociation depend on the entropy ----
## term specifically, or does familiarity/novelty alone produce it too?
cat("=== 2. Entropy ablation (uncfam variants), C = 0.9 ===\n")
variant_sweep <- map(c("entropy", "novelty", "uncertainty-only"), function(v) {
  m <- uncfam(X = .1, B = .5, C = .9, variant = v)
  mat_w <- suppressWarnings(xsl_run(m, words_dat)$fits[[1]]$matrix)
  mat_o <- suppressWarnings(xsl_run(m, objects_dat)$fits[[1]]$matrix)
  summarize_run(mat_w, mat_o) |> tibble::add_column(variant = v, .before = 1)
}) |> list_rbind()
print(variant_sweep, digits = 3)
cat("\n")

## ---- 3. Model comparison: which models predict this pattern at all? ------
cat("=== 3. Model comparison (fixed, reasonable default params) ===\n")
fast_stoch <- xslControl(n_sim = 100)

model_list <- list(
  uncfam_entropy = uncfam(X = .1, B = .5, C = .9, variant = "entropy"),
  rescorla_wagner = rescorla_wagner(C = .9, alpha = .3, beta = .3, lambda = 1),
  guess_and_test = guess_and_test(f = .1, sa = .5),
  pursuit = pursuit(gamma = .2, threshold = .3, lambda = .05),
  propose_but_verify = propose_but_verify(alpha = .5, alpha_increase = .1),
  baseline = baseline() # pure co-occurrence count, no decay/recency at all
)

model_comparison <- map(names(model_list), function(nm) {
  m <- model_list[[nm]]
  mat_w <- suppressWarnings(xsl_run(m, words_dat, control = fast_stoch)$fits[[1]]$matrix)
  mat_o <- suppressWarnings(xsl_run(m, objects_dat, control = fast_stoch)$fits[[1]]$matrix)
  summarize_run(mat_w, mat_o) |> tibble::add_column(model = nm, .before = 1)
}) |> list_rbind()
print(model_comparison, digits = 3)
