# Add temporal contiguity (Kachergis, Yu, & Shiffrin, 2009), Suanda, Mugwanya, &
# Namy (2014), and Koehne, Trueswell, & Gleitman (2013) conditions to xsl_datasets.
#
# Temporal contiguity: raw per-trial response data for conditions "1_" and "4_"
# already exists in agg_data/x8_9-22-08.txt -- the same source file whose "2_"/"3_"
# conditions already became 2_x8_39_4x4/3_x8_369_4x4 in xsl_datasets (see
# export_data.R's comments). The other 6 temp_cont_* order files listed in
# XSL-dataset-fields.csv have no accompanying accuracy data anywhere in this repo
# and are NOT added here.
#
# Suanda et al. 2014 and Koehne et al. 2013 only have a single group-mean accuracy
# value available per condition (no per-item breakdown in this repo), so
# `accuracy` is that mean value replicated across all items -- documented in each
# condition's `description`.
#
# Run with data-raw/ as the working directory.

devtools::load_all("..")

read_symmetric_order <- function(path) {
  tab <- as.matrix(read.table(path, header = FALSE, sep = "\t"))
  trials <- lapply(seq_len(nrow(tab)), function(i) unname(tab[i, ]))
  list(words = trials, objects = trials)
}

read_koehne_order <- function(path) {
  tab <- read.table(path, header = TRUE, sep = "\t")
  list(words = as.list(tab$w),
       objects = lapply(seq_len(nrow(tab)),
                        function(i) unname(unlist(tab[i, c("o1", "o2", "o3", "o4")]))))
}

## ---- Temporal contiguity (Kachergis, Yu, & Shiffrin, 2009) ----

x8 <- read.csv("agg_data/x8_9-22-08.txt", sep = "\t")
temp_cont_train <- read_symmetric_order("orders/1_max_temp_spat_cont_orig.txt")

build_temp_cont_condition <- function(cond_code, label, condition, description) {
  d <- subset(x8, ExperimentName == cond_code)
  item_acc <- aggregate(Correct ~ CorrectAns, data = d, FUN = mean)
  item_acc <- item_acc[order(item_acc$CorrectAns), ]
  xslData(
    train = temp_cont_train,
    accuracy = item_acc$Correct,
    n_subj = length(unique(d$Subject)),
    label = label,
    condition = condition,
    description = description
  )
}

temp_cont_1 <- build_temp_cont_condition(
  "1_", "1_max_temp_spat_cont_orig", "temporal+spatial contiguity",
  "Maximum temporal and spatial contiguity condition (reordering of the original
  4x4 design). Kachergis, Yu, & Shiffrin (2009). Temporal contiguity in
  cross-situational statistical learning. Proceedings of the 31st Annual Meeting
  of the Cognitive Science Society."
)

temp_cont_4 <- build_temp_cont_condition(
  "4_", "4_no_spat_orig_max_tc", "temporal contiguity only",
  "Maximum temporal contiguity without spatial contiguity -- same training order
  as 1_max_temp_spat_cont_orig, but referent screen positions were not held
  contiguous. Kachergis, Yu, & Shiffrin (2009). Temporal contiguity in
  cross-situational statistical learning. Proceedings of the 31st Annual Meeting
  of the Cognitive Science Society."
)

## ---- Suanda, Mugwanya, & Namy (2014) ----

suanda_citation <- "Suanda, S. H., Mugwanya, N., & Namy, L. L. (2014).
  Cross-situational statistical word learning in young children. Journal of
  Experimental Child Psychology, 126, 395-411.
  https://doi.org/10.1016/j.jecp.2014.06.003"

build_suanda_condition <- function(path, label, condition, mean_acc, description) {
  train <- read_symmetric_order(path)
  n_items <- length(unique(unlist(train$words)))
  xslData(
    train = train,
    accuracy = rep(mean_acc, n_items),
    n_subj = 84,
    label = label,
    condition = condition,
    description = description
  )
}

suanda_lo <- build_suanda_condition(
  "Suanda2014/Suanda2014-lowCD.txt", "Suanda2014-lowCD", "2x2 low CD", 0.34,
  paste("2 words and 2 objects per training trial; 8 low contextual-diversity",
        "pairs. Accuracy is the reported group mean (no per-item breakdown",
        "available in this dataset), replicated across items.", suanda_citation)
)

suanda_med <- build_suanda_condition(
  "Suanda2014/Suanda2014-medCD.txt", "Suanda2014-medCD", "2x2 med CD", 0.39,
  paste("2 words and 2 objects per training trial; 8 medium contextual-diversity",
        "pairs. Accuracy is the reported group mean (no per-item breakdown",
        "available in this dataset), replicated across items.", suanda_citation)
)

suanda_hi <- build_suanda_condition(
  "Suanda2014/Suanda2014-hiCD.txt", "Suanda2014-hiCD", "2x2 high CD", 0.48,
  paste("2 words and 2 objects per training trial; 8 high contextual-diversity",
        "pairs. Accuracy is the reported group mean (no per-item breakdown",
        "available in this dataset), replicated across items.", suanda_citation)
)

## ---- Koehne, Trueswell, & Gleitman (2013) ----

koehne_citation <- "Koehne, J., Trueswell, J. C., & Gleitman, L. R. (2013).
  Multiple Proposal Memory in Observational Word Learning. Proceedings of the
  Annual Meeting of the Cognitive Science Society, 35.
  https://escholarship.org/uc/item/5gp8j5qs"

koehne_description <- paste(
  "2 train/test blocks, 8 words each; 1 word and 4 objects per trial; 6",
  "repetitions per word; 2 meanings per word: one present whenever the word is",
  "heard, one present only half the time; other objects appear only once with",
  "any other word. Manipulated the order in which trials including (P) and",
  "excluding (A) the 50%-referent were presented: blocked (AAAPPP/PPPAAA) vs.",
  "unblocked (APAPAP/PAPAPA), and whether the first encounter of a word was a P",
  "or A trial. Accuracy is the group mean across Experiments 1 and 2 (no",
  "per-item breakdown available in this dataset), replicated across items.",
  koehne_citation
)

build_koehne_condition <- function(path, label, condition, mean_acc) {
  train <- read_koehne_order(path)
  n_items <- length(unique(unlist(train$words)))
  xslData(
    train = train,
    accuracy = rep(mean_acc, n_items),
    n_subj = 64,
    label = label,
    condition = condition,
    description = koehne_description
  )
}

koehne_aaappp <- build_koehne_condition(
  "KoehneTrueswellGleitman/KoehneTrueswellGleitman2013-aaappp.txt",
  "Koehne2013-aaappp", "AAAPPP (blocked, A-first)", 0.1795
)

koehne_apapap <- build_koehne_condition(
  "KoehneTrueswellGleitman/KoehneTrueswellGleitman2013-apapap.txt",
  "Koehne2013-apapap", "APAPAP (unblocked, A-first)", 0.2305
)

koehne_papapa <- build_koehne_condition(
  "KoehneTrueswellGleitman/KoehneTrueswellGleitman2013-papapa.txt",
  "Koehne2013-papapa", "PAPAPA (unblocked, P-first)", 0.2305
)

koehne_pppaaa <- build_koehne_condition(
  "KoehneTrueswellGleitman/KoehneTrueswellGleitman2013-pppaaa.txt",
  "Koehne2013-pppaaa", "PPPAAA (blocked, P-first)", 0.3215
)

## ---- Append to xsl_datasets and re-save ----

new_datasets <- list(temp_cont_1, temp_cont_4, suanda_lo, suanda_med, suanda_hi,
                     koehne_aaappp, koehne_apapap, koehne_papapa, koehne_pppaaa)

stopifnot(!any(duplicated(c(sapply(xsl_datasets, `[[`, "label"),
                            sapply(new_datasets, `[[`, "label")))))

xsl_datasets <- c(xsl_datasets, new_datasets)
usethis::use_data(xsl_datasets, overwrite = TRUE)
