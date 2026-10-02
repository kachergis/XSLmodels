# Add Vlach & DeBrock (2017) as its own package dataset `vlach_debrock2017`.
#
# ---------------------------------------------------------------------------
# WHY THIS SHIPS STANDALONE (not appended to xsl_datasets)
# ---------------------------------------------------------------------------
# data-raw/VlachDeBrock2017_2019/VlachDeBrock2017_2019.txt gives the real
# training schedule, and data-raw/XSL-dataset-fields.csv gives real summary
# statistics from the paper (overall accuracy = .5583, sd = .1975, n = 47) --
# but only the OVERALL mean and SD, not a per-word breakdown. xsl_datasets'
# scoring convention needs a per-word `accuracy` vector (length == vocabulary
# size); there is no way to recover one from a single reported mean without
# fabricating a breakdown that was never measured. Rather than claim a
# uniform per-word accuracy (which would misrepresent the data -- it is
# essentially certain some pairs were learned better than others, per the
# paper's own point about spacing effects), `accuracy` is left `NA` for
# every word, exactly like `rollins_corpus`/`fm_corpus` lack one. This
# dataset is usable for running/fitting models and comparing against the one
# real number this paper gives (overall mean accuracy), but not for the
# package's standard per-item SSE scoring.
#
# ---------------------------------------------------------------------------
# SOURCE
# ---------------------------------------------------------------------------
# data-raw/VlachDeBrock2017_2019/VlachDeBrock2017_2019.txt: 36 tab-separated
# rows, 2 numbers each -- the two to-be-learned pairs (word i <-> object i,
# a symmetric design, same convention as orig_4x4/orig_3x3/etc.) shown on
# that trial. 12 pairs total; each appears in exactly 6 of the 36 trials
# (confirmed below), matching the paper's spaced-vs-massed repetition
# manipulation ("some pairs appear on consecutive trials, others are
# spaced" -- data-raw/XSL-dataset-fields.csv's description).
#
# Run from the package root: Rscript data-raw/add_vlach_debrock2017.R

devtools::load_all(quiet = TRUE)

ord <- read.table("data-raw/VlachDeBrock2017_2019/VlachDeBrock2017_2019.txt",
                   header = FALSE, sep = "\t")
stopifnot(nrow(ord) == 36, ncol(ord) == 2)

train <- list(
  words   = lapply(seq_len(nrow(ord)), \(i) c(ord[i, 1], ord[i, 2])),
  objects = lapply(seq_len(nrow(ord)), \(i) c(ord[i, 1], ord[i, 2]))
)
stopifnot(length(unique(unlist(train$words))) == 12)
stopifnot(all(table(unlist(train$words)) == 6))

vlach_debrock2017 <- xslData(
  train = train,
  test = list(),
  accuracy = rep(NA_real_, 12),
  n_subj = 47,
  label = "VlachDeBrock2017_2019",
  condition = "2x2, 12 pairs",
  description = paste(
    "Vlach, H. A., & DeBrock, C. A. (2017). Remember dax? Relations between",
    "children's cross-situational word learning, memory, and language",
    "abilities. Journal of Memory and Language, 93, 217-230.",
    "https://doi.org/10.1016/j.jml.2016.10.001. 12 to-be-learned word-object",
    "pairs (symmetric design: word i <-> object i), 2 pairs shown per",
    "training trial, 36 trials; pairs vary in whether their 6 repetitions",
    "fall on consecutive trials (massed) or are spread out (spaced), per the",
    "paper's memory/spacing manipulation. n = 47 children (ages 2-5).",
    "No per-word accuracy is available, only the paper's overall mean:",
    "accuracy = .5583 (sd = .1975) across all pairs and participants -- see",
    "data-raw/XSL-dataset-fields.csv. `accuracy` here is NA throughout; this",
    "dataset is not part of xsl_datasets (see data-raw/add_vlach_debrock2017.R",
    "for why) and isn't scorable by xsl_run()'s built-in per-item SSE."
  )
)

print(vlach_debrock2017)

usethis::use_data(vlach_debrock2017, overwrite = TRUE)
