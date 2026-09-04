# Regenerate the condition co-occurrence animations in this directory from
# the current xsl_datasets, using the package's own plot_training_trials().
# Run from the package root:
#   Rscript animations/generate_animations.R
devtools::load_all(quiet = TRUE)

out_dir <- "animations"

for (d in xsl_datasets) {
  fname <- file.path(out_dir, paste0("cond", d$label, ".mp4"))
  message("Generating ", fname)
  plot_training_trials(d$train, filename = fname)
}
