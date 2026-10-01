# Fits minerva2(), todam(), and rem() (see R/model-minerva2.R,
# R/model-todam.R, R/model-rem.R) to the full xsl_datasets corpus via
# get_group_model_fit(), and compares each against its unfit registry
# default. Companion to memory_models_report.md.
#
# todam()/rem() are too slow for this package's usual DEoptim settings
# (NP = 100, itermax = 100 -- see xsl_model_registry()) at a reasonable
# n_sim, so all three runs below use deliberately reduced search budgets.
# Measured single-evaluation cost (n_sim = 100, all 53 datasets, on the
# machine this was run on): minerva2 ~0.6s, todam ~99s, rem ~238s -- todam's
# FFT convolution/correlation and rem's per-(word,object) trace scoring both
# dominate over minerva2's simpler dot-product-cubed echo.
#
# Actual wall-clock times from the runs behind memory_models_fit.rds:
#   minerva2: NP=15, itermax=40, n_sim=30  -> 18.0 min (fully converged by ~iter 15)
#   todam:    NP=8,  itermax=8,  n_sim=100 -> 116.8 min (plateaued by iter 3-4)
#   rem:      NP=10, itermax=9,  n_sim=100 -> 344.1 min (still improving at
#             the last iteration -- under-converged at this budget; see report)
#
# Run from the package root against the *installed* package:
#   R CMD INSTALL . && Rscript tests/bakeoff_comparison/memory_models_fit.R
# Writes: memory_models_fit.rds (this dir). Takes ~8 hours total (rem
# dominates); todam and rem are independent of each other and of minerva2,
# so consider running them as separate background jobs if you want the
# faster results sooner.

suppressMessages(library(XSLmodels))
suppressMessages(library(DEoptim))

set.seed(1)
out_dir <- "tests/bakeoff_comparison"

fit_one <- function(model_name, deoptim_control, n_sim) {
  t0 <- Sys.time()
  fit <- get_group_model_fit(
    model_name,
    datasets = xsl_datasets,
    control = xslControl(n_sim = n_sim),
    deoptim_control = deoptim_control
  )
  elapsed_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

  entry <- XSLmodels:::get_model_registry_entry(model_name)
  unfit <- xsl_run(entry$constructor(), xsl_datasets, control = xslControl(n_sim = n_sim))

  list(model = model_name, n_sim = n_sim, elapsed_min = elapsed_min,
       best_params = fit$fit_result[[1]]$optim$bestmem,
       fit_sse = fit$fit_result[[1]]$optim$bestval,
       unfit_sse = unfit$unweighted_sse)
}

results <- list(
  minerva2 = fit_one("minerva2",
    DEoptim.control(NP = 15, itermax = 40, reltol = .001, trace = 5), n_sim = 30),
  todam = fit_one("todam",
    DEoptim.control(NP = 8, itermax = 8, reltol = .001, trace = 1), n_sim = 100),
  rem = fit_one("rem",
    DEoptim.control(NP = 10, itermax = 9, reltol = .001, trace = 1), n_sim = 100)
)

for (r in results) {
  cat(sprintf("%-10s n_sim=%-4d %7.1f min  fit SSE=%.4f  unfit SSE=%.4f  params=%s\n",
              r$model, r$n_sim, r$elapsed_min, r$fit_sse, r$unfit_sse,
              paste(round(r$best_params, 3), collapse = ", ")))
}

saveRDS(results, file.path(out_dir, "memory_models_fit.rds"))
