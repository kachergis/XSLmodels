## Submission

This is a new release (first submission to CRAN).

## Test environments

* local macOS (R 4.5.x), `devtools::check(cran = TRUE)`
* win-builder (R-devel), via `devtools::check_win_devel()`

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

None (first submission; no reverse dependencies).

## Additional notes

* Two functions (`plot_training_trials()`) depend on the Suggested packages
  `gganimate` and `viridis`, and are guarded with `requireNamespace()` so the
  package degrades gracefully when they are unavailable. Its example is
  wrapped in `\donttest{}`.
* `fgt2009()`/`fgt2009_rsa()` perform MCMC inference and their examples/tests
  use small inputs to keep runtime short.
