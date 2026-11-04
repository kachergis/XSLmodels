## Submission

This is a new release (first submission to CRAN).

## Test environments

* local macOS 14 (R 4.6.0), `R CMD check --as-cran` on the built tarball,
  with the remote CRAN incoming checks enabled

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Downstream dependencies

None (first submission; no reverse dependencies).

## Additional notes

* `plot_training_trials()` depends on the Suggested packages `gganimate` and
  `viridis`, and is guarded with `requireNamespace()` so the
  package degrades gracefully when they are unavailable. Its example is
  wrapped in `\donttest{}`.
* `fgt2009()`/`fgt2009_rsa()` perform MCMC inference and their examples/tests
  use small inputs to keep runtime short.
