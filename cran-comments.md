## Submission

This is an update (0.4.0 -> 0.5.0). It adds choice-level likelihood fitting,
multi-block (learning-trajectory) designs and a new dataset, and corrects
several datasets and model implementations; see NEWS.md.

## Test environments

* local macOS 14 (R 4.6.0), `R CMD check --as-cran` on the built tarball,
  with the remote CRAN incoming checks enabled
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel,
  release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are currently no reverse dependencies.

## Additional notes

* `plot_training_trials()` depends on the Suggested packages `gganimate` and
  `viridis`, and is guarded with `requireNamespace()` so the
  package degrades gracefully when they are unavailable. Its example is
  wrapped in `\donttest{}`.
* `fgt2009()`/`fgt2009_rsa()` perform MCMC inference and their examples/tests
  use small inputs to keep runtime short.
