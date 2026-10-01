# Distributed/episodic memory models: implementation and fit

A write-up of porting three cognitive memory models -- Hintzman's MINERVA2
(1984, 1986), Murdock's TODAM (1982), and Shiffrin & Steyvers' REM (1997) --
into **XSLmodels** as `minerva2()`, `todam()`, and `rem()`, and fitting each
to the full `xsl_datasets` corpus (53 controlled cross-situational word
learning conditions). Produced from `memory_models_fit.R` in this directory;
raw results in `memory_models_fit.rds`.

## Background

The three models started as standalone prototype scripts (outside this
package, never wired in or tested) that represent word/object identity as
random feature vectors and build a single distributed or episodic memory
representation across trials, rather than tallying one word-by-object
association matrix the way most models in this package do. Porting them
required adapting each one's native similarity/odds score into a
Luce-choice-ready `voc_sz x ref_sz` matrix so it plugs into this package's
`get_perf()`/`mafc_test()`/`xsl_fit()` machinery like every other model here.

Porting also surfaced (and fixed) real bugs in the prototypes:

- **`minerva2()`**: the prototype compared its full 2D echo-content vector
  against each D-dimensional object vector -- a length mismatch that R
  silently recycles instead of erroring. Fixed by comparing only the echo's
  object half against object vectors.
- **`todam()`**: both prototype variants called `as.real()`, removed from R
  well over a decade ago. The more developed of the two also computed each
  trial's attention weights as `cos_sim(item, M)`, which is `0/0 = NaN` on
  trial 1 (before anything is stored, `M` is the zero vector) and poisons
  memory with NaN from then on. Fixed both, and consolidated the two
  prototype files into one model using the more developed one's
  attention-weighted encoding and graded softmax choice rule (the simpler
  file's hard-argmax choice doesn't fit this package's Luce-choice-based
  scoring).
- **`rem()`**: the prototype never actually implemented REM -- it declared a
  geometric-distribution feature generator, matching REM's environmental
  base-rate assumption, but its `model()`/test functions were an unmodified
  copy of the MINERVA2 prototype's continuous-Gaussian dot-product code,
  calling helper functions not even defined in that file. `rem()` here is a
  fresh adaptation of Shiffrin & Steyvers' actual storage/likelihood-ratio
  equations to this package's associative task (see `R/model-rem.R` for the
  full derivation in comments). Benchmarking it turned up a real numerical
  issue: raw per-simulation likelihood-ratio odds are heavy-tailed (a lucky
  match on one rare feature value can inflate a trace's odds by orders of
  magnitude), which would let a single lucky simulation dominate
  `xsl_run()`'s sum-across-simulations aggregation. Verified empirically
  (the same word/object pair's raw score ranged from ~10 to ~145,000 across
  otherwise-identical simulations) and fixed by row-normalizing each
  simulation's choice weights before they're summed.

All three are registered in `show_models()`/`xsl_model_registry()`, fitting
only their choice-rule temperature (`minerva2()`/`todam()`'s `X`) or storage
parameters (`rem()`'s `g`, `u`, `c`) by default. Feature-vector dimensionality
(`D` for `minerva2()`/`todam()`, `w` for `rem()`) is treated as a fixed
representational hyperparameter, not a free cognitive parameter, and is left
out of the DEoptim bounds -- the same treatment this package already gives
`fgt2009()`'s corpus-size-scaled `alpha`.

## Performance

Single-evaluation cost (one `xsl_run()` over all 53 `xsl_datasets`
conditions):

| model | n_sim=25 | n_sim=100 | dominant cost |
|---|---:|---:|---|
| minerva2 | ~0.2s | ~0.6s | dot products over stored traces |
| todam | ~25s | ~99s | FFT convolution/correlation per trial and per test pair |
| rem | ~59s | ~238s | one trace per (word, object) pair per trial; `voc_sz x ref_sz` likelihood-ratio test |

`minerva2()` is cheap enough to fit at this package's usual DEoptim settings
(`NP=100, itermax=100`). `todam()` and `rem()` are not -- at those settings
and a reasonable `n_sim`, either would take days. `rem()` is additionally the
most exposed to corpus size, since its memory grows with the *product* of
words and objects per trial (one trace per candidate pairing), not the trial
count.

## Fit results

Each model fit via `get_group_model_fit()` against all 53 `xsl_datasets`
conditions (minimizing SSE between the model's Luce-choice predictions and
human accuracy), with the DEoptim search budget cut well below this
package's defaults to keep `todam()`/`rem()` tractable:

| model | search budget | n_sim | wall time | fitted params | SSE (fit) | SSE (unfit default) |
|---|---|---:|---:|---|---:|---:|
| minerva2 | NP=15, itermax=40 | 30 | 18.0 min | X = 4.78 | **0.420** | 0.495 |
| todam | NP=8, itermax=8 | 100 | 116.8 min | X = 23.78 | 0.876 | 2.48 |
| rem | NP=10, itermax=9 | 100 | 344.1 min | g=0.92, u=0.34, c=0.99 | 0.537 | 2.09 |

(Registry defaults: `minerva2(X=5)`, `todam(X=5)`, `rem(g=.4, u=.3, c=.7)`.)

**minerva2 fits this corpus best**, and by far the cheapest to run. Its
search converged cleanly (flat by iteration ~15) and the default `X=5` was
already a decent guess -- the fit only found a modest improvement.

**todam** improved substantially over its default (SSE 2.48 -> 0.876): the
default `X=5` was much too low-temperature/diffuse, and the fit wants
something closer to winner-take-all (`X=23.78`, comfortably inside the
`[0.1, 50]` bound, not pinned at the edge). It plateaued by iteration 3-4
despite the small search budget.

**rem** also improved a lot (SSE 2.09 -> 0.537) but is the least trustworthy
of the three fits: `g` and `c` both landed near their upper registry bounds
(0.92 against a 0.95 cap, 0.99 against a 0.999 cap), and the SSE trace was
still decreasing at the final iteration rather than flat -- i.e. under-
converged at this reduced budget, not just up against a real boundary. A
longer run (larger `NP`/`itermax`, and possibly wider bounds on `g`/`c`)
would likely do better; treat 0.537 as an upper bound on `rem()`'s best
achievable SSE on this corpus, not a converged estimate.

## Caveats

- None of these fits used this package's default DEoptim rigor (`NP=100,
  itermax=100`); `minerva2()`'s fit is the closest to that standard and the
  most trustworthy. `todam()`'s converged despite the small budget; `rem()`'s
  did not.
- `rem()`'s row-normalization fix changes the *scale* of its matrix (each
  simulated "subject" contributes a genuine probability distribution) but not
  its qualitative behavior; SSE values here already reflect that fix.
- SSE is not directly comparable across models with a different number of
  free parameters without a complexity penalty -- `rem()`'s 3 free parameters
  vs. `minerva2()`/`todam()`'s 1 gives it more flexibility to fit the same
  data, independent of whether it's the better *model*.

## Reproducing

```r
# from the package root, against the installed package:
# R CMD INSTALL . && Rscript tests/bakeoff_comparison/memory_models_fit.R
```

Writes `memory_models_fit.rds` to this directory. Full run is ~8 hours
(`rem()` dominates); `todam()`/`rem()` are independent of `minerva2()` and of
each other, so consider running them as separate background jobs.
