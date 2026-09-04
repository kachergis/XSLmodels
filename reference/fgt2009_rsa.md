# Frank, Goodman & Tenenbaum word-learning model with pragmatic (RSA) reasoning

[`fgt2009()`](https://kachergis.github.io/XSLmodels/reference/fgt2009.md)
with the literal speaker replaced by a Rational Speech Act pragmatic
speaker: a speaker who chooses words to be informative to a listener who
reasons about that speaker (Frank & Goodman, 2012; Smith, Goodman &
Frank, 2013). The non-referential channel, the `kappa` penalty, and the
marginalization over intentions are unchanged, so the literal
[`fgt2009()`](https://kachergis.github.io/XSLmodels/reference/fgt2009.md)
model is the `rsa_depth = 0` special case. The recursion is what
produces *strong* mutual exclusivity – the speaker could have used a
different word – which the literal model underproduces.

## Usage

``` r
fgt2009_rsa(
  alpha,
  gamma = 1,
  kappa = 0.5,
  rsa_alpha = 3,
  rsa_depth = 1L,
  n_chains = 4L,
  n_warmup = 120L,
  n_samples = 300L,
  gibbs_per = 1L,
  edge_per = 1L,
  top_k = 6L,
  seed = 1L
)
```

## Arguments

- alpha:

  Geometric lexicon-size prior: `P(L)` proportional to
  `exp(-alpha |L|)`. Must be tuned to corpus size.

- gamma:

  Probability that a word is used referentially. `1` (the default) is
  appropriate when every word heard is a label, as in controlled XSL
  experiments.

- kappa:

  Down-weight applied to an in-lexicon word used non-referentially (only
  relevant when `gamma < 1`).

- rsa_alpha:

  Speaker rationality (softmax temperature) in the RSA recursion.

- rsa_depth:

  Number of pragmatic recursions; 1 gives the usual one-step pragmatic
  listener/speaker.

- n_chains, n_warmup, n_samples, gibbs_per, edge_per, top_k, seed:

  Sampler controls. Defaults are tuned for the small, dense corpora of
  XSL experiments; raise `n_chains`/`n_samples` to reduce Monte Carlo
  noise.

## Value

An object of class xslMod

## Examples

``` r
mod <- fgt2009_rsa(alpha = 1, rsa_alpha = 3)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> NULL
#> 
#> $fits[[1]]$responses
#>              1         2         3         4
#> [1,] 0.5081864 0.5095178 0.5225291 0.5302817
#> 
#> $fits[[1]]$perf
#>         1         2         3         4 
#> 0.5081864 0.5095178 0.5225291 0.5302817 
#> 
#> $fits[[1]]$matrix
#>              1            2            3            4
#> 1 0.3359700250 0.3243130724 0.0004163197 0.0004163197
#> 2 0.3209825146 0.3343047460 0.0004163197 0.0004163197
#> 3 0.0004163197 0.0004163197 0.2993338884 0.2726894255
#> 4 0.0004163197 0.0004163197 0.2768526228 0.3134887594
#> 
#> $fits[[1]]$sse
#> [1] 0.001582145
#> 
#> $fits[[1]]$data
#> xslData object with label "example condition" and condition "ambiguous"
#>   training trials: 2
#>       test trials: 0
#>             words: 4
#>           objects: 4
#>        accuracies: 4
#> 
#> 
#> 
#> $sse
#> [1] 0.001582145
#> 
#> $unweighted_sse
#> [1] 0.001582145
#> 
```
