# Biased associative model with learning by elimination

A variant of
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
(Kachergis et al. 2012's uncertainty- and familiarity-biased associative
model) that adds inference by elimination (mutual exclusivity) to how a
trial's associative weight is allocated. If the other words and objects
on a trial are already confidently paired with each other, the remaining
word and object probably go together, so their pairing should draw
attention even though it is still weak. In
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md),
by contrast, an already-strong pair on the trial draws weight *away*
from such a pairing.

## Usage

``` r
uncfam_elimination(X, B, C, eps)
```

## Arguments

- X:

  Associative weight to distribute

- B:

  Weighting of uncertainty vs. familiarity

- C:

  Decay

- eps:

  Weight of elimination inference (0 = identical to
  [`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md))

## Value

An object of class xslMod

## Details

On each trial, each on-trial pair \\(w', o')\\ has a mutual confidence
\\q(w', o') = P(o'\|w') P(w'\|o')\\, from the row- and column-normalized
associations. A pair \\(w, o)\\ gets elimination support \\s(w, o) =
\sum\_{w' \ne w, o' \ne o} q(w', o')\\: how much of the rest of the
trial is accounted for by other pairs. The share of the trial's weight
\\X\\ given to \\(w, o)\\ is then \$\$\frac{b(w, o) + \epsilon s(w,
o)}{1 + \epsilon \sum s}\$\$ where \\b(w, o)\\ is
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)'s
normalized familiarity-and-uncertainty share, so `eps = 0` reproduces
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
exactly.

Motivated by the initial-accuracy experiment
([kachergis_initial_accuracy](https://www.kachergis.com/XSLmodels/reference/kachergis_initial_accuracy.md)),
where learners were more accurate on an initially mis-paired word the
more of its study trials it shared with a word they already knew.

## Examples

``` r
mod <- uncfam_elimination(X = .1, B = .98, C = 1, eps = 1)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> NULL
#> 
#> $fits[[1]]$responses
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.3888889 0.3888889 0.3888889 0.3888889
#> 
#> $fits[[1]]$perf
#>         1         2         3         4 
#> 0.3888889 0.3888889 0.3888889 0.3888889 
#> 
#> $fits[[1]]$matrix
#>       1     2     3     4
#> 1 0.035 0.035 0.010 0.010
#> 2 0.035 0.035 0.010 0.010
#> 3 0.010 0.010 0.035 0.035
#> 4 0.010 0.010 0.035 0.035
#> 
#> $fits[[1]]$sse
#> [1] 0.04938272
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
#> [1] 0.04938272
#> 
#> $unweighted_sse
#> [1] 0.04938272
#> 
```
