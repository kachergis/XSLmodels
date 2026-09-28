# Kachergis 2012 with a free familiarity exponent

A variant of
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
(Kachergis et al. 2012's uncertainty- and familiarity-biased associative
model) that decouples familiarity's contribution to a trial's
attentional allocation from uncertainty's.
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)'s
allocation weight for pair (w, o) is `exp(B * entropy) * assocs` –
entropy gets a free, tunable exponent (`B`), but familiarity (`assocs`,
the pair's current association strength) enters linearly, with an
implicit weight of 1 and no free parameter of its own. This adds one
parameter, `gamma`, raising familiarity to a free power instead:
`assocs^gamma * exp(B * entropy)`. `gamma = 1` is identical to
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md);
`gamma < 1` dampens the "rich get richer" effect (diminishing returns on
an already-strong pairing); `gamma > 1` amplifies it (winner-take-all).

## Usage

``` r
uncfam_gamma(X, B, C, gamma)
```

## Arguments

- X:

  Associative weight to distribute

- B:

  Weighting of uncertainty vs. familiarity

- C:

  Decay

- gamma:

  Familiarity exponent (1 = identical to
  [`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md))

## Value

An object of class xslMod

## Details

Cross-validated against all of
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md)
(SSE to human 18AFC accuracy, the package's standard model-comparison
objective), `uncfam_gamma()` beats plain
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md) in
every fold of a 5-fold split (mean test SSE 0.48 vs. 0.56), with a
best-fit `gamma` around 0.3 – i.e. familiarity's pull *dampens* with
diminishing returns when explaining how well people learn from a
passively-received training sequence. A separate analysis fitting this
same free-`gamma` substrate to *active* cross-situational word learning
(Kachergis, Yu, & Shiffrin's paradigm where learners choose which items
to see named next, rather than receiving a fixed passive sequence) found
a best-fit `gamma` around 2 instead – amplified, not dampened. Both
contexts agree that familiarity should get its own free exponent rather
than
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)'s
hard-coded linear one, but disagree on which direction it bends: the
process governing moment-to-moment choice of what to study next doesn't
appear to be simply the same one governing how much is learned from a
given, already-fixed sequence.

## Examples

``` r
mod <- uncfam_gamma(X = .1, C = 1, B = .98, gamma = 1)
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

# gamma = 1 reproduces uncfam() exactly
sse_gamma1 <- xsl_run(uncfam_gamma(X = .1, C = 1, B = .98, gamma = 1),
                      get_example_ambiguous_condition())$sse
sse_uncfam <- xsl_run(uncfam(X = .1, C = 1, B = .98),
                      get_example_ambiguous_condition())$sse
isTRUE(all.equal(sse_gamma1, sse_uncfam))
#> [1] TRUE

mod <- uncfam_gamma(X = .1, C = 1, B = .98, gamma = 0.3) # dampened familiarity
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
