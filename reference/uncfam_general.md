# General biased associative model (nests the uncfam family)

A single biased associative model (Kachergis et al. 2012) whose
extensions are free parameters, so that the package's
[`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md)
variants are nested special cases and can be compared by fitting one
model with parameters fixed or freed. On each trial, a word-object pair
\\(w, o)\\ on the trial receives a share of the trial's associative
weight \$\$X_t \cdot \frac{b(w, o) + \epsilon s(w, o)}{1 + \epsilon \sum
s}, \quad b(w, o) \propto M\_{w,o}^{\gamma} e^{\lambda (U(w) +
U(o))}\$\$ where \\U\\ is each stimulus's uncertainty (entropy of its
associations, or novelty), \\s\\ is elimination support (see
[`uncfam_elimination()`](https://www.kachergis.com/XSLmodels/reference/uncfam_elimination.md)),
and \\X_t = X \cdot (\bar{H}\_{trial} / \bar{H}\_{all})^\kappa\\ scales
the trial's weight by its objects' relative uncertainty (see
[`uncfam_attention()`](https://www.kachergis.com/XSLmodels/reference/uncfam_attention.md)).
With finite `K`, attention is sampled rather than allocated: each word
attends to `K` objects drawn in proportion to \\b\\, and only sampled
pairs share the weight (see
[`uncfam_sampling()`](https://www.kachergis.com/XSLmodels/reference/uncfam_sampling.md));
as `K` grows every pair is sampled and this approaches `K = Inf`.

## Usage

``` r
uncfam_general(
  X,
  B,
  C,
  gamma = 1,
  eps = 0,
  kappa = 0,
  K = Inf,
  uncertainty = c("entropy", "novelty"),
  rho = 0,
  Cs = 0.5
)
```

## Arguments

- X:

  Associative weight to distribute

- B:

  Weighting of uncertainty vs. familiarity (\\\lambda\\)

- C:

  Decay

- gamma:

  Familiarity exponent (1 =
  [`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md))

- eps:

  Weight of learning by elimination (0 = none)

- kappa:

  Exponent on trial-level attention scaling (0 = none, 1 =
  [`uncfam_attention()`](https://www.kachergis.com/XSLmodels/reference/uncfam_attention.md))

- K:

  Objects sampled per word per trial (`Inf` = deterministic allocation;
  finite = sampled attention, a stochastic model)

- uncertainty:

  Uncertainty measure: `"entropy"` (default) or `"novelty"`
  (`1 / (1 + times seen)`)

- rho:

  Weight of the short-term trace in familiarity (0 = none)

- Cs:

  Per-trial decay of the short-term trace (used only if `rho > 0`)

## Value

An object of class xslMod

## Details

A short-term trace (`rho > 0`) adds recency to the familiarity bias:
every update also goes into a second matrix \\S\\ that decays by `Cs`
per trial (vs. `C` for \\M\\), and familiarity in \\b\\ becomes \\(M +
\rho S)^\gamma\\, so a pairing strengthened on a recent trial draws
extra attention. The trace affects only allocation during learning:
uncertainty, elimination, and test performance use \\M\\.

Special cases (each reproduces the named function exactly):

|  |  |
|----|----|
| defaults | [`uncfam()`](https://www.kachergis.com/XSLmodels/reference/uncfam.md) |
| `B = 0` | familiarity only |
| `gamma = 0` | `uncfam(variant = "uncertainty-only")` |
| `uncertainty = "novelty"` | `uncfam(variant = "novelty")` |
| `gamma` free | [`uncfam_gamma()`](https://www.kachergis.com/XSLmodels/reference/uncfam_gamma.md) |
| `eps` free | [`uncfam_elimination()`](https://www.kachergis.com/XSLmodels/reference/uncfam_elimination.md) |
| `kappa = 1` | [`uncfam_attention()`](https://www.kachergis.com/XSLmodels/reference/uncfam_attention.md) |
| `K` finite | [`uncfam_sampling()`](https://www.kachergis.com/XSLmodels/reference/uncfam_sampling.md) |

[`uncfam_predictive()`](https://www.kachergis.com/XSLmodels/reference/uncfam_predictive.md)
is not nested: its prediction-error update is unnormalized, a different
class of rule.

## Examples

``` r
dat <- get_example_ambiguous_condition()
xsl_run(uncfam_general(X = .1, B = .98, C = 1), dat)
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

# nested comparison: does freeing gamma improve on uncfam()?
xsl_run(uncfam_general(X = .1, B = .98, C = 1, gamma = .5), dat)$sse
#> [1] 0.04938272

# sampled attention (stochastic)
xsl_run(uncfam_general(X = .1, B = .98, C = 1, K = 1), dat, control = xslControl(n_sim = 50))
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> NULL
#> 
#> $fits[[1]]$responses
#>            [,1]      [,2]      [,3]      [,4]
#>  [1,] 0.6666667 0.6666667 0.6666667 0.6666667
#>  [2,] 0.1111111 0.6666667 0.6666667 0.6666667
#>  [3,] 0.6666667 0.1111111 0.6666667 0.1111111
#>  [4,] 0.6666667 0.6666667 0.6666667 0.1111111
#>  [5,] 0.1111111 0.1111111 0.6666667 0.1111111
#>  [6,] 0.1111111 0.6666667 0.1111111 0.1111111
#>  [7,] 0.1111111 0.1111111 0.1111111 0.6666667
#>  [8,] 0.1111111 0.6666667 0.1111111 0.6666667
#>  [9,] 0.1111111 0.6666667 0.1111111 0.1111111
#> [10,] 0.6666667 0.1111111 0.6666667 0.6666667
#> [11,] 0.1111111 0.1111111 0.6666667 0.6666667
#> [12,] 0.6666667 0.6666667 0.6666667 0.6666667
#> [13,] 0.1111111 0.1111111 0.6666667 0.6666667
#> [14,] 0.6666667 0.1111111 0.1111111 0.6666667
#> [15,] 0.1111111 0.6666667 0.6666667 0.6666667
#> [16,] 0.1111111 0.6666667 0.6666667 0.6666667
#> [17,] 0.6666667 0.1111111 0.1111111 0.1111111
#> [18,] 0.1111111 0.1111111 0.6666667 0.1111111
#> [19,] 0.1111111 0.6666667 0.6666667 0.1111111
#> [20,] 0.1111111 0.6666667 0.6666667 0.6666667
#> [21,] 0.6666667 0.1111111 0.6666667 0.6666667
#> [22,] 0.6666667 0.6666667 0.1111111 0.6666667
#> [23,] 0.6666667 0.6666667 0.6666667 0.6666667
#> [24,] 0.6666667 0.1111111 0.1111111 0.6666667
#> [25,] 0.6666667 0.1111111 0.6666667 0.6666667
#> [26,] 0.1111111 0.6666667 0.1111111 0.6666667
#> [27,] 0.1111111 0.1111111 0.6666667 0.6666667
#> [28,] 0.1111111 0.1111111 0.6666667 0.6666667
#> [29,] 0.6666667 0.6666667 0.6666667 0.1111111
#> [30,] 0.1111111 0.1111111 0.1111111 0.6666667
#> [31,] 0.1111111 0.6666667 0.6666667 0.6666667
#> [32,] 0.1111111 0.1111111 0.6666667 0.6666667
#> [33,] 0.6666667 0.1111111 0.1111111 0.1111111
#> [34,] 0.1111111 0.1111111 0.6666667 0.6666667
#> [35,] 0.6666667 0.6666667 0.1111111 0.6666667
#> [36,] 0.1111111 0.6666667 0.1111111 0.1111111
#> [37,] 0.1111111 0.6666667 0.1111111 0.1111111
#> [38,] 0.6666667 0.1111111 0.6666667 0.6666667
#> [39,] 0.1111111 0.6666667 0.1111111 0.6666667
#> [40,] 0.1111111 0.1111111 0.1111111 0.1111111
#> [41,] 0.6666667 0.6666667 0.1111111 0.6666667
#> [42,] 0.6666667 0.6666667 0.6666667 0.1111111
#> [43,] 0.1111111 0.1111111 0.6666667 0.6666667
#> [44,] 0.6666667 0.1111111 0.6666667 0.1111111
#> [45,] 0.6666667 0.1111111 0.1111111 0.1111111
#> [46,] 0.1111111 0.6666667 0.1111111 0.6666667
#> [47,] 0.1111111 0.1111111 0.1111111 0.1111111
#> [48,] 0.6666667 0.1111111 0.6666667 0.1111111
#> [49,] 0.6666667 0.1111111 0.1111111 0.1111111
#> [50,] 0.1111111 0.1111111 0.1111111 0.6666667
#> 
#> $fits[[1]]$perf
#>         1         2         3         4 
#> 0.3555556 0.3666667 0.4222222 0.4555556 
#> 
#> $fits[[1]]$matrix
#>      1    2    3    4
#> 1 1.60 1.90 0.50 0.50
#> 2 1.85 1.65 0.50 0.50
#> 3 0.50 0.50 1.90 1.60
#> 4 0.50 0.50 1.45 2.05
#> 
#> $fits[[1]]$sse
#> [1] 0.04666667
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
#> [1] 0.04666667
#> 
#> $unweighted_sse
#> [1] 0.04666667
#> 
```
