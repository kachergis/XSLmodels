# Rescorla-Wagner (1972) error-driven associative model

On each trial, the words present are cues and the objects outcomes. Each
outcome's prediction is the summed association of the present cues, and
its prediction error (`lambda - prediction` if it is present,
`0 - prediction` if absent) changes every present cue's association with
it by `alpha * beta * lambda` times the error. Cues therefore compete to
predict an outcome (e.g. blocking: a word paired with an object that
another word already predicts learns little about it). All associations
then decay by `C`.

## Usage

``` r
rescorla_wagner(
  C,
  alpha,
  beta,
  lambda,
  outcomes = c("present", "all"),
  cues = c("words", "objects"),
  phi = NULL
)
```

## Arguments

- C:

  Decay

- alpha:

  Salience (fix at 1 unless manipulated)

- beta:

  Learning rate – a proportion of lambda

- lambda:

  Maximum associative value that a CS can achieve - should be larger
  than learning rate

- outcomes:

  `"present"` (the default) updates only the objects present on a trial;
  `"all"` also updates absent objects toward 0.

- cues:

  `"words"` (the default): words predict objects. `"objects"`: objects
  predict words.

- phi:

  Optional softmax inverse temperature for the choice rule. `NULL` (the
  default) uses Luce choice.

## Value

An object of class xslMod

## Details

The defaults (`outcomes = "present"`, `cues = "words"`, Luce choice) are
the model as originally applied to cross-situational learning, which
learns only about the objects present on a trial. In designs where every
pair recurs, it then stays close to counting co-occurrences.

## Options

- `outcomes = "all"` follows Rescorla & Wagner (1972) in treating every
  possible outcome as present (target `lambda`) or absent (target 0) on
  every trial, so a word's associations with objects that are absent
  when it is heard weaken. This is discriminative learning (Ramscar et
  al., 2010; Baayen et al., 2011). Associations can then become
  negative, which Luce choice treats as 0; `phi` gives a choice rule
  defined for them.

- `cues = "objects"` reverses the direction of prediction: objects are
  cues and words outcomes, as in Ramscar et al.'s (2010) feature-label
  ordering account. The matrix keeps words as rows (`m[w, o]` is object
  `o`'s association with word `w`), and test choices are still made
  among a word's row. Cues now compete to predict words rather than
  objects, so the two directions learn different associations.

- `phi` replaces Luce choice with a softmax choice rule with inverse
  temperature `phi` (see
  [`softmax_weights()`](https://www.kachergis.com/XSLmodels/reference/softmax_weights.md)),
  applied by
  [`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
  and to the model's own `perf`.

Choice is invariant to the scale of the associations under Luce choice,
and under softmax the scale trades off against `phi`, so `lambda` is not
identifiable separately from `beta` (and `phi`): fix it (e.g. at 1) when
fitting. Likewise `alpha` only enters as `alpha * beta`.

## References

Baayen, R. H., Milin, P., Durdevic, D. F., Hendrix, P., & Marelli, M.
(2011). An amorphous model for morphological processing in visual
comprehension based on naive discriminative learning. *Psychological
Review, 118*(3), 438-481.

Ramscar, M., Yarlett, D., Dye, M., Denny, K., & Thorpe, K. (2010). The
effects of feature-label-order and their implications for symbolic
learning. *Cognitive Science, 34*(6), 909-957.

Rescorla, R. A., & Wagner, A. R. (1972). A theory of Pavlovian
conditioning: Variations in the effectiveness of reinforcement and
nonreinforcement. In A. H. Black & W. F. Prokasy (Eds.), *Classical
conditioning II* (pp. 64-99). Appleton-Century-Crofts.

## Examples

``` r
mod <- rescorla_wagner(C = 1, alpha = 1, beta = 0.1, lambda = 3)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> NULL
#> 
#> $fits[[1]]$responses
#>      [,1] [,2] [,3] [,4]
#> [1,]  0.5  0.5  0.5  0.5
#> 
#> $fits[[1]]$perf
#>   1   2   3   4 
#> 0.5 0.5 0.5 0.5 
#> 
#> $fits[[1]]$matrix
#>     1   2   3   4
#> 1 0.9 0.9 0.0 0.0
#> 2 0.9 0.9 0.0 0.0
#> 3 0.0 0.0 0.9 0.9
#> 4 0.0 0.0 0.9 0.9
#> 
#> $fits[[1]]$sse
#> [1] 0
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
#> [1] 0
#> 
#> $unweighted_sse
#> [1] 0
#> 
# discriminative learning, with a softmax choice rule
mod <- rescorla_wagner(C = .98, alpha = 1, beta = .3, lambda = 1, outcomes = "all", phi = 5)
xsl_run(mod, get_example_ambiguous_condition())
#> $fits
#> $fits[[1]]
#> $fits[[1]]$sims
#> NULL
#> 
#> $fits[[1]]$responses
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.4042738 0.4042738 0.4065287 0.4065287
#> 
#> $fits[[1]]$perf
#>         1         2         3         4 
#> 0.4042738 0.4042738 0.4065287 0.4065287 
#> 
#> $fits[[1]]$matrix
#>         1       2     3     4
#> 1 0.28812 0.28812 0.000 0.000
#> 2 0.28812 0.28812 0.000 0.000
#> 3 0.00000 0.00000 0.294 0.294
#> 4 0.00000 0.00000 0.294 0.294
#> 
#> $fits[[1]]$sse
#> [1] 0.03580079
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
#> [1] 0.03580079
#> 
#> $unweighted_sse
#> [1] 0.03580079
#> 
```
