# Predict referent selection for a single trial from an association matrix

Given a model's word-object association matrix, returns the probability
distribution over a trial's candidate objects for one heard word – the
quantity to compare against a participant's choice on a
referent-selection (m-alternative forced choice) trial. This is
model-agnostic: it works on the `matrix` of any `xslFit` (or any
word-by-object matrix), reading each entry as an association strength /
meaning weight.

## Usage

``` r
predict_referent(
  m,
  word,
  objects,
  prior = NULL,
  pragmatic = FALSE,
  threshold = NULL,
  rsa_alpha = 3,
  depth = 1
)
```

## Arguments

- m:

  A word-by-object association matrix (e.g.
  `xsl_run(mod, data)$fits[[1]]$matrix`). Assumed non-negative; negative
  entries are clamped to 0. `dimnames` are used to resolve character
  `word`/`objects`.

- word:

  The heard word: a row name of `m`, or a positive integer row index. A
  value not present in `m` is treated as a novel word.

- objects:

  The candidate objects present on the trial: column names of `m`, or
  positive integer column indices. An entry not present in `m` is
  treated as an unnamed (novel) object.

- prior:

  Optional prior over `objects` (length `length(objects)`,
  non-negative). Defaults to uniform. Set an entry to 0 to remove that
  object from consideration entirely (e.g. an object the speaker has no
  epistemic access to).

- pragmatic:

  If `TRUE`, use the RSA pragmatic listener instead of the literal one.

- threshold:

  Optional; binarize `m` at this value (`m >= threshold`) before
  predicting. The RSA layer is sharpest on a binary lexicon.

- rsa_alpha, depth:

  Speaker rationality and recursion depth for the RSA listener (only
  used when `pragmatic = TRUE`).

## Value

A numeric vector of probabilities over `objects`, in the given order,
summing to 1.

## Details

The literal rule is Bayes' rule on the word's row,
`P(object | word) proportional to m[word, object] * prior(object)`,
normalized over the objects present. With `pragmatic = TRUE` the objects
are instead resolved by a one-step Rational Speech Act pragmatic
listener
([`rsa_listener()`](https://kachergis.github.io/XSLmodels/reference/rsa_listener.md)),
which additionally reasons that the speaker could have used a different
word – this is what yields *strong* mutual exclusivity when a heard word
is lexically ambiguous and a competitor word names one of the
candidates. A truly novel (unseen) word carries no lexical evidence and
returns the prior in either mode.

## Examples

``` r
m <- xsl_run(fgt2009(alpha = 1), get_example_unambiguous_condition())$fits[[1]]$matrix
predict_referent(m, 1, c(1, 2, 3))
#> [1] 0.4637596 0.2893341 0.2469063
predict_referent(m, 1, c(1, 2, 3), prior = c(1, 0, 1)) # object 2 unavailable
#> [1] 0.6525705 0.0000000 0.3474295
```
