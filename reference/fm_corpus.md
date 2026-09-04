# Frank, Tenenbaum & Fernald naturalistic word-learning corpus

4763 caregiver utterances (24 mother-infant sessions), each paired with
the objects present in the scene. 1122 word types, 30 object types
present in scenes; roughly half the utterances are non-referential.
Unlike
[rollins_corpus](https://kachergis.github.io/XSLmodels/reference/rollins_corpus.md),
the speaker's referential intention is hand-coded per utterance.

## Usage

``` r
fm_corpus
```

## Format

An object of class `list` of length 5.

## Source

Frank, M. C., Tenenbaum, J. B., & Fernald, A. (2013). Social and
discourse contributions to the determination of reference in
cross-situational word learning. *Language Learning and Development,
9*(1), 1-24.

## Details

A list with:

- `data`:

  an
  [xslData](https://kachergis.github.io/XSLmodels/reference/xslData-class.md)
  object with the 4763 training utterances (character-vector
  `words`/`objects` per trial). No `accuracy`/`test`.

- `intents`:

  a length-4763 list; `intents[[t]]` is the object(s) the speaker
  referred to on utterance `t` (character vector, empty for
  non-referential utterances). A per-utterance gold signal.

- `gold`:

  a hand-curated gold lexicon as `list(words, objects)` (41 pairs), for
  [`get_fscore()`](https://kachergis.github.io/XSLmodels/reference/get_fscore.md)
  /
  [`get_roc()`](https://kachergis.github.io/XSLmodels/reference/get_roc.md)
  /
  [`get_tp()`](https://kachergis.github.io/XSLmodels/reference/get_tp.md).

- `gold_variants`:

  `list(strict, permissive)` – two auto-derived alternatives (39 and 116
  pairs) from looser vs. tighter co-occurrence thresholds on the coded
  intentions.

- `reference`:

  the citation string.

Not part of
[xsl_datasets](https://kachergis.github.io/XSLmodels/reference/xsl_datasets.md).
Gaze/hand attentional cues present in the source CSVs are not imported.
Imported from the wurwur package (<https://github.com/mcfrank/wurwur>);
see `data-raw/add_wurwur_corpora.R`.

## Examples

``` r
length(fm_corpus$data$train$words)
#> [1] 4763
fm_corpus$intents[[1]]
#> [1] "dog"
fm_corpus$gold_variants$strict$words
#>  [1] "ball"     "blocks"   "book"     "box"      "lid"      "brushes" 
#>  [7] "cars"     "shawns"   "car"      "doors"    "red"      "opens"   
#> [13] "wheel"    "door"     "fast"     "cheese"   "dog"      "doggie"  
#> [19] "puppy"    "doggys"   "dogs"     "doggies"  "doggy"    "doll"    
#> [25] "rosy"     "hotdog"   "mickey"   "pigs"     "piggys"   "puppet"  
#> [31] "piggie"   "squeaker" "piggy"    "pig"      "squeak"   "trucks"  
#> [37] "truck"    "teddy"    "green"   
```
