# Cross-situational word learning experiment data

Cross-situational word learning experiment data

## Usage

``` r
xsl_datasets
```

## Format

An object of class `list` of length 47.

## Details

Most conditions are scored with
[`get_perf()`](https://www.kachergis.com/XSLmodels/reference/get_perf.md)
(choice among all objects). Conditions whose test offered only some of
the objects carry a `test` specification – for each tested word, the
objects offered – and are scored with
[`mafc_test()`](https://www.kachergis.com/XSLmodels/reference/mafc_test.md):
the four 4AFC conditions (211-214), and the 12 filtering conditions
(`filt*E_*L`; Kachergis, Yu, & Shiffrin, 2012). In the filtering
conditions each word co-occurs with its own object and its partner's,
and was tested twice: with the partner's object absent (items 1-12, own
object correct) and with its own object absent (items 13-24, partner's
object correct; `test$correct`), so `accuracy` has 24 entries (see
`data-raw/fix_filtering_tests.R`).

Every condition also carries a `response_matrix`: for each test item,
how many participants chose each object, from which `accuracy` is
exactly recoverable (see `data-raw/fix_freqcd_add_responses.R` and
`data-raw/raw_responses.R`, which document the source files and each
source's participant exclusions).
