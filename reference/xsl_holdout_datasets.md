# Registry of datasets held out from xsl_datasets

A named list, one entry per dataset in the package that is **not** part
of
[xsl_datasets](https://www.kachergis.com/XSLmodels/reference/xsl_datasets.md)
and therefore never seen by
[`get_group_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_group_model_fit.md)/[`get_crossvalidated_model_fit()`](https://www.kachergis.com/XSLmodels/reference/get_crossvalidated_model_fit.md)
– i.e. every dataset available for testing a fitted model's
generalization to genuinely held-out data. This is a lookup table
(dataset name -\> how to score it), not a copy of the data itself – look
up the dataset by its own name (e.g. `benitez2020`, `vlach_debrock2017`)
to use it.

## Usage

``` r
xsl_holdout_datasets
```

## Format

An object of class `list` of length 7.

## Details

Each entry has:

- `scorable`:

  Whether the dataset can be scored with the package's standard diagonal
  convention
  ([`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md) +
  [`mafc_test()`](https://www.kachergis.com/XSLmodels/reference/mafc_test.md)/[`get_perf()`](https://www.kachergis.com/XSLmodels/reference/get_perf.md))
  out of the box.

- `scoring`:

  How to actually score a model's fit against it.

- `note`:

  Why it's held out, or any caveat for using it.

Most of these are standalone for a structural reason (partial-`NA`
accuracy, a non-square association matrix, or no human accuracy data at
all, e.g. the naturalistic corpora) – see each dataset's own
[`?help`](https://rdrr.io/r/utils/help.html) page for the full
rationale. `benitez2020` and `kachergis_initial_accuracy` are the
exceptions: both are directly scorable the same way `xsl_datasets`
conditions are, and are kept out of the group/CV fitting pool
specifically to have real held-out data to test generalization against.

## Examples

``` r
xsl_holdout_datasets$benitez2020$scoring
#> [1] "xsl_run() + mafc_test() / get_perf() (standard diagonal scoring)"
names(xsl_holdout_datasets)[sapply(xsl_holdout_datasets, `[[`, "scorable")]
#> [1] "benitez2020"                "kachergis_initial_accuracy"
```
