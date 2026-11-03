# xslData S3 class

xslData S3 class

Constructor for xslData S3 class

## Usage

``` r
xslData(
  train = list(),
  test = list(),
  accuracy = numeric(),
  n_subj = numeric(),
  label = "",
  condition = "",
  description = "",
  response_matrix = NULL
)

new_xslData(x = list())
```

## Arguments

- train:

  Training data

- test:

  Test data

- accuracy:

  Proportion correct per test item: per test trial (in `test` order) if
  `test` is given, otherwise per word. For a design with several
  training blocks, each followed by the same test, a matrix with one row
  per block (see Details)

- n_subj:

  Number of subjects

- label:

  Label

- condition:

  Condition

- description:

  Description

- response_matrix:

  Optional test-response (confusion) matrix: for each test item (rows,
  in `accuracy` order) the number of participants choosing each object
  (columns, one per object in `train`, in sorted label order). For a
  multi-block design, a list with one such matrix per block

- x:

  List with elements train, test, accuracy, n_subj, label, condition,
  description, response_matrix

## Value

An object of class xslData

## Details

**Multi-block designs.** Some experiments repeat the same training
sequence several times, testing after each repetition (a learning
trajectory). Give `train` as one block and `accuracy` as a matrix with
one row per block; the number of rows is the number of blocks.
[`xsl_run()`](https://www.kachergis.com/XSLmodels/reference/xsl_run.md)
then scores block `b` on a model trained on `b` repetitions of `train`.
