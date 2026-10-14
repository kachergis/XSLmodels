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
  `test` is given, otherwise per word

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
  (columns, one per object in `train`, in sorted label order)

- x:

  List with elements train, test, accuracy, n_subj, label, condition,
  description, response_matrix

## Value

An object of class xslData
