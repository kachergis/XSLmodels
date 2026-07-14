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

  Accuracy data

- n_subj:

  Number of subjects

- label:

  Label

- condition:

  Condition

- description:

  Description

- response_matrix:

  Response matrix

- x:

  List with elements train, test, accuracy, n_subj, label, condition,
  descrption, response_matrix

## Value

An object of class xslData
