# Update known

Utility function for several models that fills in a given co-occurrence
matrix m with startval, for cells of m that were 0 before

## Usage

``` r
update_known(m, tr_w, tr_o, startval = 0.01)
```

## Arguments

- m:

  Co-occurrence matrix

- tr_w:

  Words

- tr_o:

  Objects

- startval:

  Starting value
