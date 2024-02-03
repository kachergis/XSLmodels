<!-- badges: start -->
[![R-CMD-check](https://github.com/kachergis/XSLmodels/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kachergis/XSLmodels/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# XSLmodels

R package for running and fitting cross-situational word learning models to data.

Installation
------------

<!-- To install the released version on [CRAN](https://cran.r-project.org/package=XSLmodels): 

```
install.packages("XSLmodels")
```
-->


To install the latest development version:

```
# install.packages("devtools")
devtools::install_github("kachergis/XSLmodels")
```

Usage
-----

See what models are available:
```
models <- show_models()
```

See what datasets are available:
```
datasets <- show_datasets()
```

Get model fits for entire dataset:
```
group_fit_kachergis <- get_group_model_fit("kachergis")
```

Get cross-validated (5-fold) model fit:
```
cross_val_fit_kachergis <- get_crossvalidated_model_fit("kachergis")
```


For more details, see [this vignette](https://kachergis.github.io/XSLmodels/articles/XSLmodels.html).
