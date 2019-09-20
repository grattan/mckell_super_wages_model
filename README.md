
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Recreating McKell Institute’s super-wages model

This repo recreates the McKell Institute’s super-wages model presented
in their 2019 research paper [*‘Does higher superannuation reduce
workers’
wages?’*](https://mckellinstitute.org.au/app/uploads/Does-higher-superannuation-reduce-wages.pdf).

It uses the statistical package R and publicly-available data. Three R
scripts are used in order:

1.  `01_get_data.R`:  
2.  `02_prepare_data.R`:
3.  `03_run_regressions.R`:

They produce tables of regression results for four model specifications.

``` r
source("R/01_get_data.R")
source("R/02_prepare_data.R")
source("R/03_run_regressions.R")
```
