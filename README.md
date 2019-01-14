# exmplr

The goal of exmplr is to make the analysis of the FARS data a bit easier 

## Installation

You can install exmplr from github with:


``` r
# install.packages("devtools")
devtools::install_github("Liddlle/exmplr")
```

## You can count the number of rows

This is a basic example which shows you how to solve a common problem:

``` r
fars_summarize_years(2013)
  fars_summarize_years(c(2013,2014))
```

## Also, you can draw cool maps 

``` r
fars_map_state(1, 2013)
```

