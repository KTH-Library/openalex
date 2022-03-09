
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openalex

<!-- badges: start -->

[![R-CMD-check](https://github.com/KTH-Library/openalex/workflows/R-CMD-check/badge.svg)](https://github.com/KTH-Library/openalex/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `openalex` is to provide access to data from
[OpenAlex](https://openalex.org) - an open and comprehensive catalog of
scholarly papers, authors, institutions and more … - to R through the
[Open Alex REST API](https://docs.openalex.org/api)…

## Installation

You can install the current version of `openalex` from
[GitHub](https://github.com/kth-library/openalex) with:

``` r
#install.packages("devtools")
devtools::install_github("kth-library/openalex", dependencies = TRUE)
```

## Example

This is a basic example which shows you how to get information for
papers and authors:

``` r
library(openalex)
library(dplyr)
suppressPackageStartupMessages(library(purrr))
library(knitr)

data <- 
  openalex_crawl("works", verbose = TRUE,
    query = openalex:::openalex_query(filter = 
      "institutions.id:I86987016,publication_year:2022"))

long <- openalex_flatten_long(data)

# number of ids
#long %>% filter(name == "id") %>% nrow()
```

## Rate limits

By providing an email adress you enter the “polite pool” which provides
even less of rate limiting for API requests.

Either provide it in `~/.Renviron` or use helper fcn `openalex_polite()`
to set or unset email:

``` r
library(openalex)

# set an email to use for the session

openalex_polite("you@example.com")
#> Hint: You can provide an email to enter the polite pool
#> To have the setting stick persistently using .Renviron, do ...
#>   file.edit("~/.Renviron")
#>   # and add a line OPENALEX_USERAGENT="http://github.com/hadley/httr (mailto:you@example.com)"
#> Then reload settings for the R environment in the current session
#>   readRenviron("~/.Renviron")
#> Temporarily setting OPENALEX_USERAGENT envvar for this session to: http://github.com/hadley/httr (mailto:you@example.com)
#> [1] TRUE

# unset, and use default user agent string...

openalex_polite("")
#> Exiting from polite pool, email no longer provided in user agent header
#> [1] FALSE
```

## Data source attribution

When data from `openalex` is displayed publicly, this attribution also
needs to be displayed:

``` r
library(openalex)
openalex_attribution()
#> [1] "Data source: OpenAlex API at https://api.openalex.org/\nData license agreement: https://creativecommons.org/publicdomain/zero/1.0/"
```
