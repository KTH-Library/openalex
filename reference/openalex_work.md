# Retrieve work from OpenAlex REST API

This function retrieves works given an identifier

## Usage

``` r
openalex_work(identifier, format = "table", use_random = FALSE)
```

## Arguments

- identifier:

  string with identifier

- format:

  one of "table" or "object"

- use_random:

  logical to indicate whether to use random identifier, Default: FALSE

## Value

as per format, either a tibble or an R object

## Examples

``` r
if (FALSE) { # \dontrun{
 openalex_work(use_random = TRUE)
 } # }
```
