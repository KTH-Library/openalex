# Crawl multiple pages of results

Iterates over paged results showing a progress bar

## Usage

``` r
openalex_crawl(entity, query, verbose = FALSE, fmt = "object")
```

## Arguments

- entity:

  one of the values in openalex_entity_enum()

- query:

  an openalex_query object

- verbose:

  boolean to indicate whether to output messages during process

- fmt:

  the return format, one of "object" or "tables"

## Value

R object with results matching the query
