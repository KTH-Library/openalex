# Crawl multipage responses from queries against the API

Chunks and uses cursor based pagination to fetch works

## Usage

``` r
openalex_works_cursorcrawl(works_filter, n_max_pages = 5)
```

## Arguments

- works_filter:

  the works filter

- n_max_pages:

  the max amount of pages to fetch (50 per page)

## Value

paths to downloaded files
