# Export the results from a crawl as a duckdb database file

Export the results from a crawl as a duckdb database file

## Usage

``` r
openalex_write_duckdb(crawl, destdir = NULL, append = TRUE)
```

## Arguments

- crawl:

  the results from running the to_tbls fcn

- destdir:

  the location to save the database file

- append:

  logical, by default TRUE, set to FALSE for overwriting an existing
  database

## Value

file path to the database file
