# Lookup DOIs in OpenAlex and save to a database in chunks of 1000

Lookup DOIs in OpenAlex and save to a database in chunks of 1000

## Usage

``` r
doi_lookup_to_duckdb(dois, dest = NULL)
```

## Arguments

- dois:

  a character vector of DOIs

- dest:

  a path to an existing or wanted database file

## Value

file path to the database file
