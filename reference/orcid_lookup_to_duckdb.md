# Lookup ORCiDs in OpenAlex and save to a database in chunks of 100

Lookup ORCiDs in OpenAlex and save to a database in chunks of 100

## Usage

``` r
orcid_lookup_to_duckdb(orcids, dest = NULL)
```

## Arguments

- orcids:

  a character vector of DOIs

- dest:

  a path to an existing or wanted database file

## Value

file path to the database file
