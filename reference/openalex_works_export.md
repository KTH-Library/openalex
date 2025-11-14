# Use OpenAlex API for exporting data in tabular and wos formats

Use OpenAlex API for exporting data in tabular and wos formats

## Usage

``` r
openalex_works_export(q, fmt = c("csv", "wos-plaintext"), raw_string = FALSE)
```

## Arguments

- q:

  the query, for example
  "authorships.institutions.lineage:i86987016,authorships.institutions.lineage:!i4210161097,type:types/article,primary_location.source.type:source-types/journal\|source-types/conference,publication_year:2023"

- fmt:

  the export format, one of "csv" or "wos-plaintext" or
  "wos-plaintext-diva"

- raw_string:

  boolean to indicate whether a raw string should be returned

## Value

a character vector with a raw string with the results from the export or
a data frame
