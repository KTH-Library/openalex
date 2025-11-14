# Recently updated works based on query for matching raw affiliations

This function requires a premium subscription API key to be set.

## Usage

``` r
openalex_works_updated_since(
  raw_search_criteria = openalex_kth_rawaff_query(),
  since_minutes
)
```

## Arguments

- raw_search_criteria:

  raw affiliation string search criteria, by default
  openalex_kth_rawaff_query()

- since_minutes:

  integer indicating minutes since now

## Value

list of tables with results
