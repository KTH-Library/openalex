# Recently published works based on query for matching raw affiliations

Recently published works based on query for matching raw affiliations

## Usage

``` r
openalex_works_published_since(
  raw_search_criteria = openalex_kth_rawaff_query(),
  since_days = 7
)
```

## Arguments

- raw_search_criteria:

  raw affiliation string search criteria, by default
  openalex_kth_rawaff_query()

- since_days:

  integer indicating days back from today

## Value

list of tables with results
