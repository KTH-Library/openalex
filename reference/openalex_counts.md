# Counts from OpenAlex

Aggregates/counts can be retrieved using the group_bys query parameter

## Usage

``` r
openalex_counts(
  filter = openalex_filter_default(),
  dimensions = openalex_groupbys_default()
)
```

## Arguments

- filter:

  a set of filter criteria, see the defaults in
  openalex_filter_default()

- dimensions:

  a set of grouping dimensions, see the defaults in
  openalex_groupbys_default()

## Value

a list of tibbles
