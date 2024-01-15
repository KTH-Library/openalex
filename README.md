
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openalex

<!-- badges: start -->

[![R-CMD-check](https://github.com/KTH-Library/openalex/workflows/R-CMD-check/badge.svg)](https://github.com/KTH-Library/openalex/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `openalex` is to provide access to data from
[OpenAlex](https://openalex.org) - an open and comprehensive catalog of
scholarly papers, authors, institutions and more … - to R through the
[Open Alex REST API](https://docs.openalex.org/api)…

## Installation

You can install the current version of `openalex` from
[GitHub](https://github.com/kth-library/openalex) with:

``` r
#install.packages("devtools")
devtools::install_github("kth-library/openalex", dependencies = TRUE)
```

## Example

This is a basic example which shows you how to get information for
papers and authors:

``` r

library(openalex)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
suppressPackageStartupMessages(library(purrr))
library(knitr)

iid <- 
  openalex:::openalex_autocomplete(
    query = "Royal Institute of Technology", 
    entity_type = "institution", 
    format = "table") |> 
  head(1) |> 
  pull("id")
#> Requesting url: https://api.openalex.org/autocomplete/institutions?q=Royal%20Institute%20of%20Technology

data <- 
  openalex_crawl(entity = "works", verbose = TRUE, fmt = "tables",
    query = openalex:::openalex_query(filter =
      sprintf("institutions.id:%s,publication_year:2024", iid)))
#> About to crawl a total of 9 pages of results with a total of 207 records.
#> ■■■■■■■■ 22% | ETA: 5s ■■■■■■■■■■■ 33% | ETA: 5s ■■■■■■■■■■■■■■ 44% | ETA: 4s
#> ■■■■■■■■■■■■■■■■■■ 56% | ETA: 3s ■■■■■■■■■■■■■■■■■■■■■ 67% | ETA: 2s
#> ■■■■■■■■■■■■■■■■■■■■■■■■ 78% | ETA: 1s ■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 89% | ETA:
#> 1s

res <- data |> map(head)  # return only first six rows from each table

res
#> $work
#> # A tibble: 6 × 13
#>   id            doi   display_name title publication_year publication_date type 
#>   <chr>         <chr> <chr>        <chr>            <int> <chr>            <chr>
#> 1 https://open… http… Human Digit… Huma…             2024 2024-02-01       arti…
#> 2 https://open… http… Skeleton-RG… Skel…             2024 2024-04-01       arti…
#> 3 https://open… http… Cognitive n… Cogn…             2024 2024-02-01       arti…
#> 4 https://open… http… The Calvin … The …             2024 2024-03-01       arti…
#> 5 https://open… http… ACWGAN-GP f… ACWG…             2024 2024-02-01       arti…
#> 6 https://open… http… A biobased … A bi…             2024 2024-01-01       arti…
#> # ℹ 6 more variables: cited_by_count <int>, is_retracted <lgl>,
#> #   is_paratext <lgl>, updated_date <chr>, cited_by_api_url <chr>,
#> #   created_date <chr>
#> 
#> $work_ids
#> # A tibble: 6 × 4
#>   work_id                          doi                               pmid  mag  
#>   <chr>                            <chr>                             <chr> <chr>
#> 1 https://openalex.org/W4385324205 https://doi.org/10.1016/j.rcim.2… <NA>  <NA> 
#> 2 https://openalex.org/W4387083563 https://doi.org/10.1016/j.rcim.2… <NA>  <NA> 
#> 3 https://openalex.org/W4385217308 https://doi.org/10.1016/j.rcim.2… <NA>  <NA> 
#> 4 https://openalex.org/W4361283233 https://doi.org/10.1016/j.semcdb… http… <NA> 
#> 5 https://openalex.org/W4385437253 https://doi.org/10.1016/j.rcim.2… <NA>  <NA> 
#> 6 https://openalex.org/W4386987514 https://doi.org/10.1016/j.carbpo… http… <NA> 
#> 
#> $work_concepts
#> # A tibble: 6 × 6
#>   work_id                          id          wikidata display_name level score
#>   <chr>                            <chr>       <chr>    <chr>        <int> <dbl>
#> 1 https://openalex.org/W4385324205 https://op… https:/… Context (ar…     2 0.575
#> 2 https://openalex.org/W4385324205 https://op… https:/… Industry 4.0     2 0.559
#> 3 https://openalex.org/W4385324205 https://op… https:/… Knowledge m…     1 0.523
#> 4 https://openalex.org/W4385324205 https://op… https:/… Connotation      2 0.486
#> 5 https://openalex.org/W4385324205 https://op… https:/… Computer sc…     0 0.423
#> 6 https://openalex.org/W4385324205 https://op… https:/… Engineering      0 0.417
#> 
#> $work_authorships_institutions
#> # A tibble: 6 × 12
#>   work_id       id    display_name ror   country_code type  lineage wa_countries
#>   <chr>         <chr> <chr>        <chr> <chr>        <chr> <list>  <list>      
#> 1 https://open… http… Zhejiang Un… http… CN           educ… <list>  <list [1]>  
#> 2 https://open… http… Zhejiang Un… http… CN           educ… <list>  <list [1]>  
#> 3 https://open… http… Purdue Univ… http… US           educ… <list>  <list [1]>  
#> 4 https://open… http… Zhejiang Un… http… CN           educ… <list>  <list [1]>  
#> 5 https://open… http… Zhejiang Lab http… CN           faci… <list>  <list [1]>  
#> 6 https://open… http… Hong Kong P… http… HK           educ… <list>  <list [1]>  
#> # ℹ 4 more variables: wa_is_corresponding <lgl>, wa_raw_author_name <chr>,
#> #   wa_raw_affiliation_string <chr>, wa_raw_affiliation_strings <list>
#> 
#> $work_abstract_inverted_index
#> # A tibble: 6 × 3
#>   work_id                          aii_value                             aii_key
#>   <chr>                            <chr>                                 <chr>  
#> 1 https://openalex.org/W4385324205 0 1 43 141 164 181 2 3 4 12 19 34 60… Human-…
#> 2 https://openalex.org/W4387083563 0 1 19 2 20 26 3 4 5 12 28 62 103 13… Human–…
#> 3 https://openalex.org/W4385217308 0 105 1 2 3 4 111 5 6 7 8 42 9 10 33… In rec…
#> 4 https://openalex.org/W4361283233 0 1 31 67 156 193 2 32 68 194 3 33 6… The Ca…
#> 5 https://openalex.org/W4385437253 0 1 42 61 170 2 3 4 131 5 176 6 7 63… Tool b…
#> 6 https://openalex.org/W4386987514 0 94 148 169 1 2 105 119 177 3 4 5 8… The am…
#> 
#> $work_authorships_author
#> # A tibble: 6 × 10
#>   work_id               wa_author_position id    display_name orcid wa_countries
#>   <chr>                 <chr>              <chr> <chr>        <chr> <list>      
#> 1 https://openalex.org… first              http… Baicun Wang  http… <list [1]>  
#> 2 https://openalex.org… middle             http… Huiying Zhou http… <list [1]>  
#> 3 https://openalex.org… middle             http… Xingyu Li    http… <list [1]>  
#> 4 https://openalex.org… middle             http… Geng Yang    http… <list [1]>  
#> 5 https://openalex.org… middle             http… Pai Zheng    http… <list [1]>  
#> 6 https://openalex.org… middle             http… Song Ci      http… <list [1]>  
#> # ℹ 4 more variables: wa_is_corresponding <lgl>, wa_raw_author_name <chr>,
#> #   wa_raw_affiliation_string <chr>, wa_raw_affiliation_strings <list>
#> 
#> $work_biblio
#> # A tibble: 6 × 5
#>   work_id                          volume first_page last_page issue
#>   <chr>                            <chr>  <chr>      <chr>     <chr>
#> 1 https://openalex.org/W4385324205 85     102626     102626    <NA> 
#> 2 https://openalex.org/W4387083563 86     102659     102659    <NA> 
#> 3 https://openalex.org/W4385217308 85     102610     102610    <NA> 
#> 4 https://openalex.org/W4361283233 155    71         83        <NA> 
#> 5 https://openalex.org/W4385437253 85     102624     102624    <NA> 
#> 6 https://openalex.org/W4386987514 323    121430     121430    <NA> 
#> 
#> $work_open_access
#> # A tibble: 6 × 5
#>   work_id                          is_oa oa_status any_repository_has_f…¹ oa_url
#>   <chr>                            <lgl> <chr>     <lgl>                  <chr> 
#> 1 https://openalex.org/W4385324205 FALSE closed    FALSE                  <NA>  
#> 2 https://openalex.org/W4387083563 FALSE closed    FALSE                  <NA>  
#> 3 https://openalex.org/W4385217308 TRUE  hybrid    FALSE                  https…
#> 4 https://openalex.org/W4361283233 TRUE  hybrid    FALSE                  https…
#> 5 https://openalex.org/W4385437253 FALSE closed    FALSE                  <NA>  
#> 6 https://openalex.org/W4386987514 FALSE closed    FALSE                  <NA>  
#> # ℹ abbreviated name: ¹​any_repository_has_fulltext
#> 
#> $work_host_venue
#> # A tibble: 6 × 1
#>   work_id                         
#>   <chr>                           
#> 1 https://openalex.org/W4385324205
#> 2 https://openalex.org/W4387083563
#> 3 https://openalex.org/W4385217308
#> 4 https://openalex.org/W4361283233
#> 5 https://openalex.org/W4385437253
#> 6 https://openalex.org/W4386987514
#> 
#> $work_counts_by_year
#> # A tibble: 6 × 3
#>   work_id                           year cited_by_count
#>   <chr>                            <int>          <int>
#> 1 https://openalex.org/W4385324205  2023              4
#> 2 https://openalex.org/W4387083563  2023              4
#> 3 https://openalex.org/W4385217308  2023              2
#> 4 https://openalex.org/W4361283233  2023              2
#> 5 https://openalex.org/W4386987514  2023              1
#> 6 https://openalex.org/W4312529696  2023              1
#> 
#> $work_related_works
#> # A tibble: 6 × 2
#>   work_id                          related_works                   
#>   <chr>                            <chr>                           
#> 1 https://openalex.org/W4385324205 https://openalex.org/W2360733422
#> 2 https://openalex.org/W4385324205 https://openalex.org/W2362583379
#> 3 https://openalex.org/W4385324205 https://openalex.org/W2386886609
#> 4 https://openalex.org/W4385324205 https://openalex.org/W2394171004
#> 5 https://openalex.org/W4385324205 https://openalex.org/W2361870167
#> 6 https://openalex.org/W4385324205 https://openalex.org/W2381092078
#> 
#> $work_referenced_works
#> # A tibble: 6 × 2
#>   work_id                          referenced_works                
#>   <chr>                            <chr>                           
#> 1 https://openalex.org/W4385324205 https://openalex.org/W1706260973
#> 2 https://openalex.org/W4385324205 https://openalex.org/W2000982104
#> 3 https://openalex.org/W4385324205 https://openalex.org/W2007171144
#> 4 https://openalex.org/W4385324205 https://openalex.org/W2010613163
#> 5 https://openalex.org/W4385324205 https://openalex.org/W2020304579
#> 6 https://openalex.org/W4385324205 https://openalex.org/W2026892459
```

## Rate limits and using an API key

By providing an email address you enter the “polite pool” which provides
even less of rate limiting for API requests.

You can provide it in `~/.Renviron` by setting
`OPENALEX_USERAGENT=http://github.com/hadley/httr (mailto:your_email@your_institution.org)`.

You can also set it just for the session by using a helper fcn
`openalex_polite()` to temporarily set or unset the email used in the
user agent string when making API requests:

``` r
library(openalex)

# set an email to use for the session

openalex_polite("you@example.com")
#> Hint: You can provide an email to enter the polite pool
#> To have the setting stick persistently using .Renviron, do ...
#>   file.edit("~/.Renviron")
#>   # and add a line OPENALEX_USERAGENT="http://github.com/hadley/httr (mailto:you@example.com)"
#> Then reload settings for the R environment in the current session
#>   readRenviron("~/.Renviron")
#> Temporarily setting OPENALEX_USERAGENT envvar for this session to: http://github.com/hadley/httr (mailto:you@example.com)
#> [1] TRUE

# unset, and use default user agent string...

openalex_polite("")
#> Exiting from polite pool, email no longer provided in user agent header
#> [1] FALSE
```

A premium subscription API key can be used by setting
`OPENALEX_KEY=secret_premium_api_key` in your `.Renviron`, or
temporarily in a session using:

``` r
library(openalex)

# temporarily use a premium subscription API key
openalex_key("secret_premium_api_key")

# unset to not use the premium subscription API key
openalex_key("")
```

This will make it possible to make API calls that return the latest
available records, for example based on recent creation dates or recent
last modification timestamps.

``` r

# we do not require an API key for the publish date
published_since_ <- openalex_works_published_since(since_days = 7)
#> About to crawl a total of 4 pages of results with a total of 97 records.
#> ■■■■■■■■■■■■■■■■ 50% | ETA: 2s ■■■■■■■■■■■■■■■■■■■■■■■ 75% | ETA: 1s

# but an API key is needed when using "from_created_date" and "from_updated_date" fields.
created_since_7d <- openalex_works_created_since(since_days = 7)
#> About to crawl a total of 1 pages of results with a total of 17 records.
updated_since_1h <- openalex_works_updated_since(since_minutes = 60)
#> About to crawl a total of 8 pages of results with a total of 177 records.
#>  ■■■■■■■■■                         25% |  ETA:  6s ■■■■■■■■■■■■                      38% |  ETA:  6s ■■■■■■■■■■■■■■■■                  50% |  ETA:  5s ■■■■■■■■■■■■■■■■■■■■              62% |  ETA:  3s ■■■■■■■■■■■■■■■■■■■■■■■           75% |  ETA:  2s ■■■■■■■■■■■■■■■■■■■■■■■■■■■       88% |  ETA:  1s                                                   

# first few rows of each of these retrievals
created_since_7d |> _$work_ids |> head() |> knitr::kable()
```

| work_id                            | doi                                            | pmid |
|:-----------------------------------|:-----------------------------------------------|:-----|
| <https://openalex.org/W4390642226> | <https://doi.org/10.2478/ncr-2023-0011>        | NA   |
| <https://openalex.org/W4390639369> | <https://doi.org/10.1016/j.seja.2023.100050>   | NA   |
| <https://openalex.org/W4390638132> | <https://doi.org/10.1007/s10798-023-09868-0>   | NA   |
| <https://openalex.org/W4390640547> | <https://doi.org/10.1016/j.carbon.2024.118799> | NA   |
| <https://openalex.org/W4390642146> | <https://doi.org/10.2478/ncr-2023-0012>        | NA   |
| <https://openalex.org/W4390640114> | <https://doi.org/10.1016/j.cmpb.2024.108009>   | NA   |

``` r
updated_since_1h |> _$work_ids |> head() |> knitr::kable()
```

| work_id                            | doi                                            | mag        | pmid                                       | pmcid                                               |
|:-----------------------------------|:-----------------------------------------------|:-----------|:-------------------------------------------|:----------------------------------------------------|
| <https://openalex.org/W2808465642> | <https://doi.org/10.1038/s41467-018-04724-5>   | 2808465642 | <https://pubmed.ncbi.nlm.nih.gov/29925878> | <https://www.ncbi.nlm.nih.gov/pmc/articles/6010471> |
| <https://openalex.org/W4294783296> | <https://doi.org/10.1021/jacs.2c06920>         | NA         | <https://pubmed.ncbi.nlm.nih.gov/36066387> | NA                                                  |
| <https://openalex.org/W2103896911> | <https://doi.org/10.1016/j.jmb.2004.06.033>    | 2103896911 | <https://pubmed.ncbi.nlm.nih.gov/15288786> | NA                                                  |
| <https://openalex.org/W1974228254> | <https://doi.org/10.1109/percomw.2012.6197506> | 1974228254 | NA                                         | NA                                                  |
| <https://openalex.org/W4224866865> | <https://doi.org/10.1021/acscatal.2c00188>     | NA         | NA                                         | NA                                                  |
| <https://openalex.org/W2018035699> | <https://doi.org/10.1128/aem.03678-13>         | 2018035699 | <https://pubmed.ncbi.nlm.nih.gov/24487538> | <https://www.ncbi.nlm.nih.gov/pmc/articles/3993154> |

## Data source attribution

When data from `openalex` is displayed publicly, this attribution also
needs to be displayed:

``` r
library(openalex)
openalex_attribution()
#> [1] "Data source: OpenAlex API at https://api.openalex.org/\nData license agreement: https://creativecommons.org/publicdomain/zero/1.0/"
```
