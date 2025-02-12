
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openalex

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/KTH-Library/openalex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KTH-Library/openalex/actions/workflows/R-CMD-check.yaml)
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
      sprintf("institutions.id:%s,publication_year:2025", iid)))
#> About to crawl a total of 11 pages of results with a total of 257 records.
#>  ■■■■■■■■■                         27% |  ETA:  3s
#>  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      91% |  ETA:  0s

res <- data |> map(head)  # return only first six rows from each table

res
#> $work
#> # A tibble: 6 × 13
#>   id          doi     display_name title publication_year publication_date type 
#>   <chr>       <chr>   <chr>        <chr>            <int> <date>           <chr>
#> 1 W4405989080 https:… Molecular b… Mole…             2025 2025-01-01       arti…
#> 2 W4406001145 https:… Tracking th… Trac…             2025 2025-01-02       arti…
#> 3 W4406016239 https:… DGCR2 targe… DGCR…             2025 2025-01-02       arti…
#> 4 W4406082819 https:… Screening a… Scre…             2025 2025-01-05       arti…
#> 5 W4406172907 https:… Static deep… Stat…             2025 2025-01-01       arti…
#> 6 W4406435778 https:… Consistent,… Cons…             2025 2025-01-16       arti…
#> # ℹ 6 more variables: cited_by_count <int>, is_retracted <lgl>,
#> #   is_paratext <lgl>, updated_date <dttm>, cited_by_api_url <chr>,
#> #   created_date <date>
#> 
#> $work_ids
#> # A tibble: 6 × 3
#>   work_id     doi                                          pmid                 
#>   <chr>       <chr>                                        <chr>                
#> 1 W4405989080 https://doi.org/10.1016/j.cell.2024.11.036   https://pubmed.ncbi.…
#> 2 W4406001145 https://doi.org/10.1038/s41467-024-55688-8   https://pubmed.ncbi.…
#> 3 W4406016239 https://doi.org/10.1038/s41598-024-84574-y   https://pubmed.ncbi.…
#> 4 W4406082819 https://doi.org/10.1186/s12896-024-00926-6   https://pubmed.ncbi.…
#> 5 W4406172907 https://doi.org/10.1063/5.0248856            <NA>                 
#> 6 W4406435778 https://doi.org/10.21468/scipostphyscodeb.45 <NA>                 
#> 
#> $work_concepts
#> # A tibble: 6 × 6
#>   work_id     id         wikidata                       display_name level score
#>   <chr>       <chr>      <chr>                          <chr>        <int> <dbl>
#> 1 W4405989080 C86803240  https://www.wikidata.org/wiki… Biology          0 0.928
#> 2 W4405989080 C170493617 https://www.wikidata.org/wiki… Receptor         2 0.615
#> 3 W4405989080 C70721500  https://www.wikidata.org/wiki… Computation…     1 0.466
#> 4 W4405989080 C12426560  https://www.wikidata.org/wiki… Basis (line…     2 0.455
#> 5 W4405989080 C135285700 https://www.wikidata.org/wiki… G protein-c…     3 0.449
#> 6 W4405989080 C95444343  https://www.wikidata.org/wiki… Cell biology     1 0.389
#> 
#> $work_authorships_institutions
#> # A tibble: 6 × 12
#>   work_id     id      display_name ror   country_code type  lineage wa_countries
#>   <chr>       <chr>   <chr>        <chr> <chr>        <chr> <list>  <list>      
#> 1 W4405989080 I18067… University … http… US           fund… <list>  <list [1]>  
#> 2 W4405989080 I18067… University … http… US           fund… <list>  <list [1]>  
#> 3 W4405989080 I11402… University … http… US           fund… <list>  <list [1]>  
#> 4 W4405989080 I42101… National In… http… US           fund… <list>  <list [1]>  
#> 5 W4405989080 I28001… Science for… http… SE           fund… <list>  <list [1]>  
#> 6 W4405989080 I86987… KTH Royal I… http… SE           fund… <list>  <list [1]>  
#> # ℹ 4 more variables: wa_is_corresponding <lgl>, wa_raw_author_name <chr>,
#> #   wa_raw_affiliation_strings <list>, wa_affiliations <list>
#> 
#> $work_abstract_inverted_index
#> # A tibble: 6 × 3
#>   work_id     aii_value                                                  aii_key
#>   <chr>       <chr>                                                      <chr>  
#> 1 W4405989080 <NA>                                                       <NA>   
#> 2 W4406001145 0 1 8 15 19 23 44 60 81 86 90 99 106 123 127 138 149 166 … Regula…
#> 3 W4406016239 <NA>                                                       <NA>   
#> 4 W4406082819 0 1 2 252 279 3 26 69 77 114 118 134 142 164 193 198 211 … Abstra…
#> 5 W4406172907 0 1 88 2 3 12 4 5 6 7 8 58 9 59 10 60 11 13 14 15 20 35 6… As com…
#> 6 W4406435778 0 1 55 67 2 3 4 51 87 5 6 7 64 83 96 103 8 9 17 23 44 50 … Histog…
#> 
#> $work_authorships_author
#> # A tibble: 6 × 10
#>   work_id     wa_author_position id          display_name     orcid wa_countries
#>   <chr>       <chr>              <chr>       <chr>            <chr> <list>      
#> 1 W4405989080 first              A5035848333 Matthew K. Howa… http… <list [1]>  
#> 2 W4405989080 middle             A5106732468 Nick Hoppe       http… <list [1]>  
#> 3 W4405989080 middle             A5089436626 Xi‐Ping Huang    http… <list [1]>  
#> 4 W4405989080 middle             A5063488695 Darko Mitrovic   http… <list [1]>  
#> 5 W4405989080 middle             A5036507080 Christian B. Bi… http… <list [1]>  
#> 6 W4405989080 middle             A5080561155 Christian B. Ma… http… <list [1]>  
#> # ℹ 4 more variables: wa_is_corresponding <lgl>, wa_raw_author_name <chr>,
#> #   wa_raw_affiliation_strings <list>, wa_affiliations <list>
#> 
#> $work_biblio
#> # A tibble: 6 × 5
#>   work_id     volume issue first_page last_page
#>   <chr>        <dbl> <chr> <chr>      <chr>    
#> 1 W4405989080     NA <NA>  <NA>       <NA>     
#> 2 W4406001145     16 1     <NA>       <NA>     
#> 3 W4406016239     15 1     <NA>       <NA>     
#> 4 W4406082819     25 1     <NA>       <NA>     
#> 5 W4406172907     37 1     <NA>       <NA>     
#> 6 W4406435778     NA <NA>  <NA>       <NA>     
#> 
#> $work_open_access
#> # A tibble: 6 × 5
#>   work_id     is_oa oa_status oa_url                      any_repository_has_f…¹
#>   <chr>       <lgl> <chr>     <chr>                       <lgl>                 
#> 1 W4405989080 TRUE  green     https://doi.org/10.1101/20… TRUE                  
#> 2 W4406001145 TRUE  gold      https://doi.org/10.1038/s4… FALSE                 
#> 3 W4406016239 TRUE  gold      https://doi.org/10.1038/s4… FALSE                 
#> 4 W4406082819 TRUE  gold      https://doi.org/10.1186/s1… FALSE                 
#> 5 W4406172907 TRUE  hybrid    https://doi.org/10.1063/5.… FALSE                 
#> 6 W4406435778 TRUE  hybrid    https://doi.org/10.21468/s… TRUE                  
#> # ℹ abbreviated name: ¹​any_repository_has_fulltext
#> 
#> $work_host_venue
#> # A tibble: 6 × 1
#>   work_id    
#>   <chr>      
#> 1 W4405989080
#> 2 W4406001145
#> 3 W4406016239
#> 4 W4406082819
#> 5 W4406172907
#> 6 W4406435778
#> 
#> $work_counts_by_year
#> # A tibble: 6 × 3
#>   work_id      year cited_by_count
#>   <chr>       <int>          <int>
#> 1 W4405989080  2025              1
#> 2 W4406001145  2025              1
#> 3 W4406082819  2025              1
#> 4 W4406172907  2025              1
#> 5 W4406435778  2025              1
#> 6 W4406435863  2025              1
#> 
#> $work_related_works
#> # A tibble: 6 × 2
#>   work_id     related_works
#>   <chr>       <chr>        
#> 1 W4405989080 W4391375266  
#> 2 W4405989080 W4224216382  
#> 3 W4405989080 W416861399   
#> 4 W4405989080 W3195483439  
#> 5 W4405989080 W3011298851  
#> 6 W4405989080 W2609050007  
#> 
#> $work_referenced_works
#> # A tibble: 6 × 2
#>   work_id     referenced_works
#>   <chr>       <chr>           
#> 1 W4405989080 W1031578623     
#> 2 W4405989080 W1483147211     
#> 3 W4405989080 W1503765703     
#> 4 W4405989080 W1513618424     
#> 5 W4405989080 W1833104430     
#> 6 W4405989080 W189880865
```

## Rate limits and using an API key

By providing an email address you enter the “polite pool” which provides
even less of rate limiting for API requests.

You can provide it in `~/.Renviron` by setting
`OPENALEX_USERAGENT=http://github.com/hadley/httr
(mailto:your_email@your_institution.org)`.

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
#> About to crawl a total of 1 pages of results with a total of 21 records.
#> Warning: `type_convert()` only converts columns of type 'character'.
#> - `df` has no columns of type 'character'

# but an API key is needed when using "from_created_date" and "from_updated_date" fields.
created_since_7d <- openalex_works_created_since(since_days = 7)
#> About to crawl a total of 2 pages of results with a total of 44 records.
#> Warning: `type_convert()` only converts columns of type 'character'.
#> - `df` has no columns of type 'character'
updated_since_1h <- openalex_works_updated_since(since_minutes = 60)
#> About to crawl a total of 18 pages of results with a total of 442 records.
#>  ■■■                                6% |  ETA: 32s
#>  ■■■■                              11% |  ETA: 35s
#>  ■■■■■■                            17% |  ETA: 31s
#>  ■■■■■■■■■                         28% |  ETA: 28s
#>  ■■■■■■■■■■■                       33% |  ETA: 26s
#>  ■■■■■■■■■■■■■■                    44% |  ETA: 21s
#>  ■■■■■■■■■■■■■■■■                  50% |  ETA: 20s
#>  ■■■■■■■■■■■■■■■■■■                56% |  ETA: 17s
#>  ■■■■■■■■■■■■■■■■■■■■■             67% |  ETA: 13s
#>  ■■■■■■■■■■■■■■■■■■■■■■■           72% |  ETA: 11s
#>  ■■■■■■■■■■■■■■■■■■■■■■■■          78% |  ETA:  9s
#>  ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% |  ETA:  7s
#>  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% |  ETA:  5s
#>  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% |  ETA:  2s

# first few rows of each of these retrievals
created_since_7d |> _$work_ids |> head() |> knitr::kable()
```

| work\_id    | doi                                          | pmid                                       |
| :---------- | :------------------------------------------- | :----------------------------------------- |
| W4407223209 | <https://doi.org/10.1063/5.0238622>          | NA                                         |
| W4407212560 | <https://doi.org/10.1109/tac.2025.3539332>   | NA                                         |
| W4407347106 | <https://doi.org/10.1002/srin.202400360>     | NA                                         |
| W4407235267 | <https://doi.org/10.1002/tea.22031>          | NA                                         |
| W4407203234 | <https://doi.org/10.1038/s41467-025-56408-6> | <https://pubmed.ncbi.nlm.nih.gov/39915442> |
| W4407277358 | <https://doi.org/10.3934/cpaa.2025036>       | NA                                         |

``` r
updated_since_1h |> _$work_ids |> head() |> knitr::kable()
```

| work\_id    | doi                                              |        mag | pmid                                       | pmcid |
| :---------- | :----------------------------------------------- | ---------: | :----------------------------------------- | :---- |
| W2010417920 | <https://doi.org/10.1016/j.wasman.2007.02.015>   | 2010417920 | <https://pubmed.ncbi.nlm.nih.gov/17434726> | NA    |
| W2076716399 | <https://doi.org/10.1016/j.tra.2008.11.014>      | 2076716399 | NA                                         | NA    |
| W2130306081 | <https://doi.org/10.1016/j.trc.2015.01.015>      | 2130306081 | NA                                         | NA    |
| W2153980567 | <https://doi.org/10.1016/j.trd.2010.12.004>      | 2153980567 | NA                                         | NA    |
| W2131628642 | <https://doi.org/10.1016/j.techfore.2009.03.003> | 2131628642 | NA                                         | NA    |
| W2045503958 | <https://doi.org/10.1016/j.techfore.2010.09.005> | 2045503958 | NA                                         | NA    |

## Data source attribution

When data from `openalex` is displayed publicly, this attribution also
needs to be displayed:

``` r
library(openalex)
openalex_attribution()
#> [1] "Data source: OpenAlex API at https://api.openalex.org/\nData license agreement: https://creativecommons.org/publicdomain/zero/1.0/"
```
