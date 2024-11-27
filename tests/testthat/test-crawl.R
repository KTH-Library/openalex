
test_that("crawl works", {

  skip()

  my_filter <- paste0(collapse = ",", c(
    "authorships.institutions.lineage:i86987016", ## KTH
    "authorships.institutions.lineage:!i4210161097", ## Bolin Center
    "authorships.institutions.lineage:!i119971240", ## NORDITA   
    "type:types/article",
    "primary_location.source.type:source-types/journal|source-types/conference",
    "publication_year:2024"
  ))

  my_query <- openalex:::openalex_query(filter = my_filter)
  works <- openalex_crawl("work", query = my_query, fmt = "object")

  #readr::write_rds(works, "~/openalex-2023.rds")

  # TODO: some error here!
  library(purrr)
  library(dplyr)

  to_tbls <- function(x) {
    parsed <- x |> map(parse_work2, .progress = TRUE)
    list_transpose(parsed) |> map(bind_rows)
  }

  my_works <- works |> to_tbls()

  harvest <- 
    my_works |> 
    map(\(x) x |> mutate(across(any_of(contains("id")), \(y) gsub("https://openalex.org/", "", y, fixed = TRUE))))

  openalex_write_duckdb(harvest, "~/openalex-2023.db")

})
