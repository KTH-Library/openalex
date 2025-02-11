
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

test_that("Similar topics can be retrieved given a work", {

  skip_on_ci()

  topics_filter <-
    openalex_filter_similar_topics("W2168078104")

  my_filter <- list(filter = paste0(
#    "publication_year:2024,",
    "institution.id:I2799509149,",
    topics_filter
  ))

  res <- openalex_crawl("works", query = my_filter)

  #one <- res[[1]]$results[[1]]
  #two <- res[[1]]$results[[2]]
  #more <- res[[1]]$results
  res2 <- res |> map("results")
  results <- list(list(results = res2[[1]]))
  works <- results |> openalex_works_to_tbls()

  is_valid <- works$work |> nrow() > 5
  expect_true(is_valid)

})

test_that("Crawling 444 MB of works related to a specific topic in 2023 works", {

  skip()

  q <-
    list(
      filter = paste0(collapse = ",", c(
        "publication_year:2023",
        "primary_topic.id:T10783"
        ))
      )

  res <- openalex_crawl("works", query = q, verbose = TRUE)

  message("JSON object size is ", format(object.size(res), "MB"))

  works <-
    res |> openalex_works_to_tbls()

  message("Tables object size is ", format(object.size(works), "MB"))

  is_valid <- object.size(works) > 7000000

  expect_true(is_valid)

})

