
test_that("crawl works (not cursor based) and results can be persisted in db", {

  skip_on_ci()

  my_filter <- paste0(collapse = ",", c(
    "authorships.institutions.lineage:i86987016", ## KTH
    "authorships.institutions.lineage:!i4210161097", ## Bolin Center
    "authorships.institutions.lineage:!i119971240", ## NORDITA
    "type:types/article",
    "primary_location.source.type:source-types/journal|source-types/conference",
    "publication_year:2025"
  ))

  my_query <- openalex:::openalex_query(filter = my_filter)
  works <- openalex_crawl("work", query = my_query, fmt = "object")
  #readr::write_rds(works, "~/openalex-2023.rds")

  # TODO: some error here!
  library(purrr)
  library(dplyr)

  lol <- 
    list(list(results = reduce(works |> map("results"), c)))

  my_works <- 
    lol |> openalex_works_to_tbls()

  is_valid <- 
    attr(works[[1]], "meta")$count == nrow(my_works$work)

  harvest <-
    my_works |> map(\(x) x |> mutate(across(any_of(contains("id")), 
      \(y) gsub("https://openalex.org/", "", y, fixed = TRUE)))
    )

  dump_path <- file.path(tempdir(), "openalex-2025.db")
  harvest |> openalex_write_duckdb(dump_path)
  message("\n Persisted dump at ", dump_path)

  expect_true(is_valid)

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

  works <- openalex_crawl("works", query = my_filter)

  lol <- 
    list(list(results = reduce(works |> map("results"), c)))

  my_works <- 
    lol |> openalex_works_to_tbls()

  is_valid <- my_works$work |> nrow() > 5
  expect_true(is_valid)

})

test_that("Crawling several of works related to a specific topic works", {

  skip_on_ci()

  q <-
    list(
      filter = paste0(collapse = ",", c(
        "publication_year:2025",
        "primary_topic.id:T10783"
        ))
      )

  works <- openalex_crawl("works", query = q, verbose = TRUE)

  message("JSON object size is ", format(object.size(works), "MB"))

  lol <- 
    list(list(results = reduce(works |> map("results"), c)))

  my_works <- 
    lol |> openalex_works_to_tbls()

  message("Tables object size is ", format(object.size(my_works), "MB"))

  message("Number of records are: ", nrow(my_works$work))

  is_valid <- 
    attr(works[[1]], "meta")$count == nrow(my_works$work)

  #is_valid <- object.size(works) > 7000000

  expect_true(is_valid)

})

