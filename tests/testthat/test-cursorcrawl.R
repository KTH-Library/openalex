test_that("cursor based paging for works works", {

  works_filter <- "publication_year:2015-2023,primary_topic.id:t10783"

  cc <- 
    works_filter |> 
    openalex_works_cursorcrawl(n_max_pages = 50)

})
