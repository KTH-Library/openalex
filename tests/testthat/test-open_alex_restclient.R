# test_that("attribution works", {
#   attribution <- capture.output(cat(openalex_attribution()))
#   is_ok <- length(attribution) == 2 && nchar(attribution) > 0
#   expect_true(is_ok)
# })

test_that("fetching work works", {

  identifier <- "W2741809807"

  #is_ok <- identical(openalex_work(identifier), openalex_work(identifier))
  expected_id <- paste0("https://openalex.org/", identifier)

  table_has_ok_id <-
    subset(openalex_work(identifier), name == "id")$value ==
      expected_id

  object_has_ok_id <-
    openalex_work(identifier, format = "object")$ids$openalex ==
      expected_id

  expect_true(table_has_ok_id && object_has_ok_id)

})

test_that("error 404 is returned for when work is not found", {
  identifier <- "10.1038/nrn3241"
  expect_error(openalex_work(identifier), "404")
})

test_that("fetching random work works", {
  random <- openalex_work(use_random = TRUE)
  is_ok <- nrow(random) > 10
  expect_true(is_ok)
})

# openalex_entity_enum()
#
# openalex_list(entity = "works", query = openalex_query(page = 2))$meta
# openalex_list(entity = "concepts", query = openalex_query(page = 2))$meta
# openalex_list(entity = "institutions", query = openalex_query())$meta
# openalex_list(entity = "venues", query = openalex_query())$meta
# openalex_list(entity = "authors", query = openalex_query())$meta
#
# # works whose type is book
# openalex_list("works", query = openalex_query(
#   filter = "type:book")
# )$meta
#
# #venues that host more than 1000 works:
# openalex_list("venues", query = openalex_query(
#   filter = "works_count:>1000")
# )$meta
#
# # US-based authors who've been cited more than 100 times:
# openalex_list("authors", query = openalex_query(
#   filter = "last_known_institution.country_code:US,cited_by_count:>0")
# )$meta
#
# # works whose title is searched
# openalex_list("works", query = openalex_query(
#   filter = "title.search:'intensive treatment of diabetes'")
# )$meta
#
# res <-
#   openalex_crawl("works", query = openalex_query(
#   filter = "title.search:'intensive treatment of diabetes'")
# )
#
# library(dplyr)
# res %>% openalex_flatten_long() %>% count(name) %>% arrange(desc(n))

test_that("providing email for polite pool gives faster response...", {

  # so initial setting can be restored
  initial <- Sys.getenv("OPENALEX_USERAGENT")
  on.exit(Sys.setenv("OPENALEX_USERAGENT" = initial))

  # not polite
  openalex_polite("")
  tn <- system.time(
    c1 <- openalex_crawl("works", verbose = TRUE,
             query = openalex:::openalex_query(filter =
                 "institutions.id:I86987016,publication_year:2022"))
  )[3]

  # polite
  openalex_polite("markussk@kth.se")
  tp <- system.time(
    c2 <- openalex_crawl("works", verbose = TRUE,
             query = openalex:::openalex_query(filter =
                 "institutions.id:I86987016,publication_year:2022"))
  )[3]

  message("Polite time: ", tp)
  message("Not polite time: ", tn)

  is_faster <- tp < tn
  expect_true(is_faster)

})
