test_that("export works", {

  skip()

  my_filter <- paste0(collapse = ",", c(
    "authorships.institutions.lineage:i86987016", ## KTH
    "authorships.institutions.lineage:i4210161097", ## Bolin Center (some of these might be KTH pubs!)
    "authorships.institutions.lineage:i119971240", ## NORDITA (some of these, too!)
    "authorships.institutions.lineage:i4210147696", ## THS Tekniska Högskolans Studentkår
    "type:types/article",
    "primary_location.source.type:source-types/journal|source-types/conference",
    "publication_year:2025"
  ))

  # TODO:
  # for weekly import - include centres
  # for retroactive import - exclude centres

  my_csv <- openalex_works_export(q = my_filter, fmt = "csv")
  my_csv_string <- openalex_works_export(q = my_filter, fmt = "csv", raw_string = TRUE)
  my_wos <- openalex_works_export(q = my_filter, fmt = "wos-plaintext")
  my_wos_string <- openalex_works_export(q = my_filter, fmt = "wos-plaintext", raw_string = TRUE)

  is_valid <- nrow(my_csv) > 2000 & nrow(my_wos) > 2000
  expect_true(is_valid)
})

test_that("export for diva in wos-plain text format works", {

  skip_on_ci()

  my_filter <- paste0(collapse = ",", c(
    sprintf("publication_year:%s", 2025),
    sprintf("authorships.author.id:%s", "a5045975901") #,
  #   sprintf("raw_affiliation_strings.search:%s", openalex_kth_rawaff_query()),
  #   "authorships.institutions.lineage:!i86987016", ## KTH
  #   "authorships.institutions.lineage:!i4210161097", ## Bolin Center (some of these might be KTH pubs!)
  #   "authorships.institutions.lineage:!i119971240", ## NORDITA (some of these, too!)
  #   "authorships.institutions.lineage:!i4210147696" ## THS Tekniska Högskolans Studentkår
  ))

  gm <- openalex_works_export(q = my_filter, fmt = "wos-plaintext")

  #cat(gm)

  is_valid <- 
    (regmatches(gm, gregexpr("KTH", gm)) |> unlist()) |> length() > 10

  expect_true(is_valid)
})

test_that("export of rawff query for 2025 in wos-plain diva text format works", {

  skip_on_ci()

  my_filter <- paste0(collapse = ",", c(
    sprintf("publication_year:%s", 2025),
    sprintf("raw_affiliation_strings.search:%s", openalex_kth_rawaff_query()),
    "authorships.institutions.lineage:!i86987016", ## KTH
    "authorships.institutions.lineage:!i4210161097", ## Bolin Center (some of these might be KTH pubs!)
    "authorships.institutions.lineage:!i119971240", ## NORDITA (some of these, too!)
    "authorships.institutions.lineage:!i4210147696" ## THS Tekniska Högskolans Studentkår
  ))

  extras <- openalex_works_export(q = my_filter, fmt = "wos-plaintext")
  is_valid <- (regmatches(extras, gregexpr("ER", extras)) |> unlist()) |> length() > 0
  expect_true(is_valid)
})

test_that("export of rawff query for 2025 in csv format works", {

  skip_on_ci()

  my_filter <- paste0(collapse = ",", c(
    sprintf("publication_year:%s", 2025),
    sprintf("raw_affiliation_strings.search:%s", openalex_kth_rawaff_query())
  ))

  csv <- openalex_works_export(my_filter, "csv")

  #arrow::write_parquet(csv, "~/oa-2025-csv-export.parquet")

  is_valid <- nrow(csv) > 1

  expect_true(is_valid)

})

test_that("export of rawff query for 2025 in csv format works", {

  skip_on_ci()

  my_filter <- paste0(collapse = ",", c(
    sprintf("publication_year:%s", 2025),
    sprintf("raw_affiliation_strings.search:%s", openalex_kth_rawaff_query())
  ))

  csv <- openalex_works_export(my_filter, "csv")

  #arrow::write_parquet(csv_2023, "~/oa-2023-csv-export.parquet")

  is_valid <- nrow(csv) > 1

  expect_true(is_valid)

})



# TODO: Vilka konferenspapper finns i OA som är kopplade till KTH (behöver inte ha DOI)?
# TODO: Vad ger sökningar (autocomplete) på titlar och konferensnamn?
# Hypotes: Scopus är duktiga på konferenser, kanske pga screenscraping-superpowers, hur står sig OpenAlex där?
# TODO: Hur mycket "mer" av sådant ser man med en Premium Key?

# tf <- openalex_counts(filter = "authorships.institutions.lineage:i86987016,publication_year:2025")

# tree <- tf[grepl("Topic", names(tf))]
# topics <- openalex_topics()

# tree$`Primary Topic Domain Id` |> 
#   left_join(topics |> distinct(id_domain, domain), by = c(name = "domain")) |> 
#   rename(domain = "name")

# tree$`Primary Topic Field Id` |> 
#   left_join(topics |> distinct(id_field, field, id_domain, domain), by = c(name = "field")) |> 
#   rename(field = "name")

# tree$`Primary Topic Subfield Id` |> 
#   left_join(topics |> distinct(id_subfield, subfield, id_field, field, id_domain, domain), by = c(name = "subfield")) |> 
#   rename(subfield = "name")

# tree$`Primary Topic Id` |> 
#   left_join(topics, by = c(name = "topic")) |> 
#   rename(topic = "name")

# tt <- 
#   list(tree, names(tree)) |> purrr::pmap(\(x, y) x |> mutate(var = y) |> select(var, everything())) |> 
#   map_dfr(bind_rows) |> 
#   rename(display_name = name) |> 
#   left_join(topics, by = "display_name")

# proceed to make a treemap