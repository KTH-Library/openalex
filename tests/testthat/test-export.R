test_that("export works", {

  skip()

  my_filter <- paste0(collapse = ",", c(
    "authorships.institutions.lineage:i86987016", ## KTH
    "authorships.institutions.lineage:i4210161097", ## Bolin Center (some of these might be KTH pubs!)
    "authorships.institutions.lineage:i119971240", ## NORDITA (some of these, too!) 
    "authorships.institutions.lineage:i4210147696", ## THS Tekniska Högskolans Studentkår
    "type:types/article",
    "primary_location.source.type:source-types/journal|source-types/conference",
    "publication_year:2024"
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

  skip()

  my_filter <- paste0(collapse = ",", c(
    sprintf("publication_year:%s", 2024),    
    sprintf("authorships.author.id:%s", "a5058057533") #,
  #   sprintf("raw_affiliation_strings.search:%s", openalex_kth_rawaff_query()),
  #   "authorships.institutions.lineage:!i86987016", ## KTH
  #   "authorships.institutions.lineage:!i4210161097", ## Bolin Center (some of these might be KTH pubs!)
  #   "authorships.institutions.lineage:!i119971240", ## NORDITA (some of these, too!) 
  #   "authorships.institutions.lineage:!i4210147696" ## THS Tekniska Högskolans Studentkår    
  ))

  gm <- openalex_works_export(q = my_filter, fmt = "wos-plaintext")
  
  is_valid <- (regmatches(gm, gregexpr("Maguire", gm)) |> unlist()) |> length() > 10
  expect_true(is_valid)
})

test_that("export of rawff query for 2024 in wos-plain diva text format works", {

  my_filter <- paste0(collapse = ",", c(
    sprintf("publication_year:%s", 2024),    
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


# TODO: Vilka konferenspapper finns i OA som är kopplade till KTH (behöver inte ha DOI)?
# TODO: Vad ger sökningar (autocomplete) på titlar och konferensnamn?
# Hypotes: Scopus är duktiga på konferenser, kanske pga screenscraping-superpowers, hur står sig OpenAlex där?
# TODO: Hur mycket "mer" av sådant ser man med en Premium Key?

