test_that("converting some records to tables does not cause freeze", {

  skip_on_ci()

  works_filter <- paste0(
    "publication_year:2015-2024,",
    "primary_topic.subfield.id:subfields/3605,",
    "authorships.countries:countries/se"
  )

  cc <- 
    works_filter |> 
    openalex_works_cursorcrawl()

  mydir <- unique(dirname(cc))

  read_jsonl <- function(fn) {
      fn |> file() |> readr::read_lines() |> 
      RcppSimdJson::fparse(max_simplify_lvl = "list")
  }

  ccs <- 
    cc |> purrr::map(\(x) list(results = read_jsonl(x)))

  res <- ccs |> openalex_works_to_tbls()

  #object <- list(results = ccs[[4]]$results) 
  #object |> parse_work2()


  is_valid <- 
    all(cc %in% dir(mydir, full.names = TRUE)) &
    res$work |> nrow() == length(cc) * 50

  expect_true(is_valid)
  
})
