test_that("converting some records to tables does not cause freeze", {

  skip_on_ci()

  works_filter <- paste0(
    "publication_year:2015-2024,",
    "primary_topic.subfield.id:subfields/3605,",
    "authorships.countries:countries/se"
  )

  cc <- 
    works_filter |> 
    openalex_works_cursorcrawl(n_max_pages = 28)

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
    res$work |> nrow() == ccs |> map_int(\(x) length(x$results)) |> sum()

  expect_true(is_valid)
  
})

test_that("converting some records to tables does not cause vroom error", {

  skip_on_ci()

  works_filter <- paste0(
    "publication_year:2015-2024,",
    "topics.subfield.id:subfields/3605,",
    "authorships.countries:countries/se"
  )

  cc <- 
    works_filter |> 
    openalex_works_cursorcrawl(n_max_pages = 27)

  mydir <- unique(dirname(cc))
  fns_max_size <- max(file.size(cc))

  read_jsonl <- function(fn) {
    Sys.setenv("VROOM_CONNECTION_SIZE" = fns_max_size)
    fn |> file() |> readr::read_lines() |> 
    RcppSimdJson::fparse(max_simplify_lvl = "list")
  }

  ccs <- 
    cc |> purrr::map(\(x) list(results = read_jsonl(x)))

  res <- ccs |> openalex_works_to_tbls()

  #ccs |> map(\(x) list(results = x) |> openalex_works_to_tbls())

  #i <- ceiling(0.77 * 26)
  #x <- ccs[i] 
  #cc[i]

  #object <- list(results = ccs[[4]]$results) 
  #object |> parse_work2()

  is_valid <- 
    all(cc %in% dir(mydir, full.names = TRUE)) &
      res$work |> nrow() == ccs |> map_int(\(x) length(x$results)) |> sum()

  expect_true(is_valid)
})

