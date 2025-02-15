test_that("cursor based paging for works works", {

  skip_on_ci()

  works_filter <- "publication_year:2015-2023,primary_topic.id:t10783"

  cc <- 
    works_filter |> 
    openalex_works_cursorcrawl(n_max_pages = 10)

  mydir <- unique(dirname(cc))

  mydir <- unique(dirname(cc))
  fns_max_size <- max(file.size(cc))

  read_jsonl <- function(fn) {
    Sys.setenv("VROOM_CONNECTION_SIZE" = fns_max_size)
    fn |> file() |> readr::read_lines() |> 
    RcppSimdJson::fparse(max_simplify_lvl = "list")
  }

  ccs <- cc |> purrr::map(\(x) list(results = read_jsonl(x)))

  #object <- ccs[[1]]
  #w |> openalex_works_to_tbls()
  
  res <- 
    cc |> 
    purrr::map(\(x) list(results = read_jsonl(x))) |> 
    openalex_works_to_tbls()

  is_valid <- 
    all(cc %in% dir(mydir, full.names = TRUE)) &
    res$work |> nrow() == 500

  # TODO: Hmmm, what is a$abstract_inverted_index_v3?
  # a <- jsonlite::stream_in(file(cc[1])) |> as_tibble()

  expect_true(is_valid)

  # cmd <- paste0("flatterer --force --nocsv --parquet -m works --id-prefix work -j ",
  #   paste(collapse = " ", cc), " ", mydir, paste0(mydir, "/cursorcrawl"))
  
  # system(cmd)

})
