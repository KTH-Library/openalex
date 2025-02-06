#' Crawl multipage responses from queries against the API
#' 
#' Chunks and uses cursor based pagination to fetch works
#' @param works_filter the works filter
#' @param n_max_pages the max amount of pages to fetch (50 per page)
#' @return paths to downloaded files
#' @importFrom RcppSimdJson fminify fparse fload
#' @importFrom jqr jq
openalex_works_cursorcrawl <- function(
  works_filter,
  n_max_pages = 5
) {

  req_works <- 
    "https://api.openalex.org/" |> 
    httr2::request() |> 
    httr2::req_url_path("works")
    
  # initially, cursor is set to "*"
  q <- list(
    filter = works_filter,
    cursor = "*",
    `per-page` = 50
  )
  
  # fcn to get works based on query params
  fetch_works <- function(q) {
      req_works |> 
      httr2::req_url_query(!!!q) |> 
      httr2::req_perform() |> 
      httr2::resp_body_string() |> 
      RcppSimdJson::fminify()
  }
  
  # get the first page of results
  json_line <- fetch_works(q)

  json_header <- function(j) {
    json_line |> RcppSimdJson::fparse(query = "/meta", max_simplify_lvl = "list")
  }

  json_results <- function(j) {

    #cmd <- sprintf("%s -c '.results[]' | %s -c 'del(..|.abstract_inverted_index?)'", 
    #  jq_binary, jq_binary)
    
    #system(cmd, input = j, intern = TRUE) #|> 
    j |> jqr::jq(".results[] | del(..|.abstract_inverted_index?)")
  }

  #TODO: exclude abstract_inverted_index
  # Using JSONPath: $.*[?(@.abstract_inverted_index == null)]

  header <- json_line |> json_header()
  results <- json_line |> json_results()

  # page <- 
  #   json_line |> 
  #   RcppSimdJson::fparse("/results", max_simplify_lvl = "list") |> 
  #   (\(x) list(list(results = x)))()

  #page |> openalex_works_to_tbls()

  # compute total number of pages
  h <- header
  n_pages <- ceiling(h$count / h$per_page) + ifelse(h$count > h$per_page, 1, 0)
    
  # begin the crawl
  message("Retrieving ", n_max_pages, " out of a total of ",
    n_pages, " pages, with a total record count of ", h$count, 
    ". Starting crawl...")

  # iterate using a while loop
  i <- 1
  is_stopped <- FALSE
  is_done <- n_pages <= 1
  q$cursor <- h$next_cursor
  td <- tempdir()
  unlink(dir(td, pattern = "\\.json$", full.names = TRUE))
  fn <- file.path(td, sprintf("%04i%s", i, ".json"))
  readr::write_lines(results, fn)
  message("Wrote page ", i, " to ", fn, " and next cursor is ", q$cursor)
  #readr::write_rds(page, file = fn)
  #message("Cursor: ", q$cursor)

  while (!is_done) {
    i <- i + 1
    if (i %% 10 == 0) cat(paste(i, q$cursor, "\n")) else {
      if (i %% 100 == 0) cat("HUNDREDS_OF_PAGES!!!!\n") else cat(".")
    }
    next_page <- fetch_works(q)
    #stopifnot(!is.null(next_page))
    h <- json_header(next_page)
    q$cursor <- h$next_cursor
    fn <- file.path(td, sprintf("%04i%s", i, ".json"))
    results <- json_results(next_page)
    readr::write_lines(results, fn, append = TRUE)
    #readr::write_rds(next_page, file = fn)
    is_stopped <- i >= n_max_pages
    is_done <- is.null(q$cursor) || is_stopped
  }

  message("\nDone, fetched ", i, " pages of works, written to ", td) 
  filez <- dir(td, pattern = "\\.json$", full.names = TRUE) 
  return (filez)
}

jsonl_to_tbl <- function(fn) {
  obj <- fn |> RcppSimdJson::fload(max_simplify_lvl = "list")
  res <- list(results = obj)
  res |> parse_work2()
}

