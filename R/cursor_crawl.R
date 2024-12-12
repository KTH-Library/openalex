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
      httr2::resp_body_json()
  }
  
  # get the first page of results
  page <- fetch_works(q)

  # compute total number of pages
  h <- page$meta
  n_pages <- ceiling(h$count / h$per_page) + ifelse(h$count > h$per_page, 1, 0)
  
  # return immediately if only one page
  if (!(n_pages > 1)) 
    return(page)
  
  # begin the crawl
  message("Retrieving ", n_pages, " pages, total record count is ", 
    page$meta$count, ". Starting crawl...")  

  # iterate using a while loop
  is_stopped <- FALSE
  i <- 1
  is_done <- FALSE
  pages <- page
  q$cursor <- page$meta$next_cursor
  td <- tempdir()
  unlink(dir(td, pattern = "\\.rds$", full.names = TRUE))
  fn <- file.path(td, sprintf("%04i%s", i, ".rds"))
  readr::write_rds(page, file = fn)
  #message("Cursor: ", q$cursor)

  while (!is_done) {
    i <- i + 1
    if (i %% 10 == 0) cat(paste(i, q$cursor, "\n")) else {
      if (i %% 100 == 0) cat("HUNDREDS_OF_PAGES!!!!\n") else cat(".")
    }
    next_page <- fetch_works(q)
    stopifnot(!is.null(next_page))
    q$cursor <- next_page$meta$next_cursor
    #message("Next cursor: ", q$cursor)
    fn <- file.path(td, sprintf("%04i%s", i, ".rds"))
    readr::write_rds(next_page, file = fn)
    is_stopped <- i >= n_max_pages
    is_done <- is.null(q$cursor) || is_stopped
  }

  message("\nDone, fetched ", i, " pages of works, written to ", td) 
  filez <- dir(td, pattern = "\\.rds$", full.names = TRUE) 
  return (filez)
}

