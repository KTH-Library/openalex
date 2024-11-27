#' @param q the query, for example "authorships.institutions.lineage:i86987016,authorships.institutions.lineage:!i4210161097,type:types/article,primary_location.source.type:source-types/journal|source-types/conference,publication_year:2023"
#' @param fmt the export format, one of "csv" or "wos-plaintext" or "wos-plaintext-diva"
#' @param raw_string boolean to indicate whether a raw string should be returned
#' @return a character vector with a raw string with the results from the export or a data frame
#' @import httr2
#' @importFrom dplyr bind_cols
openalex_works_export <- function(q, fmt = c("csv", "wos-plaintext"), raw_string = FALSE) {

  query <- list(filter = q)
  query$format <- fmt
  query$truncate <- "false"
  query$api_key <- cfg()$key

  ep <- 
    "https://export.openalex.org" |> 
    httr2::request() |> 
    httr2::req_url_path("works") |> 
    httr2::req_user_agent(cfg()$user_agent) |> 
    httr2::req_url_query(!!!query) 
  
  check_progress <- function() {
    ep |> httr2::req_perform() |> httr2::resp_body_json() |> dplyr::bind_cols()
  }
  
  res <- check_progress()
  
  message("Waiting for export to be generated ...\n")
  
  while (res$status != "finished") {
    Sys.sleep(5)
    res <- check_progress()
    message(sprintf("%0.1f%%", as.double(res$progress) * 100), " (", res$status, ")") 
  }

  message("Export is ready, retrieving results.")
  out <- 
    httr2::request(res$result_url) |> 
    httr2::req_perform() |> 
    httr2::resp_body_string()

  message("Done, returning results")
  if (raw_string) return(out)
  
  res <- switch(match.arg(fmt), 
    "csv" = {
      out |> readr::read_csv(show_col_types = FALSE)
    },
    # "wos-plaintext" = {
    #   out |> strsplit(split = "\n") |> unlist() |> read_wos_plaintext()
    # },
    "wos-plaintext" = {
      out |> wos_plaintext_for_diva()
    }
  )
  return (res)
}

#' Function which converts a wos_plaintext-string into a format
#' which can be uploaded to DiVA, by adding ER tags 
#' (including a blank line) after each record
wos_plaintext_for_diva <- function(x) {
  w <- x |> strsplit("\n") |> unlist()
  i_header <- which(grepl("^FN|^VR", w))
  #i_indented <- which(grepl("^\\s+", w))
  i_eor <- which(grepl("^ER$", w))
  i_blank <- which(nchar(w) == 0)
  
  pt <- w[-c(i_eor, i_blank)]  # TODO: should i_header rows be removed too?
  i_record <- which(grepl("^PT\\s+", pt))
  n_records <- length(i_record)
  i_range <- data.frame(beg = i_record, end = c(tail(i_record, -1) - 1, length(pt)))
  pt[i_range$end] <- pt[i_range$end] |> paste0("\nER\n")
  paste0(collapse ="\n", pt)
}

#' Export the results from a crawl as a duckdb database file
#' @param harvest the results from running the to_tbls fcn
#' @param destdir the location to save the database file
#' @return file path to the database file
#' @importFrom purrr walk2
#' @import duckdb DBI
openalex_write_duckdb <- function(crawl, destdir = NULL) {

  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop(
      "Package \"duckdb\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (is.null(destdir)) {
    destdir <- file.path(tempdir(check = TRUE), "openalex", "openalex.db")
  }

  message("Ensure existing dir: ", dirname(destdir))
  if (!dir.exists(dirname(destdir))) {
    is_created <- dir.create(dirname(destdir), showWarnings = TRUE)
  } else {
    message("Removing existing file ", destdir)
    if (file.exists(destdir))
      unlink(destdir)
  }

  drv <- duckdb::duckdb()
  con <- duckdb::dbConnect(drv, dbdir = destdir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  crawl |> names() |> 
    purrr::walk(\(x) duckdb::duckdb_register(con, sprintf("view_%s", x), crawl |> getElement(x)))

  toc <- DBI::dbListTables(con)
  new_tbl <- gsub("^view_", "", toc)

  sql_create_db <- sprintf("create table %s as from %s;", new_tbl, toc) |>
    paste(collapse = "\n")

  message("Creating duckdb file at ", destdir, " using sql ", sql_create_db)
  result <- DBI::dbExecute(con, sql_create_db)
  message("Result is ", result)

  return(destdir)

}