split_chunks_of_n <- function(x, n) 
  split(x, ceiling(seq_along(x) / n))

split_n_chunks <- function(x, n)
  split(x, ceiling(seq_along(x) / (length(x) / n)))

openalex_or <- function(x) 
  paste0(collapse = "|", x)

doi_crawl <- function(dois) {

  works <- 
    openalex_crawl("works", fmt = "object",
      query = openalex_query(filter = paste0("doi:", dois))
    )
  
  lol <- 
    list(list(results = reduce(works |> map("results"), c)))

  lol |> openalex_works_to_tbls()
    
}

doi_lookup_identifiers <- function(con, doi_filter) {

  if (missing(con)) {
    con <- duckdb::dbConnect(duckdb::duckdb())
    DBI::dbSendQuery(con, "install json; load json; install httpfs; load httpfs;")
    on.exit(duckdb::dbDisconnect(con, shutdown = TRUE))  
  }

  sql <- 
    paste0("from (from read_json_auto('",
    sprintf("https://api.openalex.org/works?filter=doi:%s&per-page=50&mailto=support@openalex.org", doi_filter),
    "') select unnest(results) as r) select unnest(r.ids);")  

  DBI::dbGetQuery(con, sql) |> as_tibble()  
}

#' Lookup DOIs using OpenAlex
#' 
#' @param dois a character vector of DOIs
#' @param resolution either "all" or "identifiers" to only return other related identifiers
#' @return tibble(s)
#' @export
openalex_doi_lookup <- function(dois, resolution = c("all", "identifiers")) {

  dois <- unique(dois)

  doi_filters <- 
    split_chunks_of_n(dois, 50) |> 
    map_chr(openalex_or)

  doi_chunks <- switch(resolution, 
    "all" = {
      doi_filters |> 
        purrr::map(doi_crawl, .progress = TRUE)
    },
    "identifiers" = {
      doi_filters |> 
        map_dfr(\(x) doi_lookup_identifiers(doi_filter = x), .progress = TRUE)
    }
  )

  doi_chunks
 
}

