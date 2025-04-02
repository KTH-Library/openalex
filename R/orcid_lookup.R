orcid_crawl <- function(orcids) {

  works <-
    openalex_crawl("works", fmt = "object",
      query = openalex_query(filter = paste0("author.orcid:", orcids))
    )

  lol <-
    list(list(results = reduce(works |> map("results"), c)))

  return (lol)

}


#' Lookup ORCiDs using OpenAlex
#'
#' @param orcids a character vector of DOIs
#' @return tibble(s)
#' @export
openalex_orcid_lookup <- function(orcids) {

  orcids <- unique(orcids)

  orcid_filters <-
    split_chunks_of_n(orcids, 10) |>
    map_chr(openalex_or)

    orcid_filters |>
        purrr::map(orcid_crawl, .progress = TRUE) |>
        purrr::reduce(c) |> purrr::map("results") |> purrr::list_c() |>
        list() |> purrr::set_names(nm = "results") |> list() |>
        openalex_works_to_tbls()

}

#' Lookup ORCiDs in OpenAlex and save to a database in chunks of 100
#'
#' @param orcids a character vector of DOIs
#' @param dest a path to an existing or wanted database file
#' @importFrom purrr map
#' @return file path to the database file
#' @export
orcid_lookup_to_duckdb <- function(orcids, dest = NULL) {

  orcid_chunks <- split_chunks_of_n(orcids, 10)
  persist_chunk <- function(x) {
    res <- openalex_orcid_lookup(x)
    z <- openalex_write_duckdb(res, append = TRUE)
    z
  }

  z <- orcid_chunks |> map_chr(persist_chunk)

  return(unique(z))

}
