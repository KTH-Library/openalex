#file.edit("~/.Renviron")
#readRenviron("~/.Renviron")

cfg <- function() {

  res <- list(
    user_agent = "http://github.com/hadley/httr"
  )

  if (Sys.getenv("OPENALEX_USERAGENT") != "") {
    res$user_agent <- Sys.getenv("OPENALEX_USERAGENT")
  }

  return (res)
}

#' Endpoint used for requests to OpenAlex API
#' @export
openalex_api <- function()
  "https://api.openalex.org/"

#' Attribution
#'
#' Use this attribution whenever data from the API is publicly displayed
#'
#' @details OpenAlex provides a RESTful API for scholarly papers, authors,
#' institutions, and more. When publicly displaying data from the API,
#' it is polite to point back to OpenAlex at https://openalex.org/
#' @export
openalex_attribution <- function() {
  license <- "https://creativecommons.org/publicdomain/zero/1.0/"
  sprintf(paste0(
    "Data source: OpenAlex API at %s", "\n",
    "Data license agreement: %s"),
    openalex_api(), license
  )
}

#' Retrieve work from OpenAlex REST API
#'
#' This function retrieves works given an identifier
#' @param identifier string with identifier
#' @param format one of "table" or "object"
#' @param use_random logical to indicate whether to use random identifier, Default: FALSE
#' @return as per format, either a tibble or an R object
#' @examples
#' \dontrun{
#'  openalex_work(use_random = TRUE)
#'  }
#' @export
openalex_work <- function(identifier, format = "table", use_random = FALSE) {
  openalex_entity(identifier, entity = "works", format, use_random)
}

openalex_entity_enum <- function()
  c("works", "authors", "venues", "institutions", "concepts")

#' @importFrom httr modify_url user_agent GET status_code http_type content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble enframe
#' @importFrom dplyr mutate
openalex_entity <- function(
  identifier,
  entity = openalex_entity_enum(),
  format = c("table", "object"),
  use_random = FALSE,
  verbose = FALSE,
  query = NULL) {

  style <- match.arg(format)
  kind <- match.arg(entity)
  is_listing <- FALSE

  if (missing(identifier)) {
    if (use_random == FALSE && is.null(query)) {
      stop("Identifier is missing, please specify use_random to use a random id.")
    } else if (use_random == TRUE && is.null(query)) {
      identifier <- "random"
    } else {
      if (verbose == TRUE) message("This is a list request...")
      is_listing <- TRUE
    }
  }

  path <- if (!is_listing) sprintf("%s/%s", kind, identifier) else kind

  url <- httr::modify_url(
    openalex_api(),
    path = path,
    query = query
  )

  if (verbose == TRUE) message("Requesting url: ", url)

  ua <- httr::user_agent(cfg()$user_agent)
  res <- httr::GET(url, ua)

  if (httr::status_code(res) == 200) {

    if (httr::http_type(res) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }

    if (style == "object") {
      data <- jsonlite::fromJSON(
        httr::content(res, as = "text", encoding = "utf-8"),
        simplifyVector = FALSE #, DataFrame = TRUE, flatten = TRUE
      )
    } else {
      name <- NULL
      data <-
        tibble::enframe(unlist(httr::content(res, encoding = "utf-8"))) %>%
        dplyr::mutate(name = gsub(".", "_", name, fixed = TRUE)) #%>%
        #dplyr::mutate(item_id = cumsum(name == "concepts_id")) %>%
        #dplyr::filter(item_id > 0) %>%
        #tidyr::pivot_wider(values_fn = function(x) paste0(x, collapse = ", ")) %>%
        #dplyr::rename_with(function(x) gsub("items_", "", x)) %>%
        #dplyr::mutate(across(.fns = function(x) readr::parse_guess(x, guess_integer = TRUE)))

    }

    #class(data) <- c("tbl_df", "tbl", "data.frame")
    return(data)
  }

  if (status_code(res) == 429)
    stop("HTTP status 429 Too Many Requests")

  stop("HTTP status ", status_code(res))

}

openalex_query <- function(filter=NULL, search=NULL, sort=NULL, page=NULL) {

  # filter... use , to indicate AND
  #?filter=last_known_institution.country_code:US,cited_by_count:>0

  # search... add ".search" to a property
  #?filter=title.search:"intensive treatment of diabetes"

  # sort... By default, sort direction is ascending. You can reverse this by using sort:desc
  # ?sort:display_name,cited_by_count,works_count,publication_date,relevance_score

  # paging currently you can only use paging to read the first 10,000 results of any list
  # ?page=1

  list(
    filter = filter,
    search = search,
    sort = sort,
    page = page
  )

}

openalex_list <- function(entity, query, format = "object", verbose = TRUE) {
  res <- openalex_entity(entity = entity, format = format, verbose = verbose, query = query)
  attr(res, "page_count") <- ceiling(res$meta$count / res$meta$per_page)
  return(res)
}

#' Crawl multiple pages of results
#'
#' Iterates over paged results showing a progress bar
#'
#' @param entity one of the values in openalex_entity_enum()
#' @param query an openalex_query object
#' @param verbose boolean to indicate whether to output messages during process
#' @return R object with results matching the query
#' @importFrom progress progress_bar
#' @importFrom purrr possibly map_df
#' @export
openalex_crawl <- function(entity, query, verbose = TRUE) {

  res <- openalex_list(entity, query, format = "object", verbose = FALSE)
  n_items <- res$meta$count
  pages <- 1:attr(res, "page_count")

  if (n_items <= 0) return (list())

  if (n_items > 1e4)
    stop("A maximum of 10000 results can be paged, this query exceeds that.")

  if (verbose)
    message("About to crawl a total of ", length(pages), " pages of results",
            " with a total of ", n_items, " records.")

  pb <- progress::progress_bar$new(
    format = "  open alex resolving [:bar] :percent eta: :eta",
    total = length(pages), clear = FALSE, width = 60)

  #TODO: fixme so this can run in parallel?
  q <- query
  i <- 1
  entities <- purrr::possibly(
    .f = function(x) {
      pb$tick()
      q$page <- i
      Sys.sleep(1 / 100)
      i <<- i + 1
      openalex_list(entity, q, format = "object", verbose = FALSE)$result #%>%
        #bind_cols(page = x) %>%
        #select(page, everything())
    },
    otherwise = list() #data.frame()
  )

  pages %>% purrr::map(entities)

}

#'Flatten R object from deserialized nested JSON object
#'
#'@param nestedlist a nested list of lists
#'@return a tibble in long format
#'@export
#'@importFrom tibble enframe
#'@importFrom dplyr mutate
openalex_flatten_long <- function(nestedlist) {
  name <- NULL
  tibble::enframe(unlist(nestedlist)) %>%
    dplyr::mutate(name = gsub(".", "_", name, fixed = TRUE))
}
