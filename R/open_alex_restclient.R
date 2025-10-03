#file.edit("~/.Renviron")
#readRenviron("~/.Renviron")

#' Enter the OpenAlex API polite pool for faster requests by providing an email
#' @param email an email address, on the form "you@example.com" or "" to unset email
#' @return a logical depending on whether email was set or unset
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # to set
#'  openalex_polite("you@example.com")
#'  # to unset
#'  openalex_polite("")
#'  }
#' }
#' @export
openalex_polite <- function(email) {

  if (!nzchar(email)) {
    message("Exiting from polite pool, email no longer provided in user agent header")
    Sys.setenv("OPENALEX_USERAGENT" = "http://github.com/hadley/httr")
    return (FALSE)
  }

  stopifnot(is.character(email), length(email) == 1)
  re_email <- "^mailto:.*?@.*?\\..*?"
  if (!grepl(re_email, email))
    email <- paste0("mailto:", trimws(email))
  stopifnot(grepl(re_email, email))

  ua <- sprintf("http://github.com/hadley/httr (%s)", email)

  if (Sys.getenv("OPENALEX_USERAGENT") != "") {
    message("Hint: You can provide an email to enter the polite pool")
    message("To have the setting stick persistently using .Renviron, do ...")
    message('  file.edit("~/.Renviron")')
    message(sprintf('  # and add a line OPENALEX_USERAGENT="%s"', ua))
    message("Then reload settings for the R environment in the current session")
    message('  readRenviron("~/.Renviron")')
  }

  message("Temporarily setting OPENALEX_USERAGENT envvar for this session to: ", ua)
  Sys.setenv("OPENALEX_USERAGENT" = ua)
  return (TRUE)
}

#' Use an API key for OpenAlex Premium Subscription
#'
#' This provides access to the latest data, fresher than what snapshots provide.
#' It also enables faster requests and filtering on from_created_date and from_updated_date fields.
#' @param key a premium subscription key
#' @return a logical depending on whether key was set or unset
#' @examplesIf interactive()
#'  openalex_key("my_secret_api_key")
#'  openalex_key("")
#' @export
#' @details
#' Additional details...
#'
#' <https://github.com/ourresearch/openalex-api-tutorials/blob/main/notebooks/getting-started/premium.ipynb>
#' <https://docs.openalex.org/api-entities/works/filter-works#from_created_date>
#' <https://docs.openalex.org/api-entities/works/filter-works#from_updated_date>
openalex_key <- function(key) {

  if (!nzchar(key)) {
    message("Unsetting premium subscription key")
    Sys.setenv("OPENALEX_KEY" = "")
    return (FALSE)
  }

  stopifnot(is.character(key), length(key) == 1)

  re_key <- "[[:alnum:]]{22}"
  stopifnot(grepl(re_key, key))

  if (Sys.getenv("OPENALEX_KEY") != "") {
    message("Hint: You can provide an premium subscription api key")
    message("To have the setting stick persistently using .Renviron, do ...")
    message('  file.edit("~/.Renviron")')
    message(sprintf('  # and add a line OPENALEX_KEY="%s"', key))
    message("Then reload settings for the R environment in the current session")
    message('  readRenviron("~/.Renviron")')
  }

  message("Temporarily setting OPENALEX_KEY envvar for this session")
  Sys.setenv("OPENALEX_KEY" = key)
  return (TRUE)
}

cfg <- function() {

  res <- list(
    user_agent = "http://github.com/hadley/httr"
  )

  if (Sys.getenv("OPENALEX_USERAGENT") != "") {
    res$user_agent <- Sys.getenv("OPENALEX_USERAGENT")
  }

  if (Sys.getenv("OPENALEX_KEY") != "") {
    res$key <- Sys.getenv("OPENALEX_KEY")
  }

  return (res)
}

#' Endpoint used for requests to OpenAlex API
#' @export
openalex_api <- function() {
  "https://api.openalex.org/"
}

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
  format = c("table", "object", "raw", "tables"),
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
    query = query #paste0("filter=", URLencode(query$filter)) #, "&sort=publication_date:desc")
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
      data <- structure(data, meta = data$meta)
    } else if (style == "table") {
      payload <- httr::content(res, encoding = "utf-8")
      name <- NULL
      data <-
        tibble::enframe(unlist(payload)) %>%
        dplyr::mutate(name = gsub(".", "_", name, fixed = TRUE)) #%>%
        #dplyr::mutate(item_id = cumsum(name == "concepts_id")) %>%
        #dplyr::filter(item_id > 0) %>%
        #tidyr::pivot_wider(values_fn = function(x) paste0(x, collapse = ", ")) %>%
        #dplyr::rename_with(function(x) gsub("items_", "", x)) %>%
        #dplyr::mutate(across(.fns = function(x) readr::parse_guess(x, guess_integer = TRUE)))
      data <- structure(data, meta = payload$meta)
    } else if (style == "tables") {
      if (kind == "works") {
        payload <- httr::content(res, encoding = "utf-8")
        data <- payload$results |> parse_work()
        data <- structure(data, meta = payload$meta)
      } else {
        stop("Only works supported for now!")
      }
    } else if (style == "raw") {
      data <- res
    }

    #class(data) <- c("tbl_df", "tbl", "data.frame")
    return(data)
  }

  if (status_code(res) == 429)
    stop("HTTP status 429 Too Many Requests")

  if (status_code(res) == 403) {
    cr <- content(res)
    stop(cr$error, "\n\n", cr$message)
  }

  stop("HTTP status ", status_code(res))

}

openalex_query <- function(
    filter=NULL,
    search=NULL,
    sort=NULL,
    page=NULL,
    cursor=NULL,
    verbose = FALSE) {

  # filter... use , to indicate AND
  #?filter=last_known_institution.country_code:US,cited_by_count:>0

  # search... add ".search" to a property
  #?filter=title.search:"intensive treatment of diabetes"

  # sort... By default, sort direction is ascending. You can reverse this by using sort:desc
  # ?sort:display_name,cited_by_count,works_count,publication_date,relevance_score

  # paging currently you can only use paging to read the first 10,000 results of any list
  # ?page=1

  q <- list(
    filter = filter,
    search = search,
    sort = sort,
    page = page,
    cursor = cursor,
    api_key = cfg()$key
  ) |>
    purrr::compact()

  if (verbose)
    message("Query is:\n\n",
       paste0(collapse = "\n", utils::capture.output(print(q)))
    )

  return (q)

}

openalex_list <- function(entity, query, format = "object", verbose = FALSE) {
  res <- openalex_entity(entity = entity, format = format, verbose = verbose, query = query)
  attr(res, "page_count") <- ceiling(attr(res, "meta")$count / attr(res, "meta")$per_page)
  return(res)
}

gs <- function(x, p, r) {
  gsub(p, r, x, fixed = TRUE)
}

#' @importFrom purrr keep
# support pipe
tbl_from_slot <- function(x, slot)
  x |> map(slot) |>
  keep(.p = \(y) nrow(y) > 0) |>
  bind_rows() |>
  readr::type_convert() |>
  suppressMessages() |>
  mutate(across(is.character, \(x) x |> gs("https://openalex.org/", "")))


#' Crawl multiple pages of results
#'
#' Iterates over paged results showing a progress bar
#'
#' @param entity one of the values in openalex_entity_enum()
#' @param query an openalex_query object
#' @param verbose boolean to indicate whether to output messages during process
#' @param fmt the return format, one of "object" or "tables"
#' @return R object with results matching the query
#' @importFrom progress progress_bar
#' @importFrom purrr possibly map_df map_dfr pmap
#' @importFrom dplyr bind_rows
#' @export
openalex_crawl <- function(entity, query, verbose = FALSE, fmt = "object") {

  q <- query

  # if (use_cursor) {
  #   q$cursor <- "*"
  #   message("Using query:")
  #   print(q)
  # }

  res <- openalex_list(entity, q, format = fmt, verbose = FALSE)
  #q <- query
  n_items <- attr(res, "meta")$count
  pages <- 1:attr(res, "page_count")
  #next_cursor <- attr(res, "meta")$next_cursor
  #q$next_cursor <- next_cursor

  # if (use_cursor == TRUE && is.null(next_cursor))
  #   stop("Requested cursor paging, but no next_cursor found in response from API")

  if (n_items <= 0) {
    message("No results, returning empty list.")
    return (list())
  }

  if (n_items > 1e4) {
    stop("If there are more than 10000 results, please set use_cursor to activate cursor paging")
  }

  if (verbose)
    message("About to crawl a total of ", length(pages), " pages of results",
            " with a total of ", n_items, " records.")

  pb <- progress_bar$new(
    format = "  open alex resolving [:bar] :percent eta: :eta",
    total = length(pages), clear = FALSE, width = 60)

  #TODO: fixme so this can run in parallel?
  iq <- q
  i <- 1
  entities <- purrr::possibly(quiet = FALSE,
    .f = function(x) {
      pb$tick()
      iq$page <- i
      #print(q)
      Sys.sleep(1 / 100)
      # if (use_cursor & !is.null(next_cursor)) {
      #   iq$filter <- paste0(q$filter, "&cursor=", next_cursor)
      #   print(iq)
      # }
      res <- openalex_list(entity, iq, format = fmt, verbose = FALSE)
      # if (use_cursor) {
      #   next_cursor <<- attr(res, "meta")$next_cursor
      # }
      i <<- i + 1
      return(res)
    },
    otherwise = list() #data.frame()
  )

  if (fmt != "tables") {
    res <- pages |>  map(entities, .progress = TRUE)
    #res |>  pmap(c)
    return (res)
  }

  res <-
    pages |>  map(entities, .progress = TRUE)

  #TODO: fix so that NOT THE SAME work ids are fetched!!!!
  #TODO: do not assume entity is work below

  list(
    work = res |> tbl_from_slot("work"),
    work_ids = res |> tbl_from_slot("work_ids"),
    work_concepts = res |> tbl_from_slot("work_concepts"),
    work_authorships_institutions = res |> tbl_from_slot("work_authorships_institutions"),
    work_abstract_inverted_index = res |> tbl_from_slot("work_abstract_inverted_index"),
    work_authorships_author = res |> tbl_from_slot("work_authorships_author"),
    work_biblio = res |> tbl_from_slot("work_biblio"),
    work_open_access = res |> tbl_from_slot("work_open_access"),
    work_host_venue = res |> tbl_from_slot("work_host_venue"),
    work_counts_by_year = res |> tbl_from_slot("work_counts_by_year"),
    work_related_works = res |> tbl_from_slot("work_related_works"),
    work_referenced_works = res |> tbl_from_slot("work_referenced_works")
  )

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

openalex_autocomplete <- function(
    query,
    entity_type = openalex_entity_enum(),
    format = c("object", "table"),
    verbose = TRUE
    ) {

  #/autocomplete/<entity_type>?q=<query>

  stopifnot(nchar(query) >= 1)

  style <- match.arg(format)
  entity <- match.arg(entity_type)
  path <- sprintf("autocomplete/%s", entity)

  url <- httr::modify_url(
    openalex_api(),
    path = path,
    query = list(q = query)
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
      data <- httr::content(res, encoding = "utf-8") %>%
        purrr::pluck("results") %>%
        dplyr::bind_rows()
    }

    #class(data) <- c("tbl_df", "tbl", "data.frame")
    return(data)
  }

  if (status_code(res) == 429)
    stop("HTTP status 429 Too Many Requests")

  stop("HTTP status ", status_code(res))

}

#' Example query when searching raw affiliation strings
#'
#' This variant is specifically tailored for KTH, Royal Institute of Technology
#' and includes some affiliation string variations which might be related.
#' @export
#' @return string with query
openalex_kth_rawaff_query <- function() {
  # (roy AND inst AND tech) OR
  # "Roy. Inst. T"
  # (roy* AND tech* AND univ*)) AND (Sweden))
  # paste0(
  #   'KTH OR (roy* AND inst* AND tech*) OR ',
  #   '(alfven) OR (kung* AND tek* AND hog*) OR (kung* AND tek* AND h\\u00f6g*) OR ',
  #   '(kgl AND tek* AND hog*) OR (kung* AND tek* AND hg*)'
  # )

  '("KTH" OR

 (("roy inst" OR
 "royal in-stitute" OR
 "royal inititute" OR
 "royal institut" OR
 "royal institute" OR
 "royal institite" OR
 "royal institution" OR
 "royal institue" OR
 "royal insititu" OR
 "royal insitute" OR
 "royal inst" OR
 "royal inst." OR
 "royal intitute" OR
 "royal istitute" OR
 "royal lnstitute" OR
 "royal lnstitufe" OR
 "royal lnstltute") AND "tech") OR

 (("kgl" OR
 "kgl." OR
 "kungl" OR
 "kungl." OR
 "kungliga") AND "tekn") OR

 "r inst of technol" OR
 "r inst. of technol." OR
 "r. inst. of tech." OR
 "r. inst. of technol" OR
 "r. inst. of technol." OR
 "royal tech" OR
 "institute of technology stockholm" OR
 "royal of technology" OR
 "royal school of technology" OR
 "royal swedish institute of technology" OR
 "royal university of technology" OR
 "royal college of technology" OR
 "royalinstitute" OR
 "alfven" OR
 "alfv\u00e9n" OR
 "10044 stockholm" OR
 "100 44 stockholm")

 NOT

 ("khyber" OR
 "peshawar" OR
 "mcmaster")'

}

# There seems to be a way to fetch ngrams

## https://api.openalex.org/works/W3128409631/ngrams
## https://api.openalex.org/works/W2023271753/ngrams

# Search UI for KTH

## https://explore.openalex.org/institutions/I86987016

#' Recently published works based on query for matching raw affiliations
#' @param raw_search_criteria raw affiliation string search criteria,
#' by default openalex_kth_rawaff_query()
#' @param since_days integer indicating days back from today
#' @export
#' @return list of tables with results
openalex_works_published_since <- function(
    raw_search_criteria = openalex_kth_rawaff_query(),
    since_days = 7) {

  criteria_aff <- raw_search_criteria
  criteria_from <- format(Sys.Date() - since_days, "%Y-%m-%d")

  params <- paste0(collapse = ",", c(
      sprintf("raw_affiliation_strings.search:%s", criteria_aff),
      sprintf("from_publication_date:%s", criteria_from)
    )
  )

  openalex_crawl("works", fmt = "tables", verbose = TRUE,
    query = openalex_query(
      filter = params,
      verbose = FALSE
    )
  )

}

#' Recently updated works based on query for matching raw affiliations
#'
#' This function requires a premium subscription API key to be set.
#'
#' @param raw_search_criteria raw affiliation string search criteria,
#' by default openalex_kth_rawaff_query()
#' @param since_minutes integer indicating minutes since now
#' @export
#' @importFrom lubridate as_date format_ISO8601
#' @return list of tables with results
openalex_works_updated_since <- function(
    raw_search_criteria = openalex_kth_rawaff_query(),
    since_minutes) {

  if (is.null(cfg()$key))
    stop("This function requires a Premium Subscription API key")

  criteria_aff <- raw_search_criteria

  #criteria_from <- "2024-01-15T08:02:55Z" #"2024-01-15T04:47:14.518460"
  criteria_from <-
    lubridate::as_datetime(Sys.time() - since_minutes * 60) |>
    lubridate::format_ISO8601(usetz = "Z")

  params <- paste0(collapse = ",", c(
      sprintf("raw_affiliation_strings.search:%s", criteria_aff),
      sprintf("from_updated_date:%s", criteria_from)
    )
  )

  openalex_crawl("works", fmt = "tables", verbose = TRUE,
    query = openalex_query(
      filter = params,
      verbose = FALSE
    )
  )

}

#' Recently created works based on query for matching raw affiliations
#'
#' This function requires a premium subscription API key to be set.
#'
#' @param raw_search_criteria raw affiliation string search criteria,
#' by default openalex_kth_rawaff_query()
#' @param since_days integer indicating minutes since now
#' @export
#' @importFrom lubridate as_date
#' @return list of tables with results
openalex_works_created_since <- function(
    raw_search_criteria = openalex_kth_rawaff_query(),
    since_days = 0) {

  if (is.null(cfg()$key))
    stop("This function requires a Premium Subscription API key")

  criteria_aff <- raw_search_criteria

  criteria_from <-
    lubridate::as_date(Sys.Date() - since_days) |>
    format("%Y-%m-%d")

  params <- paste0(collapse = ",", c(
      sprintf("raw_affiliation_strings.search:%s", criteria_aff),
      sprintf("from_created_date:%s", criteria_from)
    )
  )

  openalex_crawl("works", fmt = "object", verbose = TRUE,
    query = openalex_query(
      filter = params,
      verbose = FALSE
    )
  )

}

#' @import httr2
openalex_aboutness <- function(title, abstract = NULL, verbose = FALSE, format = c("object", "tables")) {

  #  "https://api.openalex.org/text?title=type%201%20diabetes%20research%20for%20children
  #  https://groups.google.com/g/openalex-users/c/Df4dIA19adM

  is_invalid <- function(x) nchar(x) < 20 | nchar(x) > 2000

  if (is_invalid(title))
    stop("Title must be between 20 and 2000 characters long")

  if (!is.null(abstract) && is_invalid(abstract))
    stop("Abstract, if provided, must be between 20 and 2000 character long")

  q <- purrr::compact(list(title = title, abstract = abstract))

  req <-
    httr2::request(openalex_api()) |>
    httr2::req_url_path("text") |>
    httr2::req_user_agent(cfg()$user_agent) |>
    httr2::req_body_json(data = q)

  if (verbose)
    req <- req |> httr2::req_verbose()

  resp <- req |> httr2::req_perform()

  res <- switch(match.arg(format),
    "object" = resp |> httr2::resp_body_json(),
    "tables" = parse_resp_aboutness(resp |> httr2::resp_body_json())
  )

  return(res)

}

parse_topics <- function(topics) {

  ones <-
    topics |> map(\(x) purrr::discard_at(x, at = c("field", "domain", "subfield"))) |>
    bind_rows()

  manies <-
    topics |> map(\(x) purrr::keep_at(x, at = c("field", "domain", "subfield")))

  fsd <- bind_cols(
    manies |> map("field") |> bind_rows() |> rename_with(\(x) paste0("field_", x)),
    manies |> map("subfield") |> bind_rows() |> rename_with(\(x) paste0("subfield_", x)),
    manies |> map("domain") |> bind_rows() |> rename_with(\(x) paste0("domain_", x))
  )

  bind_cols(ones, fsd)

}

parse_resp_aboutness <- function(resp) {

  d <- resp

  meta <-
    d$meta |> bind_rows()

  keywords <-
    d$keywords |> bind_rows()

  topics <-
    d$topics |> parse_topics()

  primary_topic <-
    list(d$primary_topic) |> parse_topics()

  concepts <-
    bind_cols(
      d$concepts |> bind_rows() |> select(-any_of("ancestors")),
      d$concepts |> bind_rows() |> pull(any_of("ancestors")) |> map(bind_rows) |>
        bind_rows() |> rename_with(.fn = \(x) paste0("ancestors_", x))
    )

  list(meta = meta, keywords = keywords, topics = topics, concepts = concepts)

}



openalex_filter_similar_topics <- function(work_identifier, granularity = c("topic", "domain", "field", "subfield")) {

  w <- openalex_work(work_identifier, format = "object")

  topic_id <- function(w, field_type) {

    f <- switch(field_type,
      topic = "topics.id",
      domain = "topics.domain.id",
      field = "topics.field.id",
      subfield = "topics.subfield.id"
    )

    if (field_type == "topic")
      field_type <- NULL

    res <-
      w$topics |> map_chr(c(field_type, "id")) |> unique() |>
      gsub(pattern = "https://.*?/(.*?)$", replacement = "\\1")

    paste0(f, ":", paste0(collapse = "|", res))
  }

  topics_filter <- function(w) {
    fields <- granularity
    topics <- fields |> map_chr(function(x) topic_id(w, x))
    topics |> paste(collapse = ",")
  }

  topics_filter(w)

}

openalex_works_to_tbls <- function(works) {

  pw2 <- purrr::possibly(parse_work2, otherwise = NULL, quiet = FALSE)

  message("Converting record batches to tables...")
  tbls <- works |> map(pw2, .progress = TRUE)
  message("Done")

  message("Unifying and merging tables...")

  unify_slots <- function(tbls) {

    slotz <- map(tbls, names) |> unique() |> unlist()
    strip_prefix <- function(x) gsub("^https://.*?/(.*?)$", "\\1", x)
    strip_doi <- function(x) gsub("^https://doi.org/(.*?)$", "\\1", x)
    #message("Merging slots:\n", slotz |> paste0(collapse = "\n"))
    unify <- function(x) {

      tbls |> map(x) |> bind_rows() |>
      readr::type_convert(guess_integer = TRUE) |>
      suppressMessages() |> suppressWarnings() |>
      mutate(across(where(function(x) is.character(x)) & !any_of(c("doi")), .fns = strip_prefix)) |>
      #mutate(across(any_of(c("doi")), .fns = strip_doi)) |>
      select(where(Negate(is.list)))
    }
    res <- slotz |> map(unify) |> setNames(nm = slotz)
    return (res)
  }

  out <- unify_slots(tbls)

  message("Done")
  return(out)
}
