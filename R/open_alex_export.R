#' Use OpenAlex API for exporting data in tabular and wos formats
#' @param q the query, for example "authorships.institutions.lineage:i86987016,authorships.institutions.lineage:!i4210161097,type:types/article,primary_location.source.type:source-types/journal|source-types/conference,publication_year:2023"
#' @param fmt the export format, one of "csv" or "wos-plaintext" or "wos-plaintext-diva"
#' @param raw_string boolean to indicate whether a raw string should be returned
#' @return a character vector with a raw string with the results from the export or a data frame
#' @import httr2
#' @importFrom dplyr bind_cols
#' @importFrom readr read_csv
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
#' @param x character string with "wos-plaintext" format as returned from OpenAlex export API endpoint
#' @importFrom stats setNames
#' @importFrom utils tail
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
#' @param crawl the results from running the to_tbls fcn
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

openalex_fields <- function() {
  paste0(
  "abstract.search, abstract.search.no_stem, apc_list.currency, apc_list.provenance, ",
  "apc_list.value, apc_list.value_usd, apc_paid.currency, apc_paid.provenance, apc_paid.value, ",
  "apc_paid.value_usd, author.id, author.orcid, authors_count, ",
  "authorships.affiliations.institution_ids, authorships.author.id, authorships.author.orcid, ",
  "authorships.countries, authorships.institutions.continent, authorships.institutions.country_code, ",
  "authorships.institutions.id, authorships.institutions.is_global_south, ",
  "authorships.institutions.lineage, authorships.institutions.ror, authorships.institutions.type, ",
  "authorships.is_corresponding, best_oa_location.is_accepted, best_oa_location.is_oa, ",
  "best_oa_location.is_published, best_oa_location.landing_page_url, best_oa_location.license, ",
  "best_oa_location.license_id, best_oa_location.source.host_organization, ",
  "best_oa_location.source.host_organization_lineage, best_oa_location.source.id, ",
  "best_oa_location.source.is_in_doaj, best_oa_location.source.is_oa, best_oa_location.source.issn, ",
  "best_oa_location.source.type, best_oa_location.version, best_open_version, biblio.first_page, ",
  "biblio.issue, biblio.last_page, biblio.volume, citation_normalized_percentile.is_in_top_10_percent, ",
  "citation_normalized_percentile.is_in_top_1_percent, citation_normalized_percentile.value, ",
  "cited_by, cited_by_count, cited_by_percentile_year.max, cited_by_percentile_year.min, ",
  "cites, concept.id, concepts.id, concepts.wikidata, concepts_count, corresponding_author_ids, ",
  "corresponding_institution_ids, countries_distinct_count, datasets, default.search, ",
  "display_name, display_name.search, display_name.search.no_stem, doi, doi_starts_with, ",
  "from_created_date, from_publication_date, fulltext.search, fulltext_origin, fwci, ",
  "grants.award_id, grants.funder, has_abstract, has_doi, has_embeddings, has_fulltext, ",
  "has_oa_accepted_or_published_version, has_oa_submitted_version, has_old_authors, has_orcid, ",
  "has_pdf_url, has_pmcid, has_pmid, has_raw_affiliation_strings, has_references, ids.mag, ",
  "ids.openalex, ids.pmcid, ids.pmid, indexed_in, institution.id, institution_assertions.country_code, ",
  "institution_assertions.id, institution_assertions.lineage, institution_assertions.ror, ",
  "institution_assertions.type, institutions.continent, institutions.country_code, institutions.id, ",
  "institutions.is_global_south, institutions.ror, institutions.type, institutions_distinct_count, ",
  "is_corresponding, is_oa, is_paratext, is_retracted, journal, keyword.search, keywords.id, ",
  "language, locations.is_accepted, locations.is_oa, locations.is_published, locations.landing_page_url, ",
  "locations.license, locations.license_id, locations.source.has_issn, ",
  "locations.source.host_institution_lineage, locations.source.host_organization, ",
  "locations.source.host_organization_lineage, locations.source.id, ",
  "locations.source.is_core, locations.source.is_in_doaj, locations.source.is_oa, ",
  "locations.source.issn, locations.source.publisher_lineage, locations.source.type, ",
  "locations.version, locations_count, mag, mag_only, oa_status, ",
  "open_access.any_repository_has_fulltext, open_access.is_oa, open_access.oa_status, ",
  "openalex, openalex_id, pmcid, pmid, primary_location.is_accepted, primary_location.is_oa, ",
  "primary_location.is_published, primary_location.landing_page_url, primary_location.license, ",
  "primary_location.license_id, primary_location.source.has_issn, ",
  "primary_location.source.host_institution_lineage, primary_location.source.host_organization, ",
  "primary_location.source.host_organization_lineage, primary_location.source.id, ",
  "primary_location.source.is_core, primary_location.source.is_in_doaj, ",
  "primary_location.source.is_oa, primary_location.source.issn, ",
  "primary_location.source.publisher_lineage, primary_location.source.type, ",
  "primary_location.version, primary_topic.domain.id, primary_topic.field.id, primary_topic.id, ",
  "primary_topic.subfield.id, publication_date, publication_year, raw_affiliation_strings.search, ",
  "raw_author_name.search, referenced_works, referenced_works_count, related_to, ",
  "repository, semantic.search, sustainable_development_goals.id, sustainable_development_goals.score, ",
  "title.search, title.search.no_stem, title_and_abstract.search, title_and_abstract.search.no_stem, ",
  "to_created_date, to_publication_date, to_updated_date, topics.domain.id, topics.field.id, ",
  "topics.id, topics.subfield.id, topics_count, type, type_crossref, version"
) |> strsplit(split = ", ") |> unlist()
}

openalex_groupbys_default <- function() { c(
  "primary_location.source.type",
  "primary_location.source.id",
  "is_retracted",
  "primary_location.source.publisher_lineage",
  "open_access.oa_status",
  "best_oa_location.is_published",
  "best_oa_location.is_accepted",
  "best_oa_location.license",
  "authorships.institutions.type",
  "has_pmid",
  "has_orcid",
  "mag_only",
  "primary_location.source.is_in_doaj",
  "has_doi",
  "primary_location.source.is_oa",
  "open_access.any_repository_has_fulltext",
  "institutions.is_global_south",
  "primary_location.source.is_core",
  "corresponding_institution_ids",
  "corresponding_author_ids",
  "authorships.institutions.continent",
  "language",
  "keywords.id",
  "authorships.countries",
  "authorships.author.id",
  "sustainable_development_goals.id",
  "grants.funder",
  "primary_topic.subfield.id",
  "primary_topic.field.id",
  "primary_topic.domain.id",
  "primary_topic.id",
  "type",
  "authorships.institutions.lineage",
  "open_access.is_oa",
  "publication_year"
)}

openalex_filter_default <- function() {
  "authorships.institutions.lineage:i86987016,publication_year:2025"
}

openalex_groupbys <- function(q) {

  colname <- field <- colid <- i <- NULL

  csv <- 
    q |> readr::read_lines()
  
  schema <- 
    csv[1:2] |> 
    strsplit(split = ",") |> 
    setNames(c("field", "colname")) |> 
    purrr::map(\(x) na_if(x, "")) |> 
    tibble::as_tibble() |> 
    tibble::rowid_to_column(var = "colid") |> 
    tidyr::fill(any_of(c("field"))) |> 
    dplyr::filter(!is.na(colname)) |> 
    dplyr::group_by(field) |> 
    dplyr::summarize(i = min(colid), j = max(colid), colnames = list(colname)) |> 
    dplyr::arrange(-desc(i))
  
  body <- 
    csv[-c(1:2)] |> paste(collapse = "\n")
  
  all <- 
    readr::read_csv(body, col_names = NULL, show_col_types = FALSE)
  
  parse_body <- function(field, i, j, colnames) {
    all |> 
      select(c(i, j)) |> 
      setNames(nm = unlist(colnames)) |> 
      filter(!if_all(everything(), is.na))
      #filter(if_all(\(x) all(is.na(x)))) #|> 
      #list() |> setNames(nm = field)
  }

  tbls <- 
    schema |> purrr::pmap(parse_body) |> 
    setNames(nm = schema$field) |>
    map(\(x) x |> mutate(across(any_of(c("name")), as.character)))

  tbls

}

#' Counts from OpenAlex
#' 
#' Aggregates/counts can be retrieved using the group_bys query parameter
#' 
#' @param filter a set of filter criteria, see the defaults in openalex_filter_default()
#' @param dimensions a set of grouping dimensions, see the defaults in openalex_groupbys_default()
#' @return a list of tibbles
#' @export
#' @importFrom utils URLencode
openalex_counts <- function(
  filter = openalex_filter_default(), 
  dimensions = openalex_groupbys_default()
) {

  groupbys <- 
    dimensions|> paste0(collapse = ",") |> utils::URLencode(reserved = TRUE)

  url <- paste0(
    openalex_api(), "works?group_bys=", groupbys,
    "&per_page=200&format=csv&mailto=team%40ourresearch.org",
    "&filter=", filter
  )

   message("Requesting ", url)

  url |> openalex_groupbys()
}

read_page <- function(level = c("topics", "subfields", "fields", "domains"), page) {

  topic_page <- 
    "https://api.openalex.org/%s?select=id,display_name,description,subfield,field,domain&per_page=200&page=%s" |> 
    sprintf(level, page) |> 
    jsonlite::fromJSON()

  tbl <- topic_page$results |> tibble::as_tibble()
  structure(tbl, meta = topic_page$meta)

}

openalex_level <- function(l) {
  
  t <- read_page(level = l, page = 1)
  np <- ceiling(attr(t, "meta")$count / 200)

  ts <- NULL
  if (np > 1) {
    ts <- (2:np) |> map(\(x) read_page(level = l, page = x), .progress = TRUE)
  }
  
  t |> bind_rows(map_dfr(ts, bind_rows))
  
}

openalex_levels <- function() {

  display_name <- NULL

  topics <- openalex_level("topics")

  four <- topics |> select(all_of(c("subfield", "field", "domain")))

  topics |> rename(id_topic = id, topic = display_name) |> select(1:3) |> bind_cols(
    four$subfield |> as_tibble() |> rename(id_subfield = id, subfield = display_name),
    four$field |> as_tibble() |> rename(id_field = id, field = display_name),
    four$domain |> as_tibble() |> rename(id_domain = id, domain = display_name)
  ) |> 
  mutate(across(contains("id_"), \(x) gsub("https://openalex.org/", "", x)))

}

#' Topics
#' 
#' Table of current topics, subfields, fields and domains used at OpenAlex
#' @export
openalex_topics <- function() {
  openalex_levels()
}
