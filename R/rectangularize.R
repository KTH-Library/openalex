#' @importFrom dplyr rename bind_rows select tibble starts_with distinct
#' @importFrom tidyr unnest unnest_wider hoist unnest_longer
#' @importFrom purrr map_dfr map pluck
parse_work <- function(chunk) {

  openalex <- NULL

  # TODO fix this?
#  if (length(lengths(chunk)) == 1)
#    chunk <- list(chunk)

  if (length(chunk) == 0) return(list())

  work_ids <-
    chunk |>
    map_dfr("ids") |>
    rename(work_id = openalex)

  work_host_venue <-
    chunk |>
    map(function(x) c(work_id = pluck(x, "id"), pluck(x, "host_venue"))) |>
    bind_rows() #|> unnest(issn)
#    map_dfr(bind_rows)

  work_open_access <-
    chunk |>
    map(function(x) c(work_id = pluck(x, "id"), pluck(x, "open_access"))) |>
    map_dfr(bind_rows)

  work_biblio <-
    chunk |>
    map(function(x) c(work_id = pluck(x, "id"), pluck(x, "biblio"))) |>
    map_dfr(bind_rows)

  work_authorships <-
    chunk |>
    map(function(x) c(work_id = pluck(x, "id"), pluck(x, "authorships")))

  work_authorships_author <-
    tibble(wa = work_authorships) |>
      hoist("wa", "work_id") |>
      unnest("wa") |>
      unnest_wider("wa", names_sep = "_") |>
      unnest_wider("wa_author") |>
      select(-starts_with(c("wa_institutions")))
    # work_authorships |>
    #   tibble(wa = .) |>
    #   hoist("wa", "work_id") |>
    #   unnest_wider("wa", names_sep = "_") |>
    #   unnest_wider("wa_1") |>
    #   unnest_longer("author") |>
    #   pivot_wider(names_from = "author_id", values_from = "author") |>
    #  select(-starts_with(c("wa_", "institutions")))

    # map(function(x) tibble(
    #   work_id = pluck(x, "work_id"),
    #   author_position = pluck(x, 2, "author_position"),
    #   author_id = pluck(x, 2, "author", "id"),
    #   author_display_name = pluck(x, 2, "author", "display_name"),
    #   author_orcid = pluck(x, 2, "author", "orcid")
    # )) |>
    # bind_rows() |> unnest(author) |>
    # unnest_wider("author") |>
    # rename(author_id = id, author_display_name = display_name)


    work_authorships_institutions <-
      tibble(wa = work_authorships) |>
        hoist("wa", "work_id") |>
        unnest("wa") |>
        unnest_wider("wa", names_sep = "_") |>
        select(-c("wa_author_position")) |>
        unnest("wa_institutions") |>
        unnest_wider("wa_institutions") |>
        select(-c("wa_author")) |>
        distinct()

  #   work_authorships |>
  #   map_dfr(function(x) tibble(
  #     work_id = pluck(x, "work_id"),
  #     raw_affiliation_string = pluck(x, "raw_affiliation_string"),
  #     institutions = pluck(x, "institutions")
  #   )) |>
  #   map("institutions")
  #   unnest_wider("institutions") |>
  #   rename(institution_id = id, institution_display_name = display_name) |>
  #   distinct() |>
  #   filter(!is.na(raw_affiliation_string))

  work_concepts <-
    chunk |>
    map(function(x) tibble(work_id = pluck(x, "id"), pluck(x, "concepts"))) |>
    map_dfr(bind_rows) |> unnest_wider(2)

  # work_mesh <-
  #   chunk |>
  #   map(function(x) tibble(work_id = pluck(x, "id"), pluck(x, "mesh"))) |>
  #   map_dfr(bind_rows)

  aii_to_df <- function(x) {
    tibble(attr = names(x), val = x) |>
      unnest_wider("val", names_repair = function(x) paste0("i", seq_along(x) - 1))
  }

  #abstract_inverted_index <-
  #  chunk$abstract_inverted_index |> aii_to_df()

  abstract_inverted_index <-
      chunk |>
      map(function(x) tibble(
        work_id = pluck(x, "id"),
        aii_value = paste(collapse = " ", unlist(pluck(x, "abstract_inverted_index", .default = NA_integer_))),
        aii_key = paste(collapse = " ", unique(names(pluck(x, "abstract_inverted_index", .default = NA_character_))))
      )) |>
      map_dfr(bind_rows) |>
      unnest_longer("aii_value") |>
      distinct()

  # abstract_inverted_index <-
  #   chunk[1:20] |>
  #   map_dfr(function(x) tibble(work_id = pluck(x, "id"), aii = pluck(x, "abstract_inverted_index"))) |>
  #   bind_cols(aii_to_df(.$aii)) |>
  #   select(!any_of("aii"))
  #
  #   unnest_wider("aii", transform = function(x) aii_to_df(x))

  work_counts_by_year <-
    chunk |>
    map(function(x) tibble(work_id = pluck(x, "id"), cby = pluck(x, "counts_by_year"))) |>
    map_dfr(bind_rows) |>
    unnest_wider("cby")

  work_related_works <-
    chunk |>
    map(function(x) tibble(work_id = pluck(x, "id"), related_works = pluck(x, "related_works"))) |>
    map_dfr(bind_rows) |>
    unnest_longer("related_works")

  work_referenced_works <-
    chunk |>
    map(function(x) tibble(work_id = pluck(x, "id"), referenced_works = pluck(x, "referenced_works"))) |>
    map_dfr(bind_rows) |>
    unnest_longer("referenced_works")


  work <-
    chunk |> map_dfr(
      function(x) tibble(
        id = pluck(x, "id"),
        doi = pluck(x, "doi"),
        display_name = pluck(x, "display_name"),
        title = pluck(x, "title"),
        publication_year = pluck(x, "publication_year"),
        publication_date = pluck(x, "publication_date"),
        type = pluck(x, "type"),
        cited_by_count = pluck(x, "cited_by_count"),
        is_retracted = pluck(x, "is_retracted"),
        is_paratext = pluck(x, "is_paratext"),
        updated_date = pluck(x, "updated_date"),
        cited_by_api_url = pluck(x, "cited_by_api_url"),
        created_date = pluck(x, "created_date")
      )
    )

  list(
    work = work,
    work_ids = work_ids,
#    work_mesh = work_mesh,
    work_concepts = work_concepts,
    work_authorships_institutions = work_authorships_institutions,
    work_abstract_inverted_index = abstract_inverted_index,
    work_authorships_author = work_authorships_author,
    work_biblio = work_biblio,
    work_open_access = work_open_access,
    work_host_venue = work_host_venue,
    work_counts_by_year = work_counts_by_year,
    work_related_works = work_related_works,
    work_referenced_works = work_referenced_works
  )

}

#' @noRd
#' @import tidyr dplyr purrr
parse_work2 <- function(object) {

  name <- value <- work_id <- NULL

  unfwv <- function(l, field) {
    if (is.null(l$field)) return(tibble())
    l |> map(\(x) keep_at(x, c("id", field))) |>
      enframe() |>
      unnest_wider(any_of("value")) |>
      tidyr::unnest_wider(any_of(field)) |>
      select(-any_of(c("name")))
  }

  unfwvs <- function(l, field) {
    if (is.null(l$field)) return(tibble())
    l |> map(\(x) keep_at(x, c("id", field))) |>
      enframe() |>
      unnest_wider(any_of("value")) |>
      tidyr::unnest_wider(any_of(field), names_sep = "_") |>
      select(-any_of(c("name")))
  }

  unfw <- function(l, field) {
    if (is.null(l$field)) return(tibble())
    l |> map(\(x) keep_at(x, c("id", field))) |>
      compact() |>
      map_df(tibble::as_tibble) |>
      tidyr::unnest_wider(any_of(field)) |>
      compact()
  }

  unfws <- function(l, field) {
    if (is.null(l$field)) return(tibble())
    l |> map(\(x) keep_at(x, c("id", field))) |>
      compact() |> 
      map_df(tibble::as_tibble) |>
      tidyr::unnest_wider(any_of(field), names_sep = "_") |> 
      compact()
  }

  unfl <- function(l, field) {
    #has_field <- l |> map_lgl(\(x) field %in% names(x)) |> all()
    #if (!has_field) return(data.frame(0))
    if (is.null(l$field)) return(tibble())
    l |> map(\(x) keep_at(x, c("id", field))) |>
      compact() |>
      map_df(tibble::as_tibble) |>
      tidyr::unnest_longer(any_of(field)) |>
      compact()
  }

  pluck_with_id <- function(x, field) {
    if (!pluck_exists(x, field)) return (NULL)
    c(id = pluck(x, "id"), pluck(x, field))
  }

  w <- object

  colz <-
    w$results |>
    map(\(x) tibble(cols = names(x), l = lengths(x)) |>
      tidyr::pivot_wider(names_from = "cols", values_from = "l")
    ) |>
    bind_rows() |>
    summarize(across(everything(), max)) |>
    ungroup() |>
    tidyr::pivot_longer(cols = everything()) 
  
  one_to_one <- colz |> filter(value == 1, name != "versions") |> pull(name)

  # workz <-
  #   w$results  |>
  #   map(\(x) x[one_to_one]  |> compact()  |> as_tibble())  |>
  #   bind_rows()

  plf <- function(o, f) {
    l <- o |> map(\(x) purrr::pluck(x, f)) |> unlist()
    list(l) |> setNames(nm = f)
  }

  # TODO: remove keep_empty(?)
  wide <- enframe(w) |> unnest_longer(2, keep_empty = TRUE) |> unnest_wider(2) 
  
  workz <- 
    wide |> select(any_of(one_to_one))

  ids <- 
    wide |> select(work_id = id, any_of(c("ids"))) |> unnest_wider(any_of(c("ids")))

  re_ids <- paste0(
      "(https://openalex.org/)|(https://doi.org/)|",
      "(https://pubmed.ncbi.nlm.nih.gov/)|(https://www.ncbi.nlm.nih.gov/pmc/articles/)|",
      "(https://www.wikidata.org/wiki/)"
    )

  fuw <- function(fields) {
      wide |> select(work_id = "id", any_of(c(fields))) |> 
      unnest_wider(any_of(c(fields)), names_sep = "_") |> 
#          unnest_wider(any_of(c(fields))) |> 
      mutate(across(-contains("url"), \(x) gsub(re_ids, "", x)))
  }

  authorships <- 
    wide |> select(work_id = id, authorships) |> 
    unnest_longer(2) |> unnest_wider(2) |> 
    #pull("author") hoist("authorships", list("author")) |>   
    unnest_wider(any_of("author"), names_sep = "_") |>
    unnest_longer(any_of("countries")) |>
    unnest_longer(any_of("institutions")) |>
    unnest_wider(any_of("institutions"), names_sep = "_") |>
    unnest_longer(any_of("institutions_lineage")) |>
    unnest_longer(any_of("affiliations")) |>
    unnest_wider(any_of("affiliations"), names_sep = "_") |>
    unnest_longer(any_of("raw_affiliation_strings")) |>
    unnest_longer(any_of("affiliations_institution_ids"))

  fields <- c(
    "ids", "open_access", "apc_list", "apc_paid",
    "citation_normalized_percentile", "cited_by_percentile_year",
    "biblio"
  )

  fields <- fields[which(fields %in% unique(colz$name))]
  various <- fields |> map(fuw) |> set_names(fields)

  fields2 <- c("counts_by_year", "grants", "mesh")
  fields2 <- fields2[which(fields2 %in% unique(colz$name))]

  bcbr <- function(field) {
    w$results |> map_dfr(\(x) bind_cols(work_id = x$id, bind_rows(x |> getElement(field)))) |> 
      mutate(across(-contains("url"), \(x) gsub(re_ids, "", x)))
  }

  various2 <- fields2 |> map(bcbr) |> set_names(fields2)

  fields3 <- c(
    "sustainable_development_goals",
    "keywords",
    "concepts",
    "datasets"
  )
  fields3 <- fields3[which(fields3 %in% unique(colz$name))]

  various3 <- 
    fields3 |> map(bcbr) |> set_names(fields3)

  fields4 <- c(
    "referenced_works",
    "related_works",
    "indexed_in",
    "corresponding_institution_ids",
    "corresponding_author_ids"#,
#    "abstract_inverted_index"
  )

  fields4 <- fields4[which(fields4 %in% unique(colz$name))]

  bcbv <- function(field) {
    w$results |> map_dfr(\(x) bind_cols(work_id = x$id, rw = unlist(x |> getElement(field)))) |> 
      setNames(nm = c("work_id", field)) |> 
      mutate(across(-contains("url"), \(x) gsub(re_ids, "", x)))
  }

  various4 <- 
    fields4 |> map(bcbv)|> set_names(nm = fields4)

  aii_to_abstract <- function(aii) {

    value <- NULL

    abstract <-
      aii |> enframe() |>
      unnest_longer(any_of(c("value"))) |>
      arrange(-desc(value)) |>
      pull(any_of(c("name"))) |>
      paste0(collapse = " ")

    if (!nzchar(abstract))
      return (NA_character_)

    return (abstract)

  }

  abstracts <-
    w$results |>
    map(function(x) tibble(
      work_id = pluck(x, "id"),
      abstract = aii_to_abstract(pluck(x, "abstract_inverted_index"))
    )) |>
    map_dfr(bind_rows)
 
  primary_location <- 
    "primary_location" |> fuw() 
  
  primary_location_source <- 
    primary_location |> select(any_of(c("work_id", "primary_location_source"))) |> 
    mutate(primary_location_source = map(primary_location_source, \(x) eval(parse(text = x)))) |> 
    mutate(primary_location_source = map(primary_location_source, \(x) compact(x) |> as_tibble())) |> 
    unnest(2) |> unnest(any_of(c("issn")))

  primary_location <- 
    primary_location |> select(-any_of("primary_location_source"))

  primary_topic <-
    "primary_topic" |> fuw() |> 
    mutate(across(any_of(c("primary_topic_subfield", "primary_topic_field", "primary_topic_domain")), \(y) y |> map(\(x) eval(parse(text = x))))) |> 
    mutate(across(any_of(c("primary_topic_subfield", "primary_topic_field", "primary_topic_domain")), \(y) y |> map(\(x) compact(x) |> as_tibble()))) |> 
    unnest("primary_topic_subfield", names_sep = "_") |> 
    unnest("primary_topic_field", names_sep = "_") |> 
    unnest("primary_topic_domain", names_sep = "_")

  topics <-
    wide |> select(any_of(c("id", "topics"))) |> 
    unnest(topics) |> 
    unnest_wider(topics, names_sep = "_") |> 
    unnest_wider(any_of("topics_field"), names_sep = "_") |>
    unnest_wider(any_of("topics_subfield"), names_sep = "_") |>
    unnest_wider(any_of("topics_domain"), names_sep = "_") |>
    compact() |> 
    mutate(across(-contains("url"), \(x) gsub(re_ids, "", x)))

  best_oa_location <-
    "best_oa_location" |> fuw()

  best_oa_location_source <- 
    best_oa_location |> select(work_id, best_oa_location_source) |> 
    mutate(best_oa_location_source = map(best_oa_location_source, \(x) eval(parse(text = x)))) |> 
    mutate(best_oa_location_source = map(best_oa_location_source, \(x) compact(x) |> as_tibble())) |> 
    unnest(2) |> unnest(any_of(c("issn"))) |> 
    unnest_longer(any_of(c("source_host_organization_lineage", "source_host_organization_lineage_names"))) |>
    compact() |> 
    mutate(across(-contains("url"), \(x) gsub(re_ids, "", x)))

  best_oa_location <-
    best_oa_location |> select(-any_of(c("best_oa_location_source")))

  locations <-
    wide |> select(any_of(c("id", "locations"))) |> 
    unnest(locations) |> 
    unnest_wider(locations) |> 
    unnest_wider(source, names_sep = "_") |> 
    #w$results |> unfw("locations") |>
    #unnest_wider(any_of("source"), names_sep = "_") |>
    unnest_longer(any_of("source_issn")) |>
    unnest_longer(any_of(c("source_host_organization_lineage", "source_host_organization_lineage_names"))) |>
    compact() |> 
    mutate(across(-contains("url"), \(x) gsub(re_ids, "", x)))

  c(
    list(work = workz),
    list(abstracts = abstracts),
    list(authorships = authorships),
    various, various2, various3, various4,
    list(
      primary_location = primary_location,
      primary_location_source = primary_location_source,
      best_oa_location = best_oa_location,
      best_oa_location_source = best_oa_location_source,
      locations = locations,
      primary_topic = primary_topic,
      topics = topics
    )
  )

}
