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

  name <- value <- NULL

  unfwv <- function(l, field) {
    l |> map(\(x) keep_at(x, c("id", field))) |>
      enframe() |>
      unnest_wider(any_of("value")) |>
      tidyr::unnest_wider(any_of(field)) |>
      select(-any_of(c("name")))
  }

  unfwvs <- function(l, field) {
    l |> map(\(x) keep_at(x, c("id", field))) |>
      enframe() |>
      unnest_wider(any_of("value")) |>
      tidyr::unnest_wider(any_of(field), names_sep = "_") |>
      select(-any_of(c("name")))
  }

  unfw <- function(l, field) {
    l |> map(\(x) keep_at(x, c("id", field))) |>
      compact() |>
      map_df(tibble::as_tibble) |>
      tidyr::unnest_wider(any_of(field)) |>
      compact()
  }

  unfws <- function(l, field) {
    l |> map(\(x) keep_at(x, c("id", field))) |>
      map_df(tibble::as_tibble) |>
      tidyr::unnest_wider(any_of(field), names_sep = "_")
  }

  unfl <- function(l, field) {
    #has_field <- l |> map_lgl(\(x) field %in% names(x)) |> all()
    #if (!has_field) return(data.frame(0))
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


  one_to_one <-
    w$results |>
    map(\(x) tibble(cols = names(x), l = lengths(x)) |>
      tidyr::pivot_wider(names_from = "cols", values_from = "l")
    ) |>
    bind_rows() |>
    summarize(across(everything(), max)) |>
    ungroup() |>
    tidyr::pivot_longer(cols = everything()) |>
    filter(value == 1, name != "versions") |> pull(name)

  workz <-
    w$results  |>
    map(\(x) x[one_to_one]  |> compact()  |> as_tibble())  |>
    bind_rows()

  plf <- function(o, f) {
    l <- o |> map(\(x) purrr::pluck(x, f)) |> unlist()
    list(l) |> setNames(nm = f)
  }

  authorships <-
    w$results |> unfw("authorships") |>
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

  various <-
    fields |> map(\(x) w$results |> map(\(y) pluck_with_id(y, x))) |>
    set_names(fields) |> map(bind_rows)

  fields2 <- c("counts_by_year", "grants", "mesh")

  various2 <-
    fields2 |> map(\(x) w$results |> unfw(x)) |> set_names(nm = fields2)

  fields3 <- c(
    "sustainable_development_goals",
    "keywords",
    "concepts",
    "datasets"
  )
  various3 <- fields3 |> map(\(x) w$results |> unfws(x)) |> set_names(nm = fields3)

  fields4 <- c(
    "referenced_works",
    "related_works",
    "indexed_in",
    "corresponding_institution_ids",
    "corresponding_author_ids"#,
#    "abstract_inverted_index"
  )

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

  various4 <-
    fields4 |> map(\(x) w$results |> unfl(x)) |> set_names(nm = fields4)

  primary_location <-
    w$results |> unfwv("primary_location") |>
    unnest_wider(any_of("source"), names_sep = "_") |>
    unnest_longer(any_of("source_issn")) |>
    unnest_longer(any_of(c("source_host_organization_lineage", "source_host_organization_lineage_names"))) |>
    compact()

  primary_topic <-
    w$results |> unfwvs("primary_topic") |>
    unnest_wider(any_of("primary_topic"), names_sep = "_") |>
    unnest_wider(any_of("primary_topic_field"), names_sep = "_") |>
    unnest_wider(any_of("primary_topic_subfield"), names_sep = "_") |>
    unnest_wider(any_of("primary_topic_domain"), names_sep = "_") |>
    compact()

  best_oa_location <-
    w$results |> unfwv("best_oa_location") |>
    unnest_wider(any_of("source"), names_sep = "_") |>
    unnest_longer(any_of("source_issn")) |>
    unnest_longer(any_of(c("source_host_organization_lineage", "source_host_organization_lineage_names"))) |>
    compact()

  locations <-
    w$results |> unfw("locations") |>
    unnest_wider(any_of("source"), names_sep = "_") |>
    unnest_longer(any_of("source_issn")) |>
    unnest_longer(any_of(c("source_host_organization_lineage", "source_host_organization_lineage_names"))) |>
    compact()

  topics <-
    w$results |> unfws("topics") |>
    unnest_wider(any_of("topics_field"), names_sep = "_") |>
    unnest_wider(any_of("topics_subfield"), names_sep = "_") |>
    unnest_wider(any_of("topics_domain"), names_sep = "_") |>
    compact()

  c(list(work = workz),
    list(abstracts = abstracts),
    list(authorships = authorships),
    various, various2, various3, various4,
    list(
      primary_location = primary_location,
      best_oa_location = best_oa_location,
      locations = locations,
      primary_topic = primary_topic,
      topics = topics
    )
  )

}
