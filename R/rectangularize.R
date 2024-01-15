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
