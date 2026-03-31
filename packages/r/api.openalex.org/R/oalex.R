#' Search OpenAlex for scholarly works
#'
#' @param search Search query string
#' @param per_page Number of results per page. Default 10, max 200.
#' @param page Page number. Default 1.
#' @param filter Optional filter string (e.g., "publication_year:2024")
#' @return tibble: id, doi, title, publication_year, cited_by_count,
#'   type, journal, authors, open_access
#' @export
oalex_works <- function(search, per_page = 10, page = 1, filter = NULL) {
  url <- sprintf(
    "%s/works?search=%s&per_page=%d&page=%d&mailto=%s",
    .oalex_base, utils::URLencode(search, reserved = TRUE),
    as.integer(per_page), as.integer(page), .ua
  )
  if (!is.null(filter)) url <- paste0(url, "&filter=", utils::URLencode(filter))

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_works)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_works)
  if (!is.data.frame(results)) return(.schema_works)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      id = as.character(if ("id" %in% nms) id else NA_character_),
      doi = as.character(if ("doi" %in% nms) doi else NA_character_),
      title = as.character(if ("display_name" %in% nms) display_name else if ("title" %in% nms) title else NA_character_),
      publication_year = as.integer(if ("publication_year" %in% nms) publication_year else NA_integer_),
      cited_by_count = as.integer(if ("cited_by_count" %in% nms) cited_by_count else NA_integer_),
      type = as.character(if ("type" %in% nms) type else NA_character_),
      journal = as.character(if ("primary_location" %in% nms) {
        pl <- primary_location
        if (is.data.frame(pl) && "source" %in% names(pl)) {
          src <- pl$source
          if (is.data.frame(src) && "display_name" %in% names(src)) src$display_name
          else NA_character_
        } else NA_character_
      } else NA_character_),
      authors = as.character(if ("authorships" %in% nms) sapply(seq_len(n()), function(i) {
        x <- authorships[[i]]
        if (is.data.frame(x) && "author" %in% names(x)) {
          au <- x$author
          if (is.data.frame(au) && "display_name" %in% names(au)) paste(au$display_name, collapse = "; ")
          else NA_character_
        } else NA_character_
      }) else NA_character_),
      open_access = as.logical(if ("open_access" %in% nms) {
        oa <- open_access
        if (is.data.frame(oa) && "is_oa" %in% names(oa)) oa$is_oa
        else NA
      } else NA)
    )
}

#' Search OpenAlex for authors
#'
#' @param search Search query string (author name)
#' @param per_page Number of results. Default 10.
#' @return tibble: id, display_name, works_count, cited_by_count,
#'   institution, orcid
#' @export
oalex_authors <- function(search, per_page = 10) {
  url <- sprintf(
    "%s/authors?search=%s&per_page=%d&mailto=%s",
    .oalex_base, utils::URLencode(search, reserved = TRUE),
    as.integer(per_page), .ua
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_authors)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_authors)
  if (!is.data.frame(results)) return(.schema_authors)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      id = as.character(if ("id" %in% nms) id else NA_character_),
      display_name = as.character(if ("display_name" %in% nms) display_name else NA_character_),
      works_count = as.integer(if ("works_count" %in% nms) works_count else NA_integer_),
      cited_by_count = as.integer(if ("cited_by_count" %in% nms) cited_by_count else NA_integer_),
      institution = as.character(if ("last_known_institutions" %in% nms) sapply(last_known_institutions, function(x) {
        if (is.data.frame(x) && nrow(x) > 0) x$display_name[1] %||% NA_character_
        else NA_character_
      }) else NA_character_),
      orcid = as.character(if ("orcid" %in% nms) orcid else NA_character_)
    )
}

#' Search OpenAlex for institutions
#'
#' @param search Search query string (institution name)
#' @param per_page Number of results. Default 10.
#' @return tibble: id, display_name, type, country_code, works_count,
#'   cited_by_count
#' @export
oalex_institutions <- function(search, per_page = 10) {
  url <- sprintf(
    "%s/institutions?search=%s&per_page=%d&mailto=%s",
    .oalex_base, utils::URLencode(search, reserved = TRUE),
    as.integer(per_page), .ua
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_institutions)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_institutions)
  if (!is.data.frame(results)) return(.schema_institutions)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      id = as.character(if ("id" %in% nms) id else NA_character_),
      display_name = as.character(if ("display_name" %in% nms) display_name else NA_character_),
      type = as.character(if ("type" %in% nms) type else NA_character_),
      country_code = as.character(if ("country_code" %in% nms) country_code else NA_character_),
      works_count = as.integer(if ("works_count" %in% nms) works_count else NA_integer_),
      cited_by_count = as.integer(if ("cited_by_count" %in% nms) cited_by_count else NA_integer_)
    )
}

#' Print OpenAlex context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
oalex_context <- function() {
  .build_context(
    pkg_name = "api.openalex.org",
    header_lines = c(
      "# Package: api.openalex.org",
      "# OpenAlex API - open scholarly metadata",
      "# Auth: none (polite pool via mailto in User-Agent)",
      "# Rate limits: 10 req/sec without key, 100K/day",
      "#",
      "# Covers: 250M+ works, 100M+ authors, 100K+ institutions",
      "# Filters: publication_year:2024, type:article,",
      "#   cited_by_count:>100, open_access.is_oa:true"
    )
  )
}
