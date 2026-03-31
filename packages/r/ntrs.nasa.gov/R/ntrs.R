# == Schemas ===================================================================

.schema_citations <- tibble(
  id = character(), title = character(), sti_type = character(),
  distribution = character(), created = as.Date(character()),
  authors = character()
)

# == Public functions ==========================================================

#' Search NASA Technical Reports Server
#'
#' @param query Search query string
#' @param page_size Results per page (default 25)
#' @param page Page number (0-indexed, default 0)
#' @return tibble: id, title, sti_type, distribution, created, authors
#' @export
ntrs_search <- function(query, page_size = 25, page = 0) {
  body <- list(
    query = query,
    page = list(size = page_size, from = page * page_size)
  )
  raw <- .ntrs_post("/citations/search", body)
  df <- raw$results
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(.schema_citations)

  auth_strs <- vapply(seq_len(nrow(df)), function(i) {
    auths <- df$authorAffiliations[[i]]
    if (is.null(auths) || !is.data.frame(auths) || nrow(auths) == 0) return(NA_character_)
    paste(vapply(seq_len(nrow(auths)), function(j) {
      tryCatch(auths$meta$author$name[j] %||% NA_character_,
               error = function(e) NA_character_)
    }, character(1)), collapse = "; ")
  }, character(1))

  tibble(
    id = as.character(df$id),
    title = as.character(df$title),
    sti_type = as.character(df$stiType),
    distribution = as.character(df$distribution),
    created = tryCatch(as.Date(substr(df$created, 1, 10)),
                       error = function(e) rep(as.Date(NA), nrow(df))),
    authors = auth_strs
  )
}

#' Fetch a single NTRS citation by ID
#'
#' @param id NTRS citation ID
#' @return tibble: single row with citation details
#' @export
ntrs_citation <- function(id) {
  url <- sprintf("%s/citations/%s", .ntrs_base, as.character(id))
  raw <- .fetch_json(url)
  if (is.null(raw)) return(.schema_citations)

  auths <- raw$authorAffiliations
  auth_str <- if (!is.null(auths) && length(auths) > 0) {
    if (is.data.frame(auths)) {
      paste(vapply(seq_len(nrow(auths)), function(i) {
        tryCatch(auths$meta[[i]]$author$name %||% NA_character_,
                 error = function(e) NA_character_)
      }, character(1)), collapse = "; ")
    } else {
      paste(vapply(auths, function(a) a$meta$author$name %||% NA_character_, character(1)), collapse = "; ")
    }
  } else NA_character_

  tibble(
    id = as.character(raw$id %||% NA),
    title = as.character(raw$title %||% NA),
    sti_type = as.character(raw$stiType %||% NA),
    distribution = as.character(raw$distribution %||% NA),
    created = tryCatch(as.Date(substr(raw$created %||% "", 1, 10)),
                       error = function(e) as.Date(NA)),
    authors = auth_str
  )
}

#' NTRS package context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
ntrs_context <- function() {
  .build_context("ntrs.nasa.gov", header_lines = c(
    "# ntrs.nasa.gov - NASA Technical Reports Server Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (public POST search API)",
    "# STI types: REPRINT, TECHNICAL_REPORT, CONFERENCE_PAPER, etc.",
    "# Distribution: PUBLIC, LIMITED"
  ))
}
