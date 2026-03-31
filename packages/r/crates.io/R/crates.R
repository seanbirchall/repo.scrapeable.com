
# == Search ====================================================================

#' Search crates.io for Rust crates
#'
#' @param query Search query string (e.g. "serde", "async runtime")
#' @param per_page Number of results per page (default 20, max 100)
#' @return tibble: name, description, max_version, downloads, created_at,
#'   updated_at, homepage, repository, categories
#' @export
crates_search <- function(query, per_page = 20) {
  url <- sprintf("%s/crates?q=%s&per_page=%d",
                 .crates_base, utils::URLencode(query), as.integer(per_page))
  raw <- .fetch_json(url)
  crates <- raw$crates
  if (is.null(crates) || nrow(crates) == 0) return(.schema_search)

  as_tibble(crates) |>
    transmute(
      name        = as.character(name),
      description = as.character(description %||% NA_character_),
      max_version = as.character(max_version %||% NA_character_),
      downloads   = as.integer(downloads %||% NA_integer_),
      created_at  = as.POSIXct(created_at, format = "%Y-%m-%dT%H:%M:%OS"),
      updated_at  = as.POSIXct(updated_at, format = "%Y-%m-%dT%H:%M:%OS"),
      homepage    = as.character(homepage %||% NA_character_),
      repository  = as.character(repository %||% NA_character_),
      categories  = as.character(NA)
    )
}

# == Crate detail ==============================================================

#' Get crate details from crates.io
#'
#' @param name Crate name (e.g. "serde", "tokio", "rand")
#' @return tibble: name, description, max_version, downloads, created_at,
#'   updated_at, homepage, repository, license, max_stable_version
#' @export
crates_crate <- function(name) {
  url <- sprintf("%s/crates/%s", .crates_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("crates.io fetch failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_crate)

  cr <- raw$crate
  tibble(
    name               = as.character(cr$name %||% NA_character_),
    description        = as.character(cr$description %||% NA_character_),
    max_version        = as.character(cr$max_version %||% NA_character_),
    downloads          = as.integer(cr$downloads %||% NA_integer_),
    created_at         = as.POSIXct(cr$created_at %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    updated_at         = as.POSIXct(cr$updated_at %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    homepage           = as.character(cr$homepage %||% NA_character_),
    repository         = as.character(cr$repository %||% NA_character_),
    license            = as.character(cr$license %||% NA_character_),
    max_stable_version = as.character(cr$max_stable_version %||% NA_character_)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the crates.io package
#'
#' @return Character string (invisibly), also printed
#' @export
crates_context <- function() {
  .build_context("crates.io", header_lines = c(
    "# crates.io - Rust Crate Registry API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (User-Agent header sent automatically)",
    "# All functions return tibbles with typed columns."
  ))
}
