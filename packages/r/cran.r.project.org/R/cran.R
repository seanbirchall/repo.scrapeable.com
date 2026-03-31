
# == Package metadata ==========================================================

#' Get CRAN package metadata
#'
#' Returns metadata for a specific R package from CRAN via the crandb API.
#'
#' @param name Package name (e.g. "dplyr", "ggplot2", "data.table")
#' @return tibble: package, version, title, description, author, maintainer,
#'   license, depends, imports, date
#' @export
cran_package <- function(name) {
  url <- sprintf("%s/%s", .crandb_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("crandb fetch failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_package)

  deps <- if (!is.null(raw$Depends) && is.list(raw$Depends)) paste(names(raw$Depends), collapse = ", ") else as.character(raw$Depends %||% NA_character_)
  imps <- if (!is.null(raw$Imports) && is.list(raw$Imports)) paste(names(raw$Imports), collapse = ", ") else as.character(raw$Imports %||% NA_character_)
  pub_date <- tryCatch(as.Date(raw$`Date/Publication` %||% raw$date %||% NA_character_), error = function(e) as.Date(NA))

  tibble(
    package     = as.character(raw$Package %||% raw$`_id` %||% NA_character_),
    version     = as.character(raw$Version %||% NA_character_),
    title       = as.character(raw$Title %||% NA_character_),
    description = as.character(raw$Description %||% NA_character_),
    author      = as.character(raw$Author %||% NA_character_),
    maintainer  = as.character(raw$Maintainer %||% NA_character_),
    license     = as.character(raw$License %||% NA_character_),
    depends     = deps,
    imports     = imps,
    date        = pub_date
  )
}

#' Search CRAN packages by keyword
#'
#' Searches through CRAN package names and fetches metadata for matches.
#' Uses a case-insensitive grep on all CRAN package names via crandb.
#'
#' @param query Search query string (e.g. "dplyr", "time series", "plot")
#' @param limit Maximum number of results (default 20)
#' @return tibble: package, version, title, description, maintainer, score
#' @export
cran_search <- function(query, limit = 20) {
  # Get all package names from crandb
  all_names <- tryCatch(.fetch_json(sprintf("%s/-/pkgnames", .crandb_base)),
                        error = function(e) {
    message("crandb pkgnames failed: ", e$message)
    return(NULL)
  })
  if (is.null(all_names) || length(all_names) == 0) return(.schema_search)

  # Search through names
  matches <- grep(query, all_names, ignore.case = TRUE, value = TRUE)
  if (length(matches) == 0) return(.schema_search)
  matches <- head(matches, limit)

  # Fetch metadata for each match individually from crandb
  rows <- lapply(matches, function(pkg_name) {
    tryCatch({
      p <- jsonlite::fromJSON(.fetch(sprintf("%s/%s", .crandb_base, utils::URLencode(pkg_name))),
                              simplifyVector = FALSE)
      tibble(
        package     = as.character(p$Package %||% pkg_name),
        version     = as.character(p$Version %||% NA_character_),
        title       = as.character(p$Title %||% NA_character_),
        description = as.character(p$Description %||% NA_character_),
        maintainer  = as.character(p$Maintainer %||% NA_character_),
        score       = NA_real_
      )
    }, error = function(e) NULL)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(.schema_search)
  bind_rows(rows)
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the cran.r-project.org package
#'
#' @return Character string (invisibly), also printed
#' @export
cran_context <- function() {
  .build_context("cran.r.project.org", header_lines = c(
    "# cran.r-project.org - CRAN Package Metadata Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Uses crandb API (https://crandb.r-pkg.org)",
    "# All functions return tibbles with typed columns."
  ))
}
