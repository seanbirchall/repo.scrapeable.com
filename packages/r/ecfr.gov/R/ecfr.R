# == Titles ====================================================================

#' List all CFR titles
#'
#' Returns the 50 titles of the Code of Federal Regulations
#' with their latest amendment dates.
#'
#' @return tibble: number (integer), name (character),
#'   latest_amended_on (Date), latest_issue_date (Date),
#'   up_to_date_as_of (Date), reserved (logical)
#' @export
ecfr_titles <- function() {
  url <- paste0(.ecfr_base, "/versioner/v1/titles")
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("eCFR API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_titles)

  titles <- raw$titles
  if (is.null(titles) || length(titles) == 0) return(.schema_titles)

  tibble(
    number            = vapply(titles, function(t) as.integer(t$number %||% NA_integer_), integer(1)),
    name              = vapply(titles, function(t) t$name %||% NA_character_, character(1)),
    latest_amended_on = as.Date(vapply(titles, function(t) t$latest_amended_on %||% NA_character_, character(1))),
    latest_issue_date = as.Date(vapply(titles, function(t) t$latest_issue_date %||% NA_character_, character(1))),
    up_to_date_as_of  = as.Date(vapply(titles, function(t) t$up_to_date_as_of %||% NA_character_, character(1))),
    reserved          = vapply(titles, function(t) as.logical(t$reserved %||% FALSE), logical(1))
  )
}


# == Structure =================================================================

#' Get the table of contents for a CFR title
#'
#' Returns the hierarchical structure (chapters, parts, sections) for a title.
#' Flattened to a tibble for easy filtering.
#'
#' @param title_number CFR title number (1-50)
#' @param date Date for the version (default: today). Format "YYYY-MM-DD".
#' @param depth How deep to flatten: "part" (default) or "section"
#' @return tibble: identifier, label, label_description, type, reserved
#' @export
ecfr_structure <- function(title_number, date = NULL, depth = "part") {
  if (is.null(date)) {
    # Use the latest available date from the titles endpoint
    titles <- ecfr_titles()
    title_row <- titles[titles$number == title_number, ]
    if (nrow(title_row) > 0 && !is.na(title_row$up_to_date_as_of[1])) {
      date <- format(title_row$up_to_date_as_of[1], "%Y-%m-%d")
    } else {
      date <- format(Sys.Date() - 1, "%Y-%m-%d")
    }
  }
  url <- sprintf("%s/versioner/v1/structure/%s/title-%d.json",
                 .ecfr_base, date, title_number)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("eCFR API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_structure)

  # Flatten the tree
  nodes <- list()
  .flatten_tree <- function(node, target_types) {
    if (node$type %in% target_types) {
      nodes[[length(nodes) + 1]] <<- list(
        identifier        = node$identifier %||% NA_character_,
        label             = node$label %||% NA_character_,
        label_description = node$label_description %||% NA_character_,
        type              = node$type %||% NA_character_,
        reserved          = as.logical(node$reserved %||% FALSE)
      )
    }
    children <- node$children
    if (!is.null(children)) {
      for (child in children) .flatten_tree(child, target_types)
    }
  }

  target <- if (depth == "section") c("part", "section") else c("chapter", "subchapter", "part")
  .flatten_tree(raw, target)

  if (length(nodes) == 0) return(.schema_structure)

  tibble(
    identifier        = vapply(nodes, `[[`, character(1), "identifier"),
    label             = vapply(nodes, `[[`, character(1), "label"),
    label_description = vapply(nodes, `[[`, character(1), "label_description"),
    type              = vapply(nodes, `[[`, character(1), "type"),
    reserved          = vapply(nodes, `[[`, logical(1), "reserved")
  )
}


# == Search ====================================================================

#' Search the eCFR full text
#'
#' Full-text search across all CFR titles with highlighted excerpts.
#'
#' @param query Search term (e.g. "clean air", "data privacy")
#' @param per_page Results per page (default 20)
#' @param page Page number (default 1)
#' @return tibble: title (integer), part (character), section (character),
#'   heading (character), excerpt (character), starts_on (Date), score (numeric)
#' @export
ecfr_search <- function(query, per_page = 20, page = 1) {
  url <- sprintf("%s/search/v1/results?query=%s&per_page=%d&page=%d",
                 .ecfr_base, utils::URLencode(query, reserved = TRUE),
                 per_page, page)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("eCFR search error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_search)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_search)

  tibble(
    title     = vapply(results, function(r) {
      as.integer(r$hierarchy$title %||% NA_integer_)
    }, integer(1)),
    part      = vapply(results, function(r) r$hierarchy$part %||% NA_character_, character(1)),
    section   = vapply(results, function(r) r$hierarchy$section %||% NA_character_, character(1)),
    heading   = vapply(results, function(r) {
      h <- r$headings
      if (is.null(h) || length(h) == 0) NA_character_
      else paste(h, collapse = " > ")
    }, character(1)),
    excerpt   = vapply(results, function(r) {
      ex <- r$full_text_excerpt %||% ""
      gsub("<[^>]+>", "", ex)  # strip HTML tags
    }, character(1)),
    starts_on = as.Date(vapply(results, function(r) r$starts_on %||% NA_character_, character(1))),
    score     = vapply(results, function(r) as.numeric(r$score %||% NA_real_), numeric(1))
  )
}


# == Versions (change history) =================================================

#' Get change history for a CFR title
#'
#' Returns a log of which sections were amended and when.
#'
#' @param title_number CFR title number (1-50)
#' @param since Only show changes since this date (Date or "YYYY-MM-DD")
#' @return tibble: date (Date), identifier, name, part, type,
#'   substantive (logical), removed (logical)
#' @export
ecfr_versions <- function(title_number, since = NULL) {
  url <- sprintf("%s/versioner/v1/versions/title-%d", .ecfr_base, title_number)
  if (!is.null(since)) {
    url <- paste0(url, "?issue_date%5Bgte%5D=", as.character(since))
  }

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("eCFR API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_versions)

  versions <- raw$content_versions
  if (is.null(versions) || length(versions) == 0) return(.schema_versions)

  tibble(
    date        = as.Date(vapply(versions, function(v) v$date %||% NA_character_, character(1))),
    identifier  = vapply(versions, function(v) v$identifier %||% NA_character_, character(1)),
    name        = vapply(versions, function(v) v$name %||% NA_character_, character(1)),
    part        = vapply(versions, function(v) v$part %||% NA_character_, character(1)),
    type        = vapply(versions, function(v) v$type %||% NA_character_, character(1)),
    substantive = vapply(versions, function(v) as.logical(v$substantive %||% NA), logical(1)),
    removed     = vapply(versions, function(v) as.logical(v$removed %||% FALSE), logical(1))
  )
}


# == Context ===================================================================

#' Generate LLM-friendly context for the eCFR package
#'
#' @return Character string (invisibly), also printed
#' @export
ecfr_context <- function() {
  .build_context("ecfr.gov", header_lines = c(
    "# ecfr.gov - Electronic Code of Federal Regulations Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# 50 CFR titles covering all federal regulations",
    "# Common titles: 40 (Environment), 21 (Food/Drugs), 26 (Tax),",
    "#   29 (Labor), 42 (Public Health), 47 (Telecom)"
  ))
}
