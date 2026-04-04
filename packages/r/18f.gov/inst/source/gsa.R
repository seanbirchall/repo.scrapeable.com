# 18f.gov.R - Self-contained 18F / GSA government API catalog client
#
# Data source: 18F API-All-the-X project (GitHub)
# Two datasets:
#   - Individual government APIs (org -> list of APIs with name + url)
#   - Government developer hubs (org -> url)
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.apis_url <- "https://raw.githubusercontent.com/18F/API-All-the-X/master/_data/individual_apis.yml"
.hubs_url <- "https://raw.githubusercontent.com/18F/API-All-the-X/master/_data/developer_hubs.yml"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch_text <- function(url) {

  tmp <- tempfile(fileext = ".txt")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  readLines(tmp, warn = FALSE)
}

.parse_apis_yaml <- function(lines) {
  # Structure:
  # - name: Org Name
  #   apis:
  #   - name: "API Name"
  #     url: "https://..."
  orgs <- character()
  api_names <- character()
  api_urls <- character()

  current_org <- NA_character_
  i <- 1L
  n <- length(lines)

  while (i <= n) {
    line <- lines[i]
    # Match org-level "- name: ..."
    if (grepl("^- name:\\s*", line)) {
      current_org <- trimws(sub("^- name:\\s*", "", line))
      current_org <- gsub("^\"|\"$", "", current_org)
    }
    # Match api-level "  - name: ..."
    if (grepl("^  - name:\\s*", line)) {
      api_name <- trimws(sub("^  - name:\\s*", "", line))
      api_name <- gsub("^\"|\"$", "", api_name)
      api_url <- NA_character_
      # Next line should be url
      if (i + 1 <= n && grepl("^    url:\\s*", lines[i + 1])) {
        api_url <- trimws(sub("^    url:\\s*", "", lines[i + 1]))
        api_url <- gsub("^\"|\"$", "", api_url)
        i <- i + 1L
      }
      orgs <- c(orgs, current_org)
      api_names <- c(api_names, api_name)
      api_urls <- c(api_urls, api_url)
    }
    i <- i + 1L
  }

  tibble(
    organization = as.character(orgs),
    api_name     = as.character(api_names),
    api_url      = as.character(api_urls)
  )
}

.parse_hubs_yaml <- function(lines) {
  # Structure:
  # Org Name:
  #   url: http://...
  hub_names <- character()
  hub_urls  <- character()

  i <- 1L
  n <- length(lines)

  while (i <= n) {
    line <- lines[i]
    # Skip blank lines and comments
    if (nzchar(trimws(line)) && !grepl("^\\s*#", line)) {
      # Match "Org Name:" at start of line (not indented)
      if (grepl("^[A-Za-z].*:\\s*$", line)) {
        hub_name <- sub(":\\s*$", "", line)
        hub_url <- NA_character_
        # Next non-blank line should be "  url: ..."
        j <- i + 1L
        while (j <= n && !nzchar(trimws(lines[j]))) j <- j + 1L
        if (j <= n && grepl("^\\s+url:\\s*", lines[j])) {
          hub_url <- trimws(sub("^\\s+url:\\s*", "", lines[j]))
          hub_url <- gsub("^\"|\"$", "", hub_url)
        }
        hub_names <- c(hub_names, hub_name)
        hub_urls  <- c(hub_urls, hub_url)
      }
    }
    i <- i + 1L
  }

  tibble(
    organization = as.character(hub_names),
    hub_url      = as.character(hub_urls)
  )
}

# == Schemas ===================================================================

.schema_apis <- tibble(
  organization = character(),
  api_name     = character(),
  api_url      = character()
)

.schema_hubs <- tibble(
  organization = character(),
  hub_url      = character()
)

.schema_catalog <- tibble(
  name         = character(),
  type         = character(),
  url          = character(),
  organization = character()
)

# == Public functions ==========================================================

#' List all US government APIs from the 18F catalog
#'
#' Fetches and parses the individual_apis dataset from 18F's API-All-the-X
#' project. Returns a tibble with one row per API.
#'
#' @return tibble with columns: organization, api_name, api_url
#' @export
gsa_apis <- function() {
  lines <- tryCatch(.fetch_text(.apis_url), error = function(e) {
    warning("Failed to fetch APIs data: ", conditionMessage(e))
    return(character())
  })
  if (length(lines) == 0) return(.schema_apis)
  .parse_apis_yaml(lines)
}

#' List all US government developer hubs from the 18F catalog
#'
#' Fetches and parses the developer_hubs dataset from 18F's API-All-the-X
#' project. Returns a tibble with one row per developer hub.
#'
#' @return tibble with columns: organization, hub_url
#' @export
gsa_hubs <- function() {
  lines <- tryCatch(.fetch_text(.hubs_url), error = function(e) {
    warning("Failed to fetch hubs data: ", conditionMessage(e))
    return(character())
  })
  if (length(lines) == 0) return(.schema_hubs)
  .parse_hubs_yaml(lines)
}

#' Search across government APIs and developer hubs
#'
#' Performs a case-insensitive keyword search across the combined catalog of
#' APIs and developer hubs. Matches on name, organization, and URL fields.
#'
#' @param query Character string to search for (case-insensitive grep)
#' @return tibble with columns: name, type ("api" or "hub"), url, organization
#' @export
gsa_search <- function(query) {
  stopifnot(is.character(query), length(query) == 1, nzchar(query))
  catalog <- gsa_list()
  if (nrow(catalog) == 0) return(.schema_catalog)

  pattern <- query
  matches <- grepl(pattern, catalog$name, ignore.case = TRUE) |
    grepl(pattern, catalog$organization, ignore.case = TRUE) |
    grepl(pattern, catalog$url, ignore.case = TRUE)
  catalog[matches, , drop = FALSE]
}

#' Combined catalog of all government APIs and developer hubs
#'
#' Merges both datasets into a single tibble with a type column
#' distinguishing between APIs and developer hubs.
#'
#' @return tibble with columns: name, type, url, organization
#' @export
gsa_list <- function() {
  apis <- gsa_apis()
  hubs <- gsa_hubs()

  api_rows <- if (nrow(apis) > 0) {
    tibble(
      name         = apis$api_name,
      type         = "api",
      url          = apis$api_url,
      organization = apis$organization
    )
  } else {
    .schema_catalog[0, ]
  }

  hub_rows <- if (nrow(hubs) > 0) {
    tibble(
      name         = hubs$organization,
      type         = "hub",
      url          = hubs$hub_url,
      organization = hubs$organization
    )
  } else {
    .schema_catalog[0, ]
  }

  bind_rows(api_rows, hub_rows)
}

#' Return context about the 18F / GSA client functions
#'
#' Reads the source file and returns all function signatures with
#' roxygen documentation for use by LLMs or introspection tools.
#'
#' @return Character string of the full source file (invisibly), printed to console
#' @export
gsa_context <- function() {
  src <- readLines(sys.frame(environment(gsa_context))$ofile %||% {
    f <- getSrcFilename(gsa_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else "clients/18f.gov.R"
  })
  paste(src, collapse = "\n")
}
