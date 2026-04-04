# bklynlibrary.org.R - Brooklyn Public Library API client
# Self-contained. All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Endpoints:
#   - /api/locations/v1/map  (branch locations)
#   - /learning-resources/json (electronic resources)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bpl_base <- "https://www.bklynlibrary.org"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.clean_html <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- trimws(x)
  x
}

# == Schemas ===================================================================

.schema_branches <- tibble(

  title = character(), address = character(), phone = character(),
  lat = double(), lon = double(), accessibility = character(),
  branch_id = character(), region = character(), url = character(),
  hours = character(), tags = character()
)

.schema_resources <- tibble(
  title = character(), description = character(), link = character()
)

# == Public functions ==========================================================

#' List Brooklyn Public Library branch locations
#'
#' Returns all BPL branch locations with address, coordinates, phone,
#' hours, accessibility info, and tags.
#'
#' @return tibble: title, address, phone, lat, lon, accessibility,
#'   branch_id, region, url, hours, tags
#' @export
bpl_branches <- function() {
  url <- paste0(.bpl_base, "/api/locations/v1/map")
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_branches)

  df <- as_tibble(raw)

  # Parse lat/lon from "lat, lon" position string
  coords <- strsplit(df$position %||% rep(NA_character_, nrow(df)), ",\\s*")
  lats <- vapply(coords, function(x) {
    if (is.null(x) || length(x) < 2) NA_real_ else as.double(x[1])
  }, double(1))
  lons <- vapply(coords, function(x) {
    if (is.null(x) || length(x) < 2) NA_real_ else as.double(x[2])
  }, double(1))

  tibble(
    title     = as.character(df$title %||% NA),
    address   = as.character(df$address %||% NA),
    phone     = as.character(df$phone %||% NA),
    lat       = lats,
    lon       = lons,
    accessibility = as.character(df$access %||% NA),
    branch_id = as.character(df$branchid %||% NA),
    region    = as.character(df$region %||% NA),
    url       = as.character(df$path %||% NA),
    hours     = vapply(df$hours %||% rep(NA_character_, nrow(df)),
                       .clean_html, character(1)),
    tags      = as.character(df$tags %||% NA)
  )
}

#' Search Brooklyn Public Library branches by keyword
#'
#' Filters branches whose title, address, region, or tags contain the query.
#'
#' @param query Character string to search for (case-insensitive)
#' @return tibble: same columns as bpl_branches()
#' @export
bpl_search <- function(query) {
  branches <- bpl_branches()
  if (nrow(branches) == 0) return(branches)
  q <- tolower(query)
  matches <- grepl(q, tolower(branches$title), fixed = TRUE) |
    grepl(q, tolower(branches$address), fixed = TRUE) |
    grepl(q, tolower(branches$region), fixed = TRUE) |
    grepl(q, tolower(branches$tags), fixed = TRUE)
  branches[matches, ]
}

#' List Brooklyn Public Library electronic resources
#'
#' Returns all electronic databases available at BPL with
#' title, description, and access link.
#'
#' @return tibble: title, description, link
#' @export
bpl_resources <- function() {
  url <- paste0(.bpl_base, "/learning-resources/json")
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_resources)

  df <- as_tibble(raw)
  tibble(
    title       = as.character(df$title %||% NA),
    description = vapply(df$description %||% rep(NA_character_, nrow(df)),
                         .clean_html, character(1)),
    link        = as.character(df$link %||% NA)
  )
}

#' Search Brooklyn Public Library electronic resources
#'
#' Filters electronic resources by keyword in title or description.
#'
#' @param query Character string to search for (case-insensitive)
#' @return tibble: title, description, link
#' @export
bpl_search_resources <- function(query) {
  resources <- bpl_resources()
  if (nrow(resources) == 0) return(resources)
  q <- tolower(query)
  matches <- grepl(q, tolower(resources$title), fixed = TRUE) |
    grepl(q, tolower(resources$description), fixed = TRUE)
  resources[matches, ]
}

# == Context ===================================================================

#' Generate LLM-friendly context for bklynlibrary.org
#'
#' @return Character string with full function signatures and bodies
#' @export
bpl_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/bklynlibrary.org.R"
  if (!file.exists(src_file)) {
    cat("# bklynlibrary.org context - source not found\n")
    return(invisible("# bklynlibrary.org context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
