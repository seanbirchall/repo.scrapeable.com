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
#' Returns all ~68 BPL branch locations with address, coordinates, phone,
#' hours, accessibility info, and tags. Data comes from the BPL locations
#' API at \code{/api/locations/v1/map}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Branch name (e.g. "Adams Street Library", "Central Library").}
#'     \item{address}{Character. Street address (e.g. "9 Adams Street").}
#'     \item{phone}{Character. Phone number (e.g. "718.852.8089").}
#'     \item{lat}{Double. Latitude (e.g. 40.693).}
#'     \item{lon}{Double. Longitude (e.g. -73.987).}
#'     \item{accessibility}{Character. Accessibility status (e.g. "Fully accessible").}
#'     \item{branch_id}{Character. Internal branch ID (e.g. "88", "01").}
#'     \item{region}{Character. Region grouping (e.g. "ParkSlope", "NewLots", "Central").}
#'     \item{url}{Character. Relative URL path to branch page.}
#'     \item{hours}{Character. Current operating hours (HTML stripped).}
#'     \item{tags}{Character. Semicolon-separated feature tags.}
#'   }
#' @examples
#' bpl_branches()
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
#' Filters all BPL branches whose title, address, region, or tags contain
#' the query string (case-insensitive substring match). Fetches all branches
#' first, then filters client-side.
#'
#' @param query Character. Search string to match against branch title,
#'   address, region, or tags.
#'   Example: \code{"central"}, \code{"flatbush"}, \code{"accessible"}
#' @return A tibble with the same columns as \code{\link{bpl_branches}}:
#'   title, address, phone, lat, lon, accessibility, branch_id, region,
#'   url, hours, tags. Only matching rows are returned.
#' @examples
#' bpl_search("central")
#' bpl_search("flatbush")
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
#' Returns all ~165 electronic databases and online resources available
#' at BPL. Includes research databases, e-book platforms, journal
#' collections, and educational tools. Data from \code{/learning-resources/json}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Resource name (e.g. "ABC Mouse", "Academic Search Premier").}
#'     \item{description}{Character. Description with HTML stripped (e.g.
#'       "A subscription-based digital educational program...").}
#'     \item{link}{Character. URL to access the resource.}
#'   }
#' @examples
#' bpl_resources()
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
#' Filters electronic resources whose title or description contain the
#' query string (case-insensitive substring match). Fetches all resources
#' first, then filters client-side.
#'
#' @param query Character. Search string to match against title or description.
#'   Example: \code{"database"}, \code{"journal"}, \code{"children"}
#' @return A tibble with columns: title, description, link (same schema
#'   as \code{\link{bpl_resources}}).
#' @examples
#' bpl_search_resources("database")
#' bpl_search_resources("journal")
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

#' Get bklynlibrary.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bpl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bpl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bklynlibrary.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bklynlibrary.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bklynlibrary.org context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# bklynlibrary.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
