# metmuseum-org.R
# Self-contained Metropolitan Museum of Art Collection API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: 80 requests per second

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.met_base <- "https://collectionapi.metmuseum.org/public/collection/v1"

`%||%` <- function(x, y) if (is.null(x)) y else x

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
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
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_search <- tibble(
  object_id = integer(), total = integer()
)

.schema_object <- tibble(
  object_id = integer(), title = character(), artist = character(),
  date = character(), medium = character(), department = character(),
  culture = character(), period = character(), is_public_domain = logical(),
  primary_image = character(), accession_number = character()
)

.schema_departments <- tibble(
  department_id = integer(), display_name = character()
)

# == Public functions ==========================================================

#' Search the Met Museum collection
#'
#' Returns object IDs matching the query. Use met_object() to get details
#' for individual objects.
#'
#' @param query Search term
#' @param department_id Optional department ID filter
#' @param is_highlight Filter to highlighted objects only (logical)
#' @param has_images Filter to objects with images (logical)
#' @return tibble: object_id (integer), total (integer, repeated for convenience)
met_search <- function(query, department_id = NULL, is_highlight = NULL,
                       has_images = NULL) {
  url <- sprintf("%s/search?q=%s", .met_base, utils::URLencode(query))
  if (!is.null(department_id)) url <- paste0(url, "&departmentId=", department_id)
  if (isTRUE(is_highlight)) url <- paste0(url, "&isHighlight=true")
  if (isTRUE(has_images)) url <- paste0(url, "&hasImages=true")

  raw <- .fetch_json(url)
  ids <- raw$objectIDs
  if (is.null(ids) || length(ids) == 0) return(.schema_search)

  tibble(
    object_id = as.integer(ids),
    total = as.integer(raw$total %||% length(ids))
  )
}

#' Fetch a single Met Museum object
#'
#' @param id Object ID (integer)
#' @return tibble: single row with object_id, title, artist, date, medium,
#'   department, culture, period, is_public_domain, primary_image, accession_number
met_object <- function(id) {
  url <- sprintf("%s/objects/%d", .met_base, as.integer(id))
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$objectID)) return(.schema_object)

  tibble(
    object_id = as.integer(raw$objectID),
    title = as.character(raw$title %||% NA),
    artist = as.character(raw$artistDisplayName %||% NA),
    date = as.character(raw$objectDate %||% NA),
    medium = as.character(raw$medium %||% NA),
    department = as.character(raw$department %||% NA),
    culture = as.character(raw$culture %||% NA),
    period = as.character(raw$period %||% NA),
    is_public_domain = as.logical(raw$isPublicDomain %||% NA),
    primary_image = as.character(raw$primaryImage %||% NA),
    accession_number = as.character(raw$accessionNumber %||% NA)
  )
}

#' List Met Museum departments
#'
#' @return tibble: department_id (integer), display_name (character)
met_departments <- function() {
  url <- sprintf("%s/departments", .met_base)
  raw <- .fetch_json(url)
  depts <- raw$departments
  if (is.null(depts) || length(depts) == 0) return(.schema_departments)

  as_tibble(depts) |>
    transmute(
      department_id = as.integer(departmentId),
      display_name = as.character(displayName)
    )
}

#' Met Museum package context for LLM integration
#'
#' @return Invisibly returns the context string
met_context <- function() {
  .build_context("metmuseum.org", header_lines = c(
    "# metmuseum.org - Metropolitan Museum of Art Collection API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: 80 requests per second",
    "# Note: met_search returns object IDs; use met_object(id) for details",
    "# Departments: American Decorative Arts (1), Asian Art (6), European Paintings (11), etc."
  ))
}
