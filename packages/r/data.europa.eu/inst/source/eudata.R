# data-europa-eu.R
# Self-contained EU Open Data Portal API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.eudata_base <- "https://data.europa.eu/api/hub/search"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_search <- tibble(
  id = character(), title = character(), description = character(),
  country = character(), catalog = character(), modified = character(),
  format = character()
)

.schema_dataset <- tibble(
  id = character(), title = character(), description = character(),
  country = character(), catalog = character(), publisher = character(),
  modified = character(), issued = character(), format = character(),
  resource_url = character()
)

# == Parsers ===================================================================

.safe_text <- function(x, lang = "en") {
  if (is.null(x)) return(NA_character_)
  if (is.character(x)) return(x)
  if (is.list(x)) {
    val <- x[[lang]] %||% x[[1]] %||% NA_character_
    return(as.character(val))
  }
  as.character(x)
}

# == Public functions ==========================================================

#' Search the EU Open Data Portal
#'
#' @param query Search term (e.g. "climate", "transport", "energy")
#' @param limit Number of results (default 10, max 100)
#' @param page Page number (default 0, zero-indexed)
#' @return tibble: id, title, description, country, catalog, modified, format
#' @export
eudata_search <- function(query, limit = 10, page = 0) {
  url <- sprintf("%s/search?q=%s&limit=%d&page=%d",
                 .eudata_base,
                 utils::URLencode(query, reserved = TRUE),
                 as.integer(limit), as.integer(page))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$result) || is.null(raw$result$results) ||
      length(raw$result$results) == 0) {
    return(.schema_search)
  }

  rows <- lapply(raw$result$results, function(x) {
    formats <- tryCatch({
      dists <- x$distributions
      if (!is.null(dists) && length(dists) > 0) {
        fmts <- vapply(dists, function(d) as.character(d$format$label %||% d$mediaType %||% ""), character(1))
        paste(unique(fmts[fmts != ""]), collapse = ", ")
      } else NA_character_
    }, error = function(e) NA_character_)

    tibble(
      id          = as.character(x$identifier[[1]] %||% NA),
      title       = .safe_text(x$title),
      description = .safe_text(x$description),
      country     = as.character(x$country$label %||% NA),
      catalog     = as.character(x$catalog$title[[1]] %||% NA),
      modified    = as.character(x$modified %||% NA),
      format      = formats
    )
  })
  bind_rows(rows)
}

#' Get details for a specific EU dataset
#'
#' @param id Dataset identifier (e.g. "groenklimaatassen-gent")
#' @return tibble with one row: id, title, description, country, catalog,
#'   publisher, modified, issued, format, resource_url
#' @export
eudata_dataset <- function(id) {
  url <- sprintf("%s/datasets/%s", .eudata_base, id)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$result)) return(.schema_dataset)

  x <- raw$result
  formats <- tryCatch({
    dists <- x$distributions
    if (!is.null(dists) && length(dists) > 0) {
      fmts <- vapply(dists, function(d) as.character(d$format$label %||% d$mediaType %||% ""), character(1))
      paste(unique(fmts[fmts != ""]), collapse = ", ")
    } else NA_character_
  }, error = function(e) NA_character_)

  resource_url <- tryCatch({
    dists <- x$distributions
    if (!is.null(dists) && length(dists) > 0) {
      as.character(dists[[1]]$accessUrl %||% dists[[1]]$downloadUrl %||% NA)
    } else NA_character_
  }, error = function(e) NA_character_)

  tibble(
    id           = as.character(x$identifier[[1]] %||% NA),
    title        = .safe_text(x$title),
    description  = .safe_text(x$description),
    country      = as.character(x$country$label %||% NA),
    catalog      = as.character(x$catalog$title[[1]] %||% NA),
    publisher    = as.character(x$catalog$publisher$name %||% NA),
    modified     = as.character(x$modified %||% NA),
    issued       = as.character(x$issued %||% NA),
    format       = formats,
    resource_url = resource_url
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for the data.europa.eu package
#'
#' @return Character string (invisibly), also printed
#' @export
eudata_context <- function() {
  .build_context("data.europa.eu", header_lines = c(
    "# data.europa.eu - EU Open Data Portal Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limit: none documented",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Search topics: climate, energy, transport, health, agriculture,",
    "#   environment, economy, education, science, population"
  ))
}
