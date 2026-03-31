# api-osf-io.R
# Self-contained Open Science Framework (OSF) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public data
# Rate limits: none documented for read-only access

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.osf_base <- "https://api.osf.io/v2"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
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

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_preprints <- tibble(
  id = character(), title = character(), description = character(),
  date_created = as.POSIXct(character()), date_published = as.POSIXct(character()),
  doi = character(), provider = character()
)

.schema_nodes <- tibble(
  id = character(), title = character(), description = character(),
  category = character(), date_created = as.POSIXct(character()),
  date_modified = as.POSIXct(character()), public = logical(),
  tags = character()
)


# == Preprints =================================================================

#' Fetch preprints from the Open Science Framework
#'
#' Returns recent preprints, optionally filtered by provider.
#' Uses the JSON:API format from OSF v2 API.
#'
#' @param provider Preprint provider filter (default "osf"). Other values:
#'   "socarxiv", "psyarxiv", "engrxiv", "biohackrxiv"
#' @param page_size Number of results per page (default 25, max 100)
#' @return tibble: id, title, description, date_created, date_published,
#'   doi, provider
osf_preprints <- function(provider = "osf", page_size = 25) {
  url <- sprintf("%s/preprints/?filter[provider]=%s&page[size]=%d",
                 .osf_base, provider, page_size)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_preprints)

  rows <- lapply(data, function(item) {
    a <- item$attributes
    tibble(
      id             = as.character(item$id %||% NA),
      title          = as.character(a$title %||% NA),
      description    = as.character(a$description %||% NA),
      date_created   = as.POSIXct(a$date_created %||% NA,
                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date_published = as.POSIXct(a$date_published %||% NA,
                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      doi            = as.character(a$doi %||% NA),
      provider       = provider
    )
  })
  bind_rows(rows)
}


# == Nodes (projects) ==========================================================

#' Search OSF nodes (projects and components)
#'
#' Returns public OSF nodes matching a title filter.
#'
#' @param query Title search filter
#' @param page_size Number of results per page (default 25, max 100)
#' @return tibble: id, title, description, category, date_created,
#'   date_modified, public, tags
osf_nodes <- function(query, page_size = 25) {
  url <- sprintf("%s/nodes/?filter[title]=%s&page[size]=%d",
                 .osf_base, utils::URLencode(query), page_size)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_nodes)

  rows <- lapply(data, function(item) {
    a <- item$attributes
    tags_str <- paste(unlist(a$tags %||% list()), collapse = ", ")
    tibble(
      id            = as.character(item$id %||% NA),
      title         = as.character(a$title %||% NA),
      description   = as.character(a$description %||% NA),
      category      = as.character(a$category %||% NA),
      date_created  = as.POSIXct(a$date_created %||% NA,
                                  format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date_modified = as.POSIXct(a$date_modified %||% NA,
                                  format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      public        = as.logical(a$public %||% NA),
      tags          = tags_str
    )
  })
  bind_rows(rows)
}


# == Context ===================================================================

#' Show OSF package context for LLM integration
#'
#' Prints a summary of all public functions, their signatures, and
#' roxygen documentation. Designed for LLM context injection.
#'
#' @return Invisibly returns the context string
#' @export
osf_context <- function() {
  header <- c(
    "# api.osf.io - Open Science Framework API Client",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required for public data",
    "# Rate limits: none documented for read-only",
    "#",
    "# Preprint providers: osf, socarxiv, psyarxiv, engrxiv, biohackrxiv",
    "# Node categories: project, component, data, analysis"
  )
  .build_context("api.osf.io", header_lines = header)
}
