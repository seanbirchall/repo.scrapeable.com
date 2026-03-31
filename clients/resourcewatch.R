# resourcewatch.R
# Self-contained Resource Watch API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be respectful

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rw_base <- "https://api.resourcewatch.org/v1"

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

.schema_datasets <- tibble(
  id = character(), name = character(), subtitle = character(),
  provider = character(), connectorType = character(),
  published = logical(), env = character()
)

.schema_dataset_detail <- tibble(
  id = character(), name = character(), subtitle = character(),
  description = character(), provider = character(),
  connectorType = character(), tableName = character(),
  published = logical()
)

# == Dataset listing ===========================================================

#' List Resource Watch datasets
#'
#' @param page_size Number of results (default 10)
#' @param page Page number (default 1)
#' @return tibble: id, name, subtitle, provider, connectorType, published, env
rw_datasets <- function(page_size = 10, page = 1) {
  url <- paste0(.rw_base, "/dataset?page[size]=", page_size, "&page[number]=", page)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_datasets)

  attrs <- d$attributes
  if (is.null(attrs)) return(.schema_datasets)

  tibble(
    id = as.character(d$id),
    name = as.character(attrs$name %||% NA_character_),
    subtitle = as.character(if ("subtitle" %in% names(attrs)) attrs$subtitle else NA_character_),
    provider = as.character(if ("provider" %in% names(attrs)) attrs$provider else NA_character_),
    connectorType = as.character(if ("connectorType" %in% names(attrs)) attrs$connectorType else NA_character_),
    published = as.logical(if ("published" %in% names(attrs)) attrs$published else NA),
    env = as.character(if ("env" %in% names(attrs)) attrs$env else NA_character_)
  )
}

# == Dataset detail ============================================================

#' Fetch a single Resource Watch dataset by ID
#'
#' @param id Dataset ID string
#' @return tibble: one row with id, name, subtitle, description, provider,
#'   connectorType, tableName, published
rw_dataset <- function(id) {
  url <- paste0(.rw_base, "/dataset/", id)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || is.null(d$id)) return(.schema_dataset_detail)
  a <- d$attributes

  tibble(
    id = as.character(d$id),
    name = as.character(a$name %||% NA_character_),
    subtitle = as.character(a$subtitle %||% NA_character_),
    description = as.character(a$description %||% NA_character_),
    provider = as.character(a$provider %||% NA_character_),
    connectorType = as.character(a$connectorType %||% NA_character_),
    tableName = as.character(a$tableName %||% NA_character_),
    published = as.logical(a$published %||% NA)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the Resource Watch package
#'
#' @return Character string (invisibly), also printed
rw_context <- function() {
  .build_context("resourcewatch.org", header_lines = c(
    "# resourcewatch.org - Resource Watch API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Resource Watch provides environmental and social datasets.",
    "# Datasets can be from various providers (cartodb, gee, etc)."
  ))
}
