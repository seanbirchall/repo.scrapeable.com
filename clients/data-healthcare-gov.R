# data-healthcare-gov.R
# Self-contained data.healthcare.gov (DKAN/CKAN) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required.
# Rate limits: none documented — be polite.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hcgov_base <- "https://data.healthcare.gov/api/1"

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

.fetch_json <- function(url, simplify = TRUE) jsonlite::fromJSON(.fetch(url), simplifyVector = simplify)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_datasets <- tibble(
  identifier = character(), title = character(), description = character(),
  modified = character(), access_level = character(), keyword = character()
)

.schema_dataset_detail <- tibble(
  identifier = character(), title = character(), description = character(),
  modified = character(), access_level = character(),
  publisher_name = character(), contact_name = character(),
  contact_email = character(), distribution_count = integer()
)

# == Datasets ==================================================================

#' List available datasets on data.healthcare.gov
#'
#' Queries the DKAN metastore for published dataset metadata.
#'
#' @param page_size Number of results per page (default 20)
#' @param page Page number (default 1)
#' @return tibble: identifier, title, description, modified, access_level, keyword
hcgov_datasets <- function(page_size = 20, page = 1) {
  url <- sprintf("%s/metastore/schemas/dataset/items?show-reference-ids&page=%d&page-size=%d",
                 .hcgov_base, page, page_size)

  raw <- tryCatch(.fetch_json(url, simplify = FALSE), error = function(e) {
    warning("Healthcare.gov datasets query failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_datasets)

  rows <- lapply(raw, function(d) {
    kw <- d$keyword
    kw_str <- if (is.list(kw)) {
      paste(vapply(kw, function(k) k$data %||% "", character(1)), collapse = ", ")
    } else if (is.character(kw)) {
      paste(kw, collapse = ", ")
    } else {
      NA_character_
    }
    tibble(
      identifier   = as.character(d$identifier %||% NA_character_),
      title        = as.character(d$title %||% NA_character_),
      description  = as.character(d$description %||% NA_character_),
      modified     = as.character(d$modified %||% NA_character_),
      access_level = as.character(d$accessLevel %||% NA_character_),
      keyword      = kw_str
    )
  })
  bind_rows(rows)
}

# == Dataset detail ============================================================

#' Get detailed info about a specific dataset
#'
#' @param id Dataset identifier (e.g. "e4rr-zk4i")
#' @return tibble: identifier, title, description, modified, access_level,
#'   publisher_name, contact_name, contact_email, distribution_count
hcgov_dataset <- function(id) {
  url <- sprintf("%s/metastore/schemas/dataset/items/%s?show-reference-ids",
                 .hcgov_base, utils::URLencode(id))

  raw <- tryCatch(.fetch_json(url, simplify = FALSE), error = function(e) {
    warning("Healthcare.gov dataset fetch failed for '", id, "': ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_dataset_detail)

  pub <- raw$publisher
  pub_name <- if (is.list(pub) && !is.null(pub$data)) {
    pub$data$name %||% NA_character_
  } else NA_character_

  cp <- raw$contactPoint
  dist <- raw$distribution
  dist_n <- if (is.list(dist)) length(dist) else 0L


  tibble(
    identifier         = as.character(raw$identifier %||% NA_character_),
    title              = as.character(raw$title %||% NA_character_),
    description        = as.character(raw$description %||% NA_character_),
    modified           = as.character(raw$modified %||% NA_character_),
    access_level       = as.character(raw$accessLevel %||% NA_character_),
    publisher_name     = as.character(pub_name),
    contact_name       = as.character(cp$fn %||% NA_character_),
    contact_email      = as.character(cp$hasEmail %||% NA_character_),
    distribution_count = as.integer(dist_n)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the data.healthcare.gov package
#'
#' Prints package overview, function signatures and roxygen docs.
#' Intended for injection into LLM prompts.
#'
#' @return Character string (invisibly), also printed
hcgov_context <- function() {
  .build_context("data.healthcare.gov", header_lines = c(
    "# data.healthcare.gov - Healthcare Open Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Data: CMS datasets on insurance, quality, marketplace",
    "# All functions return tibbles with typed columns."
  ))
}
