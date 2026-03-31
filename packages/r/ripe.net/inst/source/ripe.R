# ripe-net.R
# Self-contained RIPE Stat API client for network data.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ripe_base <- "https://stat.ripe.net/data"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
  cat(out, "\n"); invisible(out)
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

.schema_prefixes <- tibble(
  prefix = character(), timelines = character()
)

.schema_country_resources <- tibble(
  resource = character(), type = character(), status = character()
)

.schema_whois <- tibble(
  key = character(), value = character()
)

.schema_geoloc <- tibble(
  prefix = character(), latitude = numeric(), longitude = numeric(),
  city = character(), country = character()
)

# == Announced Prefixes ========================================================

#' Get announced IP prefixes for an ASN
#'
#' Queries the RIPE Stat API for IP prefixes announced by an ASN.
#'
#' @param resource ASN number (e.g. "AS3333" or just "3333") or IP prefix
#' @return tibble: prefix, timelines
ripe_prefixes <- function(resource) {
  url <- sprintf("%s/announced-prefixes/data.json?resource=%s&sourceapp=%s",
                 .ripe_base, utils::URLencode(resource), .ua)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("RIPE prefixes failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_prefixes)

  prefixes <- raw$data$prefixes
  if (is.null(prefixes) || length(prefixes) == 0) return(.schema_prefixes)

  tibble(
    prefix    = as.character(prefixes$prefix %||% NA_character_),
    timelines = vapply(prefixes$timelines, function(t) {
      if (is.null(t) || length(t) == 0) return(NA_character_)
      paste(sprintf("%s-%s", t$starttime %||% "?", t$endtime %||% "?"), collapse = "; ")
    }, character(1))
  )
}

# == Country Resources =========================================================

#' Get internet resources for a country
#'
#' @param country Two-letter country code (e.g. "NL", "US", "DE")
#' @return tibble: resource, type, status
ripe_country_resources <- function(country) {
  url <- sprintf("%s/country-resource-list/data.json?resource=%s&sourceapp=%s",
                 .ripe_base, utils::URLencode(toupper(country)), .ua)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("RIPE country resources failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_country_resources)

  resources <- raw$data$resources
  if (is.null(resources)) return(.schema_country_resources)

  rows <- list()
  for (rtype in names(resources)) {
    vals <- resources[[rtype]]
    if (length(vals) > 0) {
      rows[[length(rows) + 1]] <- tibble(
        resource = as.character(vals),
        type     = rtype,
        status   = "active"
      )
    }
  }
  if (length(rows) == 0) return(.schema_country_resources)
  bind_rows(rows)
}

# == WHOIS =====================================================================

#' WHOIS lookup via RIPE Stat
#'
#' @param resource IP address, prefix, or ASN (e.g. "193.0.6.139", "AS3333")
#' @return tibble: key, value
ripe_whois <- function(resource) {
  url <- sprintf("%s/whois/data.json?resource=%s&sourceapp=%s",
                 .ripe_base, utils::URLencode(resource), .ua)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("RIPE whois failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_whois)

  records <- raw$data$records
  if (is.null(records) || length(records) == 0) return(.schema_whois)

  rows <- list()
  for (rec_group in records) {
    if (is.data.frame(rec_group)) {
      rows[[length(rows) + 1]] <- tibble(
        key   = as.character(rec_group$key %||% NA_character_),
        value = as.character(rec_group$value %||% NA_character_)
      )
    }
  }
  if (length(rows) == 0) return(.schema_whois)
  bind_rows(rows)
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the ripe.net package
#'
#' @return Character string (invisibly), also printed
ripe_context <- function() {
  .build_context("ripe.net", header_lines = c(
    "# ripe.net - RIPE Stat Network Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: be respectful, no documented limit",
    "# All functions return tibbles with typed columns."
  ))
}
