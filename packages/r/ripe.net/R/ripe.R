
# == Announced Prefixes ========================================================

#' Get announced IP prefixes for an ASN
#'
#' Queries the RIPE Stat API for IP prefixes announced by an ASN.
#'
#' @param resource ASN number (e.g. "AS3333" or just "3333") or IP prefix
#' @return tibble: prefix, timelines
#' @export
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
#' @export
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
#' @export
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
#' @export
ripe_context <- function() {
  .build_context("ripe.net", header_lines = c(
    "# ripe.net - RIPE Stat Network Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: be respectful, no documented limit",
    "# All functions return tibbles with typed columns."
  ))
}
