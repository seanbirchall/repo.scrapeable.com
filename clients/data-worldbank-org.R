# data-worldbank-org.R
# Self-contained World Bank Data API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://api.worldbank.org/v2

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.wb_base <- "https://api.worldbank.org/v2"

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

# WB API returns [metadata, data] array
.fetch_wb <- function(url) {
  raw <- jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)
  if (length(raw) < 2) return(list(meta = NULL, data = list()))
  list(meta = raw[[1]], data = raw[[2]])
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_data <- tibble(
  country_iso3 = character(), country = character(),
  indicator_id = character(), indicator = character(),
  date = character(), value = numeric()
)

.schema_countries <- tibble(
  iso3 = character(), iso2 = character(), name = character(),
  region = character(), income_level = character(),
  capital = character(), longitude = numeric(), latitude = numeric()
)

.schema_indicators <- tibble(
  id = character(), name = character(), unit = character(),
  source = character(), source_note = character()
)

# == Data ======================================================================

#' Fetch World Bank indicator data
#'
#' Returns time series data for one indicator across one or more countries.
#' Supports ~29,500 indicators and 296 countries/regions.
#'
#' @param indicator Indicator code (e.g. "NY.GDP.MKTP.CD" for GDP,
#'   "SP.POP.TOTL" for population, "FP.CPI.TOTL.ZG" for inflation).
#'   Use wb_indicators() to find codes.
#' @param country ISO2 country code(s): "US", "GB", c("US","GB","DE").
#'   Use "all" for all countries. Default "all".
#' @param date_range Year range: "2000:2023" or single year "2023".
#'   Default: all available years.
#' @param per_page Results per page (default 1000)
#' @return tibble: country_iso3, country, indicator_id, indicator,
#'   date (character year), value (numeric)
wb_data <- function(indicator, country = "all", date_range = NULL,
                    per_page = 1000) {
  country_str <- paste(country, collapse = ";")
  url <- sprintf("%s/country/%s/indicator/%s?format=json&per_page=%d",
                 .wb_base, country_str, indicator, per_page)
  if (!is.null(date_range)) url <- paste0(url, "&date=", date_range)

  result <- .fetch_wb(url)
  if (is.null(result$data) || length(result$data) == 0) return(.schema_data)

  # Check total pages and fetch remaining
  total_pages <- as.integer(result$meta$pages %||% 1)
  all_data <- result$data

  if (total_pages > 1) {
    for (pg in 2:min(total_pages, 20)) {
      pg_url <- paste0(url, "&page=", pg)
      pg_result <- tryCatch(.fetch_wb(pg_url), error = function(e) NULL)
      if (!is.null(pg_result$data)) all_data <- c(all_data, pg_result$data)
    }
  }

  tibble(
    country_iso3 = vapply(all_data, function(d) d$countryiso3code %||% NA_character_, character(1)),
    country      = vapply(all_data, function(d) d$country$value %||% NA_character_, character(1)),
    indicator_id = vapply(all_data, function(d) d$indicator$id %||% NA_character_, character(1)),
    indicator    = vapply(all_data, function(d) d$indicator$value %||% NA_character_, character(1)),
    date         = vapply(all_data, function(d) d$date %||% NA_character_, character(1)),
    value        = vapply(all_data, function(d) as.numeric(d$value %||% NA_real_), numeric(1))
  ) |>
    filter(!is.na(value)) |>
    arrange(country_iso3, date)
}


# == Countries =================================================================

#' List World Bank countries and regions
#'
#' @param per_page Results per page (default 300)
#' @return tibble: iso3, iso2, name, region, income_level, capital,
#'   longitude, latitude
wb_countries <- function(per_page = 300) {
  url <- sprintf("%s/country?format=json&per_page=%d", .wb_base, per_page)
  result <- .fetch_wb(url)
  if (is.null(result$data) || length(result$data) == 0) return(.schema_countries)

  tibble(
    iso3         = vapply(result$data, function(c) c$id %||% NA_character_, character(1)),
    iso2         = vapply(result$data, function(c) c$iso2Code %||% NA_character_, character(1)),
    name         = vapply(result$data, function(c) c$name %||% NA_character_, character(1)),
    region       = vapply(result$data, function(c) c$region$value %||% NA_character_, character(1)),
    income_level = vapply(result$data, function(c) c$incomeLevel$value %||% NA_character_, character(1)),
    capital      = vapply(result$data, function(c) c$capitalCity %||% NA_character_, character(1)),
    longitude    = vapply(result$data, function(c) as.numeric(c$longitude %||% NA_real_), numeric(1)),
    latitude     = vapply(result$data, function(c) as.numeric(c$latitude %||% NA_real_), numeric(1))
  )
}


# == Indicators ================================================================

#' Search World Bank indicators
#'
#' @param query Search term (e.g. "GDP", "population", "inflation")
#' @param per_page Results per page (default 100)
#' @param page Page number (default 1)
#' @return tibble: id, name, unit, source, source_note
wb_indicators <- function(query = NULL, per_page = 1000, page = 1) {
  url <- sprintf("%s/indicator?format=json&per_page=%d&page=%d",
                 .wb_base, per_page, page)

  result <- .fetch_wb(url)
  if (is.null(result$data) || length(result$data) == 0) return(.schema_indicators)

  ind <- tibble(
    id          = vapply(result$data, function(i) i$id %||% NA_character_, character(1)),
    name        = vapply(result$data, function(i) i$name %||% NA_character_, character(1)),
    unit        = vapply(result$data, function(i) i$unit %||% NA_character_, character(1)),
    source      = vapply(result$data, function(i) i$source$value %||% NA_character_, character(1)),
    source_note = vapply(result$data, function(i) {
      n <- i$sourceNote %||% ""
      if (nchar(n) > 200) paste0(substr(n, 1, 200), "...") else n
    }, character(1))
  )

  if (!is.null(query)) {
    pattern <- tolower(query)
    ind <- ind |> filter(grepl(pattern, tolower(name)) | grepl(pattern, tolower(id)))
  }
  ind
}


# == Context ===================================================================

#' Generate LLM-friendly context for the World Bank package
#'
#' @return Character string (invisibly), also printed
wb_context <- function() {
  .build_context("data.worldbank.org", header_lines = c(
    "# data.worldbank.org - World Bank Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# 29,500+ indicators, 296 countries/regions",
    "#",
    "# Common indicators:",
    "#   NY.GDP.MKTP.CD = GDP (current US$)",
    "#   SP.POP.TOTL = Population",
    "#   FP.CPI.TOTL.ZG = Inflation (CPI %)",
    "#   SL.UEM.TOTL.ZS = Unemployment (%)",
    "#   NY.GDP.PCAP.CD = GDP per capita",
    "#",
    "# Country codes: ISO2 (US, GB, DE, JP, CN)"
  ))
}
