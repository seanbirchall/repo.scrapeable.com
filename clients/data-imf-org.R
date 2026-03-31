# data-imf-org.R
# Self-contained IMF DataMapper API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, tidyr
# Auth: none required
# API: https://www.imf.org/external/datamapper/api/v1

library(dplyr, warn.conflicts = FALSE)
library(tibble)
library(tidyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.imf_base <- "https://www.imf.org/external/datamapper/api/v1"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_data <- tibble(
  country = character(), indicator = character(),
  year = integer(), value = numeric()
)

.schema_indicators <- tibble(
  id = character(), label = character(), description = character(),
  unit = character(), dataset = character()
)

.schema_countries <- tibble(
  iso = character(), label = character()
)

# == Data ======================================================================

#' Fetch IMF indicator data
#'
#' Returns annual data from the IMF World Economic Outlook and related datasets.
#' Covers ~133 macroeconomic indicators for 241 countries.
#'
#' @param indicator Indicator code (e.g. "NGDP_RPCH" for real GDP growth,
#'   "PCPIPCH" for inflation, "LUR" for unemployment).
#'   Use imf_indicators() to find codes.
#' @param countries Character vector of ISO country codes (e.g. "USA", "GBR").
#'   Default: all countries.
#' @param years Optional numeric vector of years to filter
#' @return tibble: country (character), indicator (character),
#'   year (integer), value (numeric)
imf_data <- function(indicator, countries = NULL, years = NULL) {
  if (!is.null(countries)) {
    country_str <- paste(countries, collapse = "/")
    url <- sprintf("%s/%s/%s", .imf_base, indicator, country_str)
  } else {
    url <- sprintf("%s/%s", .imf_base, indicator)
  }

  if (!is.null(years)) {
    url <- paste0(url, "?periods=", paste(years, collapse = ","))
  }

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("IMF API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_data)

  values <- raw$values
  if (is.null(values)) return(.schema_data)

  # Structure: values -> {INDICATOR} -> {COUNTRY} -> {YEAR: value}
  ind_data <- values[[indicator]]
  if (is.null(ind_data)) return(.schema_data)

  results <- lapply(names(ind_data), function(cty) {
    year_vals <- ind_data[[cty]]
    if (is.null(year_vals) || length(year_vals) == 0) return(NULL)
    tibble(
      country   = cty,
      indicator = indicator,
      year      = as.integer(names(year_vals)),
      value     = as.numeric(unlist(year_vals))
    )
  })

  result <- bind_rows(results)
  if (nrow(result) == 0) return(.schema_data)
  result |> filter(!is.na(value)) |> arrange(country, year)
}


# == Indicators ================================================================

#' List available IMF indicators
#'
#' Returns ~133 macroeconomic indicators from the DataMapper.
#'
#' @param query Optional search term to filter by name
#' @return tibble: id, label, description, unit, dataset
imf_indicators <- function(query = NULL) {
  url <- paste0(.imf_base, "/indicators")
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("IMF API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$indicators)) return(.schema_indicators)

  ind <- raw$indicators
  result <- tibble(
    id          = names(ind),
    label       = vapply(ind, function(i) i$label %||% NA_character_, character(1)),
    description = vapply(ind, function(i) {
      d <- i$description %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    unit        = vapply(ind, function(i) i$unit %||% NA_character_, character(1)),
    dataset     = vapply(ind, function(i) i$dataset %||% NA_character_, character(1))
  )

  if (!is.null(query)) {
    pattern <- tolower(query)
    result <- result |> filter(grepl(pattern, tolower(label)) | grepl(pattern, tolower(id)))
  }
  result
}


# == Countries =================================================================

#' List IMF country codes
#'
#' Returns ~241 countries and regions with ISO codes.
#'
#' @return tibble: iso (character), label (character)
imf_countries <- function() {
  url <- paste0(.imf_base, "/countries")
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("IMF API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || is.null(raw$countries)) return(.schema_countries)

  ctys <- raw$countries
  tibble(
    iso   = names(ctys),
    label = vapply(ctys, function(c) c$label %||% NA_character_, character(1))
  )
}


# == Context ===================================================================

#' Generate LLM-friendly context for the IMF package
#'
#' @return Character string (invisibly), also printed
imf_context <- function() {
  .build_context("data.imf.org", header_lines = c(
    "# data.imf.org - IMF DataMapper API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble, tidyr",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# ~133 WEO macroeconomic indicators, 241 countries",
    "#",
    "# Common indicators:",
    "#   NGDP_RPCH = Real GDP growth (%)",
    "#   PCPIPCH = Inflation (CPI %)",
    "#   LUR = Unemployment rate (%)",
    "#   BCA_NGDPD = Current account (% GDP)",
    "#   GGXWDG_NGDP = Gov debt (% GDP)",
    "#",
    "# Country codes: ISO3 (USA, GBR, DEU, JPN, CHN)"
  ))
}
