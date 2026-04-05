# data.imf.org.R - Self-contained data.imf.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# data-imf-org.R
# Self-contained IMF DataMapper API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, tidyr
# Auth: none required
# API: https://www.imf.org/external/datamapper/api/v1

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.imf_base <- "https://www.imf.org/external/datamapper/api/v1"

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

#' Fetch IMF macroeconomic indicator data
#'
#' Retrieves annual time-series data from the IMF DataMapper API, covering
#' World Economic Outlook and related datasets. Provides ~133 macroeconomic
#' indicators for up to 241 countries/regions, with data typically spanning
#' 1980 to present plus forecasts.
#'
#' @param indicator Character. Indicator code (e.g. \code{"NGDP_RPCH"} for
#'   real GDP growth, \code{"PCPIPCH"} for inflation, \code{"LUR"} for
#'   unemployment rate). Use \code{imf_indicators()} to discover codes.
#' @param countries Character vector or \code{NULL}. ISO 3-letter country
#'   codes to filter (e.g. \code{c("USA", "GBR", "DEU")}). Default
#'   \code{NULL} returns all available countries.
#' @param years Numeric vector or \code{NULL}. Specific years to retrieve
#'   (e.g. \code{2020:2025}). Default \code{NULL} returns all available years.
#' @return A tibble with columns:
#'   \describe{
#'     \item{country}{Character. ISO 3-letter country code.}
#'     \item{indicator}{Character. Indicator code (same for all rows).}
#'     \item{year}{Integer. Calendar year.}
#'     \item{value}{Numeric. Indicator value (units depend on indicator).}
#'   }
#' @examples
#' imf_data("NGDP_RPCH", countries = c("USA", "GBR"))
#' imf_data("PCPIPCH", countries = "DEU", years = 2015:2025)
#' @export
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

#' List available IMF macroeconomic indicators
#'
#' Returns the catalog of ~133 macroeconomic indicators from the IMF
#' DataMapper, including GDP measures, inflation, trade, employment, and
#' fiscal indicators. Optionally filter by keyword.
#'
#' @param query Character or \code{NULL}. Optional search term to filter
#'   indicators by name or code (case-insensitive, e.g. \code{"GDP"},
#'   \code{"inflation"}, \code{"unemployment"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Indicator code (use with \code{imf_data()}).}
#'     \item{label}{Character. Short indicator label (e.g. \code{"Real GDP growth"}).}
#'     \item{description}{Character. Longer description (truncated to 200 chars).}
#'     \item{unit}{Character. Measurement unit (e.g. \code{"Annual percent change"},
#'       \code{"Billions of U.S. dollars"}).}
#'     \item{dataset}{Character. Source dataset (e.g. \code{"WEO"}).}
#'   }
#' @examples
#' imf_indicators()
#' imf_indicators("GDP")
#' imf_indicators("unemployment")
#' @export
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

#' List IMF country and region codes
#'
#' Returns the full list of ~241 countries and aggregate regions recognized
#' by the IMF DataMapper, with their ISO codes and display labels.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{iso}{Character. ISO 3-letter country/region code (e.g.
#'       \code{"USA"}, \code{"GBR"}, \code{"OEMDC"}).}
#'     \item{label}{Character. Country or region name (e.g. \code{"United States"},
#'       \code{"Emerging market and developing economies"}).}
#'   }
#' @examples
#' imf_countries()
#' @export
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

#' Get imf.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
imf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(imf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/imf.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "imf.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# imf.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# imf.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
