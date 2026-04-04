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

#' Fetch IMF indicator data
#'
#' Returns annual macroeconomic data from the IMF DataMapper API, primarily
#' sourced from the World Economic Outlook (WEO). Covers ~133 indicators for
#' 241 countries/regions. Data is returned in long format with one row per
#' country-year observation.
#'
#' @param indicator Character. Indicator code. Common codes include:
#'   \describe{
#'     \item{\code{"NGDP_RPCH"}}{Real GDP growth (annual percent change).}
#'     \item{\code{"NGDPD"}}{GDP, current prices (billions USD).}
#'     \item{\code{"NGDPDPC"}}{GDP per capita, current prices (USD).}
#'     \item{\code{"PCPIPCH"}}{Inflation, consumer prices (annual percent change).}
#'     \item{\code{"LUR"}}{Unemployment rate (percent of labor force).}
#'     \item{\code{"BCA_NGDPD"}}{Current account balance (percent of GDP).}
#'     \item{\code{"GGXWDG_NGDP"}}{Government gross debt (percent of GDP).}
#'     \item{\code{"PPPPC"}}{GDP per capita, purchasing power parity.}
#'   }
#'   Use \code{imf_indicators()} to search for more codes.
#' @param countries Optional character vector. ISO 3-letter country codes
#'   (e.g. \code{c("USA", "GBR", "DEU")}). Default \code{NULL} returns all
#'   countries. Use \code{imf_countries()} to find codes.
#' @param years Optional integer vector. Years to filter (e.g.
#'   \code{c(2020, 2021, 2022)}). Default \code{NULL} returns all years.
#' @return A tibble with columns:
#'   \describe{
#'     \item{country}{Character. ISO 3-letter country code.}
#'     \item{indicator}{Character. The requested indicator code.}
#'     \item{year}{Integer. Calendar year.}
#'     \item{value}{Numeric. The indicator value (units depend on the indicator).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # US real GDP growth 2020-2022
#' imf_data("NGDP_RPCH", countries = c("USA"), years = c(2020, 2021, 2022))
#' # => USA NGDP_RPCH 2020 -2.1, 2021 6.2, 2022 2.5
#'
#' # Inflation for G7 countries
#' imf_data("PCPIPCH", countries = c("USA","GBR","FRA","DEU","ITA","JPN","CAN"))
#'
#' # All countries, all years for unemployment
#' imf_data("LUR")
#' }
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
#' Returns all ~133 macroeconomic indicators available from the IMF DataMapper
#' API. Each indicator has a code, human-readable label, description, and unit.
#' Use the returned \code{id} with \code{imf_data()}.
#'
#' @param query Optional character. Case-insensitive search term to filter
#'   indicators by label or code. Examples: \code{"gdp"}, \code{"inflation"},
#'   \code{"unemployment"}, \code{"debt"}, \code{"trade"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Indicator code for use with \code{imf_data()} (e.g. \code{"NGDP_RPCH"}).}
#'     \item{label}{Character. Human-readable indicator name.}
#'     \item{description}{Character. Detailed description (truncated to 200 chars).}
#'     \item{unit}{Character. Unit of measurement (e.g. \code{"Annual percent change"}, \code{"Billions of U.S. dollars"}).}
#'     \item{dataset}{Character. Source dataset (e.g. \code{"WEO"} for World Economic Outlook).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # All indicators
#' imf_indicators()
#'
#' # Search for GDP-related indicators
#' imf_indicators("gdp")
#' }
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
#' Returns all ~241 countries and regions recognized by the IMF DataMapper,
#' with their ISO 3-letter codes. Use these codes as the \code{countries}
#' argument to \code{imf_data()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{iso}{Character. ISO 3-letter country code (e.g. \code{"USA"}, \code{"GBR"}, \code{"AFG"}).}
#'     \item{label}{Character. Country or region name (e.g. \code{"United States"}, \code{"Afghanistan"}).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' imf_countries()
#' # => 241 rows: ABW/Aruba, AFG/Afghanistan, AGO/Angola, ...
#' }
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

#' Get data.imf.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.imf.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.imf.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.imf.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.imf.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
