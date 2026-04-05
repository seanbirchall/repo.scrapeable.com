# openholidaysapi.org.R - Self-contained openholidaysapi.org client



# openholidaysapi-org.R
# Self-contained Open Holidays API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.holiday_base <- "https://openholidaysapi.org"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- Name extraction helper ----------------------------------------------------

.extract_name <- function(name_list, lang = "EN") {
  if (is.null(name_list) || length(name_list) == 0) return(NA_character_)
  for (item in name_list) {
    if (identical(item$language, lang)) return(as.character(item$text))
  }
  as.character(name_list[[1]]$text)
}

# == Schemas ===================================================================

.schema_holidays <- tibble(
  id = character(), start_date = as.Date(character()),
  end_date = as.Date(character()), type = character(),
  name = character(), nationwide = logical()
)

.schema_countries <- tibble(
  iso_code = character(), name = character(),
  official_languages = character()
)


# == Public Holidays ===========================================================

#' Fetch public holidays for a country and year
#'
#' Queries the Open Holidays API for official public (statutory) holidays
#' in a given country and calendar year. Returns one row per holiday with
#' start/end dates, holiday type, localised name, and whether the holiday
#' applies nationwide or only to specific subdivisions.
#'
#' @param country ISO 3166-1 alpha-2 country code (e.g. \code{"US"},
#'   \code{"DE"}, \code{"FR"}). Use \code{\link{holiday_countries}} to
#'   list supported codes.
#' @param year Four-digit year as character or numeric (default: current year).
#' @param language ISO 639-1 language code for holiday names (default
#'   \code{"EN"}). Supported languages depend on the country.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Unique holiday identifier (character).}
#'     \item{start_date}{First day of the holiday (Date).}
#'     \item{end_date}{Last day of the holiday (Date).}
#'     \item{type}{Holiday type, e.g. \code{"Public"} (character).}
#'     \item{name}{Localised holiday name (character).}
#'     \item{nationwide}{Whether the holiday applies nationwide (logical).}
#'   }
#' @export
#' @seealso \code{\link{holiday_school}}, \code{\link{holiday_countries}}
#' @examples
#' \dontrun{
#' holiday_public("DE", 2025)
#' holiday_public("FR", 2024, language = "FR")
#' }
holiday_public <- function(country, year = format(Sys.Date(), "%Y"),
                           language = "EN") {
  url <- sprintf("%s/PublicHolidays?countryIsoCode=%s&languageIsoCode=%s&validFrom=%s-01-01&validTo=%s-12-31",
                 .holiday_base, country, language, year, year)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_holidays)

  rows <- lapply(raw, function(h) {
    tibble(
      id = as.character(h$id %||% NA),
      start_date = as.Date(h$startDate %||% NA),
      end_date = as.Date(h$endDate %||% NA),
      type = as.character(h$type %||% NA),
      name = .extract_name(h$name, language),
      nationwide = as.logical(h$nationwide %||% NA)
    )
  })
  bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Fetch school holidays for a country and year
#'
#' Queries the Open Holidays API for school holiday periods in a given
#' country and calendar year. School holidays are typically set at the
#' state/subdivision level, so the \code{nationwide} column is often
#' \code{FALSE}. Not all countries publish school holidays through this API.
#'
#' @param country ISO 3166-1 alpha-2 country code (e.g. \code{"DE"},
#'   \code{"FR"}, \code{"AT"}). Use \code{\link{holiday_countries}} to
#'   list supported codes.
#' @param year Four-digit year as character or numeric (default: current year).
#' @param language ISO 639-1 language code for holiday names (default
#'   \code{"EN"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Unique holiday identifier (character).}
#'     \item{start_date}{First day of the holiday period (Date).}
#'     \item{end_date}{Last day of the holiday period (Date).}
#'     \item{type}{Holiday type, e.g. \code{"School"} (character).}
#'     \item{name}{Localised holiday name (character).}
#'     \item{nationwide}{Whether the holiday applies nationwide (logical).}
#'   }
#' @export
#' @seealso \code{\link{holiday_public}}, \code{\link{holiday_countries}}
#' @examples
#' \dontrun{
#' holiday_school("DE", 2025)
#' holiday_school("AT", 2024, language = "DE")
#' }
holiday_school <- function(country, year = format(Sys.Date(), "%Y"),
                           language = "EN") {
  url <- sprintf("%s/SchoolHolidays?countryIsoCode=%s&languageIsoCode=%s&validFrom=%s-01-01&validTo=%s-12-31",
                 .holiday_base, country, language, year, year)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_holidays)

  rows <- lapply(raw, function(h) {
    tibble(
      id = as.character(h$id %||% NA),
      start_date = as.Date(h$startDate %||% NA),
      end_date = as.Date(h$endDate %||% NA),
      type = as.character(h$type %||% NA),
      name = .extract_name(h$name, language),
      nationwide = as.logical(h$nationwide %||% NA)
    )
  })
  bind_rows(rows)
}

#' List countries supported by the Open Holidays API
#'
#' Returns every country for which the Open Holidays API can provide
#' public and/or school holiday data, together with the country's
#' official languages. Use the \code{iso_code} values as the
#' \code{country} argument in \code{\link{holiday_public}} and
#' \code{\link{holiday_school}}.
#'
#' @param language ISO 639-1 language code for country names (default
#'   \code{"EN"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{iso_code}{ISO 3166-1 alpha-2 country code (character).}
#'     \item{name}{Localised country name (character).}
#'     \item{official_languages}{Comma-separated ISO 639-1 codes of the
#'       country's official languages (character).}
#'   }
#' @export
#' @seealso \code{\link{holiday_public}}, \code{\link{holiday_school}}
#' @examples
#' \dontrun{
#' holiday_countries()
#' holiday_countries(language = "DE")
#' }
holiday_countries <- function(language = "EN") {
  raw <- .fetch_json(sprintf("%s/Countries", .holiday_base))
  if (is.null(raw) || length(raw) == 0) return(.schema_countries)

  rows <- lapply(raw, function(c) {
    tibble(
      iso_code = as.character(c$isoCode %||% NA),
      name = .extract_name(c$name, language),
      official_languages = paste(unlist(c$officialLanguages), collapse = ", ")
    )
  })
  bind_rows(rows)
}


# == Context (LLM injection) ==================================================

#' Get openholidaysapi.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
holiday_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(holiday_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/openholidaysapi.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "openholidaysapi.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# openholidaysapi.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# openholidaysapi.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
