# openholidaysapi-org.R
# Self-contained Open Holidays API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none known

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.holiday_base <- "https://openholidaysapi.org"

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
#' @param country ISO 3166-1 alpha-2 country code (e.g. "US", "DE", "FR")
#' @param year Year (default current year)
#' @param language Language for holiday names (default "EN")
#' @return tibble: id, start_date, end_date, type, name, nationwide
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
#' @param country ISO 3166-1 alpha-2 country code (e.g. "DE", "FR", "AT")
#' @param year Year (default current year)
#' @param language Language for holiday names (default "EN")
#' @return tibble: id, start_date, end_date, type, name, nationwide
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

#' Fetch list of supported countries
#'
#' @param language Language for country names (default "EN")
#' @return tibble: iso_code, name, official_languages
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

#' Generate LLM-friendly context for the openholidays package
#'
#' @return Character string (invisibly), also printed
holiday_context <- function() {
  .build_context("openholidaysapi.org", header_lines = c(
    "# openholidaysapi.org - Public and School Holiday Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: none known",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Supported countries include: AD, AL, AT, BE, BG, BR, BY, CA, CH,",
    "#   CZ, DE, DK, EE, ES, FI, FR, GB, GR, HR, HU, IE, IT, LI, LT,",
    "#   LU, LV, MC, MD, ME, MK, MT, NL, NO, PL, PT, RO, RS, RU, SE,",
    "#   SI, SK, SM, TR, UA, US, VA"
  ))
}
