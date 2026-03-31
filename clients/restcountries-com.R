# restcountries-com.R
# Self-contained REST Countries API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rc_base <- "https://restcountries.com/v3.1"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- Parse countries list into tibble ------------------------------------------
.parse_countries <- function(raw) {
  if (is.null(raw) || length(raw) == 0) return(.schema_countries)
  rows <- lapply(raw, function(c) {
    currencies <- names(c$currencies)
    currency_str <- if (length(currencies) > 0) paste(currencies, collapse = ", ") else NA_character_
    langs <- unlist(c$languages)
    lang_str <- if (length(langs) > 0) paste(langs, collapse = ", ") else NA_character_
    data.frame(
      name       = as.character(c$name$common %||% NA_character_),
      official   = as.character(c$name$official %||% NA_character_),
      cca2       = as.character(c$cca2 %||% NA_character_),
      cca3       = as.character(c$cca3 %||% NA_character_),
      region     = as.character(c$region %||% NA_character_),
      subregion  = as.character(c$subregion %||% NA_character_),
      capital    = as.character(if (length(c$capital) > 0) c$capital[[1]] else NA_character_),
      population = as.integer(c$population %||% NA_integer_),
      area       = as.numeric(c$area %||% NA_real_),
      currencies = currency_str,
      languages  = lang_str,
      flag       = as.character(c$flag %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })
  as_tibble(do.call(rbind, rows))
}

# == Schemas ===================================================================

.schema_countries <- tibble(
  name = character(), official = character(), cca2 = character(),
  cca3 = character(), region = character(), subregion = character(),
  capital = character(), population = integer(), area = numeric(),
  currencies = character(), languages = character(), flag = character()
)

# == Public functions ==========================================================

#' Get all countries
#'
#' @return tibble: name, official, cca2, cca3, region, subregion, capital,
#'   population, area, currencies, languages, flag
#' @export
rc_all <- function() {
  url <- sprintf("%s/all?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages,flag", .rc_base)
  raw <- .fetch_json(url)
  .parse_countries(raw)
}

#' Search countries by name
#'
#' @param name Country name (full or partial, e.g. "united", "brazil")
#' @return tibble of matching countries
#' @export
rc_name <- function(name) {
  url <- sprintf("%s/name/%s?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages,flag",
                 .rc_base, utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  .parse_countries(raw)
}

#' Get country by alpha code
#'
#' @param code ISO 3166-1 alpha-2 or alpha-3 code (e.g. "US", "USA", "BR")
#' @return tibble with one row
#' @export
rc_code <- function(code) {
  url <- sprintf("%s/alpha/%s?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages,flag",
                 .rc_base, utils::URLencode(code, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_countries)
  # single country returns object, not array
  if (!is.null(raw$name)) raw <- list(raw)
  .parse_countries(raw)
}

#' Get countries by region
#'
#' @param region Region name: "Africa", "Americas", "Asia", "Europe", "Oceania"
#' @return tibble of countries in that region
#' @export
rc_region <- function(region) {
  url <- sprintf("%s/region/%s?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages,flag",
                 .rc_base, utils::URLencode(region, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  .parse_countries(raw)
}

#' Show REST Countries package context for LLM use
#'
#' @return Invisible string with full context
#' @export
rc_context <- function() {
  .build_context("restcountries.com", header_lines = c(
    "# restcountries.com -- REST Countries v3.1 API",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Regions: Africa, Americas, Asia, Europe, Oceania",
    "# Codes: ISO 3166-1 alpha-2 (US, GB) or alpha-3 (USA, GBR)"
  ))
}
