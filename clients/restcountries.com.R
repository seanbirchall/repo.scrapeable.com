# restcountries.com.R - Self-contained restcountries.com client

library(httr2)
library(jsonlite)
library(tibble)
library(tidyr)


# restcountries-com.R
# Self-contained REST Countries API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rc_base <- "https://restcountries.com/v3.1"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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
      flag       = as.character(c[["flag"]] %||% NA_character_),
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

#' Get all countries in the world
#'
#' Returns a tibble of every country with core geographic, demographic, and
#' political information. Data comes from the REST Countries API (v3.1),
#' which aggregates information from multiple authoritative sources.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Common country name (e.g., \code{"Brazil"}).}
#'     \item{official}{Character. Official country name.}
#'     \item{cca2}{Character. ISO 3166-1 alpha-2 code (e.g., \code{"BR"}).}
#'     \item{cca3}{Character. ISO 3166-1 alpha-3 code (e.g., \code{"BRA"}).}
#'     \item{region}{Character. World region (e.g., \code{"Americas"}).}
#'     \item{subregion}{Character. Sub-region (e.g., \code{"South America"}).}
#'     \item{capital}{Character. Capital city name.}
#'     \item{population}{Integer. Population count.}
#'     \item{area}{Numeric. Land area in km squared.}
#'     \item{currencies}{Character. Comma-separated ISO 4217 currency codes.}
#'     \item{languages}{Character. Comma-separated spoken languages.}
#'     \item{flag}{Character. Flag emoji.}
#'   }
#' @examples
#' rc_all()
#' @export
rc_all <- function() {
  url <- sprintf("%s/all?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages", .rc_base)
  raw <- .fetch_json(url)
  .parse_countries(raw)
}

#' Search countries by name
#'
#' Searches for countries whose common or official name contains the given
#' string. Partial matching is supported (e.g., \code{"united"} matches both
#' \code{"United States"} and \code{"United Kingdom"}).
#'
#' @param name Character string. Full or partial country name to search for
#'   (e.g., \code{"united"}, \code{"brazil"}, \code{"korea"}).
#' @return A tibble of matching countries with the same columns as
#'   \code{\link{rc_all}}.
#' @examples
#' rc_name("brazil")
#' rc_name("united")
#' @export
rc_name <- function(name) {
  url <- sprintf("%s/name/%s?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages",
                 .rc_base, utils::URLencode(name, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  .parse_countries(raw)
}

#' Get country by ISO alpha code
#'
#' Looks up a single country by its ISO 3166-1 alpha-2 (two-letter) or
#' alpha-3 (three-letter) code.
#'
#' @param code Character string. ISO 3166-1 alpha-2 or alpha-3 code
#'   (e.g., \code{"US"}, \code{"USA"}, \code{"BR"}, \code{"GBR"}).
#' @return A tibble with one row and the same columns as
#'   \code{\link{rc_all}}.
#' @examples
#' rc_code("US")
#' rc_code("BRA")
#' @export
rc_code <- function(code) {
  url <- sprintf("%s/alpha/%s?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages",
                 .rc_base, utils::URLencode(code, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_countries)
  # single country returns object, not array
  if (!is.null(raw$name)) raw <- list(raw)
  .parse_countries(raw)
}

#' Get countries by world region
#'
#' Returns all countries in a given world region.
#'
#' @param region Character string. One of \code{"Africa"}, \code{"Americas"},
#'   \code{"Asia"}, \code{"Europe"}, or \code{"Oceania"}.
#' @return A tibble of countries in that region with the same columns as
#'   \code{\link{rc_all}}.
#' @examples
#' rc_region("Europe")
#' rc_region("Oceania")
#' @export
rc_region <- function(region) {
  url <- sprintf("%s/region/%s?fields=name,cca2,cca3,region,subregion,capital,population,area,currencies,languages",
                 .rc_base, utils::URLencode(region, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  .parse_countries(raw)
}

# == Context ===================================================================

#' Get restcountries.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
rc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(rc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/restcountries.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "restcountries.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# restcountries.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# restcountries.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
