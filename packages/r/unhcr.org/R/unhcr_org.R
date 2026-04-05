# unhcr.org.R - Self-contained unhcr.org client



# unhcr-org.R
# Self-contained UNHCR Refugee Statistics API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.unhcr_base <- "https://api.unhcr.org/population/v1"

`%||%` <- function(a, b) if (is.null(a)) b else a
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_population <- tibble(
  year = integer(), country_origin = character(),
  country_origin_iso = character(), country_asylum = character(),
  country_asylum_iso = character(), refugees = integer(),
  asylum_seekers = integer(), idps = integer(),
  stateless = integer(), oip = integer(), total = integer()
)

.schema_demographics <- tibble(
  year = integer(), country_origin = character(),
  country_asylum = character(), sex = character(),
  age_group = character(), value = integer()
)

# == Population ================================================================


#' Fetch UNHCR population statistics
#'
#' Query the UNHCR Refugee Data Finder API for population statistics on
#' refugees, asylum seekers, internally displaced persons (IDPs),
#' stateless persons, and other people of concern. Data is available
#' from 1951 to the most recent reporting year.
#'
#' @param year Integer. Reporting year (e.g. 2023). Required.
#' @param country_origin Character. ISO3 country of origin code (e.g.
#'   "SYR" for Syria, "AFG" for Afghanistan). Optional.
#' @param country_asylum Character. ISO3 country of asylum code (e.g.
#'   "DEU" for Germany, "TUR" for Turkey). Optional.
#' @param limit Integer. Maximum number of results to return (default
#'   100, max 10000).
#' @param page Integer. Page number for pagination (default 1).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Reporting year.}
#'     \item{country_origin}{Character. Country of origin name.}
#'     \item{country_origin_iso}{Character. ISO3 code of origin country.}
#'     \item{country_asylum}{Character. Country of asylum name.}
#'     \item{country_asylum_iso}{Character. ISO3 code of asylum country.}
#'     \item{refugees}{Integer. Number of refugees.}
#'     \item{asylum_seekers}{Integer. Number of asylum seekers.}
#'     \item{idps}{Integer. Number of internally displaced persons.}
#'     \item{stateless}{Integer. Number of stateless persons.}
#'     \item{oip}{Integer. Other people of interest/concern.}
#'     \item{total}{Integer. Total population of concern.}
#'   }
#'
#' @examples
#' \dontrun{
#' unhcr_population(2023)
#' unhcr_population(2023, country_origin = "SYR")
#' unhcr_population(2022, country_asylum = "DEU", limit = 500)
#' }
#'
#' @export
unhcr_population <- function(year, country_origin = NULL,
                             country_asylum = NULL, limit = 100, page = 1) {
  params <- list(
    year = as.integer(year),
    limit = as.integer(limit),
    page = as.integer(page)
  )
  if (!is.null(country_origin)) params$coo <- country_origin
  if (!is.null(country_asylum)) params$coa <- country_asylum

  query_str <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/population/?%s", .unhcr_base, query_str)
  raw <- .fetch_json(url)
  items <- raw$items
  if (is.null(items) || length(items) == 0) return(.schema_population)
  if (is.data.frame(items) && nrow(items) == 0) return(.schema_population)

  as_tibble(items) |>
    transmute(
      year = as.integer(year),
      country_origin = as.character(if ("coo_name" %in% names(items)) coo_name else NA),
      country_origin_iso = as.character(if ("coo_iso" %in% names(items)) coo_iso else if ("coo" %in% names(items)) coo else NA),
      country_asylum = as.character(if ("coa_name" %in% names(items)) coa_name else NA),
      country_asylum_iso = as.character(if ("coa_iso" %in% names(items)) coa_iso else if ("coa" %in% names(items)) coa else NA),
      refugees = as.integer(if ("refugees" %in% names(items)) refugees else NA),
      asylum_seekers = as.integer(if ("asylum_seekers" %in% names(items)) asylum_seekers else NA),
      idps = as.integer(if ("idps" %in% names(items)) idps else NA),
      stateless = as.integer(if ("stateless" %in% names(items)) stateless else NA),
      oip = as.integer(if ("oip" %in% names(items)) oip else NA),
      total = as.integer(if ("total_pop" %in% names(items)) total_pop else NA)
    )
}

# == Demographics ==============================================================

#' Fetch UNHCR demographic data
#'
#' Retrieves age- and sex-disaggregated demographic breakdowns for
#' populations of concern from the UNHCR API. Useful for analyzing
#' the composition of refugee and displaced populations.
#'
#' @param year Integer. Reporting year (e.g. 2023). Required.
#' @param country_origin Character. ISO3 country of origin code
#'   (e.g. "SYR", "AFG"). Optional.
#' @param country_asylum Character. ISO3 country of asylum code
#'   (e.g. "DEU", "TUR"). Optional.
#' @param limit Integer. Maximum number of results (default 100).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Reporting year.}
#'     \item{country_origin}{Character. Country of origin name.}
#'     \item{country_asylum}{Character. Country of asylum name.}
#'     \item{sex}{Character. Sex category (e.g. "Female", "Male").}
#'     \item{age_group}{Character. Age group label (e.g. "0-4", "5-11").}
#'     \item{value}{Integer. Number of persons in this demographic group.}
#'   }
#'
#' @examples
#' \dontrun{
#' unhcr_demographics(2023, country_origin = "SYR")
#' }
#'
#' @export
unhcr_demographics <- function(year, country_origin = NULL,
                               country_asylum = NULL, limit = 100) {
  params <- list(
    year = as.integer(year),
    limit = as.integer(limit)
  )
  if (!is.null(country_origin)) params$coo <- country_origin
  if (!is.null(country_asylum)) params$coa <- country_asylum

  query_str <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/demographics/?%s", .unhcr_base, query_str)
  raw <- .fetch_json(url)
  items <- raw$items
  if (is.null(items) || length(items) == 0) return(.schema_demographics)
  if (is.data.frame(items) && nrow(items) == 0) return(.schema_demographics)

  as_tibble(items) |>
    transmute(
      year = as.integer(year),
      country_origin = as.character(if ("coo_name" %in% names(items)) coo_name else NA),
      country_asylum = as.character(if ("coa_name" %in% names(items)) coa_name else NA),
      sex = as.character(if ("sex" %in% names(items)) sex else NA),
      age_group = as.character(if ("age_group" %in% names(items)) age_group else NA),
      value = as.integer(if ("value" %in% names(items)) value else NA)
    )
}

# == Countries =================================================================

#' List all UNHCR countries
#'
#' Returns the full list of countries and territories used in UNHCR
#' population statistics, including ISO codes, nationality adjectives,
#' UN geographic region, and major area classification. Useful for
#' looking up valid ISO3 codes to pass to [unhcr_population()] and
#' [unhcr_demographics()].
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{iso}{Character. ISO3 alpha-3 country code (e.g. "AFG", "SYR").}
#'     \item{name}{Character. Country or territory name.}
#'     \item{nationality}{Character. Nationality adjective (e.g. "Afghan").}
#'     \item{region}{Character. UN geographic sub-region (e.g. "Southern Asia").}
#'     \item{major_area}{Character. UN major area (e.g. "Asia", "Africa").}
#'   }
#'
#' @examples
#' \dontrun{
#' unhcr_countries()
#' }
#'
#' @seealso [unhcr_population()], [unhcr_demographics()]
#' @export
unhcr_countries <- function() {
  schema <- tibble(iso = character(), name = character(),
                   nationality = character(), region = character(),
                   major_area = character())
  url <- sprintf("%s/countries/", .unhcr_base)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$items) || nrow(raw$items) == 0) return(schema)

  nms <- names(raw$items)
  as_tibble(raw$items) |>
    transmute(
      iso = as.character(if ("iso" %in% nms) iso else NA_character_),
      name = as.character(if ("name" %in% nms) name else NA_character_),
      nationality = as.character(if ("nationality" %in% nms) nationality else NA_character_),
      region = as.character(if ("region" %in% nms) region else NA_character_),
      major_area = as.character(if ("majorArea" %in% nms) majorArea else NA_character_)
    )
}

# == Context ===================================================================

#' Get unhcr.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
unhcr_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(unhcr_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/unhcr.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "unhcr.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# unhcr.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# unhcr.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
