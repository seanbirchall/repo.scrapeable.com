# climatewatchdata.org.R - Self-contained climatewatchdata.org client



# climatewatchdata-org.R
# Self-contained Climate Watch Data client (historical emissions).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: unknown / be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cw_base <- "https://www.climatewatchdata.org/api/v1"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_emissions <- tibble(
  iso_code3 = character(), country = character(), data_source = character(),
  sector = character(), gas = character(), unit = character(),
  year = integer(), value = numeric()
)

.schema_sources <- tibble(
  id = integer(), name = character()
)

# == Emissions =================================================================


#' Fetch historical greenhouse gas emissions data from Climate Watch
#'
#' Returns yearly emissions data from CAIT, PIK, UNFCCC, GCP, and other sources
#' hosted by the World Resources Institute Climate Watch platform. Data is
#' pivoted from wide to long format: one row per country-sector-gas-year
#' combination. Covers global emissions from 1850 to present depending on source.
#'
#' @param regions Character vector of ISO3 country codes or region names.
#'   Examples: \code{"USA"}, \code{"WORLD"}, \code{"CHN"}, \code{"EU27"}.
#'   Default \code{"WORLD"}.
#' @param source_ids Integer vector of data source IDs (default \code{NULL} = all).
#'   Use \code{cw_sources()} to see available sources (e.g., 274 = Climate Watch,
#'   275 = PIK, 276 = UNFCCC_AI, 278 = GCP).
#' @param gas_ids Integer vector of gas IDs (default \code{NULL} = all).
#'   Gases include CO2, CH4, N2O, and F-gases.
#' @param sector_ids Integer vector of sector IDs (default \code{NULL} = all).
#'   Sectors include Agriculture, Energy, Industrial Processes, Waste, LUCF.
#' @param per_page Integer. Max results per page (default 50). Each result may
#'   expand to many rows after pivoting years.
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{iso_code3}{\code{character} -- ISO 3166-1 alpha-3 country code (e.g., "USA", "ABW")}
#'     \item{country}{\code{character} -- Country or region name (e.g., "Aruba", "World")}
#'     \item{data_source}{\code{character} -- Source dataset name (e.g., "PIK", "Climate Watch")}
#'     \item{sector}{\code{character} -- Emissions sector (e.g., "Agriculture", "Energy")}
#'     \item{gas}{\code{character} -- Greenhouse gas (e.g., "CH4", "CO2", "N2O")}
#'     \item{unit}{\code{character} -- Measurement unit (e.g., "MtCO2e")}
#'     \item{year}{\code{integer} -- Calendar year (1850--present)}
#'     \item{value}{\code{numeric} -- Emissions value in the given unit}
#'   }
#' @examples
#' # Global emissions (first page)
#' cw_emissions("WORLD", per_page = 5)
#'
#' # US emissions from PIK source
#' cw_emissions("USA", source_ids = 275)
#'
#' # Multiple countries
#' cw_emissions(c("USA", "CHN", "IND"), per_page = 10)
#' @export
cw_emissions <- function(regions = "WORLD", source_ids = NULL, gas_ids = NULL,
                         sector_ids = NULL, per_page = 50, page = 1) {
  url <- paste0(.cw_base, "/data/historical_emissions?per_page=", per_page, "&page=", page)
  for (r in regions) url <- paste0(url, "&regions[]=", utils::URLencode(r))
  if (!is.null(source_ids)) for (s in source_ids) url <- paste0(url, "&source_ids[]=", s)
  if (!is.null(gas_ids)) for (g in gas_ids) url <- paste0(url, "&gas_ids[]=", g)
  if (!is.null(sector_ids)) for (s in sector_ids) url <- paste0(url, "&sector_ids[]=", s)

  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_emissions)

  rows <- lapply(seq_len(nrow(d)), function(i) {
    row <- d[i, ]
    em <- row$emissions[[1]]
    if (is.null(em) || length(em) == 0) return(NULL)
    tibble(
      iso_code3   = as.character(row$iso_code3),
      country     = as.character(row$country),
      data_source = as.character(row$data_source),
      sector      = as.character(row$sector),
      gas         = as.character(row$gas),
      unit        = as.character(row$unit),
      year        = as.integer(em$year),
      value       = as.numeric(em$value)
    )
  })
  bind_rows(rows)
}

#' List available emissions data sources on Climate Watch
#'
#' Returns the catalog of data sources available for historical emissions
#' queries. Use the \code{id} values as input to the \code{source_ids}
#' parameter in \code{cw_emissions()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{\code{integer} -- Source identifier (e.g., 274, 275)}
#'     \item{name}{\code{character} -- Source name (e.g., "Climate Watch", "PIK",
#'       "UNFCCC_AI", "UNFCCC_NAI", "GCP", "US")}
#'   }
#' @examples
#' cw_sources()
#' @export
cw_sources <- function() {
  url <- paste0(.cw_base, "/emissions/meta")
  raw <- .fetch_json(url)
  srcs <- raw$data_source
  if (is.null(srcs) || length(srcs) == 0) return(.schema_sources)
  tibble(
    id   = as.integer(srcs$id),
    name = as.character(srcs$name)
  )
}

# == Context ===================================================================

#' Get climatewatchdata.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cw_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cw_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/climatewatchdata.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "climatewatchdata.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# climatewatchdata.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# climatewatchdata.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
