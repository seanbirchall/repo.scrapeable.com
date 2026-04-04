# state.gov.R - Department of State data client


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_boundaries <- tibble(
  name = character(), iso_code = character(),
  region = character()
)

.schema_frus <- tibble(
  id = character(), title = character(), summary = character(),
  epub_url = character()
)

# == Public functions ==========================================================

#' List available State Department data resources
#'
#' Returns a catalog of publicly available data resources from the U.S.
#' Department of State, including geospatial boundaries, historical
#' diplomatic documents, and IT policy data.
#'
#' @return A tibble with one row per resource:
#' \describe{
#'   \item{name}{Character. Resource name.}
#'   \item{description}{Character. Brief description.}
#'   \item{url}{Character. Direct download or access URL.}
#'   \item{type}{Character. File format (GPKG, ZIP, OPDS/XML, JSON, HTML).}
#' }
#' @export
#' @examples
#' \dontrun{
#' state_list()
#' }
state_list <- function() {
  datasets <- list(
    list(name = "Large Scale International Boundaries (LSIB)",
         desc = "International boundary lines dataset (GeoPackage/Shapefile)",
         url  = "https://data.geodata.state.gov/LSIB.gpkg",
         type = "GPKG"),
    list(name = "LSIB Shapefile",
         desc = "International boundary lines as Shapefile",
         url  = "https://data.geodata.state.gov/LSIB.zip",
         type = "ZIP"),
    list(name = "FRUS Ebook Catalog",
         desc = "Foreign Relations of the United States historical documents",
         url  = "https://history.state.gov/api/v1/catalog/all",
         type = "OPDS/XML"),
    list(name = "NSF IT Policy Archive",
         desc = "IT policy archive documents",
         url  = "https://www.nsf.gov/digitalstrategy/policyarchive.zip",
         type = "ZIP"),
    list(name = "FITARA Milestones",
         desc = "FITARA milestones data",
         url  = "https://www.nsf.gov/digitalstrategy/FITARAmilestones.json",
         type = "JSON"),
    list(name = "Cartographic Guidance",
         desc = "Cartographic guidance bulletins from the Geographer",
         url  = "https://data.geodata.state.gov/guidance/index.html",
         type = "HTML")
  )
  tibble(
    name        = vapply(datasets, `[[`, character(1), "name"),
    description = vapply(datasets, `[[`, character(1), "desc"),
    url         = vapply(datasets, `[[`, character(1), "url"),
    type        = vapply(datasets, `[[`, character(1), "type")
  )
}

#' Get Foreign Relations of the US (FRUS) ebook catalog
#'
#' Returns the catalog of historical FRUS (Foreign Relations of the United
#' States) documents available as ebooks. FRUS is the official documentary
#' record of U.S. foreign policy, published by the Office of the Historian.
#' Requires the \code{xml2} package for parsing the OPDS/Atom XML feed.
#'
#' @param limit Integer. Maximum entries to return (default 50). The full
#'   catalog contains several hundred volumes spanning from 1861 to present.
#' @return A tibble with one row per volume:
#' \describe{
#'   \item{id}{Character. Volume identifier (e.g. "frus1961-63v01").}
#'   \item{title}{Character. Full volume title.}
#'   \item{summary}{Character. Volume summary/description.}
#'   \item{epub_url}{Character. Direct URL to download the EPUB file (may be NA).}
#' }
#' @export
#' @examples
#' \dontrun{
#' state_frus(limit = 10)
#' }
state_frus <- function(limit = 50) {
  url <- "https://history.state.gov/api/v1/catalog/all"
  tmp <- .fetch(url, ext = ".xml")
  if (!requireNamespace("xml2", quietly = TRUE)) {
    warning("xml2 package required for parsing FRUS catalog")
    return(.schema_frus)
  }
  doc <- xml2::read_xml(tmp)
  ns <- xml2::xml_ns(doc)

  entries <- xml2::xml_find_all(doc, ".//d1:entry", ns)
  if (length(entries) == 0) return(.schema_frus)
  if (length(entries) > limit) entries <- entries[seq_len(limit)]

  tibble(
    id       = xml2::xml_attr(xml2::xml_find_first(entries, ".//d1:id", ns), "id") %||%
               xml2::xml_text(xml2::xml_find_first(entries, ".//d1:id", ns)),
    title    = xml2::xml_text(xml2::xml_find_first(entries, ".//d1:title", ns)),
    summary  = xml2::xml_text(xml2::xml_find_first(entries, ".//d1:summary", ns)),
    epub_url = vapply(entries, function(e) {
      links <- xml2::xml_find_all(e, ".//d1:link[@type='application/epub+zip']", ns)
      if (length(links) > 0) xml2::xml_attr(links[1], "href") else NA_character_
    }, character(1))
  )
}

#' Search FRUS documents by keyword in title
#'
#' Filters the FRUS catalog to find volumes matching a keyword in the
#' title. Fetches up to 500 catalog entries and filters client-side
#' using case-insensitive matching.
#'
#' @param query Character. Keyword to search in titles (e.g. \code{"Vietnam"},
#'   \code{"Soviet"}, \code{"China"}, \code{"Middle East"}).
#' @param limit Integer. Maximum results to return (default 100).
#' @return A tibble of matching FRUS entries with the same columns as
#'   \code{\link{state_frus}}.
#' @export
#' @examples
#' \dontrun{
#' state_search("Vietnam")
#' state_search("Soviet")
#' }
state_search <- function(query, limit = 100) {
  catalog <- state_frus(limit = 500)
  if (nrow(catalog) == 0) return(.schema_frus)
  result <- catalog |>
    dplyr::filter(grepl(query, title, ignore.case = TRUE))
  if (nrow(result) > limit) result <- result[seq_len(limit), ]
  result
}

#' Download LSIB boundary data as GeoPackage file path
#'
#' Downloads the Large Scale International Boundaries (LSIB) dataset from
#' the Office of the Geographer. The LSIB is the authoritative source for
#' international boundary representations used by the U.S. government.
#' Returns a file path to the downloaded GeoPackage for use with
#' \code{sf::st_read()}.
#'
#' @return Character string (invisible). Path to the downloaded \code{.gpkg}
#'   file. A message is printed with the path and usage instructions.
#' @export
#' @examples
#' \dontrun{
#' path <- state_boundaries()
#' # Then read with sf:
#' # boundaries <- sf::st_read(path)
#' }
state_boundaries <- function() {
  url <- "https://data.geodata.state.gov/LSIB.gpkg"
  path <- .fetch(url, ext = ".gpkg")
  message("LSIB GeoPackage downloaded to: ", path)
  message("Read with: sf::st_read('", path, "')")
  invisible(path)
}

#' Get state.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
state_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(state_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/state.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "state.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# state.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# state.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
