# state.gov.R - Department of State data client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

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
#' @return tibble of available datasets/resources
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
#' Returns the catalog of historical FRUS documents available as ebooks.
#' @param limit Maximum entries to return (default 50).
#' @return tibble with id, title, summary, epub_url
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
#' @param query Keyword to search in titles (e.g. "Vietnam", "Soviet").
#' @param limit Max results (default 100).
#' @return tibble of matching FRUS entries
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
#' Downloads the Large Scale International Boundaries dataset.
#' Returns the local file path for use with sf::st_read().
#' @return Character string: path to downloaded .gpkg file
state_boundaries <- function() {
  url <- "https://data.geodata.state.gov/LSIB.gpkg"
  path <- .fetch(url, ext = ".gpkg")
  message("LSIB GeoPackage downloaded to: ", path)
  message("Read with: sf::st_read('", path, "')")
  invisible(path)
}

#' Return function signatures and documentation for LLM context
#'
#' @return Printed function listing (invisibly returns string)
state_context <- function() {
  src_file <- tryCatch({
    f <- getSrcFilename(state_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else NULL
  }, error = function(e) NULL)

  if (is.null(src_file)) {
    src_dir <- system.file("source", package = "state.gov")
    if (src_dir != "") {
      src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
      if (length(src_files)) src_file <- src_files[1]
    }
  }
  if (is.null(src_file)) {
    msg <- paste(
      "# state.gov R client",
      "# Functions: state_list, state_frus, state_search, state_boundaries, state_context",
      "# Dept of State - FRUS ebook catalog, LSIB international boundaries",
      sep = "\n"
    )
    cat(msg, "\n"); return(invisible(msg))
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
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }
  out <- paste(c("# state.gov R client", "# Department of State", "#",
                 "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
