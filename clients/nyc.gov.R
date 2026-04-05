# nyc.gov.R
# Self-contained NYC city government open-data client.
# Covers data hosted directly on nyc.gov subdomains (parks, DOT, etc.).
# For Socrata-hosted NYC Open Data, see cityofnewyork.us client.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2
# Auth: none required (public endpoints)
# Docs: https://www.nyc.gov/

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)
library(xml2)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.nyc_fetch_xml <- function(url) {
  tmp <- tempfile(fileext = ".xml")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  xml2::read_xml(tmp)
}

.nyc_fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  if (!file.exists(tmp) || file.size(tmp) == 0) return(tibble())
  tryCatch(
    as_tibble(utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)),
    error = function(e) tibble()
  )
}

.nyc_fetch_xlsx <- function(url) {
  tmp <- tempfile(fileext = ".xlsx")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  if (!file.exists(tmp) || file.size(tmp) == 0) return(tibble())
  if (!requireNamespace("readxl", quietly = TRUE)) {
    warning("readxl package required to read XLSX files. Install with: install.packages('readxl')")
    return(tibble())
  }
  tryCatch(
    as_tibble(readxl::read_excel(tmp)),
    error = function(e) { warning("Failed to read XLSX: ", e$message); tibble() }
  )
}

.safe_text <- function(node) {
  if (is.null(node) || length(node) == 0) return(NA_character_)
  txt <- xml2::xml_text(node)
  if (length(txt) == 0 || txt == "") return(NA_character_)
  trimws(txt)
}

.safe_child_text <- function(node, child_name) {
  kids <- xml2::xml_find_all(node, child_name)
  if (length(kids) == 0) return(NA_character_)
  trimws(xml2::xml_text(kids[[1]]))
}

# == Datasets catalogue ========================================================

.nyc_datasets <- tibble(
  name = c(
    "Marinas Directory",
    "Nature Preserves Directory",
    "Adopt-a-Highway Cleanliness Ratings",
    "Street Network Changes",
    "Citywide Low Bridges",
    "Vision Zero Data Feeds",
    "NYC 3D Building Model"
  ),
  category = c(
    "Parks", "Parks", "Transportation",
    "Transportation", "Transportation",
    "Safety", "Planning"
  ),
  format = c(
    "XML", "CSV", "ZIP",
    "XLSX", "KML",
    "HTML", "HTML"
  ),
  url = c(
    "http://nyc.gov/html/dpr/nycbigapps/DPR_marinas_001.xml",
    "http://www.nyc.gov/html/dpr/nycbigapps/DPR_naturepreserves_001.csv",
    "https://www.nyc.gov/html/dot/downloads/misc/adopt-a-highway-datafeeds.zip",
    "https://www.nyc.gov/html/dot/downloads/excel/street-network-changes.xlsx",
    "http://www.nyc.gov/html/dot/downloads/misc/lowbridges_citywide_data_71309.kml",
    "http://www.nyc.gov/html/dot/html/about/vz_datafeeds.shtml",
    "https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-nyc-3d-model-download.page"
  )
)

# == Public functions ==========================================================

#' List available NYC government datasets
#'
#' Returns a tibble of 7 known machine-readable datasets hosted on
#' nyc.gov subdomains (not Socrata). Covers parks, transportation,
#' safety, and planning data.
#'
#' @param category Character or NULL. Filter by category:
#'   \code{"Parks"}, \code{"Transportation"}, \code{"Safety"},
#'   \code{"Planning"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- dataset name (e.g. \code{"Marinas Directory"})}
#'     \item{category}{character -- topic category}
#'     \item{format}{character -- file format: \code{"XML"}, \code{"CSV"},
#'       \code{"ZIP"}, \code{"XLSX"}, \code{"KML"}, \code{"HTML"}}
#'     \item{url}{character -- download URL}
#'   }
#' @examples
#' nyc_list()
#' nyc_list(category = "Parks")
nyc_list <- function(category = NULL) {
  out <- .nyc_datasets
  if (!is.null(category)) {
    out <- out |> filter(tolower(.data$category) == tolower(category))
  }
  out
}

#' Search NYC datasets by keyword
#'
#' Simple keyword search across dataset names and categories
#' (case-insensitive).
#'
#' @param query Character. Search term (e.g. \code{"park"},
#'   \code{"bridge"}, \code{"marina"}).
#' @return A tibble of matching datasets with columns: name,
#'   category, format, url (same schema as \code{nyc_list()}).
#' @examples
#' nyc_search("park")
#' nyc_search("bridge")
nyc_search <- function(query) {
  q <- tolower(query)
  .nyc_datasets |>
    filter(grepl(q, tolower(name)) | grepl(q, tolower(category)))
}

#' Get NYC marinas directory
#'
#' Parses the NYC Parks Department marinas XML feed into a structured
#' tibble. Contains all 15 public marinas across the five boroughs.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- marina name}
#'     \item{location}{character -- address/location description}
#'     \item{phone}{character -- contact phone number}
#'     \item{operated_by_parks}{character -- \code{"Yes"} or \code{"No"}}
#'     \item{accessible}{character -- wheelchair accessible (\code{"Y"}/\code{"N"})}
#'     \item{concessionaire}{character -- operating concessionaire name}
#'     \item{hours}{character -- operating hours}
#'     \item{slips}{integer -- number of boat slips}
#'   }
#' @examples
#' nyc_marinas()
nyc_marinas <- function() {
  doc <- .nyc_fetch_xml("http://nyc.gov/html/dpr/nycbigapps/DPR_marinas_001.xml")
  marinas <- xml2::xml_find_all(doc, ".//Marina")
  if (length(marinas) == 0) return(tibble())

  tibble(
    name              = vapply(marinas, .safe_child_text, character(1), "Name"),
    location          = vapply(marinas, .safe_child_text, character(1), "Location"),
    phone             = vapply(marinas, .safe_child_text, character(1), "Phone"),
    operated_by_parks = vapply(marinas, .safe_child_text, character(1), "Operated_by_Parks"),
    accessible        = vapply(marinas, .safe_child_text, character(1), "Accessible"),
    concessionaire    = vapply(marinas, .safe_child_text, character(1), "Concessionaire"),
    hours             = vapply(marinas, .safe_child_text, character(1), "Hours"),
    slips             = vapply(marinas, function(m) {
      s <- .safe_child_text(m, "Slips")
      if (is.na(s)) return(NA_integer_)
      suppressWarnings(as.integer(gsub("[^0-9]", "", s)))
    }, integer(1))
  )
}

#' Get NYC nature preserves directory
#'
#' Downloads and parses the NYC Parks Department nature preserves CSV.
#' Contains ~51 nature preserves across the five boroughs.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{parkname}{character -- park name}
#'     \item{sanctuaryname}{character -- sanctuary/preserve name}
#'     \item{borough}{character -- borough name}
#'     \item{acres}{numeric -- preserve area in acres}
#'     \item{directions}{character -- directions/access info}
#'     \item{description}{character -- preserve description}
#'     \item{habitattype}{character -- habitat classification}
#'   }
#' @examples
#' nyc_nature_preserves()
nyc_nature_preserves <- function() {
  raw <- .nyc_fetch_csv("http://www.nyc.gov/html/dpr/nycbigapps/DPR_naturepreserves_001.csv")
  if (nrow(raw) == 0) return(tibble())
  # Clean column names
  nms <- tolower(gsub("[^a-zA-Z0-9]+", "_", names(raw)))
  nms <- sub("^_|_$", "", nms)
  names(raw) <- nms
  # Strip HTML from text fields, handling encoding issues
  strip_html <- function(x) {
    if (!is.character(x)) return(x)
    x <- iconv(x, from = "", to = "UTF-8", sub = "")
    gsub("<[^>]+>", "", x, useBytes = TRUE)
  }
  raw |>
    mutate(across(where(is.character), strip_html)) |>
    mutate(acres = suppressWarnings(as.numeric(gsub(",", "", acres %||% NA))))
}

#' Get NYC street network changes
#'
#' Downloads the DOT street network changes XLSX file and returns it
#' as a tibble. Documents street closures, one-way conversions, and
#' other network modifications. Requires the \code{readxl} package.
#'
#' @return A tibble of street network changes with columns varying
#'   by the DOT dataset version (typically includes location, type
#'   of change, date, and borough).
#' @examples
#' nyc_street_changes()
nyc_street_changes <- function() {
  .nyc_fetch_xlsx("https://www.nyc.gov/html/dot/downloads/excel/street-network-changes.xlsx")
}

#' Get NYC municipal parking facilities
#'
#' Returns a reference tibble pointing to the DOT municipal parking
#' facilities page for all five boroughs.
#'
#' @param borough Character or NULL. Filter to a single borough:
#'   \code{"manhattan"}, \code{"brooklyn"}, \code{"queens"},
#'   \code{"bronx"}, \code{"staten_island"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{page_url}{character -- URL to the DOT parking page}
#'     \item{boroughs}{character -- comma-separated borough names}
#'     \item{note}{character -- usage guidance}
#'   }
#' @examples
#' nyc_parking()
#' nyc_parking(borough = "manhattan")
nyc_parking <- function(borough = NULL) {
  boroughs <- c(
    manhattan     = "https://www1.nyc.gov/html/dot/html/motorist/parkinglist.shtml#manhattan",
    brooklyn      = "https://www1.nyc.gov/html/dot/html/motorist/parkinglist.shtml#kings",
    queens        = "https://www1.nyc.gov/html/dot/html/motorist/parkinglist.shtml#queens",
    bronx         = "https://www1.nyc.gov/html/dot/html/motorist/parkinglist.shtml#bronx",
    staten_island = "https://www1.nyc.gov/html/dot/html/motorist/parkinglist.shtml#richmond"
  )

  if (!is.null(borough)) {
    borough <- tolower(gsub(" ", "_", borough))
    if (!borough %in% names(boroughs)) {
      stop("borough must be one of: ", paste(names(boroughs), collapse = ", "))
    }
    boroughs <- boroughs[borough]
  }

  # Fetch the full page once (all boroughs on same page)
  page_url <- "https://www1.nyc.gov/html/dot/html/motorist/parkinglist.shtml"
  tmp <- tempfile(fileext = ".html")
  tryCatch({
    httr2::request(page_url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(path = tmp)
  }, error = function(e) {
    warning("Failed to fetch parking page: ", e$message)
    return(tibble())
  })

  if (!file.exists(tmp)) return(tibble())
  html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")

  # Try to extract parking facility info from the HTML
  # This is a best-effort extraction from a non-API page
  tibble(
    page_url  = page_url,
    boroughs  = paste(names(boroughs), collapse = ", "),
    note      = "Visit page_url for full interactive parking facility listings"
  )
}

#' Get NYC adopted highway cleanliness data
#'
#' Downloads the DOT adopt-a-highway data feeds ZIP file and extracts
#' CSV contents. Returns parsed CSV data if available, otherwise a
#' listing of extracted files.
#'
#' @param destdir Character or NULL. Directory to extract the ZIP file
#'   into. Defaults to a temp directory.
#' @return A tibble -- either parsed CSV data (if CSVs found in the
#'   archive) or a file listing with columns: file, path, size_bytes.
#' @examples
#' nyc_highway_cleanliness()
nyc_highway_cleanliness <- function(destdir = NULL) {
  destdir <- destdir %||% tempdir()
  zip_path <- file.path(destdir, "adopt-a-highway-datafeeds.zip")

  httr2::request("https://www.nyc.gov/html/dot/downloads/misc/adopt-a-highway-datafeeds.zip") |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = zip_path)

  if (!file.exists(zip_path) || file.size(zip_path) == 0) {
    warning("Failed to download adopt-a-highway data")
    return(tibble())
  }

  extracted <- utils::unzip(zip_path, exdir = destdir)

  # Handle nested ZIPs
  inner_zips <- extracted[grepl("\\.zip$", extracted, ignore.case = TRUE)]
  for (iz in inner_zips) {
    inner_extracted <- utils::unzip(iz, exdir = destdir)
    extracted <- c(extracted, inner_extracted)
  }

  csvs <- extracted[grepl("\\.csv$", extracted, ignore.case = TRUE)]

  if (length(csvs) >= 1) {
    all_data <- lapply(csvs, function(f) {
      tryCatch(as_tibble(utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)),
               error = function(e) tibble())
    })
    all_data <- all_data[vapply(all_data, nrow, integer(1)) > 0]
    if (length(all_data) == 1) return(all_data[[1]])
    if (length(all_data) > 1) return(bind_rows(all_data))
  }

  tibble(
    file = basename(extracted),
    path = extracted,
    size_bytes = file.size(extracted)
  )
}

#' Get NYC low bridges data
#'
#' Downloads and parses the citywide low bridges KML file from DOT.
#' Contains ~149 low-clearance bridge locations across all five boroughs.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- bridge or street name}
#'     \item{description}{character -- HTML table with clearance details}
#'     \item{coordinates}{character -- longitude,latitude,altitude string}
#'   }
#' @examples
#' nyc_low_bridges()
nyc_low_bridges <- function() {
  url <- "http://www.nyc.gov/html/dot/downloads/misc/lowbridges_citywide_data_71309.kml"
  doc <- tryCatch(.nyc_fetch_xml(url), error = function(e) NULL)
  if (is.null(doc)) return(tibble())

  # KML uses a default namespace
  ns <- xml2::xml_ns(doc)
  placemarks <- xml2::xml_find_all(doc, ".//d1:Placemark", ns)
  if (length(placemarks) == 0) {
    # Try without namespace
    placemarks <- xml2::xml_find_all(doc, ".//Placemark")
  }
  if (length(placemarks) == 0) return(tibble())

  tibble(
    name = vapply(placemarks, function(p) {
      n <- xml2::xml_find_first(p, ".//d1:name|.//name", ns)
      .safe_text(n)
    }, character(1)),
    description = vapply(placemarks, function(p) {
      d <- xml2::xml_find_first(p, ".//d1:description|.//description", ns)
      .safe_text(d)
    }, character(1)),
    coordinates = vapply(placemarks, function(p) {
      c_node <- xml2::xml_find_first(p, ".//d1:coordinates|.//coordinates", ns)
      .safe_text(c_node)
    }, character(1))
  )
}

# == Context ===================================================================

#' Get nyc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nyc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nyc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nyc.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nyc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nyc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nyc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
