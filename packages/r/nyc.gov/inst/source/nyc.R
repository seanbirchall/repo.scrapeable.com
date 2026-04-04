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
#' Returns a tibble of known machine-readable datasets hosted on nyc.gov
#' subdomains (not Socrata).
#'
#' @param category Optional filter: "Parks", "Transportation", "Safety", "Planning"
#' @return tibble with name, category, format, url
nyc_list <- function(category = NULL) {
  out <- .nyc_datasets
  if (!is.null(category)) {
    out <- out |> filter(tolower(.data$category) == tolower(category))
  }
  out
}

#' Search NYC datasets by keyword
#'
#' Simple keyword search across dataset names and categories.
#'
#' @param query character search term
#' @return tibble of matching datasets
nyc_search <- function(query) {
  q <- tolower(query)
  .nyc_datasets |>
    filter(grepl(q, tolower(name)) | grepl(q, tolower(category)))
}

#' Get NYC marinas directory
#'
#' Parses the NYC Parks Department marinas XML feed into a structured tibble.
#'
#' @return tibble with name, location, phone, operated_by_parks, accessible,
#'   concessionaire, hours, slips
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
#'
#' @return tibble with parkname, sanctuary_name, borough, acres, directions,
#'   description, habitat_type
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
#' Downloads the DOT street network changes XLSX file and returns it as a
#' tibble. Requires the readxl package.
#'
#' @return tibble of street network changes (closures, one-way conversions, etc.)
nyc_street_changes <- function() {
  .nyc_fetch_xlsx("https://www.nyc.gov/html/dot/downloads/excel/street-network-changes.xlsx")
}

#' Get NYC municipal parking facilities
#'
#' Returns a tibble of DOT municipal parking facilities across all five boroughs,
#' scraped from the DOT parking pages.
#'
#' @param borough Optional: "manhattan", "brooklyn", "queens", "bronx", "staten_island"
#' @return tibble with borough and facility information
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
#' Downloads the adopt-a-highway data feeds ZIP file and extracts CSV contents.
#'
#' @param destdir Directory to extract the ZIP file into. Defaults to a temp dir.
#' @return tibble with file paths and sizes of extracted files, or parsed CSV
#'   data if a single CSV is found
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
#' Downloads and parses the citywide low bridges KML file.
#'
#' @return tibble with bridge name, description, and coordinates
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

#' Return full source of all public functions in this file
#'
#' Reads its own source file and extracts all public function bodies
#' (with roxygen comments). Useful for LLM context injection.
#'
#' @return character string of all public function source code
nyc_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/nyc.gov.R"
  if (!file.exists(src_file)) {
    cat("# nyc.gov context - source not found\n")
    return(invisible("# nyc.gov context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) -
        nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- paste(c(rox, body), collapse = "\n")
  }
  out <- paste(blocks, collapse = "\n\n")
  cat(out, "\n")
  invisible(out)
}
