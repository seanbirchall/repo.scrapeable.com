# energystar.gov.R
# Self-contained ENERGY STAR certified products client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (Socrata SODA API, open access)
# Docs: https://data.energystar.gov
# Rate limit: unauthenticated ~1000 req/hour; app tokens increase limit
#
# All 50+ ENERGY STAR certified product datasets live on data.energystar.gov
# as Socrata open data views. Each dataset has a four-part view_id (e.g. "xmq6-bm79").

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.estar_base <- "https://data.energystar.gov"

# -- Catalog of all known ENERGY STAR datasets --------------------------------

.estar_catalog <- tibble::tribble(
  ~category, ~view_id, ~description,
  "Commercial Water Heaters", "xmq6-bm79", "Certified commercial water heaters",
  "Commercial Refrigerators and Freezers", "wati-2tfp", "Certified commercial refrigerators and freezers",
  "Residential Clothes Dryers", "t9u7-4d2j", "Certified residential clothes dryers",
  "EV Chargers DC", "t3a6-mkxz", "Certified electric vehicle supply equipment (DC output)",
  "EV Chargers AC", "5jwe-c8xm", "Certified electric vehicle supply equipment (AC output)",
  "Imaging Equipment", "t2v6-g4nf", "Certified imaging equipment (printers, copiers, scanners)",
  "Computers", "rxdj-2c88", "Certified computers V9.0 (desktops, laptops, tablets)",
  "Water Coolers", "qsc8-7f7k", "Certified water coolers",

  "Enterprise Servers", "qifb-fcj2", "Certified enterprise servers",
  "Displays", "qbg3-d468", "Certified displays (monitors, signage)",
  "Most Efficient Displays", "a437-vvgv", "Most efficient certified displays",
  "Televisions", "pd96-rr3d", "Certified televisions",
  "Room Air Cleaners", "gaa3-swy6", "Certified room air cleaners V3.0",
  "Most Efficient Room Air Cleaners", "x6va-j82u", "Most efficient room air cleaners",
  "Dehumidifiers", "mgiu-hu4z", "Certified dehumidifiers",
  "Most Efficient Dehumidifiers", "b88x-mifp", "Most efficient dehumidifiers",
  "Air-Source Heat Pumps", "w7cv-9xjt", "Certified air-source heat pumps",
  "Most Efficient Air-Source Heat Pumps", "8ac2-2j4q", "Most efficient air-source heat pumps",
  "Ducted Heat Pumps", "3m3x-a2hy", "Certified ducted heat pumps",
  "Most Efficient Ducted Heat Pumps", "dsuw-rfiv", "Most efficient ducted heat pumps",
  "Mini-Split Heat Pumps", "akti-mt5s", "Certified mini-split heat pumps",
  "Most Efficient Mini-Split Heat Pumps", "fbwy-abps", "Most efficient mini-split heat pumps",
  "Heat Pumps (General)", "83eb-xbyy", "Certified heat pumps (general listing)",
  "Geothermal Heat Pumps Most Efficient", "4c82-7ysy", "Most efficient geothermal heat pumps",
  "Central Air Conditioners", "tyr2-hhgu", "Certified central air conditioners",
  "Mini-Split Air Conditioners", "qj64-j3bn", "Certified mini-split air conditioners",
  "Room Air Conditioners", "5xn2-dv4h", "Certified room air conditioners",
  "Most Efficient Room Air Conditioners", "irdz-jn2s", "Most efficient room air conditioners",
  "Tax Credit Central AC", "bfxp-wkhy", "Tax credit eligible central and mini-split air conditioners",
  "Tax Credit Heat Pumps", "tzuf-wwcc", "Tax credit eligible air-source heat pumps",
  "Solar Water Heaters", "xs5y-vwyz", "Certified solar water heaters",
  "Gas Water Heaters", "6sbi-yuk2", "Certified gas (storage and tankless) water heaters",
  "Heat Pump Water Heaters", "v7jr-74b4", "Certified heat pump water heaters",
  "Water Heaters (General)", "pbpq-swnu", "Certified water heaters (general listing)",
  "Residential Dishwashers", "q8py-6w3f", "Certified residential dishwashers",
  "Most Efficient Dishwashers", "butk-3ni4", "Most efficient dishwashers",
  "Residential Clothes Washers", "bghd-e2wd", "Certified residential clothes washers",
  "Most Efficient Clothes Washers", "d36s-eh9f", "Most efficient clothes washers",
  "Most Efficient Clothes Dryers", "xgv7-5e6b", "Most efficient clothes dryers",
  "Combo Washer-Dryer", "9jai-gs6t", "Certified residential combo washer-dryer",
  "Commercial Clothes Washers", "9g6r-cpdt", "Certified commercial clothes washers",
  "Residential Refrigerators", "p5st-her9", "Certified residential refrigerators",
  "Most Efficient Residential Refrigerators", "hgxv-ux9b", "Most efficient residential refrigerators",
  "Residential Freezers", "8t9c-g3tn", "Certified residential freezers",
  "Most Efficient Residential Freezers", "teze-bgsr", "Most efficient residential freezers",
  "Lab Refrigerators and Freezers", "g242-ysjw", "Certified laboratory grade refrigerators and freezers",
  "Commercial Ovens", "c8av-ccf7", "Certified commercial ovens",
  "Commercial Electric Cooktops", "nt9t-yxu3", "Certified commercial electric cooktops",
  "Residential Electric Cooking", "m6gi-ng33", "Certified residential electric cooking products",
  "Commercial Ice Machines", "nak5-fsjf", "Certified commercial ice machines",
  "Commercial Boilers", "3393-mxju", "Certified commercial boilers",
  "Commercial Coffee Brewers", "6xa2-5c2t", "Certified commercial coffee brewers",
  "Furnaces", "i97v-e8au", "Certified furnaces",
  "Light Commercial HVAC", "e4mh-a2u3", "Certified light commercial HVAC",
  "Ceiling Fans", "2te3-nmxp", "Certified ceiling fans",
  "Most Efficient Ceiling Fans", "ufj6-xsix", "Most efficient ceiling fans",
  "Connected Ceiling Fans", "a8vq-hx7n", "Connected ceiling fans",
  "Most Efficient Ventilating Fans", "ga9m-7gtz", "Most efficient ventilating fans",
  "Light Fixtures Downlights", "ej6d-wb4c", "Certified light fixtures (downlights)",
  "Connected Light Bulbs", "msj5-kqfv", "Connected light bulbs",
  "Connected Light Fixtures", "5tzz-d4ju", "Connected light fixtures",
  "Insulation", "kphf-22jd", "Certified insulation",
  "Pool Pumps", "m8cf-pkii", "Certified pool pumps",
  "Smart Thermostats", "7p2p-wkbf", "Certified smart thermostats",
  "Telephones", "bpzy-9tg8", "Certified telephones",
  "UPS", "ifxy-2uty", "Certified uninterruptible power supplies",
  "Large Network Equipment", "n8cx-m62r", "Certified large network equipment",
  "Data Center Storage Block IO", "3uec-2gqf", "Certified data center storage (block I/O)",
  "Data Center Storage File IO", "put7-uu67", "Certified data center storage (file I/O)",
  "Storm Windows", "qaxz-ikcb", "Certified storm windows",
  "UPC Codes", "8edu-y555", "Certified products UPC codes"
)

# -- Core SODA fetch engine ---------------------------------------------------

.estar_soda <- function(view_id, where = NULL, select = NULL, q = NULL,
                        order = NULL, limit = 1000, offset = 0) {
  url <- paste0(.estar_base, "/resource/", view_id, ".json")
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(
      `$limit`  = min(limit, 50000),
      `$offset` = offset
    )
  if (!is.null(where))  req <- req |> httr2::req_url_query(`$where` = where)
  if (!is.null(select)) req <- req |> httr2::req_url_query(`$select` = select)
  if (!is.null(q))      req <- req |> httr2::req_url_query(`$q` = q)
  if (!is.null(order))  req <- req |> httr2::req_url_query(`$order` = order)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  req |> httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

# -- Paginated fetch returning a tibble ---------------------------------------

.estar_fetch <- function(view_id, where = NULL, select = NULL, q = NULL,
                         order = NULL, limit = 1000) {
  collected <- list()
  offset <- 0
  page_size <- min(limit, 1000)

  repeat {
    chunk <- .estar_soda(view_id, where = where, select = select, q = q,
                         order = order, limit = page_size, offset = offset)
    if (is.null(chunk) || (is.data.frame(chunk) && nrow(chunk) == 0) ||
        (is.list(chunk) && length(chunk) == 0)) break

    if (is.data.frame(chunk)) {
      collected[[length(collected) + 1]] <- chunk
      offset <- offset + nrow(chunk)
      if (nrow(chunk) < page_size) break
    } else {
      break
    }
    if (offset >= limit) break
  }

  if (length(collected) == 0) return(tibble::tibble())
  out <- dplyr::bind_rows(collected)
  if (nrow(out) > limit) out <- out[seq_len(limit), ]

  # Type coercion: dates
  date_cols <- grep("^date_", names(out), value = TRUE)
  for (col in date_cols) {
    out[[col]] <- as.Date(substr(out[[col]], 1, 10))
  }

  tibble::as_tibble(out)
}

# == Public functions ==========================================================

#' List all known ENERGY STAR product categories
#'
#' Returns a tibble of ~70 product categories with view_id for use in
#' estar_products() or estar_view().
#'
#' @return tibble with columns: category, view_id, description
estar_list <- function() {
  .estar_catalog
}

#' Search ENERGY STAR product categories by keyword
#'
#' @param query Character string to match against category names and descriptions
#' @return tibble of matching categories
estar_search <- function(query) {
  q <- tolower(query)
  .estar_catalog |>
    dplyr::filter(
      grepl(q, tolower(category)) | grepl(q, tolower(description))
    )
}

#' Fetch certified products by category name
#'
#' @param category Category name (partial match against catalog). Use estar_list()
#'   to see valid names.
#' @param limit Maximum number of records to return (default 1000)
#' @param where Optional SoQL WHERE clause for filtering (e.g. "brand_name='LG'")
#' @param q Optional full-text search query
#' @return tibble of certified products
estar_products <- function(category, limit = 1000, where = NULL, q = NULL) {
  cat_lower <- tolower(category)
  match <- .estar_catalog |>
    dplyr::filter(grepl(cat_lower, tolower(.data$category), fixed = TRUE))
  if (nrow(match) == 0) {
    stop("No category matching '", category, "'. Use estar_list() to see options.")
  }
  if (nrow(match) > 1) {
    message("Multiple matches found, using first: ", match$category[1])
  }
  .estar_fetch(match$view_id[1], where = where, q = q, limit = limit)
}

#' Generic SODA query escape hatch
#'
#' @param view_id Socrata view ID (e.g. "rxdj-2c88")
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param select Optional SoQL SELECT clause
#' @param q Optional full-text search query
#' @param offset Starting row offset (default 0)
#' @return tibble
estar_view <- function(view_id, limit = 1000, where = NULL, select = NULL,
                       q = NULL, offset = 0) {
  .estar_fetch(view_id, where = where, select = select, q = q, limit = limit)
}

#' Fetch certified computers
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause (e.g. "brand_name='Apple'")
#' @param q Optional full-text search
#' @return tibble of certified computers
estar_computers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("rxdj-2c88", where = where, q = q, limit = limit)
}

#' Fetch certified commercial refrigerators and freezers
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_refrigerators <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("wati-2tfp", where = where, q = q, limit = limit)
}

#' Fetch certified residential clothes dryers
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_dryers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("t9u7-4d2j", where = where, q = q, limit = limit)
}

#' Fetch certified EV supply equipment (DC output)
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_ev_chargers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("t3a6-mkxz", where = where, q = q, limit = limit)
}

#' Fetch certified displays
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_displays <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("qbg3-d468", where = where, q = q, limit = limit)
}

#' Fetch certified televisions
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_televisions <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("pd96-rr3d", where = where, q = q, limit = limit)
}

#' Fetch certified residential dishwashers
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_dishwashers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("q8py-6w3f", where = where, q = q, limit = limit)
}

#' Fetch certified residential clothes washers
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_washers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("bghd-e2wd", where = where, q = q, limit = limit)
}

#' Fetch certified residential refrigerators
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_home_refrigerators <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("p5st-her9", where = where, q = q, limit = limit)
}

#' Fetch certified smart thermostats
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_thermostats <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("7p2p-wkbf", where = where, q = q, limit = limit)
}

#' Fetch certified heat pumps (general)
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_heat_pumps <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("83eb-xbyy", where = where, q = q, limit = limit)
}

#' Fetch certified furnaces
#'
#' @param limit Maximum records (default 1000)
#' @param where Optional SoQL WHERE clause
#' @param q Optional full-text search
#' @return tibble
estar_furnaces <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("i97v-e8au", where = where, q = q, limit = limit)
}

#' Return full source code of this client for LLM context
#'
#' @return character string with the full contents of this file
estar_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/energystar.gov.R"
  if (!file.exists(src_file)) {
    pkg_dir <- system.file("source", "energystar.R", package = "energystar.gov")
    if (nzchar(pkg_dir)) src_file <- pkg_dir
  }
  if (!file.exists(src_file)) return("Source not available")
  paste(readLines(src_file, warn = FALSE), collapse = "\n")
}
