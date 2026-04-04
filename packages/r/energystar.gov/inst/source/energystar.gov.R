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

#' List all ENERGY STAR certified product categories
#'
#' Returns a catalog of ~70 ENERGY STAR certified product categories
#' with their Socrata view IDs. Use this to discover what product types
#' are available, then fetch data with \code{estar_products()} (by name)
#' or \code{estar_view()} (by view_id). Categories span appliances, HVAC,
#' electronics, lighting, water heaters, and more.
#'
#' @return A tibble with columns:
#'   - category (character): Product category name (e.g. "Computers",
#'     "Residential Dishwashers", "Heat Pumps (General)")
#'   - view_id (character): Socrata 4x4 dataset identifier for SODA queries
#'   - description (character): Brief description of the product category
#' @examples
#' \dontrun{
#' # Browse all product categories
#' estar_list()
#'
#' # Find heat pump categories
#' estar_list() |> dplyr::filter(grepl("Heat Pump", category))
#' }
#' @export
estar_list <- function() {
  .estar_catalog
}

#' Search ENERGY STAR product categories by keyword
#'
#' Searches the product category catalog by name and description using
#' case-insensitive matching. Returns matching categories with their
#' view IDs for use in data fetching functions.
#'
#' @param query Character string to match (case-insensitive). Examples:
#'   "washer", "heat pump", "refrigerator", "EV", "computer", "thermostat".
#' @return A tibble of matching categories with columns: category, view_id,
#'   description. Empty tibble if no matches.
#' @examples
#' \dontrun{
#' # Find all washer-related categories
#' estar_search("washer")
#'
#' # Find EV charger categories
#' estar_search("EV")
#' }
#' @export
estar_search <- function(query) {
  q <- tolower(query)
  .estar_catalog |>
    dplyr::filter(
      grepl(q, tolower(category)) | grepl(q, tolower(description))
    )
}

#' Fetch ENERGY STAR certified products by category name
#'
#' Retrieves certified product listings for any ENERGY STAR category by
#' name. Supports SoQL WHERE filtering and full-text search. If the
#' category name matches multiple entries, uses the first match. Use
#' this when you know the product type; use \code{estar_view()} for
#' direct view_id access.
#'
#' @param category Category name (partial, case-insensitive match against
#'   catalog). Use \code{estar_list()} to see valid names. Examples:
#'   "Computers", "Dishwashers", "Heat Pumps", "Televisions".
#' @param limit Maximum number of records to return (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause for filtering. Examples:
#'   "brand_name='LG'", "energy_star_partner='Samsung Electronics'".
#'   NULL for no filter.
#' @param q Optional full-text search query string. NULL for no search.
#' @return A tibble of certified products. Columns vary by category but
#'   typically include: brand_name, model_name, model_number,
#'   energy_star_partner, date_available_on_market, date_qualified,
#'   and category-specific energy performance metrics.
#' @examples
#' \dontrun{
#' # Get certified dishwashers
#' estar_products("Dishwashers", limit = 50)
#'
#' # Get LG certified products in a category
#' estar_products("Clothes Washers", where = "brand_name='LG'")
#'
#' # Search for a specific model
#' estar_products("Computers", q = "MacBook")
#' }
#' @export
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

#' Query any ENERGY STAR dataset by Socrata view ID
#'
#' Generic escape hatch for querying any data.energystar.gov dataset
#' directly by its Socrata view ID. Supports SoQL filtering, column
#' selection, and full-text search. Use \code{estar_list()} to find
#' view IDs. Automatically paginates up to the specified limit.
#'
#' @param view_id Socrata 4x4 view identifier (e.g. "rxdj-2c88" for
#'   computers, "q8py-6w3f" for dishwashers). Use \code{estar_list()}.
#' @param limit Maximum records to return (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause for filtering (e.g.
#'   "brand_name='Samsung'"). NULL for no filter.
#' @param select Optional SoQL SELECT clause for column projection
#'   (e.g. "brand_name, model_number, annual_energy_use_kwh").
#'   NULL for all columns.
#' @param q Optional full-text search query. NULL for no search.
#' @param offset Starting row offset for manual pagination (default 0).
#' @return A tibble of results. Columns depend on the dataset.
#' @examples
#' \dontrun{
#' # Query smart thermostats directly by view_id
#' estar_view("7p2p-wkbf", limit = 50)
#'
#' # Get just brand and model from computers
#' estar_view("rxdj-2c88", select = "brand_name, model_number", limit = 100)
#' }
#' @export
estar_view <- function(view_id, limit = 1000, where = NULL, select = NULL,
                       q = NULL, offset = 0) {
  .estar_fetch(view_id, where = where, select = select, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified computers
#'
#' Returns certified computers (V9.0) including desktops, laptops, tablets,
#' and workstations. Shortcut for \code{estar_products("Computers")}.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause. Examples:
#'   "brand_name='Apple'", "type='Notebook/Laptop'". NULL for no filter.
#' @param q Optional full-text search (e.g. "ThinkPad"). NULL for no search.
#' @return A tibble with columns including: brand_name, model_number, type,
#'   energy_star_partner, tec_of_model_kwh, date_qualified.
#' @examples
#' \dontrun{
#' # Get Apple certified computers
#' estar_computers(where = "brand_name='Apple'", limit = 50)
#' }
#' @export
estar_computers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("rxdj-2c88", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified commercial refrigerators and freezers
#'
#' Returns certified commercial refrigeration units. For residential
#' refrigerators use \code{estar_home_refrigerators()}.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause (e.g. "brand_name='True'").
#'   NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   type, volume_cu_ft, daily_energy_consumption_kwh, date_qualified.
#' @examples
#' \dontrun{
#' estar_refrigerators(limit = 50)
#' }
#' @export
estar_refrigerators <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("wati-2tfp", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified residential clothes dryers
#'
#' Returns certified residential clothes dryers including gas and
#' electric models with energy performance data.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause. NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   fuel_type, combined_energy_factor, date_qualified.
#' @examples
#' \dontrun{
#' estar_dryers(limit = 50)
#' }
#' @export
estar_dryers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("t9u7-4d2j", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified EV chargers (DC output)
#'
#' Returns certified electric vehicle supply equipment with DC output
#' (Level 3 / DC fast chargers). Includes efficiency metrics and
#' standby power consumption.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause. NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   output_power_kw, efficiency, standby_power_w, date_qualified.
#' @examples
#' \dontrun{
#' estar_ev_chargers(limit = 50)
#' }
#' @export
estar_ev_chargers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("t3a6-mkxz", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified displays (monitors and signage)
#'
#' Returns certified displays including computer monitors and digital
#' signage with on-mode power, sleep mode power, and screen size data.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause (e.g. "brand_name='Dell'").
#'   NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   screen_size_inches, on_mode_power_w, sleep_mode_power_w, date_qualified.
#' @examples
#' \dontrun{
#' estar_displays(where = "brand_name='Dell'", limit = 50)
#' }
#' @export
estar_displays <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("qbg3-d468", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified televisions
#'
#' Returns certified TVs with on-mode power consumption, screen size,
#' resolution, and standby power data.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause (e.g. "brand_name='Samsung'").
#'   NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   screen_size_inches, on_mode_power_w, standby_power_w, date_qualified.
#' @examples
#' \dontrun{
#' estar_televisions(where = "brand_name='Samsung'", limit = 50)
#' }
#' @export
estar_televisions <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("pd96-rr3d", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified residential dishwashers
#'
#' Returns certified residential dishwashers with water and energy
#' consumption data, soil sensing, and cycle information.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause (e.g. "brand_name='Bosch'").
#'   NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   annual_energy_use_kwh, water_consumption_gallons_cycle, date_qualified.
#' @examples
#' \dontrun{
#' estar_dishwashers(where = "brand_name='Bosch'", limit = 50)
#' }
#' @export
estar_dishwashers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("q8py-6w3f", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified residential clothes washers
#'
#' Returns certified residential clothes washers with water factor,
#' integrated water factor, and energy consumption metrics.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause. NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   integrated_water_factor, annual_energy_use_kwh, date_qualified.
#' @examples
#' \dontrun{
#' estar_washers(limit = 50)
#' }
#' @export
estar_washers <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("bghd-e2wd", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified residential refrigerators
#'
#' Returns certified residential refrigerators and refrigerator-freezers
#' with annual energy use and volume data. For commercial units use
#' \code{estar_refrigerators()}.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause. NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   type, volume_cu_ft, annual_energy_use_kwh, date_qualified.
#' @examples
#' \dontrun{
#' estar_home_refrigerators(limit = 50)
#' }
#' @export
estar_home_refrigerators <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("p5st-her9", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified smart thermostats
#'
#' Returns certified smart thermostats with connected functionality
#' and energy savings metrics.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause (e.g. "brand_name='Nest'").
#'   NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   connected_criteria, date_qualified.
#' @examples
#' \dontrun{
#' estar_thermostats(limit = 50)
#' }
#' @export
estar_thermostats <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("7p2p-wkbf", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified heat pumps (general listing)
#'
#' Returns the general heat pump listing. For specific types see also
#' \code{estar_products("Air-Source Heat Pumps")},
#' \code{estar_products("Ducted Heat Pumps")}, and
#' \code{estar_products("Mini-Split Heat Pumps")}.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause. NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   type, seer, hspf, date_qualified.
#' @examples
#' \dontrun{
#' estar_heat_pumps(limit = 50)
#' }
#' @export
estar_heat_pumps <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("83eb-xbyy", where = where, q = q, limit = limit)
}

#' Fetch ENERGY STAR certified furnaces
#'
#' Returns certified residential furnaces (gas and oil) with AFUE
#' (Annual Fuel Utilization Efficiency) ratings and capacity data.
#'
#' @param limit Maximum records (default 1000, max 50000).
#' @param where Optional SoQL WHERE clause. NULL for no filter.
#' @param q Optional full-text search. NULL for no search.
#' @return A tibble with columns including: brand_name, model_number,
#'   fuel_type, afue, input_capacity_btu_h, date_qualified.
#' @examples
#' \dontrun{
#' estar_furnaces(limit = 50)
#' }
#' @export
estar_furnaces <- function(limit = 1000, where = NULL, q = NULL) {
  .estar_fetch("i97v-e8au", where = where, q = q, limit = limit)
}

#' Get energystar.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
estar_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(estar_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/energystar.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "energystar.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# energystar.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# energystar.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
