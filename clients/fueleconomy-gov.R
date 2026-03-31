# fueleconomy-gov.R
# Self-contained FuelEconomy.gov API client (XML API).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2
# Auth: none
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fuel_base <- "https://www.fueleconomy.gov/ws/rest"

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

.fetch <- function(url, ext = ".xml") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_xml <- function(url) {
  xml2::read_xml(.fetch(url, ext = ".xml"))
}

# -- Parse menu items from XML ------------------------------------------------
.parse_menu <- function(xml_doc) {
  items <- xml2::xml_find_all(xml_doc, ".//menuItem")
  if (length(items) == 0) return(tibble(text = character(), value = character()))
  tibble(
    text  = xml2::xml_text(xml2::xml_find_all(xml_doc, ".//menuItem/text")),
    value = xml2::xml_text(xml2::xml_find_all(xml_doc, ".//menuItem/value"))
  )
}

# == Schemas ===================================================================

.schema_menu <- tibble(text = character(), value = character())

.schema_vehicle <- tibble(
  id = integer(), year = integer(), make = character(), model = character(),
  trany = character(), drive = character(), cylinders = integer(),
  displ = numeric(), fuel_type = character(),
  city_mpg = numeric(), highway_mpg = numeric(), comb_mpg = numeric(),
  co2 = numeric()
)

# == Public functions ==========================================================

#' Get available model years
#'
#' @return tibble: text (display year), value (year string)
#' @export
fuel_years <- function() {
  url <- sprintf("%s/vehicle/menu/year", .fuel_base)
  .parse_menu(.fetch_xml(url))
}

#' Get makes available for a given year
#'
#' @param year Model year (e.g. 2024)
#' @return tibble: text (make name), value (make name)
#' @export
fuel_makes <- function(year) {
  url <- sprintf("%s/vehicle/menu/make?year=%s", .fuel_base, as.integer(year))
  .parse_menu(.fetch_xml(url))
}

#' Get models available for a given year and make
#'
#' @param year Model year (e.g. 2024)
#' @param make Make name (e.g. "Toyota", "Ford")
#' @return tibble: text (model name), value (model id)
#' @export
fuel_models <- function(year, make) {
  url <- sprintf("%s/vehicle/menu/model?year=%s&make=%s",
                 .fuel_base, as.integer(year),
                 utils::URLencode(as.character(make), reserved = TRUE))
  .parse_menu(.fetch_xml(url))
}

#' Get vehicle details by ID
#'
#' @param id Vehicle ID from fuel_models()
#' @return tibble with one row: id, year, make, model, trany, drive, cylinders,
#'   displ, fuel_type, city_mpg, highway_mpg, comb_mpg, co2
#' @export
fuel_vehicle <- function(id) {
  url <- sprintf("%s/vehicle/%s", .fuel_base, as.integer(id))
  doc <- .fetch_xml(url)

  .xml_val <- function(node, tag) {
    v <- xml2::xml_text(xml2::xml_find_first(node, tag))
    if (is.na(v) || v == "") return(NA_character_)
    v
  }

  as_tibble(data.frame(
    id          = as.integer(.xml_val(doc, ".//id")),
    year        = as.integer(.xml_val(doc, ".//year")),
    make        = as.character(.xml_val(doc, ".//make")),
    model       = as.character(.xml_val(doc, ".//model")),
    trany       = as.character(.xml_val(doc, ".//trany")),
    drive       = as.character(.xml_val(doc, ".//drive")),
    cylinders   = as.integer(.xml_val(doc, ".//cylinders")),
    displ       = as.numeric(.xml_val(doc, ".//displ")),
    fuel_type   = as.character(.xml_val(doc, ".//fuelType")),
    city_mpg    = as.numeric(.xml_val(doc, ".//city08")),
    highway_mpg = as.numeric(.xml_val(doc, ".//highway08")),
    comb_mpg    = as.numeric(.xml_val(doc, ".//comb08")),
    co2         = as.numeric(.xml_val(doc, ".//co2TailpipeGpm")),
    stringsAsFactors = FALSE
  ))
}

#' Show FuelEconomy.gov package context for LLM use
#'
#' @return Invisible string with full context
#' @export
fuel_context <- function() {
  .build_context("fueleconomy.gov", header_lines = c(
    "# fueleconomy.gov -- EPA Fuel Economy data",
    "# Deps: httr2, dplyr, tibble, xml2",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Workflow: fuel_years() -> fuel_makes(year) -> fuel_models(year, make)",
    "#   -> fuel_vehicle(id) for details",
    "# Years available: 1984-2026"
  ))
}
