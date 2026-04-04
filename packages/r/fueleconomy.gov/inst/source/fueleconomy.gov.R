# fueleconomy.gov.R - Self-contained fueleconomy.gov client
# EPA/DOE Fuel Economy Data via REST API

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# fueleconomy.gov.R
# Self-contained FuelEconomy.gov API client (JSON REST API + CSV downloads).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none documented
# Source: https://www.fueleconomy.gov/ws/rest


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fecon_base <- "https://www.fueleconomy.gov/ws/rest"
.fecon_csv_base <- "https://www.fueleconomy.gov/feg/epadata"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(
      `User-Agent` = .ua,
      Accept = "application/json"
    ) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  utils::read.csv(tmp, stringsAsFactors = FALSE)
}

.parse_menu <- function(data) {
  if (is.null(data) || identical(data, "null")) {
    return(tibble(text = character(), value = character()))
  }
  items <- data[["menuItem"]]
  if (is.null(items)) return(tibble(text = character(), value = character()))
  # Single item comes as a named list, not data.frame
  if (is.data.frame(items)) {
    return(as_tibble(items))
  }
  if (is.list(items) && !is.null(items[["text"]])) {
    return(tibble(text = items[["text"]], value = items[["value"]]))
  }
  tibble(text = character(), value = character())
}


# == Schemas ===================================================================

.schema_menu <- tibble(text = character(), value = character())

.schema_vehicle <- tibble(
  id = integer(), year = integer(), make = character(), model = character(),
  base_model = character(), trany = character(), drive = character(),
  cylinders = integer(), displ = numeric(), fuel_type = character(),
  city_mpg = numeric(), highway_mpg = numeric(), comb_mpg = numeric(),
  co2_gpm = numeric(), fe_score = integer(), ghg_score = integer(),
  fuel_cost = integer(), you_save_spend = integer(),
  v_class = character(), ev_motor = character(), range_ev = numeric(),
  start_stop = character()
)

.schema_emissions <- tibble(
  vehicle_id = integer(), efid = character(), sales_area = character(),
  score = numeric(), smartway = integer(),
  standard = character(), std_text = character()
)

.schema_prices <- tibble(
  fuel = character(), price = numeric()
)

# == Private parsers ===========================================================

.parse_vehicle <- function(v) {
  if (is.null(v)) return(.schema_vehicle)
  tibble(
    id             = as.integer(v[["id"]] %||% NA),
    year           = as.integer(v[["year"]] %||% NA),
    make           = as.character(v[["make"]] %||% NA),
    model          = as.character(v[["model"]] %||% NA),
    base_model     = as.character(v[["baseModel"]] %||% NA),
    trany          = as.character(v[["trany"]] %||% NA),
    drive          = as.character(v[["drive"]] %||% NA),
    cylinders      = as.integer(v[["cylinders"]] %||% NA),
    displ          = as.numeric(v[["displ"]] %||% NA),
    fuel_type      = as.character(v[["fuelType"]] %||% NA),
    city_mpg       = as.numeric(v[["city08"]] %||% NA),
    highway_mpg    = as.numeric(v[["highway08"]] %||% NA),
    comb_mpg       = as.numeric(v[["comb08"]] %||% NA),
    co2_gpm        = as.numeric(v[["co2TailpipeGpm"]] %||% NA),
    fe_score       = as.integer(v[["feScore"]] %||% NA),
    ghg_score      = as.integer(v[["ghgScore"]] %||% NA),
    fuel_cost      = as.integer(v[["fuelCost08"]] %||% NA),
    you_save_spend = as.integer(v[["youSaveSpend"]] %||% NA),
    v_class        = as.character(v[["VClass"]] %||% NA),
    ev_motor       = as.character(v[["evMotor"]] %||% NA),
    range_ev       = as.numeric(v[["range"]] %||% NA),
    start_stop     = as.character(v[["startStop"]] %||% NA)
  )
}

.parse_emissions <- function(data, vehicle_id) {
  if (is.null(data)) return(.schema_emissions)
  info <- data[["emissionsInfo"]]
  if (is.null(info) || length(info) == 0) return(.schema_emissions)
  if (is.data.frame(info)) {
    return(tibble(
      vehicle_id = as.integer(info[["id"]] %||% vehicle_id),
      efid       = as.character(info[["efid"]] %||% NA),
      sales_area = as.character(info[["salesArea"]] %||% NA),
      score      = as.numeric(info[["score"]] %||% NA),
      smartway   = as.integer(info[["smartwayScore"]] %||% NA),
      standard   = as.character(info[["standard"]] %||% NA),
      std_text   = as.character(info[["stdText"]] %||% NA)
    ))
  }
  # List of lists
  rows <- lapply(info, function(e) {
    tibble(
      vehicle_id = as.integer(e[["id"]] %||% vehicle_id),
      efid       = as.character(e[["efid"]] %||% NA),
      sales_area = as.character(e[["salesArea"]] %||% NA),
      score      = as.numeric(e[["score"]] %||% NA),
      smartway   = as.integer(e[["smartwayScore"]] %||% NA),
      standard   = as.character(e[["standard"]] %||% NA),
      std_text   = as.character(e[["stdText"]] %||% NA)
    )
  })
  bind_rows(rows)
}


# == Public functions ==========================================================

#' List available model years
#'
#' Returns all model years available in the EPA fuel economy database,
#' from 1984 to the current model year. Start here to drill down via
#' \code{\link{fecon_makes}} and \code{\link{fecon_models}}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{text}{Display year (e.g. "2024")}
#'     \item{value}{Year value for use in other functions}
#'   }
#' @examples
#' fecon_years()
#' @export
fecon_years <- function() {
  url <- sprintf("%s/vehicle/menu/year", .fecon_base)
  .parse_menu(.fetch_json(url))
}

#' List vehicle makes available for a given model year
#'
#' Returns all manufacturers with vehicles in the EPA database for the
#' specified year.
#'
#' @param year Integer model year (e.g. 2024).
#' @return A tibble with columns:
#'   \describe{
#'     \item{text}{Make name (e.g. "Toyota", "Ford")}
#'     \item{value}{Make value for use in \code{\link{fecon_models}}}
#'   }
#' @examples
#' fecon_makes(2024)
#' @export
fecon_makes <- function(year) {
  url <- sprintf("%s/vehicle/menu/make?year=%s", .fecon_base, as.integer(year))
  .parse_menu(.fetch_json(url))
}

#' List vehicle models available for a given year and make
#'
#' Returns all models offered by a manufacturer in a given year.
#'
#' @param year Integer model year (e.g. 2024).
#' @param make Character make name (e.g. "Toyota"). Use
#'   \code{\link{fecon_makes}} to list available makes.
#' @return A tibble with columns:
#'   \describe{
#'     \item{text}{Model name (e.g. "Camry", "RAV4")}
#'     \item{value}{Model value for use in \code{\link{fecon_options}}}
#'   }
#' @examples
#' fecon_models(2024, "Toyota")
#' @export
fecon_models <- function(year, make) {
  url <- sprintf("%s/vehicle/menu/model?year=%s&make=%s",
                 .fecon_base, as.integer(year),
                 utils::URLencode(as.character(make), reserved = TRUE))
  .parse_menu(.fetch_json(url))
}

#' List vehicle trim/engine options for a year/make/model
#'
#' Returns vehicle IDs for each trim level and engine configuration.
#' These IDs are required by \code{\link{fecon_vehicle}} and
#' \code{\link{fecon_emissions}}.
#'
#' @param year Integer model year.
#' @param make Character make name.
#' @param model Character model name (from \code{\link{fecon_models}}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{text}{Trim/engine description}
#'     \item{value}{Vehicle ID (use with \code{\link{fecon_vehicle}})}
#'   }
#' @examples
#' fecon_options(2024, "Toyota", "Camry")
#' @export
fecon_options <- function(year, make, model) {
  url <- sprintf("%s/vehicle/menu/options?year=%s&make=%s&model=%s",
                 .fecon_base, as.integer(year),
                 utils::URLencode(as.character(make), reserved = TRUE),
                 utils::URLencode(as.character(model), reserved = TRUE))
  .parse_menu(.fetch_json(url))
}

#' Get detailed vehicle fuel economy data by ID
#'
#' Returns comprehensive EPA fuel economy data for a single vehicle,
#' including MPG ratings, emissions scores, and annual fuel cost estimates.
#'
#' @param id Integer vehicle ID (from \code{\link{fecon_options}}).
#' @return A one-row tibble with columns:
#'   \describe{
#'     \item{id}{Vehicle ID}
#'     \item{year, make, model, base_model}{Vehicle identification}
#'     \item{trany}{Transmission type}
#'     \item{drive}{Drivetrain (e.g. "Front-Wheel Drive")}
#'     \item{cylinders, displ}{Engine cylinders and displacement (liters)}
#'     \item{fuel_type}{Primary fuel type}
#'     \item{city_mpg, highway_mpg, comb_mpg}{EPA MPG ratings}
#'     \item{co2_gpm}{CO2 emissions in grams per mile}
#'     \item{fe_score, ghg_score}{EPA fuel economy and GHG scores (1-10)}
#'     \item{fuel_cost}{Estimated annual fuel cost (USD)}
#'     \item{you_save_spend}{5-year savings vs average (negative = spend more)}
#'     \item{v_class}{Vehicle class}
#'     \item{ev_motor, range_ev}{EV motor description and range (if electric)}
#'     \item{start_stop}{Start-stop technology indicator}
#'   }
#' @examples
#' \dontrun{
#' # Get data for a specific vehicle ID
#' fecon_vehicle(48685)
#' }
#' @export
fecon_vehicle <- function(id) {
  url <- sprintf("%s/vehicle/%s", .fecon_base, as.integer(id))
  .parse_vehicle(.fetch_json(url))
}

#' Get emissions data for a vehicle
#'
#' Returns EPA emissions certification data for a vehicle, including
#' emissions scores, SmartWay ratings, and standards compliance.
#'
#' @param id Integer vehicle ID (from \code{\link{fecon_options}}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{vehicle_id}{Vehicle ID}
#'     \item{efid}{Emissions family ID}
#'     \item{sales_area}{Sales area (e.g. "FC" for federal/California)}
#'     \item{score}{Emissions score}
#'     \item{smartway}{SmartWay score}
#'     \item{standard}{Emissions standard code}
#'     \item{std_text}{Emissions standard description}
#'   }
#' @examples
#' \dontrun{
#' fecon_emissions(48685)
#' }
#' @export
fecon_emissions <- function(id) {
  url <- sprintf("%s/vehicle/emissions/%s", .fecon_base, as.integer(id))
  .parse_emissions(.fetch_json(url), vehicle_id = as.integer(id))
}

#' Get current EPA reference fuel prices
#'
#' Returns the fuel prices used by the EPA to calculate annual fuel cost
#' estimates on vehicle labels. Covers gasoline grades, diesel, E85, CNG,
#' LPG, and electricity.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{fuel}{Fuel type name (e.g. "Regular Gasoline", "Diesel")}
#'     \item{price}{Price in USD (per gallon, or per kWh for electricity)}
#'   }
#' @examples
#' fecon_prices()
#' @export
fecon_prices <- function() {
  url <- sprintf("%s/fuelprices", .fecon_base)
  data <- .fetch_json(url)
  fuel_names <- c(
    regular = "Regular Gasoline", midgrade = "Midgrade Gasoline",
    premium = "Premium Gasoline", diesel = "Diesel",
    e85 = "E85", cng = "CNG", lpg = "LPG",
    electric = "Electricity ($/kWh)"
  )
  tibble(
    fuel  = as.character(fuel_names[names(data)]),
    price = as.numeric(unlist(data))
  ) |> filter(!is.na(fuel))
}

#' Search for vehicles by year, make, and optionally model
#'
#' Convenience function that walks the year -> make -> model -> options chain
#' and returns full vehicle details for all matching trims. Makes multiple
#' API calls under the hood.
#'
#' @param year Integer model year.
#' @param make Character make name (e.g. "Toyota").
#' @param model Optional character model name filter (case-insensitive
#'   substring match). If NULL, returns all models for that year/make.
#' @param max_vehicles Integer maximum vehicles to fetch details for
#'   (default 20). Controls API call volume.
#' @return A tibble with the same columns as \code{\link{fecon_vehicle}}.
#' @examples
#' # All 2024 Toyota Camry trims
#' fecon_search(2024, "Toyota", "Camry")
#'
#' # All 2024 Honda vehicles (capped at 20)
#' fecon_search(2024, "Honda")
#' @export
fecon_search <- function(year, make, model = NULL, max_vehicles = 20) {
  models <- fecon_models(year, make)
  if (nrow(models) == 0) return(.schema_vehicle)

  if (!is.null(model)) {
    models <- models |>
      filter(grepl(tolower(model), tolower(text), fixed = TRUE))
  }
  if (nrow(models) == 0) return(.schema_vehicle)

  # Get options (vehicle IDs) for each model
  all_ids <- list()
  for (i in seq_len(nrow(models))) {
    opts <- tryCatch(
      fecon_options(year, make, models$value[i]),
      error = function(e) .schema_menu
    )
    if (nrow(opts) > 0) {
      all_ids <- c(all_ids, as.list(opts$value))
    }
    if (length(all_ids) >= max_vehicles) break
  }

  if (length(all_ids) == 0) return(.schema_vehicle)
  all_ids <- utils::head(all_ids, max_vehicles)

  # Fetch vehicle details for each ID
  vehicles <- lapply(all_ids, function(vid) {
    tryCatch(fecon_vehicle(as.integer(vid)), error = function(e) .schema_vehicle)
  })
  bind_rows(vehicles)
}

#' Download the full EPA vehicles dataset (CSV)
#'
#' Downloads the complete vehicles.csv from fueleconomy.gov containing all
#' vehicles from 1984 to present (~45,000 rows). Returns a tibble with
#' selected, cleaned, and properly typed columns.
#'
#' @param cols Character. Either "default" (key columns with clean names),
#'   or "all" (every column from the raw CSV). Default is "default".
#' @return A tibble of all EPA vehicle records. With "default" cols:
#'   id, year, make, model, trany, drive, cylinders, displ, fuel_type,
#'   fuel_type1, v_class, city_mpg, highway_mpg, comb_mpg, city_mpg_u,
#'   highway_mpg_u, co2_gpm, fe_score, ghg_score, fuel_cost,
#'   you_save_spend, range_ev, ev_motor, start_stop, base_model.
#' @examples
#' \dontrun{
#' # Download with default columns
#' all_vehicles <- fecon_bulk()
#'
#' # Download with all raw columns
#' all_raw <- fecon_bulk(cols = "all")
#' }
#' @export
fecon_bulk <- function(cols = "default") {
  url <- sprintf("%s/vehicles.csv", .fecon_csv_base)
  raw <- .fetch_csv(url)
  raw <- as_tibble(raw)

  if (identical(cols, "all")) return(raw)

  # Default: return most useful columns with clean names
  col_map <- c(
    id = "id", year = "year", make = "make", model = "model",
    trany = "trany", drive = "drive", cylinders = "cylinders",
    displ = "displ", fuel_type = "fuelType", fuel_type1 = "fuelType1",
    v_class = "VClass",
    city_mpg = "city08", highway_mpg = "highway08", comb_mpg = "comb08",
    city_mpg_u = "UCity", highway_mpg_u = "UHighway",
    co2_gpm = "co2TailpipeGpm", fe_score = "feScore", ghg_score = "ghgScore",
    fuel_cost = "fuelCost08", you_save_spend = "youSaveSpend",
    range_ev = "range", ev_motor = "evMotor",
    start_stop = "startStop", base_model = "baseModel"
  )

  # Keep only columns that exist
  avail <- col_map[col_map %in% names(raw)]
  result <- raw[, avail, drop = FALSE]
  names(result) <- names(avail)

  result |>
    mutate(
      id          = as.integer(id),
      year        = as.integer(year),
      cylinders   = as.integer(cylinders),
      displ       = as.numeric(displ),
      city_mpg    = as.numeric(city_mpg),
      highway_mpg = as.numeric(highway_mpg),
      comb_mpg    = as.numeric(comb_mpg),
      co2_gpm     = as.numeric(co2_gpm),
      fe_score    = as.integer(fe_score),
      ghg_score   = as.integer(ghg_score),
      fuel_cost   = as.integer(fuel_cost),
      range_ev    = as.numeric(range_ev)
    )
}


# == Context ===================================================================

#' Get fueleconomy.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fecon_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fecon_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fueleconomy.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fueleconomy.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fueleconomy.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fueleconomy.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
