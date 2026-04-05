# eia.gov.R - Self-contained EIA (Energy Information Administration) API client
# https://api.eia.gov/v2/
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key (free registration at https://www.eia.gov/opendata/register.php)
#       DEMO_KEY works but has strict rate limits (10 req/hr)
# Rate limits: varies by key tier; DEMO_KEY = 10/hr
#
# API v2 structure:
#   /v2/{route}             -> metadata (description, sub-routes, facets, data fields)
#   /v2/{route}/data/       -> actual data (requires frequency + data[] params)
#   Routes: coal, crude-oil-imports, electricity, international, natural-gas,
#           nuclear-outages, petroleum, seds, steo, total-energy, aeo, ieo,
#           densified-biomass, co2-emissions (deprecated)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.eia_base <- "https://api.eia.gov/v2"

`%||%` <- function(a, b) if (is.null(a)) b else a

.eia_api_key <- function(api_key = NULL) {
 key <- api_key %||% Sys.getenv("EIA_API_KEY", unset = "DEMO_KEY")
 if (key == "" || is.na(key)) key <- "DEMO_KEY"
 if (key == "DEMO_KEY") {
   message("Using DEMO_KEY (10 req/hr). Set EIA_API_KEY env var or pass api_key= for higher limits.\n",
           "Register free: https://www.eia.gov/opendata/register.php")
 }
 key
}

.eia_fetch <- function(path, params = list(), api_key = NULL) {
  key <- .eia_api_key(api_key)
  url <- paste0(.eia_base, path)
  params[["api_key"]] <- key
  tmp <- tempfile(fileext = ".json")
  resp <- tryCatch({
    req <- httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 3, backoff = ~ 2)
    httr2::req_perform(req, path = tmp)
  }, error = function(e) {
    warning("EIA API request failed: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(resp)) return(NULL)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

.safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# == Schemas ===================================================================

.schema_routes <- tibble(
  id = character(), name = character(), description = character()
)

.schema_data <- tibble(
  period = character(), value = numeric(), unit = character(),
  series_description = character()
)

.schema_electricity <- tibble(
  period = character(), state = character(), state_description = character(),
  sector = character(), sector_name = character(),
  price = numeric(), sales = numeric(), revenue = numeric(),
  customers = numeric()
)

.schema_petroleum <- tibble(
  period = character(), product = character(), product_name = character(),
  area = character(), area_name = character(),
  value = numeric(), unit = character()
)

.schema_seds <- tibble(
  period = integer(), state = character(), series_id = character(),
  series_description = character(), value = numeric(), unit = character()
)

# == Public functions ==========================================================

#' List top-level EIA API routes
#'
#' Returns the major data categories available through the EIA API v2.
#' Use this to discover which datasets are available before calling
#' \code{eia_explore()} or \code{eia_data()}.
#'
#' @param api_key Character. EIA API key. Default uses \code{EIA_API_KEY} env
#'   var or \code{DEMO_KEY} (10 requests/hour). Register free at
#'   \url{https://www.eia.gov/opendata/register.php}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- route ID (e.g. \code{"coal"}, \code{"electricity"},
#'       \code{"petroleum"}, \code{"natural-gas"}, \code{"total-energy"}, \code{"seds"})}
#'     \item{name}{character -- human-readable name}
#'     \item{description}{character -- detailed description of the dataset}
#'   }
#' @examples
#' eia_routes()
#' @export
eia_routes <- function(api_key = NULL) {
  raw <- .eia_fetch("/", api_key = api_key)
  if (is.null(raw) || is.null(raw$response$routes)) return(.schema_routes)
  routes <- raw$response$routes
  tibble(
    id = routes$id %||% NA_character_,
    name = routes$name %||% NA_character_,
    description = routes$description %||% NA_character_
  )
}

#' Explore an EIA API route
#'
#' Returns metadata for a given route or sub-route, including child routes,
#' available frequencies, facets, and data column names. Essential for
#' discovering parameters before calling \code{eia_data()}.
#'
#' @param route Character. API route path (e.g. \code{"electricity"},
#'   \code{"petroleum/pri/spt"}, \code{"natural-gas/pri/sum"}).
#'   Use \code{"/"} or \code{""} for the top level.
#' @param api_key Character. EIA API key.
#' @return A list with elements:
#'   \describe{
#'     \item{id}{character -- route identifier}
#'     \item{name}{character -- human-readable name}
#'     \item{description}{character -- detailed description}
#'     \item{routes}{tibble(id, name, description) -- child sub-routes}
#'     \item{frequencies}{tibble(id, description, format) -- available frequencies}
#'     \item{facets}{tibble(id, description) -- filterable dimensions}
#'     \item{data_fields}{character vector -- available data column names}
#'     \item{start_period}{character -- earliest available period}
#'     \item{end_period}{character -- latest available period}
#'   }
#' @examples
#' eia_explore("electricity")
#' eia_explore("petroleum")
#' @export
eia_explore <- function(route = "", api_key = NULL) {
  path <- if (nchar(route) == 0 || route == "/") "/" else paste0("/", route, "/")
  raw <- .eia_fetch(path, api_key = api_key)
  if (is.null(raw) || is.null(raw$response)) return(NULL)
  resp <- raw$response

  routes_tbl <- if (!is.null(resp$routes) && length(resp$routes) > 0) {
    tibble(
      id = resp$routes$id %||% NA_character_,
      name = resp$routes$name %||% NA_character_,
      description = resp$routes$description %||% NA_character_
    )
  } else {
    .schema_routes
  }

  freq_tbl <- if (!is.null(resp$frequency) && length(resp$frequency) > 0) {
    tibble(
      id = resp$frequency$id %||% NA_character_,
      description = resp$frequency$description %||% NA_character_,
      format = resp$frequency$format %||% NA_character_
    )
  } else {
    tibble(id = character(), description = character(), format = character())
  }

  facets_tbl <- if (!is.null(resp$facets) && length(resp$facets) > 0) {
    tibble(
      id = resp$facets$id %||% NA_character_,
      description = resp$facets$description %||% NA_character_
    )
  } else {
    tibble(id = character(), description = character())
  }

  data_fields <- if (!is.null(resp$data)) names(resp$data) else character()

  list(
    id = resp$id %||% NA_character_,
    name = resp$name %||% NA_character_,
    description = resp$description %||% NA_character_,
    routes = routes_tbl,
    frequencies = freq_tbl,
    facets = facets_tbl,
    data_fields = data_fields,
    start_period = resp$startPeriod %||% NA_character_,
    end_period = resp$endPeriod %||% NA_character_
  )
}

#' Fetch data from any EIA API v2 route
#'
#' General-purpose data retrieval from the EIA API. This is the core
#' workhorse function. Use \code{eia_explore()} first to discover
#' available routes, frequencies, facets, and data fields.
#'
#' @param route Character. API route (e.g. \code{"electricity/retail-sales"},
#'   \code{"petroleum/pri/spt"}, \code{"total-energy"}, \code{"natural-gas/pri/sum"},
#'   \code{"seds"}, \code{"coal"}).
#' @param frequency Character. One of \code{"monthly"}, \code{"weekly"},
#'   \code{"daily"}, \code{"hourly"}, \code{"annual"}, \code{"quarterly"}.
#'   Check \code{eia_explore()} for valid options per route.
#' @param data Character vector. Data column names to retrieve
#'   (e.g. \code{c("price", "sales")}). Use \code{eia_explore()} to see available fields.
#' @param facets Named list. Facet filters where each element is a character
#'   vector (e.g. \code{list(stateid = c("CA", "TX"), sectorid = "RES")}).
#' @param start Character. Start period (e.g. \code{"2020"}, \code{"2020-01"}).
#' @param end Character. End period.
#' @param sort_col Character. Column to sort by (default \code{"period"}).
#' @param sort_dir Character. Sort direction: \code{"asc"} or \code{"desc"} (default).
#' @param length Integer. Max rows to return (max 5000, default 5000).
#' @param offset Integer. Offset for pagination (default 0).
#' @param api_key Character. EIA API key.
#' @return A tibble with columns varying by route. Common columns include
#'   \code{period} (character), \code{value} (numeric), plus dimension columns
#'   like \code{stateid}, \code{sectorid}, description columns, and unit columns.
#'   Hyphens in column names are replaced with underscores.
#' @examples
#' eia_data("electricity/retail-sales", frequency = "annual",
#'          data = c("price"), facets = list(stateid = "CA"), length = 10)
#' @export
eia_data <- function(route, frequency = NULL, data = NULL, facets = NULL,
                     start = NULL, end = NULL, sort_col = "period",
                     sort_dir = "desc", length = 5000, offset = 0,
                     api_key = NULL) {
  path <- paste0("/", route, "/data/")

  # Build query params manually because of array notation
  params <- list()
  if (!is.null(frequency)) params[["frequency"]] <- frequency
  if (!is.null(start)) params[["start"]] <- start
  if (!is.null(end)) params[["end"]] <- end
  if (!is.null(length)) params[["length"]] <- as.character(length)
  if (offset > 0) params[["offset"]] <- as.character(offset)
  if (!is.null(sort_col)) {
    params[["sort[0][column]"]] <- sort_col
    params[["sort[0][direction]"]] <- sort_dir
  }

  # Data columns: data[0]=price, data[1]=sales, ...
  if (!is.null(data)) {
    for (i in seq_along(data)) {
      params[[paste0("data[", i - 1, "]")]] <- data[i]
    }
  }

  # Facets: facets[stateid][0]=CA, facets[stateid][1]=TX, ...
  if (!is.null(facets)) {
    for (fname in names(facets)) {
      vals <- facets[[fname]]
      for (i in seq_along(vals)) {
        params[[paste0("facets[", fname, "][", i - 1, "]")]] <- vals[i]
      }
    }
  }

  raw <- .eia_fetch(path, params = params, api_key = api_key)
  if (is.null(raw) || is.null(raw$response$data)) {
    if (!is.null(raw$error)) warning("EIA API error: ", raw$error$message)
    return(tibble())
  }

  df <- as_tibble(raw$response$data)

  # Convert known numeric columns
  numeric_hints <- c("value", "price", "sales", "revenue", "customers",
                     "generation", "consumption", "production", "capacity",
                     "receipts", "cost", "stocks", "imports", "exports",
                     "quantity")
  for (col in names(df)) {
    if (tolower(col) %in% numeric_hints ||
        grepl("-units$", col) == FALSE && any(grepl(col, numeric_hints, fixed = TRUE))) {
      # Only convert if the column looks numeric
      test_vals <- df[[col]][!is.na(df[[col]]) & df[[col]] != ""]
      if (length(test_vals) > 0) {
        converted <- suppressWarnings(as.numeric(test_vals))
        if (sum(!is.na(converted)) > sum(is.na(converted))) {
          df[[col]] <- .safe_numeric(df[[col]])
        }
      }
    }
  }

  # Clean column names: replace hyphens with underscores
  names(df) <- gsub("-", "_", names(df))

  df
}

#' Get electricity retail sales data by state and sector
#'
#' Convenience wrapper around \code{eia_data()} for electricity retail sales.
#' Returns price, sales volume, revenue, and customer counts.
#'
#' @param state Character vector. State abbreviation(s) (e.g. \code{"CA"},
#'   \code{c("CA", "TX")}, \code{"US"} for national). Default \code{NULL}
#'   returns all states.
#' @param sector Character. Sector ID(s): \code{"RES"} (residential),
#'   \code{"COM"} (commercial), \code{"IND"} (industrial), \code{"OTH"} (other),
#'   \code{"ALL"} (all sectors). Default \code{"ALL"}.
#' @param frequency Character. \code{"monthly"}, \code{"quarterly"}, or
#'   \code{"annual"} (default).
#' @param start Character. Start period (e.g. \code{"2020"}). Default \code{NULL}.
#' @param end Character. End period. Default \code{NULL}.
#' @param length Integer. Max rows (default 5000).
#' @param api_key Character. EIA API key.
#' @return A tibble with columns including \code{period}, \code{stateid},
#'   \code{stateDescription}, \code{sectorid}, \code{sectorName},
#'   \code{price} (numeric, cents/kWh), \code{sales} (numeric, MWh),
#'   \code{revenue} (numeric, thousand dollars), \code{customers} (numeric).
#' @examples
#' eia_electricity(state = "CA", frequency = "annual", length = 10)
#' @export
eia_electricity <- function(state = NULL, sector = "ALL", frequency = "annual",
                            start = NULL, end = NULL, length = 5000,
                            api_key = NULL) {
  facets <- list()
  if (!is.null(state)) facets[["stateid"]] <- state
  if (!is.null(sector)) facets[["sectorid"]] <- sector

  eia_data(
    route = "electricity/retail-sales",
    frequency = frequency,
    data = c("price", "sales", "revenue", "customers"),
    facets = if (length(facets) > 0) facets else NULL,
    start = start, end = end, length = length,
    api_key = api_key
  )
}

#' Get petroleum data (prices, production, stocks, etc.)
#'
#' Convenience wrapper for petroleum sub-routes. Common sub-routes:
#' \code{"pri/spt"} (spot prices), \code{"sum/sndw"} (supply/disposition weekly),
#' \code{"stoc/wstk"} (weekly stocks), \code{"cons/psup"} (product supplied).
#'
#' @param sub_route Character. Petroleum sub-route (e.g. \code{"pri/spt"}).
#'   Use \code{eia_explore("petroleum")} to see all sub-routes.
#' @param frequency Character. Default \code{"weekly"} for spot prices.
#' @param data Character vector. Data columns to request (default \code{c("value")}).
#' @param facets Named list. Facet filters.
#' @param start Character. Start period.
#' @param end Character. End period.
#' @param length Integer. Max rows (default 5000).
#' @param api_key Character. EIA API key.
#' @return A tibble with columns varying by sub-route. Typically includes
#'   \code{period}, \code{product_name}, \code{area_name}, \code{value} (numeric),
#'   and unit description columns.
#' @examples
#' eia_petroleum("pri/spt", frequency = "weekly", length = 10)
#' @export
eia_petroleum <- function(sub_route = "pri/spt", frequency = "weekly",
                          data = c("value"), facets = NULL,
                          start = NULL, end = NULL, length = 5000,
                          api_key = NULL) {
  route <- paste0("petroleum/", sub_route)
  eia_data(
    route = route, frequency = frequency, data = data,
    facets = facets, start = start, end = end, length = length,
    api_key = api_key
  )
}

#' Get natural gas data (prices, production, storage, etc.)
#'
#' Convenience wrapper for natural gas sub-routes. Common sub-routes:
#' \code{"pri/sum"} (price summary), \code{"prod/sum"} (production summary),
#' \code{"stor/sum"} (storage summary), \code{"cons/sum"} (consumption summary).
#'
#' @param sub_route Character. Natural gas sub-route (e.g. \code{"pri/sum"}).
#'   Use \code{eia_explore("natural-gas")} to see all sub-routes.
#' @param frequency Character. Default \code{"monthly"}.
#' @param data Character vector. Data columns (default \code{c("value")}).
#' @param facets Named list. Facet filters.
#' @param start Character. Start period.
#' @param end Character. End period.
#' @param length Integer. Max rows (default 5000).
#' @param api_key Character. EIA API key.
#' @return A tibble with columns varying by sub-route. Typically includes
#'   \code{period}, \code{process_name}, \code{area_name}, \code{value} (numeric),
#'   and unit description columns.
#' @examples
#' eia_natural_gas("pri/sum", frequency = "monthly", length = 10)
#' @export
eia_natural_gas <- function(sub_route = "pri/sum", frequency = "monthly",
                            data = c("value"), facets = NULL,
                            start = NULL, end = NULL, length = 5000,
                            api_key = NULL) {
  route <- paste0("natural-gas/", sub_route)
  eia_data(
    route = route, frequency = frequency, data = data,
    facets = facets, start = start, end = end, length = length,
    api_key = api_key
  )
}

#' Get total energy data (Monthly Energy Review)
#'
#' Returns comprehensive energy statistics across all sources, including
#' production, consumption, stocks, trade, and prices from the Monthly
#' Energy Review.
#'
#' @param msn Character vector. Unique series identifier(s) to filter
#'   (e.g. \code{"TETCBUS"} for total energy consumption, \code{"CLPRPUS"}
#'   for coal production). Default \code{NULL} returns all.
#' @param frequency Character. \code{"monthly"} or \code{"annual"} (default).
#' @param start Character. Start period (e.g. \code{"2020"}).
#' @param end Character. End period.
#' @param length Integer. Max rows (default 5000).
#' @param api_key Character. EIA API key.
#' @return A tibble with columns including \code{period} (character),
#'   \code{msn} (character), \code{series_description} (character),
#'   \code{value} (numeric), and \code{unit} (character).
#' @examples
#' eia_total_energy(frequency = "annual", length = 10)
#' @export
eia_total_energy <- function(msn = NULL, frequency = "annual",
                             start = NULL, end = NULL, length = 5000,
                             api_key = NULL) {
  facets <- if (!is.null(msn)) list(msn = msn) else NULL
  df <- eia_data(
    route = "total-energy", frequency = frequency, data = c("value"),
    facets = facets, start = start, end = end, length = length,
    api_key = api_key
  )
  if (nrow(df) > 0 && "seriesDescription" %in% names(df)) {
    df <- df |> rename(series_description = seriesDescription)
  }
  df
}

#' Get State Energy Data System (SEDS) data
#'
#' Returns state-level production, consumption, price, and expenditure
#' data from SEDS, the most comprehensive state-level energy dataset.
#'
#' @param state Character vector. State abbreviation(s) (e.g. \code{"CA"},
#'   \code{c("CA", "TX")}). Default \code{NULL} returns all.
#' @param series_id Character vector. SEDS series ID(s) (e.g. \code{"TETCB"}
#'   for total energy consumption in billion Btu, \code{"ESTCB"} for
#'   electricity total consumption). Default \code{NULL} returns all.
#' @param start Character. Start year (e.g. \code{"2015"}). Default \code{NULL}.
#' @param end Character. End year. Default \code{NULL}.
#' @param length Integer. Max rows (default 5000).
#' @param api_key Character. EIA API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{period}{integer -- year}
#'     \item{state}{character -- 2-letter state abbreviation}
#'     \item{series_id}{character -- SEDS series identifier}
#'     \item{series_description}{character -- human-readable description}
#'     \item{value}{numeric -- data value}
#'     \item{unit}{character -- units of measurement}
#'   }
#' @examples
#' eia_seds(state = "CA", start = "2020", length = 10)
#' @export
eia_seds <- function(state = NULL, series_id = NULL, start = NULL, end = NULL,
                     length = 5000, api_key = NULL) {
  facets <- list()
  if (!is.null(state)) facets[["stateId"]] <- state
  if (!is.null(series_id)) facets[["seriesId"]] <- series_id

  df <- eia_data(
    route = "seds", frequency = "annual", data = c("value"),
    facets = if (length(facets) > 0) facets else NULL,
    start = start, end = end, length = length,
    api_key = api_key
  )
  if (nrow(df) > 0) {
    rename_map <- c()
    if ("seriesId" %in% names(df)) rename_map <- c(rename_map, series_id = "seriesId")
    if ("stateId" %in% names(df)) rename_map <- c(rename_map, state = "stateId")
    if ("seriesDescription" %in% names(df)) rename_map <- c(rename_map, series_description = "seriesDescription")
    if ("stateDescription" %in% names(df)) rename_map <- c(rename_map, state_description = "stateDescription")
    if (length(rename_map) > 0) df <- df |> rename(!!!rename_map)
    if ("period" %in% names(df)) df$period <- as.integer(df$period)
  }
  df
}

#' Get international energy data
#'
#' Returns country-level production, consumption, imports, and exports
#' by energy source (petroleum, natural gas, electricity, etc.).
#'
#' @param frequency Character. \code{"monthly"} or \code{"annual"} (default).
#' @param data Character vector. Data columns (default \code{c("value")}).
#' @param facets Named list. Facet filters (e.g. \code{list(activityId = "1",
#'   productId = "44", countryRegionId = "USA")}).
#' @param start Character. Start period.
#' @param end Character. End period.
#' @param length Integer. Max rows (default 5000).
#' @param api_key Character. EIA API key.
#' @return A tibble with columns varying by facet selection. Typically
#'   includes \code{period}, \code{activityName}, \code{productName},
#'   \code{countryRegionName}, \code{value} (numeric), and \code{unit}.
#' @examples
#' eia_international(frequency = "annual", length = 10)
#' @export
eia_international <- function(frequency = "annual", data = c("value"),
                              facets = NULL, start = NULL, end = NULL,
                              length = 5000, api_key = NULL) {
  eia_data(
    route = "international", frequency = frequency, data = data,
    facets = facets, start = start, end = end, length = length,
    api_key = api_key
  )
}

#' Get coal data
#'
#' Convenience wrapper for coal data routes. Use \code{eia_explore("coal")}
#' to discover available sub-routes.
#'
#' @param sub_route Character. Coal sub-route (default \code{""}). Use
#'   \code{eia_explore("coal")} to see options.
#' @param frequency Character. Default \code{"annual"}.
#' @param data Character vector. Data columns (default \code{c("value")}).
#' @param facets Named list. Facet filters.
#' @param start Character. Start period.
#' @param end Character. End period.
#' @param length Integer. Max rows (default 5000).
#' @param api_key Character. EIA API key.
#' @return A tibble with columns varying by sub-route. Typically includes
#'   \code{period}, \code{value} (numeric), plus dimension/description columns.
#' @examples
#' eia_coal(frequency = "annual", length = 10)
#' @export
eia_coal <- function(sub_route = "", frequency = "annual",
                     data = c("value"), facets = NULL,
                     start = NULL, end = NULL, length = 5000,
                     api_key = NULL) {
  route <- if (nchar(sub_route) > 0) paste0("coal/", sub_route) else "coal"
  eia_data(
    route = route, frequency = frequency, data = data,
    facets = facets, start = start, end = end, length = length,
    api_key = api_key
  )
}

#' Get Short-Term Energy Outlook (STEO) data
#'
#' Returns monthly short-term (18-month) energy projections covering
#' prices, supply, demand, and production forecasts.
#'
#' @param series_id Character vector. STEO series ID(s). Default \code{NULL}
#'   returns all available series.
#' @param frequency Character. Default \code{"monthly"}.
#' @param start Character. Start period (e.g. \code{"2024-01"}).
#' @param end Character. End period.
#' @param length Integer. Max rows (default 5000).
#' @param api_key Character. EIA API key.
#' @return A tibble with columns including \code{period}, \code{seriesId},
#'   \code{seriesDescription}, \code{value} (numeric), and \code{unit}.
#' @examples
#' eia_steo(frequency = "monthly", length = 10)
#' @export
eia_steo <- function(series_id = NULL, frequency = "monthly",
                     start = NULL, end = NULL, length = 5000,
                     api_key = NULL) {
  facets <- if (!is.null(series_id)) list(seriesId = series_id) else NULL
  eia_data(
    route = "steo", frequency = frequency, data = c("value"),
    facets = facets, start = start, end = end, length = length,
    api_key = api_key
  )
}

# == Context ===================================================================

#' Get eia.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
eia_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(eia_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/eia.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "eia.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# eia.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# eia.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
