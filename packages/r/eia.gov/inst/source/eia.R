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
#'
#' @param api_key EIA API key. Default uses EIA_API_KEY env var or DEMO_KEY.
#' @return tibble: id, name, description
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
#' available frequencies, facets, and data fields.
#'
#' @param route API route path (e.g. "electricity", "petroleum/pri/spt").
#'   Use "/" or "" for the top level.
#' @param api_key EIA API key.
#' @return list with elements: id, name, description, routes (tibble),
#'   frequencies (tibble), facets (tibble), data_fields (character vector),
#'   start_period, end_period
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
#' General-purpose data retrieval from the EIA API. Specify a route,
#' frequency, data columns, optional facet filters, and date range.
#'
#' @param route API route (e.g. "electricity/retail-sales",
#'   "petroleum/pri/spt", "total-energy", "natural-gas/pri/sum",
#'   "seds").
#' @param frequency One of "monthly", "weekly", "daily", "hourly",
#'   "annual", "quarterly". Check eia_explore() for valid options.
#' @param data Character vector of data column names to retrieve
#'   (e.g. c("price", "sales")). Use eia_explore() to see available fields.
#' @param facets Named list of facet filters. Each element is a character
#'   vector (e.g. list(stateid = c("CA", "TX"), sectorid = "RES")).
#' @param start Start period (e.g. "2020", "2020-01", "2020-01-01").
#' @param end End period.
#' @param sort_col Column to sort by. Default "period".
#' @param sort_dir Sort direction: "asc" or "desc". Default "desc".
#' @param length Max rows to return (max 5000). Default 5000.
#' @param offset Offset for pagination. Default 0.
#' @param api_key EIA API key.
#' @return tibble of data rows with all returned columns
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
#' Convenience wrapper around eia_data() for electricity retail sales.
#' Returns price, sales volume, revenue, and customer counts.
#'
#' @param state State abbreviation(s) (e.g. "CA", c("CA", "TX"), "US" for national).
#'   Default NULL returns all states.
#' @param sector Sector ID(s): "RES" (residential), "COM" (commercial),
#'   "IND" (industrial), "OTH" (other), "ALL" (all sectors).
#'   Default "ALL".
#' @param frequency "monthly", "quarterly", or "annual". Default "annual".
#' @param start Start period. Default NULL.
#' @param end End period. Default NULL.
#' @param length Max rows. Default 5000.
#' @param api_key EIA API key.
#' @return tibble: period, state, state_description, sector, sector_name,
#'   price, sales, revenue, customers (with units columns)
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
#' "pri/spt" (spot prices), "sum/sndw" (supply/disposition weekly),
#' "stoc/wstk" (weekly stocks), "cons/psup" (product supplied).
#'
#' @param sub_route Petroleum sub-route (e.g. "pri/spt", "sum/sndw").
#'   Use eia_explore("petroleum") to see all sub-routes.
#' @param frequency Frequency. Default "weekly" for spot prices.
#' @param data Data columns to request. Default c("value").
#' @param facets Named list of facet filters.
#' @param start Start period.
#' @param end End period.
#' @param length Max rows. Default 5000.
#' @param api_key EIA API key.
#' @return tibble of petroleum data
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
#' "pri/sum" (price summary), "prod/sum" (production summary),
#' "stor/sum" (storage summary), "cons/sum" (consumption summary).
#'
#' @param sub_route Natural gas sub-route (e.g. "pri/sum").
#'   Use eia_explore("natural-gas") to see all sub-routes.
#' @param frequency Frequency. Default "monthly".
#' @param data Data columns. Default c("value").
#' @param facets Named list of facet filters.
#' @param start Start period.
#' @param end End period.
#' @param length Max rows. Default 5000.
#' @param api_key EIA API key.
#' @return tibble of natural gas data
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
#' production, consumption, stocks, trade, and prices.
#'
#' @param msn Unique series identifier(s) to filter (e.g. "TETCBUS" for
#'   total energy consumption). Default NULL returns all.
#' @param frequency "monthly" or "annual". Default "annual".
#' @param start Start period.
#' @param end End period.
#' @param length Max rows. Default 5000.
#' @param api_key EIA API key.
#' @return tibble: period, msn, series_description, value, unit
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
#' Returns state-level production, consumption, price, and expenditure data.
#'
#' @param state State abbreviation(s) (e.g. "CA", c("CA", "TX")).
#'   Default NULL returns all.
#' @param series_id SEDS series ID(s) (e.g. "TETCB" for total energy
#'   consumption in billion Btu). Default NULL returns all.
#' @param start Start year. Default NULL.
#' @param end End year. Default NULL.
#' @param length Max rows. Default 5000.
#' @param api_key EIA API key.
#' @return tibble: period, state, series_id, series_description, value, unit
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
#' Returns country-level production, consumption, imports, exports by energy source.
#'
#' @param frequency "monthly" or "annual". Default "annual".
#' @param data Data columns. Default c("value").
#' @param facets Named list of facets (e.g. list(activityId = "1",
#'   productId = "44", countryRegionId = "USA")).
#' @param start Start period.
#' @param end End period.
#' @param length Max rows. Default 5000.
#' @param api_key EIA API key.
#' @return tibble of international energy data
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
#' Convenience wrapper for coal data routes.
#'
#' @param sub_route Coal sub-route. Use eia_explore("coal") to see options.
#' @param frequency Frequency. Default "annual".
#' @param data Data columns. Default c("value").
#' @param facets Named list of facet filters.
#' @param start Start period.
#' @param end End period.
#' @param length Max rows. Default 5000.
#' @param api_key EIA API key.
#' @return tibble of coal data
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
#' Returns monthly short-term (18 month) energy projections.
#'
#' @param series_id STEO series ID(s).
#' @param frequency Frequency. Default "monthly".
#' @param start Start period.
#' @param end End period.
#' @param length Max rows. Default 5000.
#' @param api_key EIA API key.
#' @return tibble of STEO projection data
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

#' Generate LLM-friendly context for eia.gov
#'
#' @return Character string with full function signatures and bodies
eia_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/eia.gov.R"
  if (!file.exists(src_file)) {
    cat("# eia.gov context - source not found\n")
    return(invisible("# eia.gov context - source not found"))
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
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
