# bea.gov.R
# Self-contained Bureau of Economic Analysis (BEA) API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: REQUIRED — free API key from https://apps.bea.gov/API/signup/
#   Pass as api_key argument or set BEA_API_KEY env var.
# Docs: https://apps.bea.gov/api/_pdf/bea_web_service_api_user_guide.pdf

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.bea_base <- "https://apps.bea.gov/api/data"

.bea_key <- function(api_key = NULL) {
  key <- api_key %||% Sys.getenv("BEA_API_KEY", unset = "")
  if (nchar(key) == 0) {
    stop("BEA API key required. Register at https://apps.bea.gov/API/signup/ ",
         "then pass api_key or set BEA_API_KEY env var.", call. = FALSE)
  }
  key
}

# Core fetch engine
.bea_get <- function(method, params = list(), api_key = NULL) {
  key <- .bea_key(api_key)
  params$UserID <- key
  params$method <- method
  params$ResultFormat <- "JSON"

  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.bea_base, "?", query)

  tmp <- tempfile(fileext = ".json")
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)

  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  # Check for API errors

  err <- raw$BEAAPI$Results$Error
  if (!is.null(err)) {
    stop("BEA API error: ", err$APIErrorDescription %||% "unknown", call. = FALSE)
  }
  raw$BEAAPI$Results
}

# == Dataset listing ===========================================================

#' List available BEA datasets
#'
#' @param api_key BEA API key (or set BEA_API_KEY env var)
#' @return tibble: dataset_name, description
bea_list <- function(api_key = NULL) {
  res <- .bea_get("GetDataSetList", api_key = api_key)
  datasets <- res$Dataset
  if (is.null(datasets) || length(datasets) == 0) return(tibble())
  bind_rows(lapply(datasets, function(d) {
    tibble(
      dataset_name = as.character(d$DatasetName %||% NA),
      description  = as.character(d$DatasetDescription %||% NA)
    )
  }))
}

# == Parameter listing =========================================================

#' List parameters for a BEA dataset
#'
#' @param dataset Dataset name (e.g. "NIPA", "Regional", "GDPbyIndustry")
#' @param api_key BEA API key
#' @return tibble: parameter_name, description, required, default_value, allowed_values
bea_parameters <- function(dataset, api_key = NULL) {
  res <- .bea_get("GetParameterList",
                  params = list(DatasetName = dataset),
                  api_key = api_key)
  params <- res$Parameter
  if (is.null(params) || length(params) == 0) return(tibble())
  bind_rows(lapply(params, function(p) {
    tibble(
      parameter_name = as.character(p$ParameterName %||% NA),
      description    = as.character(p$ParameterDescription %||% NA),
      required       = as.character(p$ParameterIsRequiredFlag %||% NA),
      default_value  = as.character(p$ParameterDefaultValue %||% NA),
      allowed_values = as.character(p$AllowedValue %||% NA)
    )
  }))
}

# == Parameter values ==========================================================

#' Get allowed values for a BEA dataset parameter
#'
#' @param dataset Dataset name
#' @param parameter Parameter name (e.g. "TableName", "Frequency", "GeoFips")
#' @param api_key BEA API key
#' @return tibble of allowed values (columns vary by parameter)
bea_parameter_values <- function(dataset, parameter, api_key = NULL) {
  res <- .bea_get("GetParameterValues",
                  params = list(DatasetName = dataset,
                                ParameterName = parameter),
                  api_key = api_key)
  vals <- res$ParamValue
  if (is.null(vals) || length(vals) == 0) return(tibble())
  bind_rows(lapply(vals, function(v) {
    flat <- lapply(v, function(x) {
      if (is.null(x)) NA_character_ else as.character(x)
    })
    as_tibble(flat)
  }))
}

# == Search (via parameter values) =============================================

#' Search BEA datasets and tables by keyword
#'
#' Searches dataset descriptions and NIPA table names for matches.
#'
#' @param query Search term (case-insensitive)
#' @param api_key BEA API key
#' @return tibble of matching datasets/tables
bea_search <- function(query, api_key = NULL) {
  # Search datasets first
  ds <- bea_list(api_key = api_key)
  q <- tolower(query)
  matches <- ds |> filter(grepl(q, tolower(dataset_name)) | grepl(q, tolower(description)))

  # Also search NIPA table names if possible
  tryCatch({
    nipa_tables <- bea_parameter_values("NIPA", "TableName", api_key = api_key)
    if (nrow(nipa_tables) > 0) {
      tbl_matches <- nipa_tables |>
        filter(if_any(everything(), ~ grepl(q, tolower(as.character(.)))))
      if (nrow(tbl_matches) > 0) {
        tbl_matches <- tbl_matches |> mutate(dataset_name = "NIPA", .before = 1)
        matches <- bind_rows(matches, tbl_matches)
      }
    }
  }, error = function(e) NULL)

  matches
}

# == NIPA data =================================================================

#' Fetch National Income and Product Accounts (NIPA) data
#'
#' @param table_name NIPA table (e.g. "T10101" for GDP). Use
#'   bea_parameter_values("NIPA", "TableName") to list all.
#' @param frequency "A" (annual), "Q" (quarterly), or "M" (monthly)
#' @param year Year(s) as comma-separated string (e.g. "2020,2021,2022") or "ALL"
#' @param api_key BEA API key
#' @return tibble: line_number, line_description, time_period, value, metric, units
bea_nipa <- function(table_name, frequency = "A", year = "ALL", api_key = NULL) {
  res <- .bea_get("GetData",
                  params = list(DatasetName = "NIPA",
                                TableName = table_name,
                                Frequency = frequency,
                                Year = year),
                  api_key = api_key)
  data <- res$Data
  if (is.null(data) || length(data) == 0) return(tibble())
  bind_rows(lapply(data, function(d) {
    tibble(
      table_name       = as.character(d$TableName %||% NA),
      line_number      = as.integer(d$LineNumber %||% NA),
      line_description = as.character(d$LineDescription %||% NA),
      time_period      = as.character(d$TimePeriod %||% NA),
      value            = suppressWarnings(as.numeric(gsub(",", "", d$DataValue %||% NA))),
      metric           = as.character(d$METRIC_NAME %||% NA),
      units            = as.character(d$CL_UNIT %||% NA)
    )
  }))
}

# == GDP by industry ===========================================================

#' Fetch GDP by Industry data
#'
#' @param table_id Table ID (e.g. "1" for value added). Use
#'   bea_parameter_values("GDPbyIndustry", "TableID") to list.
#' @param frequency "A" (annual) or "Q" (quarterly)
#' @param year Year(s) or "ALL"
#' @param industry "ALL" or specific industry code
#' @param api_key BEA API key
#' @return tibble of industry GDP data
bea_gdp_by_industry <- function(table_id = "1", frequency = "A",
                                year = "ALL", industry = "ALL",
                                api_key = NULL) {
  res <- .bea_get("GetData",
                  params = list(DatasetName = "GDPbyIndustry",
                                TableID = table_id,
                                Frequency = frequency,
                                Year = year,
                                Industry = industry),
                  api_key = api_key)
  data <- res$Data
  if (is.null(data) || length(data) == 0) return(tibble())
  bind_rows(lapply(data, function(d) {
    flat <- lapply(d, function(x) {
      if (is.null(x)) NA_character_ else as.character(x)
    })
    as_tibble(flat)
  })) |>
    mutate(
      DataValue = suppressWarnings(as.numeric(gsub(",", "", DataValue)))
    )
}

# == Regional data =============================================================

#' Fetch BEA Regional data (income, GDP by state/metro)
#'
#' @param table_name Table name (e.g. "CAINC1" for personal income).
#'   Use bea_parameter_values("Regional", "TableName") to list.
#' @param line_code Line code (default "1"). Use
#'   bea_parameter_values("Regional", "LineCode") for options.
#' @param geo_fips Geographic FIPS code(s). "STATE" for all states,
#'   "COUNTY" for all counties, "MSA" for metro areas, or specific FIPS.
#' @param year Year(s) or "ALL"
#' @param api_key BEA API key
#' @return tibble of regional data
bea_regional <- function(table_name, line_code = "1", geo_fips = "STATE",
                         year = "ALL", api_key = NULL) {
  res <- .bea_get("GetData",
                  params = list(DatasetName = "Regional",
                                TableName = table_name,
                                LineCode = line_code,
                                GeoFips = geo_fips,
                                Year = year),
                  api_key = api_key)
  data <- res$Data
  if (is.null(data) || length(data) == 0) return(tibble())
  bind_rows(lapply(data, function(d) {
    tibble(
      geo_fips    = as.character(d$GeoFips %||% NA),
      geo_name    = as.character(d$GeoName %||% NA),
      time_period = as.character(d$TimePeriod %||% NA),
      value       = suppressWarnings(as.numeric(gsub(",", "", d$DataValue %||% NA))),
      unit        = as.character(d$CL_UNIT %||% NA),
      metric      = as.character(d$METRIC_NAME %||% NA)
    )
  }))
}

# == International trade =======================================================

#' Fetch BEA International Transactions data
#'
#' @param indicator Indicator code (e.g. "BalGds" for balance on goods).
#'   Use bea_parameter_values("ITA", "Indicator") to list.
#' @param area_or_country Country code or "ALL"
#' @param frequency "A" or "QSA" (quarterly seasonally adjusted)
#' @param year Year(s) or "ALL"
#' @param api_key BEA API key
#' @return tibble of international transactions data
bea_international <- function(indicator = "BalGds", area_or_country = "ALL",
                              frequency = "A", year = "ALL", api_key = NULL) {
  res <- .bea_get("GetData",
                  params = list(DatasetName = "ITA",
                                Indicator = indicator,
                                AreaOrCountry = area_or_country,
                                Frequency = frequency,
                                Year = year),
                  api_key = api_key)
  data <- res$Data
  if (is.null(data) || length(data) == 0) return(tibble())
  bind_rows(lapply(data, function(d) {
    flat <- lapply(d, function(x) {
      if (is.null(x)) NA_character_ else as.character(x)
    })
    as_tibble(flat)
  })) |>
    mutate(
      DataValue = suppressWarnings(as.numeric(gsub(",", "", DataValue)))
    )
}

# == Context ===================================================================

#' Return full function source for LLM context
#'
#' @return Prints and invisibly returns all public function bodies
bea_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/bea.gov.R"
  if (!file.exists(src_file)) {
    cat("# bea.gov context - source not found\n")
    return(invisible("# bea.gov context - source not found"))
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
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
