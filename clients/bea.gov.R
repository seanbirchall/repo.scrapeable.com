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

#' @title List available BEA datasets
#'
#' Returns all datasets available through the Bureau of Economic Analysis
#' API, including NIPA (national accounts), Regional (state/metro data),
#' GDPbyIndustry, and International Transactions. Use dataset names with
#' \code{bea_parameters()} and \code{bea_parameter_values()} to explore
#' available tables and parameters before fetching data.
#'
#' @param api_key BEA API key string. Register for free at
#'   \url{https://apps.bea.gov/API/signup/}. Alternatively, set the
#'   \code{BEA_API_KEY} environment variable. Default \code{NULL} (uses env var).
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{dataset_name} (character): Dataset identifier (e.g. \code{"NIPA"}, \code{"Regional"}, \code{"GDPbyIndustry"}, \code{"ITA"})
#'     \item \code{description} (character): Human-readable dataset description
#'   }
#' @examples
#' \dontrun{
#' # List all BEA datasets
#' bea_list()
#'
#' # With explicit API key
#' bea_list(api_key = "YOUR-API-KEY")
#' }
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

#' @title List parameters for a BEA dataset
#'
#' Returns the parameters required or accepted by a specific BEA dataset.
#' Use this to discover what arguments are needed before calling data
#' functions like \code{bea_nipa()} or \code{bea_regional()}. Follow up
#' with \code{bea_parameter_values()} to see allowed values for each parameter.
#'
#' @param dataset Dataset name. Valid values from \code{bea_list()}, e.g.
#'   \code{"NIPA"}, \code{"Regional"}, \code{"GDPbyIndustry"}, \code{"ITA"}.
#' @param api_key BEA API key. Default \code{NULL} (uses \code{BEA_API_KEY} env var).
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{parameter_name} (character): Parameter name (e.g. \code{"TableName"}, \code{"Frequency"})
#'     \item \code{description} (character): Parameter description
#'     \item \code{required} (character): Whether required (\code{"1"} = yes, \code{"0"} = no)
#'     \item \code{default_value} (character): Default value if not specified
#'     \item \code{allowed_values} (character): Description of allowed values
#'   }
#' @examples
#' \dontrun{
#' # What parameters does NIPA need?
#' bea_parameters("NIPA")
#'
#' # What parameters does Regional need?
#' bea_parameters("Regional")
#' }
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

#' @title Get allowed values for a BEA dataset parameter
#'
#' Returns all valid values for a specific parameter within a BEA dataset.
#' For example, list all NIPA table names or all regional FIPS codes.
#' Columns vary by parameter -- typically include an identifier and
#' description.
#'
#' @param dataset Dataset name. Examples: \code{"NIPA"}, \code{"Regional"},
#'   \code{"GDPbyIndustry"}.
#' @param parameter Parameter name to enumerate. Examples: \code{"TableName"},
#'   \code{"Frequency"}, \code{"GeoFips"}, \code{"Industry"}.
#'   Find valid parameter names with \code{bea_parameters()}.
#' @param api_key BEA API key. Default \code{NULL} (uses env var).
#' @return A tibble of allowed values. Columns vary by parameter but typically
#'   include an identifier key and a description.
#' @examples
#' \dontrun{
#' # List all NIPA tables
#' bea_parameter_values("NIPA", "TableName")
#'
#' # List available frequencies for NIPA
#' bea_parameter_values("NIPA", "Frequency")
#'
#' # List all industry codes for GDP by Industry
#' bea_parameter_values("GDPbyIndustry", "Industry")
#' }
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

#' @title Search BEA datasets and tables by keyword
#'
#' Searches BEA dataset descriptions and NIPA table names for keyword
#' matches (case-insensitive). Combines results from the dataset list and
#' NIPA table catalog into a single tibble. Use this to find the right
#' table name before calling \code{bea_nipa()} or other data functions.
#'
#' @param query Search term (case-insensitive). Examples: \code{"gdp"},
#'   \code{"personal income"}, \code{"trade"}.
#' @param api_key BEA API key. Default \code{NULL} (uses env var).
#' @return A tibble of matching datasets and/or NIPA tables. Columns include
#'   \code{dataset_name} and \code{description} for dataset matches, plus
#'   NIPA table metadata columns for table matches.
#' @examples
#' \dontrun{
#' # Search for GDP-related tables
#' bea_search("gdp")
#'
#' # Search for personal income data
#' bea_search("personal income")
#' }
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

#' @title Fetch National Income and Product Accounts (NIPA) data
#'
#' Returns data from the BEA's National Income and Product Accounts tables,
#' which include GDP, personal income, corporate profits, government spending,
#' and other core macroeconomic indicators. Each row is one line item for
#' one time period with its value and measurement units.
#'
#' @param table_name NIPA table identifier. Common tables:
#'   \code{"T10101"} (GDP), \code{"T20100"} (Personal Income),
#'   \code{"T30100"} (Government Receipts), \code{"T60100"} (Corporate Profits).
#'   Use \code{bea_parameter_values("NIPA", "TableName")} to list all available tables.
#' @param frequency Time frequency. \code{"A"} (annual), \code{"Q"} (quarterly),
#'   or \code{"M"} (monthly, where available). Default \code{"A"}.
#' @param year Year(s) as comma-separated string (e.g. \code{"2020,2021,2022"})
#'   or \code{"ALL"} for all available years. Default \code{"ALL"}.
#' @param api_key BEA API key. Default \code{NULL} (uses env var).
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{table_name} (character): NIPA table identifier
#'     \item \code{line_number} (integer): Line number within the table
#'     \item \code{line_description} (character): Description of the line item (e.g. "Gross domestic product")
#'     \item \code{time_period} (character): Time period (e.g. \code{"2023"}, \code{"2023Q1"})
#'     \item \code{value} (numeric): Data value (commas removed, converted to numeric)
#'     \item \code{metric} (character): Metric name (e.g. "Current Dollars")
#'     \item \code{units} (character): Measurement units (e.g. "Billions of dollars")
#'   }
#' @examples
#' \dontrun{
#' # Annual GDP data
#' bea_nipa("T10101", frequency = "A", year = "2020,2021,2022,2023")
#'
#' # Quarterly personal income
#' bea_nipa("T20100", frequency = "Q", year = "2023")
#'
#' # All years of GDP
#' bea_nipa("T10101")
#' }
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

#' @title Fetch GDP by Industry data
#'
#' Returns GDP data broken down by industry sector. Includes value added,
#' gross output, intermediate inputs, and KLEMS components by industry.
#' Each row is one industry for one time period.
#'
#' @param table_id Table ID. Common values: \code{"1"} (value added by industry),
#'   \code{"5"} (gross output), \code{"6"} (intermediate inputs).
#'   Use \code{bea_parameter_values("GDPbyIndustry", "TableID")} to list all.
#'   Default \code{"1"}.
#' @param frequency \code{"A"} (annual) or \code{"Q"} (quarterly). Default \code{"A"}.
#' @param year Year(s) as comma-separated string or \code{"ALL"}. Default \code{"ALL"}.
#' @param industry Industry code. \code{"ALL"} for all industries, or a specific
#'   code. Use \code{bea_parameter_values("GDPbyIndustry", "Industry")} to list
#'   valid codes. Default \code{"ALL"}.
#' @param api_key BEA API key. Default \code{NULL} (uses env var).
#' @return A tibble with columns including \code{DataValue} (numeric, commas removed)
#'   and industry/time period identifiers. Column names follow BEA conventions
#'   (e.g. \code{TableID}, \code{Industry}, \code{InduDesc}, \code{DataValue}).
#' @examples
#' \dontrun{
#' # Value added by all industries, 2023
#' bea_gdp_by_industry(table_id = "1", year = "2023")
#'
#' # Gross output, annual, all years
#' bea_gdp_by_industry(table_id = "5")
#'
#' # Quarterly data for manufacturing
#' bea_gdp_by_industry(table_id = "1", frequency = "Q", year = "2023",
#'                     industry = "MFG")
#' }
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

#' @title Fetch BEA Regional data (income, GDP by state/metro)
#'
#' Returns regional economic data at the state, county, or metropolitan area
#' level. Covers personal income, GDP by state, employment, and compensation.
#' Each row is one geographic area for one time period.
#'
#' @param table_name Regional table name. Common tables:
#'   \code{"CAINC1"} (personal income summary),
#'   \code{"CAGDP2"} (GDP by state),
#'   \code{"CAINC4"} (personal income and employment).
#'   Use \code{bea_parameter_values("Regional", "TableName")} to list all.
#' @param line_code Line code within the table. Default \code{"1"} (typically
#'   the top-level aggregate). Use \code{bea_parameter_values("Regional", "LineCode")}
#'   to see options.
#' @param geo_fips Geographic scope. Special values: \code{"STATE"} (all states),
#'   \code{"COUNTY"} (all counties), \code{"MSA"} (metro areas). Or a specific
#'   FIPS code (e.g. \code{"06000"} for California). Default \code{"STATE"}.
#' @param year Year(s) as comma-separated string or \code{"ALL"}. Default \code{"ALL"}.
#' @param api_key BEA API key. Default \code{NULL} (uses env var).
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{geo_fips} (character): FIPS code for the geographic area
#'     \item \code{geo_name} (character): Geographic area name (e.g. "California")
#'     \item \code{time_period} (character): Year
#'     \item \code{value} (numeric): Data value (commas removed)
#'     \item \code{unit} (character): Measurement unit
#'     \item \code{metric} (character): Metric name
#'   }
#' @examples
#' \dontrun{
#' # Personal income by state, 2023
#' bea_regional("CAINC1", line_code = "1", geo_fips = "STATE", year = "2023")
#'
#' # GDP by state, all years
#' bea_regional("CAGDP2", geo_fips = "STATE")
#'
#' # California personal income, all years
#' bea_regional("CAINC1", geo_fips = "06000")
#' }
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

#' @title Fetch BEA International Transactions data
#'
#' Returns data from the International Transactions Accounts (ITA), covering
#' trade balances, current account, financial account, and capital transfers.
#' Each row is one indicator value for one country/area and time period.
#'
#' @param indicator Indicator code. Examples: \code{"BalGds"} (balance on goods),
#'   \code{"BalSvcs"} (balance on services), \code{"BalCurAcct"} (current account).
#'   Use \code{bea_parameter_values("ITA", "Indicator")} to list all. Default \code{"BalGds"}.
#' @param area_or_country Country code or \code{"ALL"} for all countries.
#'   Use \code{bea_parameter_values("ITA", "AreaOrCountry")} to list codes.
#'   Default \code{"ALL"}.
#' @param frequency \code{"A"} (annual) or \code{"QSA"} (quarterly, seasonally
#'   adjusted). Default \code{"A"}.
#' @param year Year(s) as comma-separated string or \code{"ALL"}. Default \code{"ALL"}.
#' @param api_key BEA API key. Default \code{NULL} (uses env var).
#' @return A tibble with columns including \code{DataValue} (numeric, commas removed)
#'   and indicator/country/time identifiers. Column names follow BEA conventions.
#' @examples
#' \dontrun{
#' # Annual balance on goods, all countries
#' bea_international(indicator = "BalGds")
#'
#' # Quarterly current account, 2023
#' bea_international(indicator = "BalCurAcct", frequency = "QSA", year = "2023")
#'
#' # Trade with China
#' bea_international(indicator = "BalGds", area_or_country = "China")
#' }
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

#' Get bea.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bea_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bea_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bea.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bea.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bea.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bea.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
