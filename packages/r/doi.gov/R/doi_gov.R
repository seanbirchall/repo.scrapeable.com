# doi.gov.R
# Self-contained U.S. Department of the Interior — Natural Resources Revenue Data client.
# Data source: Office of Natural Resources Revenue (ONRR) via revenuedata.doi.gov
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public CSV downloads)
# Docs: https://revenuedata.doi.gov/downloads/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.doi_base <- "https://revenuedata.doi.gov/downloads"

# -- Context generator ---------------------------------------------------------

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

# -- CSV fetcher ---------------------------------------------------------------

.doi_fetch_csv <- function(filename) {
  url <- paste0(.doi_base, "/", filename)
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE) |>
    as_tibble()
}

.doi_filter <- function(df, year = NULL, state = NULL, commodity = NULL) {
  # Find the year column (varies across datasets)
  yr_col <- intersect(c("Calendar Year", "Fiscal Year"), names(df))
  if (length(yr_col) > 0 && !is.null(year)) {
    df <- df[df[[yr_col[1]]] %in% year, , drop = FALSE]
  }
  if ("State" %in% names(df) && !is.null(state)) {
    df <- df[grepl(state, df$State, ignore.case = TRUE), , drop = FALSE]
  }
  if ("Commodity" %in% names(df) && !is.null(commodity)) {
    df <- df[grepl(commodity, df$Commodity, ignore.case = TRUE), , drop = FALSE]
  }
  if ("Product" %in% names(df) && !is.null(commodity) && !"Commodity" %in% names(df)) {
    df <- df[grepl(commodity, df$Product, ignore.case = TRUE), , drop = FALSE]
  }
  as_tibble(df)
}

.doi_clean_names <- function(df) {
  nms <- names(df)
  nms <- gsub("[/ ()-]+", "_", nms)
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  nms <- tolower(nms)
  names(df) <- nms
  df
}


# == Schemas ===================================================================

.schema_revenue <- tibble(
  calendar_year = integer(), land_class = character(), land_category = character(),
  state = character(), county = character(), fips_code = character(),
  offshore_region = character(), revenue_type = character(),
  mineral_lease_type = character(), commodity = character(),
  product = character(), revenue = numeric()
)

.schema_production <- tibble(
  calendar_year = integer(), land_class = character(), land_category = character(),
  state = character(), county = character(), fips_code = character(),
  offshore_region = character(), product = character(), volume = numeric()
)

.schema_disbursements <- tibble(
  fiscal_year = integer(), fund_type = character(), source = character(),
  state = character(), county = character(), disbursement = numeric()
)


# == Discovery: list available datasets ========================================

#' List available DOI natural resources revenue datasets
#'
#' Returns a catalog of all downloadable CSV datasets from the Office of
#' Natural Resources Revenue (ONRR) at revenuedata.doi.gov. Use this to
#' discover datasets before fetching with the specific access functions.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Machine-readable dataset name (e.g., "calendar_year_revenue").}
#'     \item{description}{Character. Human-readable description of the dataset.}
#'     \item{frequency}{Character. Update frequency: "annual" or "monthly".}
#'     \item{url}{Character. Direct download URL for the CSV file.}
#'   }
#' @examples
#' doi_list()
doi_list <- function() {
  tibble(
    name = c(
      "calendar_year_revenue",
      "monthly_revenue",
      "fiscal_year_revenue",
      "calendar_year_production",
      "monthly_production",
      "fiscal_year_production",
      "fiscal_year_disbursements",
      "monthly_disbursements",
      "federal_sales",
      "federal_revenue_by_company"
    ),
    description = c(
      "Revenue from natural resources by calendar year",
      "Revenue from natural resources by month",
      "Revenue from natural resources by fiscal year",
      "Commodity production volumes by calendar year",
      "Commodity production volumes (oil, gas, coal) by month",
      "Commodity production volumes by fiscal year",
      "Disbursements to Treasury, states, tribes by fiscal year",
      "Disbursements to Treasury, states, tribes by month",
      "Federal oil, gas, NGL sales volumes and royalties by calendar year",
      "Federal revenue by company and calendar year"
    ),
    frequency = c("annual", "monthly", "annual", "annual", "monthly",
                   "annual", "annual", "monthly", "annual", "annual"),
    url = paste0(.doi_base, "/", c(
      "calendar_year_revenue.csv",
      "monthly_revenue.csv",
      "fiscal_year_revenue.csv",
      "calendar_year_production.csv",
      "monthly_production.csv",
      "fiscal_year_production.csv",
      "fiscal_year_disbursements.csv",
      "monthly_disbursements.csv",
      "federal_sales.csv",
      "federal_revenue_by_company.csv"
    ))
  )
}


# == Discovery: search by keyword ==============================================

#' Search DOI revenue datasets by keyword
#'
#' Searches dataset names and descriptions for a keyword match.
#' A convenience wrapper around \code{doi_list()} with text filtering.
#'
#' @param query Character. Search term to match against dataset names and
#'   descriptions (case-insensitive), e.g. \code{"revenue"}, \code{"production"},
#'   \code{"disbursement"}, \code{"monthly"}.
#' @return A tibble with the same columns as \code{doi_list()}: name, description,
#'   frequency, url. Only rows matching the query are returned.
#' @examples
#' doi_search("revenue")
#' doi_search("monthly")
doi_search <- function(query) {
  ds <- doi_list()
  pattern <- tolower(query)
  matches <- grepl(pattern, tolower(ds$name)) | grepl(pattern, tolower(ds$description))
  ds[matches, ]
}


# == Revenue ===================================================================

#' Natural resources revenue by calendar year
#'
#' Revenue from federal lands, offshore, and Native American lands by year.
#' Includes breakdowns by state, county, revenue type, commodity, and product.
#' Downloads the full CSV from ONRR and applies optional filters.
#'
#' @param year Integer vector of calendar years to filter (e.g., \code{2020:2023}).
#'   \code{NULL} returns all years.
#' @param state Character. Regex pattern for state name (e.g., \code{"Wyoming"},
#'   \code{"New Mexico"}). Case-insensitive. \code{NULL} returns all states.
#' @param commodity Character. Regex pattern for commodity (e.g., \code{"Oil"},
#'   \code{"Gas"}, \code{"Coal"}). Case-insensitive. \code{NULL} returns all.
#' @return A tibble with columns:
#'   \describe{
#'     \item{calendar_year}{Integer. Calendar year of the revenue record.}
#'     \item{land_class}{Character. Land class (e.g., "Federal", "Native American").}
#'     \item{land_category}{Character. Category (e.g., "Onshore", "Offshore").}
#'     \item{state}{Character. U.S. state name.}
#'     \item{county}{Character. County name.}
#'     \item{fips_code}{Integer. FIPS county code.}
#'     \item{offshore_region}{Character. Offshore planning area, if applicable.}
#'     \item{revenue_type}{Character. Type (e.g., "Royalties", "Bonus", "Rents").}
#'     \item{mineral_lease_type}{Character. Lease type classification.}
#'     \item{commodity}{Character. Commodity name (e.g., "Oil", "Gas", "Coal").}
#'     \item{product}{Character. Specific product type.}
#'     \item{revenue}{Numeric. Revenue amount in USD.}
#'   }
#' @examples
#' doi_revenue(year = 2023, state = "Wyoming")
#' doi_revenue(year = 2020:2023, commodity = "Oil")
doi_revenue <- function(year = NULL, state = NULL, commodity = NULL) {
  df <- .doi_fetch_csv("calendar_year_revenue.csv")
  df <- .doi_clean_names(df)
  df$revenue <- suppressWarnings(as.numeric(df$revenue))
  df$calendar_year <- as.integer(df$calendar_year)
  if (!is.null(year)) df <- df[df$calendar_year %in% year, ]
  if (!is.null(state)) df <- df[grepl(state, df$state, ignore.case = TRUE), ]
  if (!is.null(commodity)) df <- df[grepl(commodity, df$commodity, ignore.case = TRUE), ]
  as_tibble(df)
}


#' Natural resources revenue by month
#'
#' Monthly revenue broken down by location, revenue type, commodity, and product.
#' Downloads the full monthly CSV from ONRR and applies optional filters.
#'
#' @param year Integer vector of years to filter (e.g., \code{2023}).
#'   \code{NULL} returns all years.
#' @param state Character. Regex pattern for state name. Case-insensitive.
#'   \code{NULL} returns all states.
#' @param commodity Character. Regex pattern for commodity (e.g., \code{"Oil"}).
#'   Case-insensitive. \code{NULL} returns all.
#' @return A tibble with columns including date (Date), land_class, land_category,
#'   state, county, fips_code, offshore_region, revenue_type, mineral_lease_type,
#'   commodity, product, and revenue (numeric, USD).
#' @examples
#' doi_revenue_monthly(year = 2023, state = "Texas", commodity = "Oil")
doi_revenue_monthly <- function(year = NULL, state = NULL, commodity = NULL) {
  df <- .doi_fetch_csv("monthly_revenue.csv")
  df <- .doi_clean_names(df)
  df$date <- as.Date(df$date, format = "%m/%d/%Y")
  df$revenue <- suppressWarnings(as.numeric(df$revenue))
  if (!is.null(year)) df <- df[as.integer(format(df$date, "%Y")) %in% year, ]
  if (!is.null(state)) df <- df[grepl(state, df$state, ignore.case = TRUE), ]
  if (!is.null(commodity)) df <- df[grepl(commodity, df$commodity, ignore.case = TRUE), ]
  as_tibble(df)
}


# == Production ================================================================

#' Natural resources production by calendar year
#'
#' Commodity production volumes (oil, gas, coal, etc.) from federal lands,
#' offshore, and Native American lands by year.
#'
#' @param year Integer vector of calendar years (e.g., \code{2020:2023}).
#'   \code{NULL} returns all years.
#' @param state Character. Regex pattern for state name. Case-insensitive.
#'   \code{NULL} returns all states.
#' @param product Character. Regex pattern for product name (e.g., \code{"Oil"},
#'   \code{"Gas"}, \code{"Coal"}). Case-insensitive. \code{NULL} returns all.
#' @return A tibble with columns:
#'   \describe{
#'     \item{calendar_year}{Integer. Calendar year.}
#'     \item{land_class}{Character. Land class (e.g., "Federal", "Native American").}
#'     \item{land_category}{Character. Category (e.g., "Onshore", "Offshore").}
#'     \item{state}{Character. U.S. state name.}
#'     \item{county}{Character. County name.}
#'     \item{fips_code}{Integer. FIPS county code.}
#'     \item{offshore_region}{Character. Offshore planning area, if applicable.}
#'     \item{product}{Character. Product type name.}
#'     \item{volume}{Numeric. Production volume (units vary by product).}
#'   }
#' @examples
#' doi_production(year = 2023, state = "Wyoming", product = "Oil")
doi_production <- function(year = NULL, state = NULL, product = NULL) {
  df <- .doi_fetch_csv("calendar_year_production.csv")
  df <- .doi_clean_names(df)
  df$volume <- suppressWarnings(as.numeric(df$volume))
  df$calendar_year <- as.integer(df$calendar_year)
  if (!is.null(year)) df <- df[df$calendar_year %in% year, ]
  if (!is.null(state)) df <- df[grepl(state, df$state, ignore.case = TRUE), ]
  if (!is.null(product)) df <- df[grepl(product, df$product, ignore.case = TRUE), ]
  as_tibble(df)
}


#' Natural resources production by month
#'
#' Monthly production volumes for oil, gas, and coal from federal and
#' Native American lands. Downloads the full monthly CSV and filters.
#'
#' @param year Integer vector of years (e.g., \code{2023}). \code{NULL} returns all.
#' @param commodity Character. Regex pattern for commodity (e.g., \code{"Oil"},
#'   \code{"Gas"}, \code{"Coal"}). Case-insensitive. \code{NULL} returns all.
#' @return A tibble with columns including date (Date), land_class, land_category,
#'   commodity, and volume (numeric).
#' @examples
#' doi_production_monthly(year = 2023, commodity = "Oil")
doi_production_monthly <- function(year = NULL, commodity = NULL) {
  df <- .doi_fetch_csv("monthly_production.csv")
  df <- .doi_clean_names(df)
  df$date <- as.Date(df$date, format = "%m/%d/%Y")
  df$volume <- suppressWarnings(as.numeric(df$volume))
  if (!is.null(year)) df <- df[as.integer(format(df$date, "%Y")) %in% year, ]
  if (!is.null(commodity)) df <- df[grepl(commodity, df$commodity, ignore.case = TRUE), ]
  as_tibble(df)
}


# == Disbursements =============================================================

#' Natural resources disbursements by fiscal year
#'
#' Disbursements from natural resource extraction revenues to the U.S. Treasury,
#' states, local governments, tribes, and conservation funds (e.g., Land and
#' Water Conservation Fund, Historic Preservation Fund).
#'
#' @param year Integer vector of fiscal years (e.g., \code{2020:2023}).
#'   \code{NULL} returns all years.
#' @param state Character. Regex pattern for state name. Case-insensitive.
#'   \code{NULL} returns all states.
#' @param fund_type Character. Regex pattern for fund type (e.g., \code{"State"},
#'   \code{"Treasury"}, \code{"Land and Water"}). Case-insensitive. \code{NULL}
#'   returns all.
#' @return A tibble with columns:
#'   \describe{
#'     \item{fiscal_year}{Integer. Federal fiscal year (Oct-Sep).}
#'     \item{fund_type}{Character. Recipient fund classification.}
#'     \item{source}{Character. Revenue source description.}
#'     \item{state}{Character. U.S. state name.}
#'     \item{county}{Character. County name.}
#'     \item{disbursement}{Numeric. Disbursement amount in USD.}
#'   }
#' @examples
#' doi_disbursements(year = 2023, state = "Wyoming")
#' doi_disbursements(fund_type = "Land and Water")
doi_disbursements <- function(year = NULL, state = NULL, fund_type = NULL) {
  df <- .doi_fetch_csv("fiscal_year_disbursements.csv")
  df <- .doi_clean_names(df)
  df$disbursement <- suppressWarnings(as.numeric(df$disbursement))
  df$fiscal_year <- as.integer(df$fiscal_year)
  if (!is.null(year)) df <- df[df$fiscal_year %in% year, ]
  if (!is.null(state)) df <- df[grepl(state, df$state, ignore.case = TRUE), ]
  if (!is.null(fund_type)) df <- df[grepl(fund_type, df$fund_type, ignore.case = TRUE), ]
  as_tibble(df)
}


#' Natural resources disbursements by month
#'
#' Monthly disbursement data with fund type and commodity breakdown.
#' Downloads the full monthly CSV and applies optional filters.
#'
#' @param year Integer vector of years (e.g., \code{2023}). \code{NULL} returns all.
#' @param state Character. Regex pattern for state name. Case-insensitive.
#'   \code{NULL} returns all states.
#' @return A tibble with columns including date (Date), fund_type, source,
#'   state, county, commodity, and disbursement (numeric, USD).
#' @examples
#' doi_disbursements_monthly(year = 2023, state = "Texas")
doi_disbursements_monthly <- function(year = NULL, state = NULL) {
  df <- .doi_fetch_csv("monthly_disbursements.csv")
  df <- .doi_clean_names(df)
  df$date <- as.Date(df$date, format = "%m/%d/%Y")
  df$disbursement <- suppressWarnings(as.numeric(df$disbursement))
  if (!is.null(year)) df <- df[as.integer(format(df$date, "%Y")) %in% year, ]
  if (!is.null(state)) df <- df[grepl(state, df$state, ignore.case = TRUE), ]
  as_tibble(df)
}


# == Federal Sales =============================================================

#' Federal oil, gas, and NGL sales data
#'
#' Sales volumes, royalty values, and effective royalty rates for oil, gas,
#' and natural gas liquids (NGL) on federal lands and offshore areas.
#'
#' @param year Integer vector of calendar years (e.g., \code{2020:2023}).
#'   \code{NULL} returns all years.
#' @param state Character. Regex pattern for state or offshore region name.
#'   Case-insensitive. \code{NULL} returns all.
#' @param commodity Character. Regex pattern for commodity (e.g., \code{"Oil"},
#'   \code{"Gas"}, \code{"NGL"}). Case-insensitive. \code{NULL} returns all.
#' @return A tibble with columns including calendar_year, state_offshore_region,
#'   commodity, sales_volume, gas_mmbtu_volume, sales_value,
#'   royalty_value_prior_to_allowances_rvpa, transportation_allowances_ta,
#'   processing_allowances_pa, royalty_value_less_allowances_rvla, and
#'   effective_royalty_rate (all numeric where applicable).
#' @examples
#' doi_federal_sales(year = 2023, commodity = "Oil")
doi_federal_sales <- function(year = NULL, state = NULL, commodity = NULL) {
  df <- .doi_fetch_csv("federal_sales.csv")
  df <- .doi_clean_names(df)
  num_cols <- c("sales_volume", "gas_mmbtu_volume", "sales_value",
                "royalty_value_prior_to_allowances_rvpa",
                "transportation_allowances_ta", "processing_allowances_pa",
                "royalty_value_less_allowances_rvla", "effective_royalty_rate")
  for (col in num_cols) {
    if (col %in% names(df)) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df$calendar_year <- as.integer(df$calendar_year)
  if (!is.null(year)) df <- df[df$calendar_year %in% year, ]
  if (!is.null(state)) df <- df[grepl(state, df$state_offshore_region, ignore.case = TRUE), ]
  if (!is.null(commodity)) df <- df[grepl(commodity, df$commodity, ignore.case = TRUE), ]
  as_tibble(df)
}


# == Revenue by Company ========================================================

#' Federal revenue by company
#'
#' Natural resource revenue broken down by company name, revenue type,
#' and commodity for calendar years. Useful for tracking individual
#' company contributions to federal revenue.
#'
#' @param year Integer vector of calendar years (e.g., \code{2020:2023}).
#'   \code{NULL} returns all years.
#' @param company Character. Regex pattern for company name (e.g.,
#'   \code{"ExxonMobil"}, \code{"Chevron"}). Case-insensitive.
#'   \code{NULL} returns all companies.
#' @param commodity Character. Regex pattern for commodity (e.g., \code{"Oil"},
#'   \code{"Gas"}). Case-insensitive. \code{NULL} returns all.
#' @return A tibble with columns including calendar_year (integer),
#'   company_name (character), revenue_type (character), commodity (character),
#'   and revenue (numeric, USD).
#' @examples
#' doi_revenue_by_company(year = 2023, commodity = "Oil")
doi_revenue_by_company <- function(year = NULL, company = NULL, commodity = NULL) {
  df <- .doi_fetch_csv("federal_revenue_by_company.csv")
  df <- .doi_clean_names(df)
  df$revenue <- suppressWarnings(as.numeric(df$revenue))
  df$calendar_year <- as.integer(df$calendar_year)
  if (!is.null(year)) df <- df[df$calendar_year %in% year, ]
  if (!is.null(company)) df <- df[grepl(company, df$company_name, ignore.case = TRUE), ]
  if (!is.null(commodity)) df <- df[grepl(commodity, df$commodity, ignore.case = TRUE), ]
  as_tibble(df)
}


# == Context ===================================================================

#' Get doi.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
doi_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(doi_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/doi.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "doi.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# doi.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# doi.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
