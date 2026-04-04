# doi.gov.R
# Self-contained U.S. Department of the Interior — Natural Resources Revenue Data client.
# Data source: Office of Natural Resources Revenue (ONRR) via revenuedata.doi.gov
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public CSV downloads)
# Docs: https://revenuedata.doi.gov/downloads/

library(dplyr, warn.conflicts = FALSE)
library(tibble)

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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
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
#' Returns a tibble describing each downloadable CSV dataset from
#' the Office of Natural Resources Revenue (ONRR).
#'
#' @return tibble with dataset name, description, frequency, and download URL
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
#'
#' @param query Character string to search for (case-insensitive)
#' @return tibble of matching datasets
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
#'
#' @param year Integer vector of calendar years to filter. NULL = all.
#' @param state Regex pattern for state name. NULL = all.
#' @param commodity Regex pattern for commodity. NULL = all.
#' @return tibble with columns: calendar_year, land_class, land_category,
#'   state, county, fips_code, offshore_region, revenue_type,
#'   mineral_lease_type, commodity, product, revenue
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
#'
#' @param year Integer vector of years to filter. NULL = all.
#' @param state Regex pattern for state name. NULL = all.
#' @param commodity Regex pattern for commodity. NULL = all.
#' @return tibble with date column and revenue details
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
#' Commodity production volumes from federal lands, offshore, and
#' Native American lands by year.
#'
#' @param year Integer vector of calendar years. NULL = all.
#' @param state Regex pattern for state. NULL = all.
#' @param product Regex pattern for product (e.g. "Oil", "Gas", "Coal"). NULL = all.
#' @return tibble with columns: calendar_year, land_class, land_category,
#'   state, county, fips_code, offshore_region, product, volume
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
#' Monthly production volumes for oil, gas, and coal.
#'
#' @param year Integer vector of years. NULL = all.
#' @param commodity Regex pattern for commodity. NULL = all.
#' @return tibble with date, land_class, land_category, commodity, volume
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
#' Disbursements from natural resource extraction to Treasury,
#' states, local governments, tribes, and conservation funds.
#'
#' @param year Integer vector of fiscal years. NULL = all.
#' @param state Regex pattern for state. NULL = all.
#' @param fund_type Regex pattern for fund type (e.g. "State", "Treasury",
#'   "Land and Water"). NULL = all.
#' @return tibble with columns: fiscal_year, fund_type, source, state,
#'   county, disbursement
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
#'
#' @param year Integer vector of years. NULL = all.
#' @param state Regex pattern for state. NULL = all.
#' @return tibble with monthly disbursement details
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
#' Sales volumes, royalty values, and effective royalty rates
#' for oil, gas, and natural gas liquids on federal lands.
#'
#' @param year Integer vector of calendar years. NULL = all.
#' @param state Regex pattern for state/offshore region. NULL = all.
#' @param commodity Regex pattern for commodity (e.g. "Oil", "Gas"). NULL = all.
#' @return tibble with sales volumes, values, and royalty information
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
#' and commodity for calendar years.
#'
#' @param year Integer vector of calendar years. NULL = all.
#' @param company Regex pattern for company name. NULL = all.
#' @param commodity Regex pattern for commodity. NULL = all.
#' @return tibble with calendar_year, company_name, revenue_type, commodity, revenue
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

#' Show DOI Revenue Data client context
#'
#' Returns function signatures and documentation for this client.
#' Useful for LLM tool integration and discoverability.
#'
#' @return Character string of function signatures (printed and returned invisibly)
doi_context <- function() {
  src <- this_src_file()
  .build_context(
    pkg_name = "doi.gov",
    src_file = src,
    header_lines = c(
      "# DOI Natural Resources Revenue Data Client",
      "# Source: revenuedata.doi.gov (Office of Natural Resources Revenue)",
      "# Datasets: revenue, production, disbursements, federal sales, revenue by company",
      "# Temporal: calendar year, fiscal year, monthly",
      "# No authentication required"
    )
  )
}

this_src_file <- function() {
  # Try inst/source first (installed package), then this file
  pkg_src <- system.file("source", "doi.R", package = "doi.gov")
  if (pkg_src != "") return(pkg_src)
  # Fallback: sys.frame
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)
    fn <- get0("ofile", envir = f, ifnotFound = NULL)
    if (!is.null(fn)) return(fn)
  }
  NULL
}
