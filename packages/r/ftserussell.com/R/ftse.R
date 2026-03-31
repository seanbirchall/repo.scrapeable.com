# == Russell Index Values ======================================================

#' List available Russell indexes
#'
#' Fetches the full inventory of Russell indexes with download links.
#' Returns 131 indexes across 17 families.
#'
#' @param family Optional family name to filter by (e.g. "Russell U.S.")
#' @return tibble: family (character), index (character),
#'   historical_url (character), ytd_url (character)
#' @export
ftse_russell_indexes <- function(family = NULL) {
  raw <- .fetch_json(.russell_inventory_url)
  d <- raw$Data
  if (is.null(d) || length(d) == 0) return(.schema_russell_inventory)

  result <- tibble(
    family         = as.character(d$family),
    index          = as.character(d$index),
    historical_url = as.character(d$historical_url),
    ytd_url        = as.character(d$ytd_url)
  )

  if (!is.null(family)) {
    result <- result |> filter(grepl(family, .data$family, ignore.case = TRUE))
  }
  result
}


#' Fetch Russell index values (year-to-date)
#'
#' Downloads YTD daily index values for a Russell index.
#' Data includes price return and total return values in USD.
#'
#' @param code Russell index code (e.g. "US2000" for Russell 2000,
#'   "US1000" for Russell 1000, "US3000" for Russell 3000).
#'   Use ftse_russell_indexes() to find codes from URLs.
#' @return tibble: date (Date), index_name (character),
#'   price_return (numeric), total_return (numeric)
#' @export
ftse_russell_ytd <- function(code) {
  url <- sprintf(
    "https://research.ftserussell.com/products/russell-index-values/home/getfile?id=valuesytd_%s.csv",
    code
  )
  .parse_russell_csv(url)
}


#' Fetch Russell index values (full history)
#'
#' Downloads full historical daily index values for a Russell index.
#' Typically covers ~20 years of daily data.
#'
#' @param code Russell index code (e.g. "US2000", "US1000", "US3000")
#' @return tibble: date (Date), index_name (character),
#'   price_return (numeric), total_return (numeric)
#' @export
ftse_russell_history <- function(code) {
  url <- sprintf(
    "https://research.ftserussell.com/products/russell-index-values/home/getfile?id=valueshist_%s.csv",
    code
  )
  .parse_russell_csv(url)
}


#' Fetch Russell index values for multiple indexes
#'
#' Downloads and combines data for multiple Russell indexes.
#'
#' @param codes Character vector of Russell index codes
#' @param period "ytd" or "history" (default "ytd")
#' @param sleep Seconds between requests (default 0.5)
#' @return tibble: date, index_name, price_return, total_return
#' @export
ftse_russell_bulk <- function(codes, period = "ytd", sleep = 0.5) {
  fn <- if (period == "history") ftse_russell_history else ftse_russell_ytd
  results <- lapply(seq_along(codes), function(i) {
    if (i > 1) Sys.sleep(sleep)
    message(sprintf("[%d/%d] %s", i, length(codes), codes[i]))
    tryCatch(fn(codes[i]), error = function(e) {
      message("  Failed: ", e$message)
      NULL
    })
  })
  bind_rows(results)
}


# == FTSE Historic Index Values ================================================

#' List available FTSE historic indexes
#'
#' Fetches the inventory of FTSE historic index data (~397 indexes).
#' Includes global equity, real estate, UK, fixed income, and more.
#'
#' @param group Optional group name to filter by (e.g. "FTSE Global Equity")
#' @param currency Optional currency code to filter by (e.g. "USD", "GBP")
#' @return tibble: group (character), index_name (character),
#'   index_id (character), currency (character), url (character)
#' @export
ftse_historic_indexes <- function(group = NULL, currency = NULL) {
  raw <- .fetch_json(.ftse_inventory_url)
  d <- raw$Data
  if (is.null(d) || length(d) == 0) return(.schema_ftse_inventory)

  result <- tibble(
    group      = as.character(d$GroupName),
    index_name = as.character(d$IndexName),
    index_id   = as.character(d$IndexId),
    currency   = as.character(d$Currency),
    url        = as.character(d$TotalReturnCapitalUrl)
  )

  if (!is.null(group)) {
    result <- result |> filter(grepl(group, .data$group, ignore.case = TRUE))
  }
  if (!is.null(currency)) {
    result <- result |> filter(.data$currency == !!currency)
  }
  result
}


#' Fetch FTSE historic index values from XLSX
#'
#' Downloads and parses an FTSE historic index XLSX file.
#' Returns daily or quarterly index values depending on the source.
#'
#' @param index_id FTSE index ID (e.g. "AWORLDS" for FTSE All-World)
#' @param currency Currency code (default "USD")
#' @param url Optional: provide the full download URL directly
#'   (overrides index_id and currency). Get URLs from ftse_historic_indexes().
#' @return tibble: date (Date), index_name (character),
#'   index_id (character), value (numeric)
#' @export
ftse_historic_values <- function(index_id = NULL, currency = "USD", url = NULL) {
  if (is.null(url)) {
    if (is.null(index_id)) stop("Provide either index_id or url")
    # Look up URL from inventory
    inv <- ftse_historic_indexes()
    match <- inv |> filter(.data$index_id == !!index_id, .data$currency == !!currency)
    if (nrow(match) == 0) {
      warning("No FTSE index found for id='", index_id, "', currency='", currency, "'")
      return(.schema_ftse_values)
    }
    url <- match$url[1]
    idx_name <- match$index_name[1]
  } else {
    idx_name <- NA_character_
    index_id <- NA_character_
  }

  xlsx_path <- tryCatch(.fetch_xlsx(url), error = function(e) {
    warning("Failed to fetch FTSE XLSX: ", e$message)
    return(NULL)
  })
  if (is.null(xlsx_path)) return(.schema_ftse_values)

  # Read sheets - data is in the first sheet
  sheets <- readxl::excel_sheets(xlsx_path)
  # First sheet is data (named after IndexId)
  df <- tryCatch(
    readxl::read_excel(xlsx_path, sheet = 1, skip = 4, col_names = FALSE),
    error = function(e) {
      warning("Failed to parse FTSE XLSX: ", e$message)
      return(NULL)
    }
  )
  if (is.null(df) || nrow(df) == 0) return(.schema_ftse_values)

  # First column is date (Excel serial number), second is index value
  # Skip header row if present
  df <- df |> filter(!is.na(df[[1]]))

  # Parse dates - readxl typically returns POSIXct for date columns
  date_col <- df[[1]]
  if (inherits(date_col, "POSIXct") || inherits(date_col, "Date")) {
    dates <- as.Date(date_col)
  } else if (is.numeric(date_col)) {
    dates <- as.Date(date_col, origin = "1899-12-30")
  } else {
    # Text column - try numeric serial then string parse
    num_vals <- suppressWarnings(as.numeric(as.character(date_col)))
    if (all(!is.na(num_vals[!is.na(date_col)]))) {
      dates <- as.Date(num_vals, origin = "1899-12-30")
    } else {
      dates <- as.Date(as.character(date_col))
    }
  }

  # Value is second column (Capital Return or Index Value)
  values <- suppressWarnings(as.numeric(df[[2]]))

  result <- tibble(
    date       = dates,
    index_name = idx_name,
    index_id   = as.character(index_id),
    value      = values
  ) |>
    filter(!is.na(date), !is.na(value)) |>
    arrange(date)

  result
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the ftserussell package
#'
#' @return Character string (invisibly), also printed
#' @export
ftse_context <- function() {
  .build_context("ftserussell.com", header_lines = c(
    "# ftserussell.com - FTSE Russell Index Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble, readxl",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Two data sources:",
    "#   1. Russell indexes (CSV): ~131 indexes, daily price & total return",
    "#      Common codes: US2000 (Russell 2000), US1000 (Russell 1000), US3000 (Russell 3000)",
    "#   2. FTSE historic indexes (XLSX): ~397 indexes, daily/quarterly values",
    "#      Groups: Global Equity, UK, Real Estate, Fixed Income, etc.",
    "#",
    "# Russell functions: ftse_russell_indexes, ftse_russell_ytd, ftse_russell_history, ftse_russell_bulk",
    "# FTSE functions: ftse_historic_indexes, ftse_historic_values"
  ))
}
