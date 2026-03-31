# ftserussell-com.R
# Self-contained FTSE Russell index data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none required
# Data sources: research.ftserussell.com (CSV for Russell, XLSX for FTSE)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.russell_inventory_url <- "https://www.lseg.com/content/lseg/en_us/ftse-russell/index-resources/russell-index-values/jcr:content/root/container/page_content_region/page-content-region/section/section-fw/data_table.russellindexvalues.json"

.ftse_inventory_url <- "https://www.lseg.com/content/lseg/en_us/ftse-russell/index-resources/historic-index-values/jcr:content/root/container/page_content_region/page-content-region/section/section-fw/data_table.historicindex.json"

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
    j <- fi - 1
    rox_start <- fi
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

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

.fetch_csv <- function(url) {
  tmp <- .fetch(url, ext = ".csv")
  utils::read.csv(tmp, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                  check.names = FALSE)
}

.fetch_xlsx <- function(url) {
  tmp <- .fetch(url, ext = ".xlsx")
  tmp
}

# == Schemas ===================================================================

.schema_russell_inventory <- tibble(
  family = character(), index = character(),
  historical_url = character(), ytd_url = character()
)

.schema_russell_values <- tibble(
  date = as.Date(character()), index_name = character(),
  price_return = numeric(), total_return = numeric()
)

.schema_ftse_inventory <- tibble(
  group = character(), index_name = character(), index_id = character(),
  currency = character(), url = character()
)

.schema_ftse_values <- tibble(
  date = as.Date(character()), index_name = character(),
  index_id = character(), value = numeric()
)

# == Russell Index Values ======================================================

#' List available Russell indexes
#'
#' Fetches the full inventory of Russell indexes with download links.
#' Returns 131 indexes across 17 families.
#'
#' @param family Optional family name to filter by (e.g. "Russell U.S.")
#' @return tibble: family (character), index (character),
#'   historical_url (character), ytd_url (character)
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


# -- Russell CSV parser (private) ----------------------------------------------

.parse_russell_csv <- function(url) {
  df <- tryCatch(.fetch_csv(url), error = function(e) {
    warning("Failed to fetch Russell CSV: ", e$message)
    return(NULL)
  })
  if (is.null(df) || nrow(df) == 0) return(.schema_russell_values)

  # Column names may have underscores for spaces and currency suffix
  # Normalize: find columns by pattern
  cols <- names(df)
  date_col <- cols[grepl("Date", cols, ignore.case = TRUE)][1]
  name_col <- cols[grepl("Index.?Name|Index_Name", cols, ignore.case = TRUE)][1]
  price_col <- cols[grepl("Without.?Dividends|Without_Dividends", cols, ignore.case = TRUE)][1]
  total_col <- cols[grepl("With.?Dividends|With_Dividends", cols, ignore.case = TRUE)][1]

  if (is.na(date_col) || is.na(price_col)) {
    warning("Unexpected CSV column structure: ", paste(cols, collapse = ", "))
    return(.schema_russell_values)
  }

  result <- tibble(
    date         = as.Date(df[[date_col]], format = "%m/%d/%Y"),
    index_name   = if (!is.na(name_col)) as.character(df[[name_col]]) else NA_character_,
    price_return = as.numeric(df[[price_col]]),
    total_return = if (!is.na(total_col)) as.numeric(df[[total_col]]) else NA_real_
  ) |>
    filter(!is.na(date)) |>
    arrange(date)

  result
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
