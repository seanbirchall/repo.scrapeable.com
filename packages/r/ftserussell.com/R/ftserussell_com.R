# ftserussell.com.R - Self-contained ftserussell.com client




# ftserussell-com.R
# Self-contained FTSE Russell index data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none required
# Data sources: research.ftserussell.com (CSV for Russell, XLSX for FTSE)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.russell_inventory_url <- "https://www.lseg.com/content/lseg/en_us/ftse-russell/index-resources/russell-index-values/jcr:content/root/container/page_content_region/page-content-region/section/section-fw/data_table.russellindexvalues.json"

.ftse_inventory_url <- "https://www.lseg.com/content/lseg/en_us/ftse-russell/index-resources/historic-index-values/jcr:content/root/container/page_content_region/page-content-region/section/section-fw/data_table.historicindex.json"
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

# == Russell Index Values ======================================================

#' List available Russell indexes
#'
#' Fetches the full inventory of Russell indexes with download links from
#' LSEG (London Stock Exchange Group). Returns approximately 131 indexes
#' across 17 families including U.S., global, and RAFI indexes.
#'
#' @param family Optional family name to filter by (case-insensitive regex,
#'   e.g. "Russell U.S.", "Russell RAFI", "Russell Global").
#' @return A tibble with columns:
#'   \describe{
#'     \item{family}{Index family (e.g. "Russell U.S.")}
#'     \item{index}{Index name (e.g. "Russell 2000")}
#'     \item{historical_url}{URL for full historical data CSV}
#'     \item{ytd_url}{URL for year-to-date data CSV}
#'   }
#' @examples
#' # All Russell indexes
#' ftse_russell_indexes()
#'
#' # Filter to U.S. indexes only
#' ftse_russell_indexes(family = "Russell U.S.")
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
#' Downloads YTD daily index values for a Russell index from
#' research.ftserussell.com. Data includes both price return and total
#' return (with dividends) values in USD.
#'
#' @param code Russell index code (e.g. "US2000" for Russell 2000,
#'   "US1000" for Russell 1000, "US3000" for Russell 3000).
#'   Use \code{\link{ftse_russell_indexes}} to find codes from URLs.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Trading date}
#'     \item{index_name}{Index name}
#'     \item{price_return}{Price return index level (without dividends)}
#'     \item{total_return}{Total return index level (with dividends)}
#'   }
#' @examples
#' \dontrun{
#' # Russell 2000 year-to-date
#' ftse_russell_ytd("US2000")
#' }
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
#' Typically covers approximately 20 years of daily data. Large download.
#'
#' @param code Russell index code (e.g. "US2000", "US1000", "US3000").
#' @return A tibble with columns: date, index_name, price_return, total_return
#'   (same as \code{\link{ftse_russell_ytd}}).
#' @examples
#' \dontrun{
#' # Full history for the Russell 1000
#' ftse_russell_history("US1000")
#' }
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
#' Downloads and row-binds data for multiple Russell indexes sequentially.
#' Progress messages are printed to the console.
#'
#' @param codes Character vector of Russell index codes (e.g.
#'   \code{c("US1000", "US2000", "US3000")}).
#' @param period Either "ytd" (year-to-date, default) or "history"
#'   (full historical data).
#' @param sleep Seconds to pause between requests (default 0.5).
#' @return A tibble with columns: date, index_name, price_return, total_return
#'   -- rows stacked across all requested indexes.
#' @examples
#' \dontrun{
#' ftse_russell_bulk(c("US1000", "US2000"), period = "ytd")
#' }
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
#' Fetches the inventory of FTSE historic index data from LSEG. Approximately
#' 397 indexes across global equity, real estate, UK, fixed income, and more.
#' Each entry includes a download URL for an XLSX file.
#'
#' @param group Optional group name to filter by (case-insensitive regex,
#'   e.g. "FTSE Global Equity", "UK").
#' @param currency Optional currency code to filter by (e.g. "USD", "GBP",
#'   "EUR").
#' @return A tibble with columns:
#'   \describe{
#'     \item{group}{Index group (e.g. "FTSE Global Equity Index Series")}
#'     \item{index_name}{Index name (e.g. "FTSE All-World")}
#'     \item{index_id}{Index identifier (e.g. "AWORLDS")}
#'     \item{currency}{Currency of the index values}
#'     \item{url}{Download URL for XLSX data}
#'   }
#' @examples
#' # All FTSE historic indexes
#' ftse_historic_indexes()
#'
#' # Filter to USD global equity indexes
#' ftse_historic_indexes(group = "Global Equity", currency = "USD")
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
#' Downloads and parses an FTSE historic index XLSX file from LSEG.
#' Returns daily or quarterly index values depending on the source.
#' Handles Excel date formats and multiple column layouts.
#'
#' @param index_id FTSE index ID (e.g. "AWORLDS" for FTSE All-World).
#'   Use \code{\link{ftse_historic_indexes}} to browse available IDs.
#' @param currency Currency code (default "USD"). Must match an available
#'   currency for the given index_id.
#' @param url Optional: provide the full download URL directly (overrides
#'   index_id and currency). Get URLs from \code{\link{ftse_historic_indexes}}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Observation date}
#'     \item{index_name}{Index name}
#'     \item{index_id}{Index identifier}
#'     \item{value}{Index value (capital return)}
#'   }
#' @examples
#' \dontrun{
#' # FTSE All-World index in USD
#' ftse_historic_values("AWORLDS", currency = "USD")
#' }
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


# == Context ===================================================================

#' Get ftserussell.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ftse_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ftse_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ftserussell.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ftserussell.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ftserussell.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ftserussell.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
