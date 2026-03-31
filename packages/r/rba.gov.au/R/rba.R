
# == Table data ================================================================

#' Fetch an RBA statistical table by table code
#'
#' @param table_code RBA table code (e.g. "f11" for exchange rates,
#'   "f01" for interest rates, "a01" for RBA balance sheet,
#'   "d01" for growth in financial aggregates)
#' @return tibble: date (Date), series_id (character), series_name (character),
#'   value (numeric)
#' @export
rba_table <- function(table_code) {
  url <- sprintf("%s/%s-data.csv", .rba_base, tolower(table_code))
  f <- .fetch(url, ext = ".csv")
  lines <- readLines(f, warn = FALSE)

  # RBA CSV format: first rows are metadata (Series ID, Description, Frequency, etc.)
  # Then data rows with date in first column
  # Find the "Series ID" row to get column identifiers
  sid_idx <- which(grepl("^Series ID", lines))
  title_idx <- which(grepl("^Title", lines))

  if (length(sid_idx) == 0) {
    # Simple CSV — try direct read
    df <- utils::read.csv(f, stringsAsFactors = FALSE)
    if (nrow(df) == 0) return(.schema_table)
    # Melt to long format
    date_col <- names(df)[1]
    value_cols <- setdiff(names(df), date_col)
    result <- lapply(value_cols, function(col) {
      tibble(
        date = tryCatch(as.Date(df[[date_col]]), error = function(e) {
          tryCatch(as.Date(df[[date_col]], format = "%d-%b-%Y"), error = function(e2) {
            as.Date(rep(NA, nrow(df)))
          })
        }),
        series_id = col,
        series_name = col,
        value = suppressWarnings(as.numeric(df[[col]]))
      )
    })
    return(bind_rows(result))
  }

  # Parse metadata rows
  sid_parts <- strsplit(lines[sid_idx[1]], ",")[[1]]
  title_parts <- if (length(title_idx) > 0) strsplit(lines[title_idx[1]], ",")[[1]] else sid_parts
  series_ids <- trimws(sid_parts[-1])
  series_names <- trimws(title_parts[-1])

  # Find where data starts (after all metadata rows)
  data_start <- max(sid_idx[1], title_idx[1]) + 1
  # Skip rows that are still metadata
  while (data_start <= length(lines) && grepl("^(Description|Frequency|Type|Units|Source|Publication|Last)", lines[data_start])) {
    data_start <- data_start + 1
  }

  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]
  if (length(data_lines) == 0) return(.schema_table)

  tc <- textConnection(data_lines)
  on.exit(close(tc))
  df <- tryCatch(utils::read.csv(tc, header = FALSE, stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_table)

  # First column is date
  n_series <- min(ncol(df) - 1, length(series_ids))
  result <- lapply(seq_len(n_series), function(i) {
    tibble(
      date = tryCatch(as.Date(df[[1]]), error = function(e) {
        tryCatch(as.Date(df[[1]], format = "%d-%b-%Y"), error = function(e2) {
          as.Date(rep(NA, nrow(df)))
        })
      }),
      series_id = series_ids[i],
      series_name = if (i <= length(series_names)) series_names[i] else series_ids[i],
      value = suppressWarnings(as.numeric(df[[i + 1]]))
    )
  })
  bind_rows(result)
}

#' Fetch RBA exchange rate data (table F11)
#'
#' Convenience wrapper around rba_table("f11") for exchange rates.
#' @return tibble: date, series_id, series_name, value
#' @export
rba_exchange_rates <- function() {
  rba_table("f11")
}

#' Fetch RBA interest rate data (table F01)
#'
#' Convenience wrapper around rba_table("f01") for interest rates.
#' @return tibble: date, series_id, series_name, value
#' @export
rba_interest_rates <- function() {
  rba_table("f01")
}

# == Context ===================================================================

#' Show RBA API context for LLMs
#'
#' Displays package overview, common table codes, and function signatures.
#' @return Invisibly returns the context string
#' @export
rba_context <- function() {
  .build_context(
    "rba.gov.au",
    header_lines = c(
      "# rba.gov.au",
      "# Reserve Bank of Australia statistical tables client",
      "# Auth: none required",
      "# Format: CSV download",
      "#",
      "# Common tables: f11 (exchange rates), f01 (interest rates),",
      "#   a01 (RBA balance sheet), d01 (financial aggregates),",
      "#   f06 (term deposit rates), f17 (real TWI)"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
