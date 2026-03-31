# rba-gov-au.R
# Self-contained Reserve Bank of Australia (RBA) CSV client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rba_base <- "https://www.rba.gov.au/statistics/tables/csv"

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

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.parse_rba_csv <- function(file) {
  lines <- readLines(file, warn = FALSE)
  # RBA CSVs have metadata header rows; find the actual data header
  # Usually "Series ID" row or the first row that looks like column headers
  header_idx <- which(grepl("^Series ID", lines))
  if (length(header_idx) == 0) {
    # Try to find first line with multiple commas (data header)
    header_idx <- which(grepl("^Title,", lines) | grepl("^\"Title\"", lines))
  }
  if (length(header_idx) == 0) {
    # Fallback: skip lines starting with common metadata prefixes
    skip <- sum(grepl("^(Title|Description|Frequency|Type|Units|Source|Publication)", lines[1:min(20, length(lines))]))
    if (skip == 0) skip <- 0
    return(utils::read.csv(file, stringsAsFactors = FALSE, skip = skip))
  }
  # Read from header_idx
  data_lines <- lines[header_idx[1]:length(lines)]
  tc <- textConnection(data_lines)
  on.exit(close(tc))
  utils::read.csv(tc, stringsAsFactors = FALSE)
}

# == Schemas ===================================================================

.schema_table <- tibble(
  date = as.Date(character()), series_id = character(),
  series_name = character(), value = numeric()
)

# == Table data ================================================================

#' Fetch an RBA statistical table by table code
#'
#' @param table_code RBA table code (e.g. "f11" for exchange rates,
#'   "f01" for interest rates, "a01" for RBA balance sheet,
#'   "d01" for growth in financial aggregates)
#' @return tibble: date (Date), series_id (character), series_name (character),
#'   value (numeric)
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
rba_exchange_rates <- function() {
  rba_table("f11")
}

#' Fetch RBA interest rate data (table F01)
#'
#' Convenience wrapper around rba_table("f01") for interest rates.
#' @return tibble: date, series_id, series_name, value
rba_interest_rates <- function() {
  rba_table("f01")
}

# == Context ===================================================================

#' Show RBA API context for LLMs
#'
#' Displays package overview, common table codes, and function signatures.
#' @return Invisibly returns the context string
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
