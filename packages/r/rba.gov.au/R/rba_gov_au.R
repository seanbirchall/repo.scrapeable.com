# rba.gov.au.R - Self-contained rba.gov.au client



# rba-gov-au.R
# Self-contained Reserve Bank of Australia (RBA) CSV client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rba_base <- "https://www.rba.gov.au/statistics/tables/csv"

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
#' Downloads a CSV statistical table from the Reserve Bank of Australia and
#' parses it into a tidy long-format tibble. The RBA publishes hundreds of
#' time series across tables covering exchange rates, interest rates, money
#' supply, credit, and more. Each table contains multiple series that are
#' melted into long format with one observation per row.
#'
#' @param table_code Character string. RBA table code (case-insensitive).
#'   Common codes include:
#'   \itemize{
#'     \item \code{"f11"} -- Exchange rates
#'     \item \code{"f01"} -- Interest rates (cash rate)
#'     \item \code{"f13"} -- Measures of consumer price inflation
#'     \item \code{"a01"} -- RBA balance sheet
#'     \item \code{"d01"} -- Growth in financial aggregates
#'   }
#'   See \url{https://www.rba.gov.au/statistics/tables/} for the full list.
#' @return A tibble in long format with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{series_id}{Character. RBA series identifier (e.g., \code{"FXRUSD"}).}
#'     \item{series_name}{Character. Human-readable series description.}
#'     \item{value}{Numeric. Observation value.}
#'   }
#'   Returns an empty tibble if the table code is invalid.
#' @examples
#' rba_table("f11")
#' rba_table("f01")
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
#' Convenience wrapper around \code{rba_table("f11")}. Returns historical
#' exchange rates for the Australian dollar against major currencies (USD,
#' EUR, GBP, JPY, CNY, and the Trade-Weighted Index).
#'
#' @return A tibble with columns: \code{date} (Date), \code{series_id}
#'   (Character), \code{series_name} (Character), \code{value} (Numeric).
#' @seealso \code{\link{rba_table}} for arbitrary table codes.
#' @examples
#' rba_exchange_rates()
#' @export
rba_exchange_rates <- function() {
  rba_table("f11")
}

#' Fetch RBA measures of consumer price inflation (table F13)
#'
#' Convenience wrapper around \code{rba_table("f13")}. Returns quarterly
#' measures of consumer price inflation including CPI, trimmed mean, and
#' weighted median.
#'
#' @return A tibble with columns: \code{date} (Date), \code{series_id}
#'   (Character), \code{series_name} (Character), \code{value} (Numeric).
#' @seealso \code{\link{rba_table}} for arbitrary table codes.
#' @examples
#' rba_inflation()
#' @export
rba_inflation <- function() {
  rba_table("f13")
}
`%||%` <- function(x, y) if (is.null(x)) y else x

# == Context ===================================================================

#' Get rba.gov.au client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
rba_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(rba_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/rba.gov.au.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "rba.gov.au")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# rba.gov.au context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# rba.gov.au", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
