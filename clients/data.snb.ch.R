# data.snb.ch.R - Self-contained data.snb.ch client

library(httr2)
library(tibble)
library(dplyr)
library(tidyr)


# data-snb-ch.R
# Self-contained Swiss National Bank (SNB) data portal client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.snb_base <- "https://data.snb.ch/api/cube"

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_snb_csv <- function(url) {
  f <- .fetch(url, ext = ".csv")
  lines <- readLines(f, warn = FALSE)
  # SNB CSVs have metadata header lines (quoted); find the header row with "Date"
  header_idx <- which(grepl("^\"Date\"", lines) | grepl("^Date;", lines))
  if (length(header_idx) == 0) {
    return(utils::read.csv(f, stringsAsFactors = FALSE, sep = ";"))
  }
  data_lines <- lines[header_idx[1]:length(lines)]
  # Remove empty trailing lines
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]
  tc <- textConnection(data_lines)
  on.exit(close(tc))
  utils::read.csv(tc, stringsAsFactors = FALSE, sep = ";", quote = "\"")
}

# == Schemas ===================================================================

.schema_data <- tibble(
  date = character(), variable = character(), value = numeric()
)

.schema_dimensions <- tibble(
  dimension = character(), code = character(), label = character()
)


# == Data =====================================================================

#' Fetch data from an SNB data cube
#'
#' Downloads time-series data from the Swiss National Bank's statistical data
#' portal. Data cubes cover interest rates, exchange rates, balance of
#' payments, monetary aggregates, and other Swiss financial statistics.
#' The raw CSV is parsed into a long-format tibble.
#'
#' @param cube_id Character. Cube identifier. Common cubes:
#'   \itemize{
#'     \item "rendopar" -- Interest rates (Nelson-Siegel parameters)
#'     \item "devkum" -- Exchange rates
#'     \item "snbbipo" -- Balance of payments
#'     \item "snbmonagg" -- Monetary aggregates
#'     \item "snboffzisa" -- Official interest rates
#'   }
#' @param lang Character. Language: "en" (default), "de", "fr", "it".
#' @return A tibble in long format with columns:
#' \describe{
#'   \item{date}{Character. Date string as provided by the SNB (e.g. "1988-01-04", "2024-Q1").}
#'   \item{variable}{Character. Pipe-separated dimension labels identifying the series (e.g. "b0", "b1", "t1").}
#'   \item{value}{Numeric. The observed value.}
#' }
#' @export
#' @examples
#' \dontrun{
#' rates <- snb_data("rendopar")
#' fx    <- snb_data("devkum")
#' }
snb_data <- function(cube_id, lang = "en") {
  url <- sprintf("%s/%s/data/csv/%s", .snb_base, cube_id, lang)
  df <- tryCatch(.fetch_snb_csv(url), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_data)
  df <- as_tibble(df)
  # SNB format: Date, D0, D1, ..., Value  (dimension columns + Value)
  # Build a variable label from dimension columns
  date_col <- intersect(c("Date", "date", "DATE"), names(df))
  value_col <- intersect(c("Value", "value"), names(df))
  if (length(date_col) > 0 && length(value_col) > 0) {
    dim_cols <- setdiff(names(df), c(date_col[1], value_col[1]))
    var_label <- if (length(dim_cols) > 0) {
      apply(df[, dim_cols, drop = FALSE], 1, paste, collapse = "|")
    } else {
      rep(cube_id, nrow(df))
    }
    return(tibble(
      date = as.character(df[[date_col[1]]]),
      variable = as.character(var_label),
      value = suppressWarnings(as.numeric(df[[value_col[1]]]))
    ))
  }
  # Fallback: melt non-date columns
  non_date <- setdiff(names(df), date_col)
  if (length(date_col) > 0 && length(non_date) > 0) {
    result <- lapply(non_date, function(col) {
      tibble(
        date = as.character(df[[date_col[1]]]),
        variable = col,
        value = suppressWarnings(as.numeric(df[[col]]))
      )
    })
    return(bind_rows(result))
  }
  df
}

# == Dimensions ================================================================

#' Fetch dimension metadata for an SNB data cube
#'
#' Returns the dimension structure and valid codes for a given data cube.
#' Useful for understanding which variables and categories are available
#' before querying with \code{snb_data()}.
#'
#' @param cube_id Character. Cube identifier (e.g. "snbbipo", "rendopar", "devkum").
#' @param lang Character. Language: "en" (default), "de", "fr", "it".
#' @return A tibble with columns:
#' \describe{
#'   \item{dimension}{Character. Dimension name (e.g. "D0", "D1"). May be NA if not labeled.}
#'   \item{code}{Character. Code for the dimension value. May be NA if not labeled.}
#'   \item{label}{Character. Human-readable label for the code. May be NA if not labeled.}
#' }
#' @export
#' @examples
#' \dontrun{
#' snb_dimensions("rendopar")
#' snb_dimensions("devkum", lang = "de")
#' }
snb_dimensions <- function(cube_id, lang = "en") {
  url <- sprintf("%s/%s/dimensions/csv/%s", .snb_base, cube_id, lang)
  df <- tryCatch({
    f <- .fetch(url, ext = ".csv")
    lines <- readLines(f, warn = FALSE)
    # SNB dimension CSV can be semicolon-separated
    utils::read.csv(f, stringsAsFactors = FALSE, sep = ";")
  }, error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_dimensions)
  df <- as_tibble(df)
  names(df) <- tolower(names(df))
  dim_col <- intersect(c("dimension", "dim", "dimname"), names(df))
  code_col <- intersect(c("code", "key", "id"), names(df))
  label_col <- intersect(c("label", "name", "description", "value"), names(df))
  tibble(
    dimension = if (length(dim_col) > 0) as.character(df[[dim_col[1]]]) else NA_character_,
    code = if (length(code_col) > 0) as.character(df[[code_col[1]]]) else NA_character_,
    label = if (length(label_col) > 0) as.character(df[[label_col[1]]]) else NA_character_
  )
}
`%||%` <- function(x, y) if (is.null(x)) y else x

# == Context ===================================================================

#' Get data.snb.ch client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
snb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(snb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.snb.ch.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.snb.ch")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.snb.ch context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.snb.ch", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
