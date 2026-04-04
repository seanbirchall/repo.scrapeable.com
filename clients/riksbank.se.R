# riksbank.se.R - Self-contained riksbank.se client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# riksbank-se.R
# Self-contained Riksbank (Swedish central bank) SWEA API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.riksbank_base <- "https://api.riksbank.se/swea/v1"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_observations <- tibble(
  date = as.Date(character()), value = numeric(), series = character()
)

.schema_series <- tibble(
  seriesid = character(), name = character(), description = character()
)


# == Observations ==============================================================

#' Fetch observations for a Riksbank SWEA series
#'
#' Retrieves time-series observations from the Riksbank (Swedish central bank)
#' SWEA API for a given series. Returns daily or periodic values between the
#' specified date range. Common series include exchange rates, policy rates,
#' and monetary aggregates.
#'
#' @param series Character string. SWEA series ID. Common examples:
#'   \itemize{
#'     \item \code{"SEKUSDPMI"} -- SEK/USD exchange rate
#'     \item \code{"SEKEURPMI"} -- SEK/EUR exchange rate
#'     \item \code{"SECBREPOEFF"} -- Repo rate (policy rate)
#'   }
#'   Use \code{\link{riksbank_series}} to discover available series IDs.
#' @param from Character string. Start date in \code{"YYYY-MM-DD"} format
#'   (default \code{"2024-01-01"}).
#' @param to Character string. End date in \code{"YYYY-MM-DD"} format
#'   (default: today).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{value}{Numeric. Observation value.}
#'     \item{series}{Character. The series ID (echoed back for convenience).}
#'   }
#'   Returns an empty tibble if the series is not found or the date range
#'   contains no data.
#' @examples
#' riksbank_observations("SEKUSDPMI", from = "2024-01-01")
#' @export
riksbank_observations <- function(series, from = "2024-01-01",
                                  to = format(Sys.Date(), "%Y-%m-%d")) {
  url <- sprintf("%s/Observations/%s/%s/%s", .riksbank_base, series, from, to)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_observations)
  # API returns a list of objects with date and value
  if (is.data.frame(raw)) {
    df <- as_tibble(raw)
  } else {
    df <- bind_rows(lapply(raw, as_tibble))
  }
  date_col <- intersect(c("date", "Date", "period"), names(df))
  value_col <- intersect(c("value", "Value", "average"), names(df))
  if (length(date_col) == 0 || length(value_col) == 0) return(.schema_observations)
  tibble(
    date = as.Date(df[[date_col[1]]]),
    value = suppressWarnings(as.numeric(df[[value_col[1]]])),
    series = series
  )
}

# == Series listing ============================================================

#' List all available Riksbank SWEA series
#'
#' Returns a catalogue of all time-series available from the Riksbank SWEA
#' API, including series identifiers and descriptions. Use the \code{seriesid}
#' column values with \code{\link{riksbank_observations}} to fetch data.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{seriesid}{Character. Series identifier (e.g., \code{"SEKUSDPMI"}).}
#'     \item{name}{Character. Human-readable series name (may be \code{NA}).}
#'     \item{description}{Character. Source or description text.}
#'   }
#' @seealso \code{\link{riksbank_observations}} to fetch data for a series.
#' @examples
#' riksbank_series()
#' @export
riksbank_series <- function() {
  url <- sprintf("%s/Series", .riksbank_base)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_series)
  if (is.data.frame(raw)) {
    df <- as_tibble(raw)
  } else {
    df <- bind_rows(lapply(raw, as_tibble))
  }
  # Normalize column names
  names(df) <- tolower(names(df))
  id_col <- intersect(c("seriesid", "id", "series_id", "key"), names(df))
  name_col <- intersect(c("name", "seriesname", "title", "longnameen"), names(df))
  desc_col <- intersect(c("description", "descriptionen", "source"), names(df))
  tibble(
    seriesid = if (length(id_col) > 0) as.character(df[[id_col[1]]]) else NA_character_,
    name = if (length(name_col) > 0) as.character(df[[name_col[1]]]) else NA_character_,
    description = if (length(desc_col) > 0) as.character(df[[desc_col[1]]]) else NA_character_
  )
}
`%||%` <- function(x, y) if (is.null(x)) y else x

# == Context ===================================================================

#' Get riksbank.se client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
riksbank_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(riksbank_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/riksbank.se.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "riksbank.se")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# riksbank.se context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# riksbank.se", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
