# droughtmonitor.unl.edu.R - Self-contained droughtmonitor.unl.edu client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# droughtmonitor-unl-edu.R
# Self-contained US Drought Monitor client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known
# Note: API returns CSV format. Uses FIPS codes for states.
#   statisticsType=2 returns percent of area.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.drought_base <- "https://usdmdataservices.unl.edu/api"

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url, ext = ".json"))

`%||%` <- function(a, b) if (is.null(a)) b else a

# State FIPS lookup
.state_fips <- c(
  AL="01", AK="02", AZ="04", AR="05", CA="06", CO="08", CT="09", DE="10",
  FL="12", GA="13", HI="15", ID="16", IL="17", IN="18", IA="19", KS="20",
  KY="21", LA="22", ME="23", MD="24", MA="25", MI="26", MN="27", MS="28",
  MO="29", MT="30", NE="31", NV="32", NH="33", NJ="34", NM="35", NY="36",
  NC="37", ND="38", OH="39", OK="40", OR="41", PA="42", RI="44", SC="45",
  SD="46", TN="47", TX="48", UT="49", VT="50", VA="51", WA="53", WV="54",
  WI="55", WY="56"
)

.to_fips <- function(state) {
  state <- toupper(state)
  if (nchar(state) == 2 && state %in% names(.state_fips)) return(.state_fips[state])
  if (grepl("^\\d{2}$", state)) return(state)
  state
}

# == Schemas ===================================================================

.schema_drought <- tibble(
  date = as.Date(character()), state = character(), none = numeric(),
  d0 = numeric(), d1 = numeric(), d2 = numeric(),
  d3 = numeric(), d4 = numeric()
)

.schema_drought_county <- tibble(
  date = as.Date(character()), county = character(), state = character(),
  none = numeric(), d0 = numeric(), d1 = numeric(),
  d2 = numeric(), d3 = numeric(), d4 = numeric()
)

# == Private helpers ===========================================================

.parse_drought_csv <- function(tmp, area_col = "StateAbbreviation") {
  raw_lines <- readLines(tmp, warn = FALSE)
  if (length(raw_lines) < 2) return(NULL)
  con <- textConnection(raw_lines)
  df <- tryCatch(read.csv(con, stringsAsFactors = FALSE), error = function(e) NULL)
  close(con)
  df
}

# == Public functions ==========================================================


#' Get state-level drought severity statistics (percent area)
#'
#' Returns weekly drought severity percentages for a given state from the
#' US Drought Monitor. Each row represents one weekly report. Severity
#' levels follow the standard USDM classification: D0 (Abnormally Dry),
#' D1 (Moderate Drought), D2 (Severe Drought), D3 (Extreme Drought),
#' and D4 (Exceptional Drought). Values represent the percentage of the
#' state's land area in each category.
#'
#' @param state Character. Two-letter state abbreviation (e.g., "CA", "TX")
#'   or two-digit FIPS code (e.g., "06" for California).
#' @param start Character. Start date in "M/D/YYYY" format (e.g., "1/1/2024").
#' @param end Character. End date in "M/D/YYYY" format (e.g., "12/31/2024").
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Week ending date for the drought assessment.}
#'     \item{state}{Character. Two-letter state abbreviation.}
#'     \item{none}{Numeric. Percent of area with no drought conditions.}
#'     \item{d0}{Numeric. Percent of area classified as D0 (Abnormally Dry).}
#'     \item{d1}{Numeric. Percent classified as D1 (Moderate Drought).}
#'     \item{d2}{Numeric. Percent classified as D2 (Severe Drought).}
#'     \item{d3}{Numeric. Percent classified as D3 (Extreme Drought).}
#'     \item{d4}{Numeric. Percent classified as D4 (Exceptional Drought).}
#'   }
#'
#' @examples
#' \dontrun{
#' drought_state("CA", "1/1/2024", "6/30/2024")
#' drought_state("TX", "1/1/2023", "12/31/2023")
#' }
#'
#' @export
drought_state <- function(state, start = "1/1/2024", end = "12/31/2024") {
  fips <- .to_fips(state)
  url <- sprintf(
    "%s/StateStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=%s&startdate=%s&enddate=%s&statisticsType=2",
    .drought_base, fips, start, end
  )
  tmp <- .fetch(url, ext = ".csv")
  df <- .parse_drought_csv(tmp)
  if (is.null(df) || nrow(df) == 0) return(.schema_drought)

  as_tibble(df) |>
    transmute(
      date = as.Date(as.character(MapDate), format = "%Y%m%d"),
      state = as.character(StateAbbreviation),
      none = as.numeric(None),
      d0 = as.numeric(D0),
      d1 = as.numeric(D1),
      d2 = as.numeric(D2),
      d3 = as.numeric(D3),
      d4 = as.numeric(D4)
    )
}

#' Get county-level drought severity statistics (percent area)
#'
#' Returns weekly drought severity percentages for all counties within
#' a given state from the US Drought Monitor. Useful for identifying
#' local drought hotspots within a state.
#'
#' @param state Character. Two-letter state abbreviation (e.g., "CA", "TX")
#'   or two-digit FIPS code.
#' @param start Character. Start date in "M/D/YYYY" format (e.g., "1/1/2024").
#' @param end Character. End date in "M/D/YYYY" format (e.g., "12/31/2024").
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Week ending date for the drought assessment.}
#'     \item{county}{Character. County name or FIPS code.}
#'     \item{state}{Character. Two-letter state abbreviation.}
#'     \item{none}{Numeric. Percent of area with no drought.}
#'     \item{d0}{Numeric. Percent D0 (Abnormally Dry).}
#'     \item{d1}{Numeric. Percent D1 (Moderate Drought).}
#'     \item{d2}{Numeric. Percent D2 (Severe Drought).}
#'     \item{d3}{Numeric. Percent D3 (Extreme Drought).}
#'     \item{d4}{Numeric. Percent D4 (Exceptional Drought).}
#'   }
#'
#' @examples
#' \dontrun{
#' drought_county("CA", "1/1/2024", "3/1/2024")
#' }
#'
#' @seealso [drought_state()]
#' @export
drought_county <- function(state, start = "1/1/2024", end = "12/31/2024") {
  fips <- .to_fips(state)
  url <- sprintf(
    "%s/CountyStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=%s&startdate=%s&enddate=%s&statisticsType=2",
    .drought_base, fips, start, end
  )
  tmp <- .fetch(url, ext = ".csv")
  df <- .parse_drought_csv(tmp)
  if (is.null(df) || nrow(df) == 0) return(.schema_drought_county)

  nms <- names(df)
  county_col <- if ("County" %in% nms) "County" else if ("CountyName" %in% nms) "CountyName" else if ("FIPS" %in% nms) "FIPS" else nms[2]

  as_tibble(df) |>
    transmute(
      date = as.Date(as.character(MapDate), format = "%Y%m%d"),
      county = as.character(.data[[county_col]]),
      state = as.character(if ("StateAbbreviation" %in% nms) StateAbbreviation else state),
      none = as.numeric(None),
      d0 = as.numeric(D0),
      d1 = as.numeric(D1),
      d2 = as.numeric(D2),
      d3 = as.numeric(D3),
      d4 = as.numeric(D4)
    )
}

#' Get most recent drought data for all states
#'
#' Fetches the latest week's drought severity percentages for all 50 US
#' states. Internally queries each state for the most recent two-week
#' window and keeps only the latest report date. This involves 50
#' sequential API calls so may take 30-60 seconds.
#'
#' @return A tibble with the same columns as [drought_state()]: date, state,
#'   none, d0, d1, d2, d3, d4. One row per state.
#'
#' @examples
#' \dontrun{
#' drought_latest()
#' }
#'
#' @seealso [drought_state()], [drought_compare()]
#' @export
drought_latest <- function() {
  # Use recent date range to get latest data
  end <- format(Sys.Date(), "%m/%d/%Y")
  start <- format(Sys.Date() - 14, "%m/%d/%Y")

  all_states <- lapply(names(.state_fips), function(st) {
    tryCatch({
      result <- drought_state(st, start = start, end = end)
      if (nrow(result) > 0) {
        # Keep only the most recent date
        result[result$date == max(result$date), , drop = FALSE]
      } else NULL
    }, error = function(e) NULL)
  })
  all_states <- all_states[!vapply(all_states, is.null, logical(1))]
  if (length(all_states) == 0) return(.schema_drought)
  bind_rows(all_states)
}

#' Compare drought severity across states for a given date range
#'
#' Fetches and combines state-level drought data for multiple states,
#' making it easy to compare drought conditions across regions over
#' the same time period.
#'
#' @param states Character vector. Two-letter state abbreviations
#'   (e.g., \code{c("CA", "TX", "AZ")}).
#' @param start Character. Start date in "M/D/YYYY" format.
#' @param end Character. End date in "M/D/YYYY" format.
#'
#' @return A tibble with the same columns as [drought_state()]: date, state,
#'   none, d0, d1, d2, d3, d4. Multiple rows per state (one per week).
#'
#' @examples
#' \dontrun{
#' drought_compare(c("CA", "TX", "AZ"), "1/1/2024", "6/30/2024")
#' }
#'
#' @seealso [drought_state()], [drought_latest()]
#' @export
drought_compare <- function(states, start = "1/1/2024", end = "12/31/2024") {
  results <- lapply(states, function(st) {
    tryCatch(drought_state(st, start = start, end = end),
             error = function(e) NULL)
  })
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) return(.schema_drought)
  bind_rows(results)
}

# == Context ===================================================================

#' Get unl.edu client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
drought_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(drought_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/unl.edu.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "unl.edu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# unl.edu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# unl.edu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
