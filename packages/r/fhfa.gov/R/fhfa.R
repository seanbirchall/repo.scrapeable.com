# fhfa.gov.R
# Self-contained FHFA (Federal Housing Finance Agency) client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Docs: https://www.fhfa.gov/data/hpi


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

.ua <- "support@scrapeable.com"
.fhfa_base <- "https://www.fhfa.gov"

# -- Core fetch: download CSV to temp file, then read locally -----------------

.fhfa_fetch_csv <- function(url, header = TRUE, col_names = NULL) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  if (file.info(tmp)$size == 0) return(tibble())
  if (!is.null(col_names)) {
    read.csv(tmp, header = FALSE, stringsAsFactors = FALSE,
             col.names = col_names, na.strings = c("", "-", "NA", "."))
  } else {
    read.csv(tmp, header = header, stringsAsFactors = FALSE,
             na.strings = c("", "-", "NA", "."))
  }
}

# -- Schemas -------------------------------------------------------------------

.schema_hpi_master <- tibble(
  hpi_type = character(), hpi_flavor = character(), frequency = character(),
  level = character(), place_name = character(), place_id = character(),
  yr = integer(), period = integer(), index_nsa = numeric(), index_sa = numeric()
)

.schema_hpi_state <- tibble(
  state = character(), yr = integer(), qtr = integer(),
  index_nsa = numeric(), index_sa = numeric()
)

.schema_hpi_metro <- tibble(
  metro_name = character(), metro_code = character(), yr = integer(),
  qtr = integer(), index_nsa = numeric(), index_sa = numeric()
)

.schema_hpi_summary <- tibble(
  name = character(), rank = integer(),
  change_1qtr = numeric(), change_1yr = numeric(),
  change_5yr = numeric(), change_since_1991 = numeric()
)

# == Public functions ==========================================================

#' List available FHFA HPI datasets
#'
#' Returns a reference table of all downloadable House Price Index datasets
#' from the Federal Housing Finance Agency, with descriptions, frequencies,
#' and direct download URLs.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{dataset}{Character. Dataset identifier (e.g. "hpi_master", "hpi_state").}
#'     \item{description}{Character. Brief description of the dataset.}
#'     \item{frequency}{Character. Data frequency: "monthly", "quarterly", or "annual".}
#'     \item{url}{Character. Direct download URL for the CSV file.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fhfa_list()
#' }
fhfa_list <- function() {
  tibble(
    dataset = c(
      "hpi_master", "hpi_state", "hpi_metro",
      "hpi_us_census", "hpi_tract",
      "summary_state", "summary_city", "summary_regional"
    ),
    description = c(
      "Master HPI file (all types, flavors, geographies)",
      "All-Transactions quarterly index by state (no header)",
      "All-Transactions quarterly index by metro area (no header)",
      "All-Transactions quarterly index for US and Census divisions (no header)",
      "Annual All-Transactions index by census tract",
      "State HPI summary (1-qtr, 1-yr, 5-yr, since 1991 pct change)",
      "Metro/city HPI summary (1-qtr, 1-yr, 5-yr, since 1991 pct change)",
      "Regional HPI summary (1-qtr, 1-yr, 5-yr, since 1991 pct change)"
    ),
    frequency = c("monthly", "quarterly", "quarterly", "quarterly",
                   "annual", "quarterly", "quarterly", "quarterly"),
    url = c(
      "https://www.fhfa.gov/hpi/download/monthly/hpi_master.csv",
      "https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_at_state.csv",
      "https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_at_metro.csv",
      "https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_at_us_and_census.csv",
      "https://www.fhfa.gov/hpi/download/annual/hpi_at_tract.csv",
      "https://www.fhfa.gov/hpi-state/csv",
      "https://www.fhfa.gov/hpi-city/csv",
      "https://www.fhfa.gov/hpi-regional/csv"
    )
  )
}

#' Search HPI master data
#'
#' Search the FHFA master HPI file, which contains all index types, flavors,
#' frequencies, and geographies in a single dataset. The master file is
#' downloaded and filtered locally.
#'
#' @param query Character. Regex pattern to match against place_name
#'   (e.g. "California", "New York").
#' @param hpi_type Character. Filter by index type: \code{"traditional"},
#'   \code{"distortion-free"}, or \code{"expanded-data"}.
#' @param hpi_flavor Character. Filter by index flavor: \code{"purchase-only"},
#'   \code{"all-transactions"}, or \code{"refinancing"}.
#' @param frequency Character. Filter by frequency: \code{"monthly"},
#'   \code{"quarterly"}, or \code{"annual"}.
#' @param level Character. Filter by geographic level: \code{"State"},
#'   \code{"MSA"}, \code{"USA or Census Division"}.
#' @param max_rows Integer. Maximum rows to return (default 500).
#' @return A tibble with columns:
#'   \describe{
#'     \item{hpi_type}{Character. Index type.}
#'     \item{hpi_flavor}{Character. Index flavor.}
#'     \item{frequency}{Character. Data frequency.}
#'     \item{level}{Character. Geographic level.}
#'     \item{place_name}{Character. Place name (e.g. "California", "USA").}
#'     \item{place_id}{Character. Place identifier (e.g. state abbreviation).}
#'     \item{yr}{Integer. Year.}
#'     \item{period}{Integer. Period within year (month or quarter number).}
#'     \item{index_nsa}{Numeric. Index value, not seasonally adjusted.}
#'     \item{index_sa}{Numeric. Index value, seasonally adjusted (if available).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fhfa_search("California", hpi_type = "traditional")
#' fhfa_search(level = "State", frequency = "quarterly", max_rows = 100)
#' }
fhfa_search <- function(query = NULL, hpi_type = NULL, hpi_flavor = NULL,
                        frequency = NULL, level = NULL, max_rows = 500) {
  url <- paste0(.fhfa_base, "/hpi/download/monthly/hpi_master.csv")
  df <- .fhfa_fetch_csv(url) |> as_tibble()
  if (nrow(df) == 0) return(.schema_hpi_master)

  df <- df |>
    mutate(
      yr = as.integer(yr),
      period = as.integer(period),
      index_nsa = suppressWarnings(as.numeric(index_nsa)),
      index_sa = suppressWarnings(as.numeric(index_sa))
    )

  if (!is.null(query))      df <- df |> filter(grepl(query, place_name, ignore.case = TRUE))
  if (!is.null(hpi_type))   df <- df |> filter(.data$hpi_type == .env$hpi_type)
  if (!is.null(hpi_flavor)) df <- df |> filter(.data$hpi_flavor == .env$hpi_flavor)
  if (!is.null(frequency))  df <- df |> filter(.data$frequency == .env$frequency)
  if (!is.null(level))      df <- df |> filter(.data$level == .env$level)

  df |> head(max_rows)
}

#' Get HPI data by state (All-Transactions quarterly index)
#'
#' Downloads the FHFA All-Transactions House Price Index by state. This is
#' the broadest HPI measure, based on purchase prices and appraisal values
#' for mortgages purchased or securitized by Fannie Mae or Freddie Mac.
#'
#' @param state Character. Two-letter state abbreviation to filter
#'   (e.g. \code{"CA"}). If \code{NULL}, returns all states.
#' @param start_year Integer. Filter to years >= this value.
#' @param end_year Integer. Filter to years <= this value.
#' @return A tibble with columns:
#'   \describe{
#'     \item{state}{Character. Two-letter state abbreviation.}
#'     \item{yr}{Integer. Year.}
#'     \item{qtr}{Integer. Quarter (1-4).}
#'     \item{index_nsa}{Numeric. Index value, not seasonally adjusted (base = 100 in 1980 Q1).}
#'     \item{index_sa}{Numeric. Seasonally adjusted index (NA for most states).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fhfa_hpi_state(state = "CA", start_year = 2020)
#' fhfa_hpi_state(start_year = 2023, end_year = 2024)
#' }
fhfa_hpi_state <- function(state = NULL, start_year = NULL, end_year = NULL) {
  url <- paste0(.fhfa_base, "/hpi/download/quarterly_datasets/hpi_at_state.csv")
  df <- .fhfa_fetch_csv(url, header = FALSE,
                        col_names = c("state", "yr", "qtr", "index_nsa", "index_sa")) |>
    as_tibble() |>
    mutate(
      yr = as.integer(yr),
      qtr = as.integer(qtr),
      index_nsa = suppressWarnings(as.numeric(index_nsa)),
      index_sa = suppressWarnings(as.numeric(index_sa))
    )

  if (!is.null(state))      df <- df |> filter(state == !!state)
  if (!is.null(start_year)) df <- df |> filter(yr >= start_year)
  if (!is.null(end_year))   df <- df |> filter(yr <= end_year)
  df
}

#' Get HPI data by metro area (All-Transactions quarterly index)
#'
#' Downloads the FHFA All-Transactions House Price Index by metropolitan
#' statistical area (MSA). Supports regex filtering on metro area names.
#'
#' @param query Character. Regex pattern to match against metro area name
#'   (e.g. "Chicago", "San Francisco"). If \code{NULL}, returns all metros.
#' @param start_year Integer. Filter to years >= this value.
#' @param end_year Integer. Filter to years <= this value.
#' @return A tibble with columns:
#'   \describe{
#'     \item{metro_name}{Character. Metro area name (e.g.
#'       "Chicago-Naperville-Schaumburg, IL (MSAD)").}
#'     \item{metro_code}{Integer. CBSA/MSAD code.}
#'     \item{yr}{Integer. Year.}
#'     \item{qtr}{Integer. Quarter (1-4).}
#'     \item{index_nsa}{Numeric. Index value, not seasonally adjusted.}
#'     \item{index_sa}{Numeric. Seasonally adjusted index (typically NA).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fhfa_hpi_metro("Chicago", start_year = 2020)
#' fhfa_hpi_metro("San Francisco")
#' }
fhfa_hpi_metro <- function(query = NULL, start_year = NULL, end_year = NULL) {
  url <- paste0(.fhfa_base, "/hpi/download/quarterly_datasets/hpi_at_metro.csv")
  df <- .fhfa_fetch_csv(url, header = FALSE,
                        col_names = c("metro_name", "metro_code", "yr", "qtr",
                                      "index_nsa", "index_sa")) |>
    as_tibble() |>
    mutate(
      yr = as.integer(yr),
      qtr = as.integer(qtr),
      index_nsa = suppressWarnings(as.numeric(index_nsa)),
      index_sa = suppressWarnings(as.numeric(index_sa))
    )

  if (!is.null(query))      df <- df |> filter(grepl(query, metro_name, ignore.case = TRUE))
  if (!is.null(start_year)) df <- df |> filter(yr >= start_year)
  if (!is.null(end_year))   df <- df |> filter(yr <= end_year)
  df
}

#' Get HPI summary table by state
#'
#' Returns the FHFA state-level HPI summary showing ranked percentage changes
#' over multiple time horizons. States are ranked by most recent quarterly change.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. State name.}
#'     \item{rank}{Integer. Rank by quarterly change.}
#'     \item{change_1qtr}{Numeric. Percentage change over 1 quarter.}
#'     \item{change_1yr}{Numeric. Percentage change over 1 year.}
#'     \item{change_5yr}{Numeric. Percentage change over 5 years.}
#'     \item{change_since_1991}{Numeric. Percentage change since 1991 Q1.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fhfa_summary_state()
#' }
fhfa_summary_state <- function() {
  url <- paste0(.fhfa_base, "/hpi-state/csv")
  df <- .fhfa_fetch_csv(url) |> as_tibble()
  if (nrow(df) == 0) return(.schema_hpi_summary)
  names(df) <- c("name", "rank", "change_1qtr", "change_1yr",
                  "change_5yr", "change_since_1991")
  df |>
    mutate(
      rank = as.integer(rank),
      change_1qtr = as.numeric(change_1qtr),
      change_1yr = as.numeric(change_1yr),
      change_5yr = as.numeric(change_5yr),
      change_since_1991 = as.numeric(change_since_1991)
    )
}

#' Get HPI summary table by city/metro
#'
#' Returns the FHFA metro-level HPI summary showing ranked percentage changes
#' over multiple time horizons. Metro areas are ranked by most recent quarterly change.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Metro area name (e.g. "Reading, PA").}
#'     \item{rank}{Integer. Rank by quarterly change.}
#'     \item{change_1qtr}{Numeric. Percentage change over 1 quarter.}
#'     \item{change_1yr}{Numeric. Percentage change over 1 year.}
#'     \item{change_5yr}{Numeric. Percentage change over 5 years.}
#'     \item{change_since_1991}{Numeric. Percentage change since 1991 Q1.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fhfa_summary_city()
#' }
fhfa_summary_city <- function() {
  url <- paste0(.fhfa_base, "/hpi-city/csv")
  df <- .fhfa_fetch_csv(url) |> as_tibble()
  if (nrow(df) == 0) return(.schema_hpi_summary)
  names(df) <- c("name", "rank", "change_1qtr", "change_1yr",
                  "change_5yr", "change_since_1991")
  df |>
    mutate(
      rank = as.integer(rank),
      change_1qtr = as.numeric(change_1qtr),
      change_1yr = as.numeric(change_1yr),
      change_5yr = as.numeric(change_5yr),
      change_since_1991 = as.numeric(change_since_1991)
    )
}

#' Get HPI summary table by region
#'
#' Returns the FHFA regional HPI summary showing ranked percentage changes
#' over multiple time horizons for Census divisions and the US as a whole.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Region name (e.g. "East North Central Division").}
#'     \item{rank}{Integer. Rank by quarterly change (NA for US total).}
#'     \item{change_1qtr}{Numeric. Percentage change over 1 quarter.}
#'     \item{change_1yr}{Numeric. Percentage change over 1 year.}
#'     \item{change_5yr}{Numeric. Percentage change over 5 years.}
#'     \item{change_since_1991}{Numeric. Percentage change since 1991 Q1.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fhfa_summary_regional()
#' }
fhfa_summary_regional <- function() {
  url <- paste0(.fhfa_base, "/hpi-regional/csv")
  df <- .fhfa_fetch_csv(url) |> as_tibble()
  if (nrow(df) == 0) return(.schema_hpi_summary)
  names(df) <- c("name", "rank", "change_1qtr", "change_1yr",
                  "change_5yr", "change_since_1991")
  df |>
    mutate(
      rank = as.integer(rank),
      change_1qtr = as.numeric(change_1qtr),
      change_1yr = as.numeric(change_1yr),
      change_5yr = as.numeric(change_5yr),
      change_since_1991 = as.numeric(change_since_1991)
    )
}

#' Get fhfa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fhfa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fhfa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fhfa.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fhfa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fhfa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fhfa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
