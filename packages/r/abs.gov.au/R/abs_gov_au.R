# abs.gov.au.R - Self-contained abs.gov.au client


`%||%` <- function(a, b) if (is.null(a)) b else a


# abs.R
# Self-contained Australian Bureau of Statistics (ABS) SDMX API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be respectful
# Format: SDMX-CSV


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.abs_base <- "https://data.api.abs.gov.au/rest"

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/vnd.sdmx.data+csv;file=true") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_csv <- function(url) {
  f <- .fetch(url, ext = ".csv")
  tryCatch(
    utils::read.csv(f, stringsAsFactors = FALSE) |> as_tibble(),
    error = function(e) tibble()
  )
}

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}

# == Schemas ===================================================================

.schema_data <- tibble(
  DATAFLOW = character(), FREQ = character(), TIME_PERIOD = character(),
  OBS_VALUE = numeric()
)

.schema_dataflows <- tibble(
  id = character(), name = character(), version = character(),
  agencyID = character()
)

# == Data ======================================================================

#' Fetch ABS SDMX data
#'
#' Retrieves statistical data from the Australian Bureau of Statistics via the
#' SDMX REST API. Returns data in SDMX-CSV format with one row per observation.
#' Use \code{abs_dataflows()} to discover available dataflow identifiers.
#'
#' @param dataflow Character. Dataflow identifier. Use \code{abs_dataflows()}
#'   to browse ~1200 available flows. Examples: \code{"CPI"} (Consumer Price
#'   Index), \code{"LF"} (Labour Force), \code{"ERP"} (Estimated Resident
#'   Population), \code{"RES_DWELL"} (Residential Dwellings).
#' @param key Character. SDMX dimension key path. Default \code{".."} returns
#'   all dimension combinations. Use dot-separated values to filter specific
#'   dimensions (e.g. \code{"1.10001.10.Q"}).
#' @param start Character. Start period filter. Accepts \code{"2023"},
#'   \code{"2023-Q1"}, or \code{"2023-01"} formats.
#' @param end Character. End period filter. Same format as \code{start}.
#' @param agency Character. Agency ID (default \code{"ABS"}).
#' @param version Character. Dataflow version (default \code{"1.0.0"}).
#' @return A tibble with SDMX-CSV columns. Common columns include:
#'   \describe{
#'     \item{DATAFLOW}{Character. Dataflow identifier.}
#'     \item{FREQ}{Character. Frequency code (e.g. "Q", "M", "A").}
#'     \item{TIME_PERIOD}{Character. Observation period (e.g. "2024-Q1").}
#'     \item{OBS_VALUE}{Numeric. The observed value.}
#'   }
#'   Additional dimension columns vary by dataflow.
#' @export
#' @examples
#' \dontrun{
#' # Fetch CPI data from 2023 onward
#' abs_data("CPI", start = "2023")
#'
#' # Fetch labour force data
#' abs_data("LF", start = "2024")
#' }
abs_data <- function(dataflow, key = "..", start = NULL, end = NULL,
                     agency = "ABS", version = "1.0.0") {
  url <- paste0(.abs_base, "/data/", agency, ",", dataflow, ",", version, "/", key)
  params <- character()
  if (!is.null(start)) params <- c(params, paste0("startPeriod=", start))
  if (!is.null(end)) params <- c(params, paste0("endPeriod=", end))
  params <- c(params, "dimensionAtObservation=AllDimensions")
  if (length(params) > 0) url <- paste0(url, "?", paste(params, collapse = "&"))

  df <- .fetch_csv(url)
  if (nrow(df) == 0) return(.schema_data)

  if ("OBS_VALUE" %in% names(df)) {
    df$OBS_VALUE <- as.numeric(df$OBS_VALUE)
  }
  df
}

# == Dataflows =================================================================

#' List available ABS SDMX dataflows
#'
#' Queries the ABS SDMX API for all published dataflows. Returns ~1200 dataflow
#' definitions covering economic, demographic, and social statistics. Use the
#' \code{id} column values as the \code{dataflow} argument to \code{abs_data()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Dataflow identifier (e.g. "CPI", "LF",
#'       "ABORIGINAL_POP_PROJ"). Pass this to \code{abs_data()}.}
#'     \item{name}{Character. Human-readable name describing the dataset.}
#'     \item{version}{Character. Version string (e.g. "1.0", "1.3.0").}
#'     \item{agencyID}{Character. Publishing agency (typically "ABS").}
#'   }
#' @export
#' @examples
#' \dontrun{
#' flows <- abs_dataflows()
#' flows
#' # Search for CPI-related flows
#' flows[grepl("CPI|price", flows$name, ignore.case = TRUE), ]
#' }
abs_dataflows <- function() {
  url <- paste0(.abs_base, "/dataflow/ABS")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua,
                       Accept = "application/vnd.sdmx.structure+json") |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp)

  dfs <- tryCatch({
    structures <- raw$data$dataflows %||% raw$`Dataflow`
    if (is.null(structures)) structures <- raw$data$dataStructures
    structures
  }, error = function(e) NULL)

  if (is.null(dfs) || length(dfs) == 0) return(.schema_dataflows)

  # Handle nested SDMX JSON structure
  if (is.data.frame(dfs)) {
    nm <- if ("name" %in% names(dfs)) {
      vapply(dfs$name, function(x) {
        if (is.list(x) || is.data.frame(x)) as.character(x[[1]])
        else as.character(x)
      }, character(1))
    } else NA_character_

    return(tibble(
      id = as.character(dfs$id %||% NA_character_),
      name = nm,
      version = as.character(dfs$version %||% NA_character_),
      agencyID = as.character(dfs$agencyID %||% NA_character_)
    ))
  }

  .schema_dataflows
}

# == CPI convenience ===========================================================

#' Fetch ABS Consumer Price Index data
#'
#' Convenience wrapper around \code{abs_data("CPI")} that fetches Australian
#' Consumer Price Index observations. Returns quarterly CPI data in SDMX-CSV
#' format with all dimension combinations.
#'
#' @param start Character. Start period filter (default \code{"2020"}).
#'   Accepts year (\code{"2020"}), quarter (\code{"2020-Q1"}), or month
#'   (\code{"2020-01"}).
#' @return A tibble with SDMX-CSV columns including:
#'   \describe{
#'     \item{DATAFLOW}{Character. Always "ABS:CPI".}
#'     \item{FREQ}{Character. Frequency (typically "Q" for quarterly).}
#'     \item{TIME_PERIOD}{Character. Quarter (e.g. "2024-Q1").}
#'     \item{OBS_VALUE}{Numeric. CPI index value.}
#'   }
#'   Returns empty schema tibble on error.
#' @export
#' @examples
#' \dontrun{
#' cpi <- abs_cpi("2023")
#' cpi
#' }
abs_cpi <- function(start = "2020") {
  tryCatch(
    abs_data("CPI", key = "..", start = start),
    error = function(e) .schema_data
  )
}

# == Context ===================================================================

#' Get abs.gov.au client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
abs_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(abs_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/abs.gov.au.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "abs.gov.au")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# abs.gov.au context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# abs.gov.au", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
