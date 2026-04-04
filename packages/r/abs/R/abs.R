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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' Retrieve statistical data from the Australian Bureau of Statistics via the
#' SDMX REST API. Returns observation-level rows in SDMX-CSV format. Use
#' \code{abs_dataflows()} to discover available dataflow identifiers.
#'
#' @param dataflow Dataflow identifier (e.g. \code{"CPI"}, \code{"LF"},
#'   \code{"ERP"}). Use \code{abs_dataflows()} to browse available IDs.
#' @param key SDMX key path selecting dimensions (e.g. \code{"1.10001.10.Q"}
#'   or \code{".."} for all). Default \code{".."} returns all dimension
#'   combinations.
#' @param start Start period (e.g. \code{"2023-Q1"}, \code{"2023-01"},
#'   \code{"2023"}).
#' @param end End period in the same format as \code{start}.
#' @param agency Agency ID (default \code{"ABS"}).
#' @param version Dataflow version string (default \code{"1.0.0"}).
#' @return A tibble with SDMX-CSV columns. The exact columns depend on the
#'   dataflow but always include:
#'   \describe{
#'     \item{DATAFLOW}{\code{character} -- Dataflow identifier.}
#'     \item{FREQ}{\code{character} -- Frequency code (e.g. \code{"Q"}, \code{"M"}).}
#'     \item{TIME_PERIOD}{\code{character} -- Observation time period.}
#'     \item{OBS_VALUE}{\code{numeric} -- Observation value.}
#'   }
#' @examples
#' \dontrun{
#' # Consumer Price Index, quarterly, 2023 onward
#' abs_data("CPI", start = "2023-Q1")
#'
#' # Estimated Resident Population, all dimensions
#' abs_data("ERP")
#' }
#' @export
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
#' Query the ABS SDMX API for all published dataflows. Each row represents one
#' statistical dataset that can be fetched with \code{abs_data()}.
#'
#' @return A tibble with one row per dataflow:
#'   \describe{
#'     \item{id}{\code{character} -- Dataflow identifier (pass to \code{abs_data()}).}
#'     \item{name}{\code{character} -- Human-readable dataflow name.}
#'     \item{version}{\code{character} -- Dataflow version string.}
#'     \item{agencyID}{\code{character} -- Maintaining agency (typically \code{"ABS"}).}
#'   }
#' @examples
#' \dontrun{
#' abs_dataflows()
#' }
#' @export
abs_dataflows <- function() {
  url <- paste0(.abs_base, "/dataflow/ABS")
  raw <- .fetch_json(url)

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

# == Context (LLM injection) ==================================================

#' Get ABS client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' abs_context()
#' }
#' @export
abs_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(abs_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/abs.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "abs")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# abs context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# abs", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
