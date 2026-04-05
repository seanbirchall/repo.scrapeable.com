# nyctmc.org.R
# Self-contained NYC Traffic Management Center client.
# Returns real-time EZ-Pass traffic speed data from Midtown in Motion sensors.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Data refreshes every 5 minutes


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nyctmc_base <- "https://linkdata.nyctmc.org/data"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_speeds <- tibble(

  sid = character(),
  link_name = character(),
  borough = character(),
  link_length_ft = numeric(),
  aggregation_period_sec = integer(),
  n_samples = integer(),
  timestamp = as.POSIXct(character()),
  median_tt_sec = numeric(),
  median_speed_fps = numeric(),
  median_speed_mph = numeric()
)

# == Public functions ==========================================================

#' Get real-time NYC traffic speeds from EZ-Pass sensors
#'
#' Returns the latest Midtown in Motion traffic speed readings across
#' all sensor links in Manhattan and other boroughs. Data is sourced from
#' EZ-Pass tag readers deployed throughout the NYC road network and
#' refreshes approximately every five minutes.
#'
#' Each row represents a single sensor link segment with its most recent
#' median travel time and speed calculation. Speed is derived from the
#' travel time and link length.
#'
#' @return A tibble with one row per sensor link and the following columns:
#' \describe{
#'   \item{sid}{Character. Unique sensor link identifier.}
#'   \item{link_name}{Character. Human-readable description of the road segment
#'     (e.g. "42nd Street - Eastbound - 5th Ave to Madison Ave").}
#'   \item{borough}{Character. NYC borough name (Manhattan, Brooklyn, Queens,
#'     Bronx, Staten Island).}
#'   \item{link_length_ft}{Numeric. Length of the sensor link segment in feet.}
#'   \item{aggregation_period_sec}{Integer. Time window in seconds over which
#'     the median was computed (typically 900 = 15 minutes).}
#'   \item{n_samples}{Integer. Number of EZ-Pass tag reads in the aggregation
#'     window. Zero indicates no data for the period.}
#'   \item{timestamp}{POSIXct. Date-time of the median calculation.}
#'   \item{median_tt_sec}{Numeric. Median travel time across the link in seconds.}
#'   \item{median_speed_fps}{Numeric. Median speed in feet per second.}
#'   \item{median_speed_mph}{Numeric. Median speed converted to miles per hour.}
#' }
#'
#' @examples
#' \dontrun{
#' speeds <- nyctmc_speeds()
#' speeds
#'
#' # Slowest links right now
#' speeds |> dplyr::filter(n_samples > 0) |> dplyr::arrange(median_speed_mph)
#' }
#'
#' @export
nyctmc_speeds <- function() {
  url <- sprintf("%s/mim_data_opendata.csv", .nyctmc_base)
  tmp <- .fetch(url, ext = ".csv")
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_speeds)

  as_tibble(raw) |>
    transmute(
      sid = as.character(sid),
      link_name = as.character(link_name),
      borough = as.character(borough),
      link_length_ft = as.numeric(link_length_ft),
      aggregation_period_sec = as.integer(aggregation_period_sec),
      n_samples = as.integer(n_samples),
      timestamp = as.POSIXct(median_calculation_timestamp,
                             format = "%m/%d/%Y %H:%M:%S"),
      median_tt_sec = as.numeric(median_tt_sec),
      median_speed_fps = as.numeric(median_speed_fps),
      median_speed_mph = round(as.numeric(median_speed_fps) * 0.681818, 2)
    )
}

#' List unique traffic sensor links
#'
#' Returns the distinct set of EZ-Pass sensor link segments currently
#' reporting data, de-duplicated from the live speed feed. Useful for
#' discovering which road corridors are instrumented and their physical
#' lengths.
#'
#' @return A tibble with one row per unique sensor link and the following columns:
#' \describe{
#'   \item{sid}{Character. Unique sensor link identifier.}
#'   \item{link_name}{Character. Human-readable segment description.}
#'   \item{borough}{Character. NYC borough name.}
#'   \item{link_length_ft}{Numeric. Segment length in feet.}
#' }
#'
#' @examples
#' \dontrun{
#' links <- nyctmc_links()
#' links |> dplyr::count(borough, sort = TRUE)
#' }
#'
#' @export
nyctmc_links <- function() {
  nyctmc_speeds() |>
    distinct(sid, link_name, borough, link_length_ft) |>
    arrange(borough, link_name)
}

#' Get traffic speeds filtered by borough
#'
#' Fetches the current speed feed and filters to links in the specified
#' borough. The match is case-insensitive and uses partial matching, so
#' \code{"man"} will match \code{"Manhattan"}.
#'
#' @param borough Character string. Borough name or partial match
#'   (e.g. \code{"Manhattan"}, \code{"Brooklyn"}, \code{"Queens"}).
#'
#' @return A tibble with the same columns as \code{\link{nyctmc_speeds}},
#'   filtered to the requested borough.
#'
#' @examples
#' \dontrun{
#' nyctmc_borough("Brooklyn")
#' nyctmc_borough("Manhattan") |> dplyr::filter(n_samples > 0)
#' }
#'
#' @seealso \code{\link{nyctmc_speeds}} for full unfiltered data.
#' @export
nyctmc_borough <- function(borough) {
  pat <- borough[[1]]
  nyctmc_speeds() |>
    filter(grepl(pat, .data$borough, ignore.case = TRUE))
}

#' Search traffic links by name
#'
#' Fetches the live speed feed and filters to links whose names match the
#' query string. Useful for finding specific corridors like "FDR",
#' "Broadway", or "Queens Blvd".
#'
#' @param query Character string. Search pattern matched against link names
#'   using case-insensitive regular expression matching.
#'
#' @return A tibble with the same columns as \code{\link{nyctmc_speeds}},
#'   filtered to links whose \code{link_name} matches \code{query}.
#'
#' @examples
#' \dontrun{
#' nyctmc_search("FDR")
#' nyctmc_search("Broadway")
#' }
#'
#' @seealso \code{\link{nyctmc_speeds}}, \code{\link{nyctmc_borough}}
#' @export
nyctmc_search <- function(query) {
  nyctmc_speeds() |>
    filter(grepl(query, .data$link_name, ignore.case = TRUE))
}

#' Get nyctmc.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nyctmc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nyctmc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nyctmc.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nyctmc.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nyctmc.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nyctmc.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
