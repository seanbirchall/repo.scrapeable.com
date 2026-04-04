# spacexdata-com.R
# Self-contained SpaceX launch and rocket data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none known

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.spacex_base <- "https://api.spacexdata.com/v4"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_launches <- tibble(
  id = character(), name = character(), flight_number = integer(),
  date_utc = as.POSIXct(character()), success = logical(),
  details = character(), rocket = character(),
  launchpad = character(), upcoming = logical()
)

.schema_rockets <- tibble(
  id = character(), name = character(), type = character(),
  active = logical(), stages = integer(), boosters = integer(),
  cost_per_launch = numeric(), first_flight = as.Date(character()),
  country = character(), company = character(), description = character()
)

# -- Parse helpers -------------------------------------------------------------

.parse_launches <- function(raw) {
  if (length(raw) == 0) return(.schema_launches)

  rows <- lapply(raw, function(l) {
    tibble(
      id = as.character(l$id %||% NA),
      name = as.character(l$name %||% NA),
      flight_number = as.integer(l$flight_number %||% NA),
      date_utc = as.POSIXct(l$date_utc %||% NA, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      success = as.logical(l$success %||% NA),
      details = as.character(l$details %||% NA),
      rocket = as.character(l$rocket %||% NA),
      launchpad = as.character(l$launchpad %||% NA),
      upcoming = as.logical(l$upcoming %||% NA)
    )
  })
  bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Launches ==================================================================

#' Fetch SpaceX launches
#'
#' Returns all SpaceX launches (past and upcoming) from the SpaceX v4 API.
#' Results are sorted by date descending (most recent first). The
#' \code{rocket} and \code{launchpad} columns contain internal IDs that
#' can be cross-referenced with \code{\link{spacex_rockets}}.
#'
#' @param limit Integer or NULL. Optional maximum number of launches to
#'   return (most recent first). Default \code{NULL} returns all launches.
#' @return A tibble with one row per launch:
#' \describe{
#'   \item{id}{Character. Unique launch identifier.}
#'   \item{name}{Character. Mission name (e.g. "Crew-5", "Starlink 4-37").}
#'   \item{flight_number}{Integer. Sequential flight number.}
#'   \item{date_utc}{POSIXct. Launch date/time in UTC.}
#'   \item{success}{Logical. Whether the mission was successful (NA if upcoming).}
#'   \item{details}{Character. Mission details (may be NA).}
#'   \item{rocket}{Character. Rocket ID (cross-reference with spacex_rockets()).}
#'   \item{launchpad}{Character. Launchpad ID.}
#'   \item{upcoming}{Logical. TRUE if the launch has not yet occurred.}
#' }
#' @export
#' @examples
#' \dontrun{
#' spacex_launches(limit = 10)
#' # Successful launches only
#' spacex_launches() |> dplyr::filter(success == TRUE)
#' }
spacex_launches <- function(limit = NULL) {
  raw <- .fetch_json(sprintf("%s/launches", .spacex_base))
  if (is.null(raw) || length(raw) == 0) return(.schema_launches)

  result <- .parse_launches(raw)
  result <- result |> arrange(desc(date_utc))
  if (!is.null(limit)) result <- head(result, limit)
  result
}

#' Fetch the latest SpaceX launch
#'
#' Returns information about the most recent SpaceX launch that has
#' already occurred (not upcoming).
#'
#' @return A tibble with a single row and the same columns as
#'   \code{\link{spacex_launches}}.
#' @export
#' @examples
#' \dontrun{
#' spacex_latest()
#' }
spacex_latest <- function() {
  raw <- .fetch_json(sprintf("%s/launches/latest", .spacex_base))
  if (is.null(raw)) return(.schema_launches)
  .parse_launches(list(raw))
}

#' Fetch SpaceX rockets
#'
#' Returns specifications for all SpaceX rocket vehicles including
#' Falcon 1, Falcon 9, Falcon Heavy, and Starship. Includes cost,
#' physical specifications, and operational status.
#'
#' @return A tibble with one row per rocket:
#' \describe{
#'   \item{id}{Character. Unique rocket identifier.}
#'   \item{name}{Character. Rocket name (e.g. "Falcon 9", "Falcon Heavy").}
#'   \item{type}{Character. Vehicle type (e.g. "rocket").}
#'   \item{active}{Logical. Whether the rocket is currently in active service.}
#'   \item{stages}{Integer. Number of stages.}
#'   \item{boosters}{Integer. Number of side boosters (0 for most, 2 for Falcon Heavy).}
#'   \item{cost_per_launch}{Numeric. Estimated cost per launch in USD.}
#'   \item{first_flight}{Date. Date of the rocket's maiden flight.}
#'   \item{country}{Character. Country of origin.}
#'   \item{company}{Character. Operating company ("SpaceX").}
#'   \item{description}{Character. Text description of the rocket.}
#' }
#' @export
#' @examples
#' \dontrun{
#' spacex_rockets()
#' # Active rockets only
#' spacex_rockets() |> dplyr::filter(active)
#' }
spacex_rockets <- function() {
  raw <- .fetch_json(sprintf("%s/rockets", .spacex_base))
  if (is.null(raw) || length(raw) == 0) return(.schema_rockets)

  rows <- lapply(raw, function(r) {
    tibble(
      id = as.character(r$id %||% NA),
      name = as.character(r$name %||% NA),
      type = as.character(r$type %||% NA),
      active = as.logical(r$active %||% NA),
      stages = as.integer(r$stages %||% NA),
      boosters = as.integer(r$boosters %||% NA),
      cost_per_launch = as.numeric(r$cost_per_launch %||% NA),
      first_flight = as.Date(r$first_flight %||% NA),
      country = as.character(r$country %||% NA),
      company = as.character(r$company %||% NA),
      description = as.character(r$description %||% NA)
    )
  })
  bind_rows(rows)
}


# == Context (LLM injection) ==================================================

#' Get spacexdata-com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
spacex_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(spacex_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/spacexdata-com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "spacexdata-com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# spacexdata-com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# spacexdata-com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
