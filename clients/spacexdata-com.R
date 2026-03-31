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
#' Returns all SpaceX launches (past and upcoming).
#'
#' @param limit Optional maximum number of launches to return (most recent first)
#' @return tibble: id, name, flight_number, date_utc, success, details,
#'   rocket, launchpad, upcoming
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
#' @return tibble: single row with same columns as spacex_launches
spacex_latest <- function() {
  raw <- .fetch_json(sprintf("%s/launches/latest", .spacex_base))
  if (is.null(raw)) return(.schema_launches)
  .parse_launches(list(raw))
}

#' Fetch SpaceX rockets
#'
#' @return tibble: id, name, type, active, stages, boosters,
#'   cost_per_launch, first_flight, country, company, description
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

#' Generate LLM-friendly context for the spacexdata package
#'
#' @return Character string (invisibly), also printed
spacex_context <- function() {
  .build_context("spacexdata.com", header_lines = c(
    "# spacexdata.com - SpaceX Launch and Rocket Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: none known",
    "# All functions return tibbles with typed columns."
  ))
}
