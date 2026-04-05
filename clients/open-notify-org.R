# open-notify-org.R
# Self-contained ISS tracking client (Open Notify API).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none known

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.iss_base <- "http://api.open-notify.org"

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
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_position <- tibble(
  timestamp = as.POSIXct(character()), latitude = numeric(), longitude = numeric()
)

.schema_astronauts <- tibble(
  name = character(), craft = character()
)

# == ISS Position ==============================================================

#' Fetch current ISS position
#'
#' Returns the real-time latitude and longitude of the International
#' Space Station, updated every few seconds. The ISS orbits at
#' approximately 408 km altitude and completes one orbit every 90 minutes.
#'
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{timestamp}{Observation time in UTC (POSIXct)}
#'     \item{latitude}{Current latitude in decimal degrees, -90 to 90 (numeric)}
#'     \item{longitude}{Current longitude in decimal degrees, -180 to 180 (numeric)}
#'   }
#' @examples
#' iss_position()
#' @seealso [iss_astronauts()], [iss_context()]
#' @source <http://api.open-notify.org>
#' @export
iss_position <- function() {
  raw <- .fetch_json(sprintf("%s/iss-now.json", .iss_base))
  if (is.null(raw) || raw$message != "success") return(.schema_position)

  tibble(
    timestamp = as.POSIXct(as.numeric(raw$timestamp), origin = "1970-01-01", tz = "UTC"),
    latitude = as.numeric(raw$iss_position$latitude),
    longitude = as.numeric(raw$iss_position$longitude)
  )
}

#' Fetch current astronauts in space
#'
#' Returns all people currently aboard orbital spacecraft, including
#' the ISS, Tiangong, and any other crewed vehicles.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Astronaut/cosmonaut full name (character)}
#'     \item{craft}{Spacecraft name, e.g. "ISS", "Tiangong" (character)}
#'   }
#' @examples
#' iss_astronauts()
#' @seealso [iss_position()], [iss_context()]
#' @source <http://api.open-notify.org>
#' @export
iss_astronauts <- function() {
  raw <- .fetch_json(sprintf("%s/astros.json", .iss_base))
  if (is.null(raw) || raw$message != "success") return(.schema_astronauts)

  people <- raw$people
  if (is.null(people) || nrow(people) == 0) return(.schema_astronauts)

  as_tibble(people) |>
    transmute(
      name = as.character(name),
      craft = as.character(craft)
    )
}


# == Context (LLM injection) ==================================================

#' Get open-notify-org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
iss_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(iss_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/open-notify-org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "open-notify-org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# open-notify-org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# open-notify-org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
