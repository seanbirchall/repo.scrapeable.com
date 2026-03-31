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
#' Returns the current latitude and longitude of the International Space Station.
#'
#' @return tibble: timestamp (POSIXct), latitude (numeric), longitude (numeric)
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
#' Returns all people currently in space, including their spacecraft.
#'
#' @return tibble: name (character), craft (character)
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

#' Generate LLM-friendly context for the open-notify package
#'
#' @return Character string (invisibly), also printed
iss_context <- function() {
  .build_context("open.notify.org", header_lines = c(
    "# open.notify.org - ISS Tracking and Astronaut Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: none known",
    "# All functions return tibbles with typed columns."
  ))
}
