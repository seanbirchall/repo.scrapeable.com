# psmsl-org.R
# Self-contained Permanent Service for Mean Sea Level (PSMSL) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.psmsl_base <- "https://psmsl.org"

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

.fetch <- function(url, ext = ".txt") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_stations <- tibble(
  station_id = integer(), latitude = numeric(), longitude = numeric(),
  name = character(), coastline_code = integer(),
  station_code = integer(), quality = character()
)

.schema_annual <- tibble(
  year = integer(), level_mm = numeric(), missing = logical(),
  flag = character(), station_id = integer()
)


# == Station listing ===========================================================

#' List all PSMSL tide gauge stations
#'
#' Returns the complete station catalog from the Revised Local Reference
#' (RLR) dataset. Includes station coordinates, names, and metadata.
#'
#' @return tibble: station_id (integer), latitude (numeric),
#'   longitude (numeric), name (character), coastline_code (integer),
#'   station_code (integer), quality (character: Y/N for RLR quality)
psmsl_stations <- function() {
  url <- sprintf("%s/data/obtaining/rlr.monthly.data/filelist.txt", .psmsl_base)
  f <- .fetch(url)
  lines <- readLines(f, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]

  rows <- lapply(lines, function(line) {
    parts <- strsplit(line, ";")[[1]]
    if (length(parts) < 6) return(NULL)
    tibble(
      station_id     = as.integer(trimws(parts[1])),
      latitude       = as.numeric(trimws(parts[2])),
      longitude      = as.numeric(trimws(parts[3])),
      name           = trimws(parts[4]),
      coastline_code = as.integer(trimws(parts[5])),
      station_code   = as.integer(trimws(parts[6])),
      quality        = if (length(parts) >= 7) trimws(parts[7]) else NA_character_
    )
  })
  bind_rows(rows)
}


# == Annual sea level data =====================================================

#' Fetch annual mean sea level data for a PSMSL station
#'
#' Returns the Revised Local Reference (RLR) annual mean sea level
#' record for a specific tide gauge station. Values of -99999 indicate
#' missing data and are returned as NA.
#'
#' @param station_id PSMSL station ID (from psmsl_stations)
#' @return tibble: year (integer), level_mm (numeric, mm above RLR datum),
#'   missing (logical), flag (character), station_id (integer)
psmsl_annual <- function(station_id) {
  url <- sprintf("%s/data/obtaining/rlr.annual.data/%d.rlrdata",
                 .psmsl_base, station_id)
  f <- tryCatch(.fetch(url), error = function(e) NULL)
  if (is.null(f)) return(.schema_annual)

  lines <- readLines(f, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(.schema_annual)

  rows <- lapply(lines, function(line) {
    parts <- strsplit(line, ";")[[1]]
    if (length(parts) < 2) return(NULL)
    yr <- as.integer(trimws(parts[1]))
    val <- as.numeric(trimws(parts[2]))
    is_missing <- !is.na(val) && val == -99999
    tibble(
      year       = yr,
      level_mm   = if (is_missing) NA_real_ else val,
      missing    = is_missing,
      flag       = if (length(parts) >= 3) trimws(parts[3]) else NA_character_,
      station_id = as.integer(station_id)
    )
  })
  bind_rows(rows)
}


# == Context ===================================================================

#' Show PSMSL package context for LLM integration
#'
#' Prints a summary of all public functions, their signatures, and
#' roxygen documentation. Designed for LLM context injection.
#'
#' @return Invisibly returns the context string
#' @export
psmsl_context <- function() {
  header <- c(
    "# psmsl.org - Permanent Service for Mean Sea Level Client",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Notable stations:",
    "#   1 - Brest, France (1807-present, longest record)",
    "#   10 - San Francisco, USA",
    "#   12 - New York (The Battery), USA",
    "#   14 - Helsinki, Finland",
    "#",
    "# Data: RLR (Revised Local Reference) annual means in mm",
    "# Missing values coded as -99999, returned as NA"
  )
  .build_context("psmsl.org", header_lines = header)
}
