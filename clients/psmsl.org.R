# psmsl.org.R - Self-contained psmsl.org client

library(httr2)
library(tibble)
library(dplyr)


# psmsl-org.R
# Self-contained Permanent Service for Mean Sea Level (PSMSL) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.psmsl_base <- "https://psmsl.org"
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
#' Returns the complete station catalog (~1,611 stations) from the
#' Permanent Service for Mean Sea Level (PSMSL) Revised Local Reference
#' (RLR) dataset. This is the primary discovery function: use the
#' returned \code{station_id} values with \code{psmsl_annual()} or
#' \code{psmsl_monthly()} to fetch sea level data.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{station_id}{Integer. Unique PSMSL station identifier.}
#'     \item{latitude}{Numeric. Station latitude (decimal degrees).}
#'     \item{longitude}{Numeric. Station longitude (decimal degrees).}
#'     \item{name}{Character. Station name (often a city or port).}
#'     \item{coastline_code}{Integer. PSMSL coastline region code.}
#'     \item{station_code}{Integer. Station code within the coastline.}
#'     \item{quality}{Character. RLR quality flag (\code{"Y"} = included
#'       in RLR dataset, \code{"N"} = not).}
#'   }
#'
#' @details Data is parsed from the semicolon-delimited file at
#'   \code{psmsl.org/data/obtaining/rlr.monthly.data/filelist.txt}.
#'
#' @examples
#' stations <- psmsl_stations()
#' nrow(stations)   # ~1611
#'
#' # Find stations by name
#' stations[grepl("New York", stations$name), ]
#'
#' @seealso \code{\link{psmsl_annual}}, \code{\link{psmsl_monthly}}
#' @export
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
#' Downloads the Revised Local Reference (RLR) annual mean sea level
#' record for a specific tide gauge station. Many stations have records
#' spanning over a century. Sentinel values of -99999 in the raw data
#' are converted to \code{NA} and flagged via the \code{missing} column.
#'
#' @param station_id Integer. PSMSL station ID (obtain from
#'   \code{psmsl_stations()}).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year.}
#'     \item{level_mm}{Numeric. Annual mean sea level in millimeters above
#'       the RLR datum, or \code{NA} for missing years.}
#'     \item{missing}{Logical. \code{TRUE} if the original value was
#'       -99999 (no data).}
#'     \item{flag}{Character. Quality/interpolation flag, or \code{NA}.}
#'     \item{station_id}{Integer. The queried station ID.}
#'   }
#'
#' @examples
#' # Brest, France (one of the longest records)
#' brest <- psmsl_annual(1)
#' nrow(brest)
#' range(brest$year, na.rm = TRUE)
#'
#' @seealso \code{\link{psmsl_stations}}, \code{\link{psmsl_monthly}}
#' @export
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


# == Monthly data ==============================================================

#' Fetch monthly mean sea level data for a PSMSL station
#'
#' Downloads the Revised Local Reference (RLR) monthly mean sea level
#' record for a specific tide gauge station. The raw data encodes
#' month as a fractional year (e.g. 1990.0417 = January 1990); this
#' function decodes it into separate \code{year} and \code{month}
#' integer columns. Sentinel values of -99999 are converted to \code{NA}.
#'
#' @param station_id Integer. PSMSL station ID (obtain from
#'   \code{psmsl_stations()}).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year.}
#'     \item{month}{Integer. Month number (1--12).}
#'     \item{level_mm}{Numeric. Monthly mean sea level in millimeters
#'       above the RLR datum, or \code{NA} for missing months.}
#'     \item{missing}{Logical. \code{TRUE} if the original value was
#'       -99999.}
#'     \item{station_id}{Integer. The queried station ID.}
#'   }
#'
#' @examples
#' # Monthly data for Holyhead, UK
#' holyhead <- psmsl_monthly(5)
#' head(holyhead, 10)
#'
#' @seealso \code{\link{psmsl_stations}}, \code{\link{psmsl_annual}}
#' @export
psmsl_monthly <- function(station_id) {
  schema <- tibble(year = integer(), month = integer(), level_mm = numeric(),
                   missing = logical(), station_id = integer())
  url <- sprintf("%s/data/obtaining/rlr.monthly.data/%d.rlrdata",
                 .psmsl_base, station_id)
  f <- tryCatch(.fetch(url), error = function(e) NULL)
  if (is.null(f)) return(schema)

  lines <- readLines(f, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(schema)

  rows <- lapply(lines, function(line) {
    parts <- strsplit(line, ";")[[1]]
    if (length(parts) < 2) return(NULL)
    date_part <- as.numeric(trimws(parts[1]))
    yr <- as.integer(floor(date_part))
    mon <- as.integer(round((date_part - yr) * 12) + 1)
    val <- as.numeric(trimws(parts[2]))
    is_missing <- !is.na(val) && val == -99999
    tibble(
      year = yr, month = mon,
      level_mm = if (is_missing) NA_real_ else val,
      missing = is_missing,
      station_id = as.integer(station_id)
    )
  })
  bind_rows(rows)
}

# == Context ===================================================================

#' Get psmsl.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
psmsl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(psmsl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/psmsl.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "psmsl.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# psmsl.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# psmsl.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
