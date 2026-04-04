# swpc.noaa.gov.R - Self-contained swpc.noaa.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# swpc-noaa-gov.R
# Self-contained NOAA Space Weather Prediction Center client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.swpc_base <- "https://services.swpc.noaa.gov/products"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- Parse array-of-arrays format (first row = header) ------------------------

.parse_swpc_array <- function(raw) {
  if (length(raw) < 2) return(tibble())
  headers <- unlist(raw[[1]])
  rows <- raw[-1]
  mat <- do.call(rbind, lapply(rows, function(r) unlist(r)))
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- headers
  as_tibble(df)
}

# == Schemas ===================================================================

.schema_plasma <- tibble(
  time_tag = as.POSIXct(character()), density = numeric(),
  speed = numeric(), temperature = numeric()
)

.schema_mag <- tibble(
  time_tag = as.POSIXct(character()), bx_gsm = numeric(),
  by_gsm = numeric(), bz_gsm = numeric(),
  lon_gsm = numeric(), lat_gsm = numeric(), bt = numeric()
)

.schema_kp <- tibble(
  time_tag = as.POSIXct(character()), kp = numeric(),
  a_running = numeric(), station_count = integer()
)

.schema_alerts <- tibble(
  product_id = character(), issue_datetime = as.POSIXct(character()),
  message = character()
)



# == Solar wind plasma =========================================================

#' Fetch NOAA SWPC solar wind plasma data
#'
#' Returns real-time solar wind plasma measurements from the DSCOVR
#' satellite at the L1 Lagrange point. Data includes proton density,
#' bulk speed, and temperature at approximately 1-minute resolution.
#' Useful for space weather monitoring and geomagnetic storm prediction.
#'
#' @param days Numeric. Time window to retrieve. Must be one of:
#' \describe{
#'   \item{\code{0.08}}{Last 2 hours}
#'   \item{\code{0.25}}{Last 6 hours}
#'   \item{\code{1}}{Last 1 day}
#'   \item{\code{3}}{Last 3 days}
#'   \item{\code{7}}{Last 7 days (default)}
#' }
#' @return A tibble with 4 columns:
#' \describe{
#'   \item{time_tag}{POSIXct. UTC timestamp of measurement.}
#'   \item{density}{Numeric. Proton density in particles/cm^3.}
#'   \item{speed}{Numeric. Solar wind bulk speed in km/s.}
#'   \item{temperature}{Numeric. Proton temperature in Kelvin.}
#' }
#' @examples
#' \dontrun{
#' swpc_plasma(1)      # last 24 hours
#' swpc_plasma(0.25)   # last 6 hours
#' }
#' @seealso \code{\link{swpc_mag}} for magnetic field data from the same satellite.
#' @export
swpc_plasma <- function(days = 7) {
  slug <- switch(as.character(days),
    "0.08" = "2-hour", "0.25" = "6-hour",
    "1" = "1-day", "3" = "3-day", "7" = "7-day",
    stop("days must be one of: 0.08, 0.25, 1, 3, 7")
  )
  url <- sprintf("%s/solar-wind/plasma-%s.json", .swpc_base, slug)
  raw <- .fetch_json(url)
  df <- .parse_swpc_array(raw)
  if (nrow(df) == 0) return(.schema_plasma)

  df |>
    transmute(
      time_tag    = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      density     = as.numeric(density),
      speed       = as.numeric(speed),
      temperature = as.numeric(temperature)
    )
}


# == Solar wind magnetic field =================================================

#' Fetch NOAA SWPC solar wind magnetic field data
#'
#' Returns real-time interplanetary magnetic field (IMF) measurements in
#' Geocentric Solar Magnetospheric (GSM) coordinates from the DSCOVR
#' satellite. The Bz component is especially important for geomagnetic
#' storm forecasting: sustained negative Bz values indicate southward
#' IMF which can trigger geomagnetic storms.
#'
#' @param days Numeric. Time window to retrieve. Must be one of:
#'   \code{0.08} (2hr), \code{0.25} (6hr), \code{1}, \code{3}, or
#'   \code{7} (default).
#' @return A tibble with 7 columns:
#' \describe{
#'   \item{time_tag}{POSIXct. UTC timestamp of measurement.}
#'   \item{bx_gsm}{Numeric. Bx component in GSM coordinates (nT).}
#'   \item{by_gsm}{Numeric. By component in GSM coordinates (nT).}
#'   \item{bz_gsm}{Numeric. Bz component in GSM coordinates (nT). Negative values indicate southward IMF.}
#'   \item{lon_gsm}{Numeric. GSM longitude (degrees).}
#'   \item{lat_gsm}{Numeric. GSM latitude (degrees).}
#'   \item{bt}{Numeric. Total magnetic field magnitude (nT).}
#' }
#' @examples
#' \dontrun{
#' swpc_mag(1)    # last 24 hours of IMF data
#' }
#' @seealso \code{\link{swpc_plasma}} for solar wind plasma data.
#' @export
swpc_mag <- function(days = 7) {
  slug <- switch(as.character(days),
    "0.08" = "2-hour", "0.25" = "6-hour",
    "1" = "1-day", "3" = "3-day", "7" = "7-day",
    stop("days must be one of: 0.08, 0.25, 1, 3, 7")
  )
  url <- sprintf("%s/solar-wind/mag-%s.json", .swpc_base, slug)
  raw <- .fetch_json(url)
  df <- .parse_swpc_array(raw)
  if (nrow(df) == 0) return(.schema_mag)

  df |>
    transmute(
      time_tag = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      bx_gsm   = as.numeric(bx_gsm),
      by_gsm   = as.numeric(by_gsm),
      bz_gsm   = as.numeric(bz_gsm),
      lon_gsm  = as.numeric(lon_gsm),
      lat_gsm  = as.numeric(lat_gsm),
      bt       = as.numeric(bt)
    )
}


# == Kp index ==================================================================

#' Fetch NOAA planetary Kp index
#'
#' Returns the planetary K-index (Kp), a global measure of geomagnetic
#' disturbance on a 0-9 scale derived from ground-based magnetometer
#' stations. The Kp index is issued every 3 hours by NOAA SWPC.
#'
#' Kp scale interpretation:
#' \itemize{
#'   \item 0-1: Quiet
#'   \item 2-3: Unsettled
#'   \item 4: Active
#'   \item 5: Minor storm (G1)
#'   \item 6: Moderate storm (G2)
#'   \item 7: Strong storm (G3)
#'   \item 8: Severe storm (G4)
#'   \item 9: Extreme storm (G5)
#' }
#'
#' @return A tibble with 4 columns:
#' \describe{
#'   \item{time_tag}{POSIXct. UTC timestamp for the 3-hour interval.}
#'   \item{kp}{Numeric. Planetary K-index value (0-9).}
#'   \item{a_running}{Numeric. Running A-index (linear equivalent of Kp).}
#'   \item{station_count}{Integer. Number of reporting magnetometer stations.}
#' }
#' @examples
#' \dontrun{
#' swpc_kp_index()
#' }
#' @export
swpc_kp_index <- function() {
  url <- sprintf("%s/noaa-planetary-k-index.json", .swpc_base)
  raw <- .fetch_json(url)
  df <- .parse_swpc_array(raw)
  if (nrow(df) == 0) return(.schema_kp)

  df |>
    transmute(
      time_tag      = as.POSIXct(time_tag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      kp            = as.numeric(Kp),
      a_running     = as.numeric(a_running),
      station_count = as.integer(station_count)
    )
}


# == Space weather alerts ======================================================

#' Fetch NOAA space weather alerts
#'
#' Returns recent space weather alerts, watches, and warnings issued by
#' the NOAA Space Weather Prediction Center. Includes geomagnetic storm
#' watches, solar flare alerts, radiation storm warnings, and other
#' advisories. Messages contain full text with severity levels and
#' expected impacts.
#'
#' @return A tibble with 3 columns:
#' \describe{
#'   \item{product_id}{Character. Alert product code (e.g. "K05A" for Kp5 alert,
#'     "XM5A" for X-ray M5 alert).}
#'   \item{issue_datetime}{POSIXct. UTC timestamp when the alert was issued.}
#'   \item{message}{Character. Full text of the alert message including serial
#'     number, issue time, and detailed description.}
#' }
#' @examples
#' \dontrun{
#' swpc_alerts()
#' }
#' @export
swpc_alerts <- function() {
  url <- sprintf("%s/alerts.json", .swpc_base)
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_alerts)

  rows <- lapply(raw, function(x) {
    tibble(
      product_id     = as.character(x$product_id %||% NA),
      issue_datetime = as.POSIXct(x$issue_datetime %||% NA,
                                  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      message        = as.character(x$message %||% NA)
    )
  })
  bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Context ===================================================================

#' Get swpc.noaa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
swpc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(swpc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/swpc.noaa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "swpc.noaa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# swpc.noaa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# swpc.noaa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
