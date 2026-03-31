

# == Solar wind plasma =========================================================

#' Fetch NOAA SWPC solar wind plasma data
#'
#' Returns solar wind plasma measurements (density, speed, temperature)
#' from the DSCOVR satellite. Available in 2-hour, 6-hour, 1-day,
#' 3-day, and 7-day windows.
#'
#' @param days Time window: 0.08 (2hr), 0.25 (6hr), 1, 3, or 7 (default 7)
#' @return tibble: time_tag (POSIXct), density (numeric), speed (numeric),
#'   temperature (numeric)
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
#' Returns interplanetary magnetic field (IMF) measurements in GSM
#' coordinates from the DSCOVR satellite.
#'
#' @param days Time window: 0.08 (2hr), 0.25 (6hr), 1, 3, or 7 (default 7)
#' @return tibble: time_tag (POSIXct), bx_gsm, by_gsm, bz_gsm, lon_gsm,
#'   lat_gsm, bt (all numeric, nT)
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
#' Returns the planetary K-index (Kp), a measure of geomagnetic storm
#' intensity on a 0-9 scale. Values >= 5 indicate geomagnetic storms.
#'
#' @return tibble: time_tag (POSIXct), kp (numeric), a_running (numeric),
#'   station_count (integer)
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
#' Returns recent space weather alerts, watches, and warnings issued
#' by the Space Weather Prediction Center.
#'
#' @return tibble: product_id (character), issue_datetime (POSIXct),
#'   message (character)
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


# == Context ===================================================================

#' Show SWPC package context for LLM integration
#'
#' Prints a summary of all public functions, their signatures, and
#' roxygen documentation. Designed for LLM context injection.
#'
#' @return Invisibly returns the context string
#' @export
#' @export
swpc_context <- function() {
  header <- c(
    "# swpc.noaa.gov - NOAA Space Weather Prediction Center Client",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Common Kp values:",
    "#   0-3: Quiet   4: Unsettled   5: Minor storm (G1)",
    "#   6: Moderate storm (G2)   7: Strong storm (G3)",
    "#   8: Severe storm (G4)   9: Extreme storm (G5)"
  )
  .build_context("swpc.noaa.gov", header_lines = header)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
