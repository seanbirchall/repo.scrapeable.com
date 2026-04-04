


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

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Context ===================================================================

#' Generate LLM-friendly context for swpc.noaa.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
swpc_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/swpc.noaa.gov.R"
  if (!file.exists(src_file)) {
    cat("# swpc.noaa.gov context - source not found\n")
    return(invisible("# swpc.noaa.gov context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

