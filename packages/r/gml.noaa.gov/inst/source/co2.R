


# gml-noaa-gov.R
# Self-contained NOAA GML CO2/CH4 trends client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none
# Rate limits: none - static CSV files


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gml_base <- "https://gml.noaa.gov/webdata/ccgg/trends"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_trend <- tibble(
  date = as.Date(character()), smoothed = numeric(), trend = numeric()
)

.schema_annual <- tibble(
  year = integer(), mean = numeric(), unc = numeric()
)

.schema_monthly <- tibble(
  year = integer(), month = integer(), decimal = numeric(),
  average = numeric(), average_unc = numeric(),
  trend = numeric(), trend_unc = numeric()
)

# == CO2 data ==================================================================


#' Fetch daily global CO2 trend data
#'
#' Smoothed and trend values from NOAA GML. Updated daily.
#'
#' @return tibble: date (Date), smoothed (numeric ppm), trend (numeric ppm)
#' @export
co2_global_trend <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_trend_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_trend)
  raw |>
    as_tibble() |>
    transmute(
      date     = as.Date(sprintf("%04d-%02d-%02d", as.integer(year), as.integer(month), as.integer(day))),
      smoothed = as.numeric(smoothed),
      trend    = as.numeric(trend)
    )
}

#' Fetch annual global mean CO2
#'
#' @return tibble: year (integer), mean (numeric ppm), unc (numeric)
#' @export
co2_annual <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_annmean_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_annual)
  raw |>
    as_tibble() |>
    transmute(
      year = as.integer(year),
      mean = as.numeric(mean),
      unc  = as.numeric(unc)
    )
}

#' Fetch monthly global CO2 data
#'
#' @return tibble: year, month, decimal, average, average_unc, trend, trend_unc
#' @export
co2_monthly <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/co2/co2_mm_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_monthly)
  raw |>
    as_tibble() |>
    transmute(
      year        = as.integer(year),
      month       = as.integer(month),
      decimal     = as.numeric(decimal),
      average     = as.numeric(average),
      average_unc = as.numeric(average_unc),
      trend       = as.numeric(trend),
      trend_unc   = as.numeric(trend_unc)
    )
}

#' Fetch monthly global CH4 (methane) data
#'
#' @return tibble: year, month, decimal, average, average_unc, trend, trend_unc
#' @export
co2_ch4_monthly <- function() {
  f <- .fetch_csv(paste0(.gml_base, "/ch4/ch4_mm_gl.csv"))
  raw <- read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (nrow(raw) == 0) return(.schema_monthly)
  # ch4 csv may have blank rows
  raw <- raw[!is.na(raw$year) & raw$year != "", ]
  raw |>
    as_tibble() |>
    transmute(
      year        = as.integer(year),
      month       = as.integer(month),
      decimal     = as.numeric(decimal),
      average     = as.numeric(average),
      average_unc = as.numeric(average_unc),
      trend       = as.numeric(trend),
      trend_unc   = as.numeric(trend_unc)
    )
}

# == Context ===================================================================

#' Generate LLM-friendly context for gml.noaa.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
co2_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/gml.noaa.gov.R"
  if (!file.exists(src_file)) {
    cat("# gml.noaa.gov context - source not found\n")
    return(invisible("# gml.noaa.gov context - source not found"))
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

