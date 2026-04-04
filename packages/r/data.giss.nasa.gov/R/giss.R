# data.giss.nasa.gov.R - Self-contained data.giss.nasa.gov client



# data-giss-nasa-gov.R
# Self-contained NASA GISS Surface Temperature (GISTEMP) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.giss_base <- "https://data.giss.nasa.gov/gistemp/tabledata_v4"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url, ext = ".json"))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_global_temp <- tibble(
  year = integer(), jan = numeric(), feb = numeric(), mar = numeric(),
  apr = numeric(), may = numeric(), jun = numeric(), jul = numeric(),
  aug = numeric(), sep = numeric(), oct = numeric(), nov = numeric(),
  dec = numeric(), j_d = numeric(), d_n = numeric(), djf = numeric(),
  mam = numeric(), jja = numeric(), son = numeric()
)

# == Public functions ==========================================================


#' Download and parse GISTEMP global temperature anomalies
#'
#' Fetches monthly global mean surface temperature anomalies (degrees Celsius)
#' relative to the 1951-1980 base period from the NASA Goddard Institute for
#' Space Studies (GISS). Data spans from 1880 to present (~147 rows). Four
#' dataset variants are available covering different surface types and
#' hemispheres.
#'
#' @param dataset Character. One of:
#'   \describe{
#'     \item{\code{"global"}}{Land + ocean combined (GLB.Ts+dSST.csv). Default.}
#'     \item{\code{"land"}}{Land-only (GLB.Ts.csv).}
#'     \item{\code{"northern"}}{Northern hemisphere land + ocean (NH.Ts+dSST.csv).}
#'     \item{\code{"southern"}}{Southern hemisphere land + ocean (SH.Ts+dSST.csv).}
#'   }
#' @return A tibble with 19 columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year (1880--present).}
#'     \item{jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec}{Numeric. Monthly temperature anomaly in degrees Celsius.}
#'     \item{j_d}{Numeric. January-December annual mean anomaly.}
#'     \item{d_n}{Numeric. December-November annual mean anomaly.}
#'     \item{djf}{Numeric. Winter (Dec-Jan-Feb) seasonal mean.}
#'     \item{mam}{Numeric. Spring (Mar-Apr-May) seasonal mean.}
#'     \item{jja}{Numeric. Summer (Jun-Jul-Aug) seasonal mean.}
#'     \item{son}{Numeric. Autumn (Sep-Oct-Nov) seasonal mean.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Global land+ocean anomalies
#' giss_global_temp()
#'
#' # Northern hemisphere only
#' giss_global_temp(dataset = "northern")
#' }
giss_global_temp <- function(dataset = "global") {
  file_map <- list(
    global   = "GLB.Ts+dSST.csv",
    land     = "GLB.Ts.csv",
    northern = "NH.Ts+dSST.csv",
    southern = "SH.Ts+dSST.csv"
  )
  fname <- file_map[[dataset]]
  if (is.null(fname)) stop("dataset must be one of: global, land, northern, southern")

  url <- paste0(.giss_base, "/", fname)
  tmp <- .fetch(url, ext = ".csv")
  raw_lines <- readLines(tmp, warn = FALSE)

  # Find the header row (starts with "Year")
  header_idx <- grep("^Year", raw_lines)[1]
  if (is.na(header_idx)) stop("Could not find header row in CSV")

  # Read from header row, replacing *** with NA
  data_lines <- raw_lines[header_idx:length(raw_lines)]
  data_lines <- gsub("\\*\\*\\*", "NA", data_lines)

  # Remove any trailing non-data lines (like "Year" appearing again)

  con <- textConnection(data_lines)
  df <- tryCatch(
    read.csv(con, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  close(con)

  if (is.null(df) || nrow(df) == 0) return(.schema_global_temp)

  # Clean column names
  names(df) <- tolower(names(df))
  names(df) <- gsub("-", "_", names(df))

  # Remove non-numeric year rows

  df <- df[grepl("^\\d{4}$", trimws(df$year)), , drop = FALSE]

  df <- as_tibble(df) |>
    mutate(
      year = as.integer(year),
      across(-year, ~ suppressWarnings(as.numeric(.x)))
    )

  df
}

#' Download GISTEMP zonal mean temperature anomalies
#'
#' Fetches annual zonal mean surface temperature anomalies by latitude band
#' from NASA GISS. Each row is one year with anomaly values for global,
#' hemispheric, and 8 latitude bands. Useful for analyzing spatial patterns
#' in warming trends.
#'
#' @param dataset Character. One of:
#'   \describe{
#'     \item{\code{"combined"}}{Land + ocean combined (ZonAnn.Ts+dSST.csv). Default.}
#'     \item{\code{"land"}}{Land-only (ZonAnn.Ts.csv).}
#'   }
#' @return A tibble with 15 columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year (1880--present).}
#'     \item{glob}{Numeric. Global annual mean anomaly (degrees C).}
#'     \item{nhem}{Numeric. Northern hemisphere mean.}
#'     \item{shem}{Numeric. Southern hemisphere mean.}
#'     \item{24n_90n}{Numeric. 24N to 90N latitude band.}
#'     \item{24s_24n}{Numeric. 24S to 24N (tropics).}
#'     \item{90s_24s}{Numeric. 90S to 24S latitude band.}
#'     \item{64n_90n, 44n_64n, 24n_44n, equ_24n, 24s_equ, 44s_24s, 64s_44s, 90s_64s}{Numeric. Finer latitude band anomalies.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' giss_zonal_temp()
#' giss_zonal_temp(dataset = "land")
#' }
giss_zonal_temp <- function(dataset = "combined") {
  file_map <- list(
    combined = "ZonAnn.Ts+dSST.csv",
    land     = "ZonAnn.Ts.csv"
  )
  fname <- file_map[[dataset]]
  if (is.null(fname)) stop("dataset must be one of: combined, land")

  url <- paste0(.giss_base, "/", fname)
  tmp <- .fetch(url, ext = ".csv")
  raw_lines <- readLines(tmp, warn = FALSE)

  header_idx <- grep("^Year", raw_lines)[1]
  if (is.na(header_idx)) stop("Could not find header row in CSV")

  data_lines <- raw_lines[header_idx:length(raw_lines)]
  data_lines <- gsub("\\*\\*\\*", "NA", data_lines)

  con <- textConnection(data_lines)
  df <- tryCatch(
    read.csv(con, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  close(con)

  if (is.null(df) || nrow(df) == 0) return(tibble(year = integer(), glob = numeric()))

  names(df) <- tolower(names(df))
  names(df) <- gsub("-", "_", names(df))
  df <- df[grepl("^\\d{4}$", trimws(df$year)), , drop = FALSE]

  as_tibble(df) |>
    mutate(
      year = as.integer(year),
      across(-year, ~ suppressWarnings(as.numeric(.x)))
    )
}

#' Get GISTEMP temperature anomaly for a specific year
#'
#' Convenience wrapper around \code{giss_global_temp()} that returns a single
#' row for the specified year. Includes all 12 monthly anomalies plus seasonal
#' and annual means.
#'
#' @param year Integer. Calendar year to retrieve (1880--present). Example:
#'   \code{2023}.
#' @param dataset Character. One of \code{"global"}, \code{"land"},
#'   \code{"northern"}, \code{"southern"}. Default \code{"global"}.
#' @return A tibble with one row and the same 19 columns as
#'   \code{giss_global_temp()}: year, jan--dec, j_d, d_n, djf, mam, jja, son.
#'   Returns zero rows if the year is not found.
#' @export
#' @examples
#' \dontrun{
#' giss_year(2023)
#' # => year=2023, jan=0.87, feb=0.96, ..., j_d=1.17
#'
#' giss_year(1880, dataset = "land")
#' }
giss_year <- function(year, dataset = "global") {
  df <- giss_global_temp(dataset = dataset)
  df[df$year == as.integer(year), , drop = FALSE]
}

#' Get GISTEMP trend summary (first and last decade averages)
#'
#' Computes the average annual temperature anomaly for the first and last full
#' decades in the GISTEMP record, providing a quick summary of long-term
#' warming. Uses the Jan-Dec annual mean (\code{j_d} column).
#'
#' @param dataset Character. One of \code{"global"}, \code{"land"},
#'   \code{"northern"}, \code{"southern"}. Default \code{"global"}.
#' @return A tibble with 2 rows and columns:
#'   \describe{
#'     \item{period}{Character. Decade range (e.g. \code{"1880-1889"}, \code{"2017-2026"}).}
#'     \item{avg_annual_anomaly}{Numeric. Mean annual anomaly in degrees Celsius for the decade.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' giss_trend()
#' # => 1880-1889: -0.214, 2017-2026: 1.01
#'
#' giss_trend(dataset = "northern")
#' }
giss_trend <- function(dataset = "global") {
  df <- giss_global_temp(dataset = dataset)
  if (nrow(df) == 0) return(tibble(period = character(), avg_annual_anomaly = numeric()))

  # Use j_d (Jan-Dec annual mean) if available
  annual_col <- if ("j_d" %in% names(df)) "j_d" else if ("d_n" %in% names(df)) "d_n" else NULL
  if (is.null(annual_col)) return(tibble(period = character(), avg_annual_anomaly = numeric()))

  min_yr <- min(df$year, na.rm = TRUE)
  max_yr <- max(df$year, na.rm = TRUE)

  first_decade <- df[df$year >= min_yr & df$year < min_yr + 10, ]
  last_decade <- df[df$year > max_yr - 10 & df$year <= max_yr, ]

  tibble(
    period = c(
      paste0(min_yr, "-", min_yr + 9),
      paste0(max_yr - 9, "-", max_yr)
    ),
    avg_annual_anomaly = c(
      mean(first_decade[[annual_col]], na.rm = TRUE),
      mean(last_decade[[annual_col]], na.rm = TRUE)
    )
  )
}

# == Context ===================================================================

#' Get data.giss.nasa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
giss_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(giss_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/data.giss.nasa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "data.giss.nasa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# data.giss.nasa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# data.giss.nasa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
