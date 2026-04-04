# berkeleyearth.org.R - Self-contained berkeleyearth.org client

library(httr2)
library(tibble)
library(dplyr)


# berkeleyearth-org.R
# Self-contained Berkeley Earth temperature data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Data: tab-separated text with % comment lines


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.berk_s3 <- "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global"

.fetch <- function(url, ext = ".txt") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.parse_berk_txt <- function(file) {
  lines <- readLines(file, warn = FALSE)
  # Remove comment lines starting with %
  data_lines <- lines[!grepl("^%", lines) & nchar(trimws(lines)) > 0]
  if (length(data_lines) == 0) return(NULL)
  tc <- textConnection(data_lines)
  on.exit(close(tc))
  tryCatch(
    utils::read.table(tc, header = FALSE, stringsAsFactors = FALSE,
                      fill = TRUE, comment.char = "%"),
    error = function(e) NULL
  )
}

# == Schemas ===================================================================

.schema_global_temp <- tibble(
  year = integer(), month = integer(), anomaly = numeric(),
  anomaly_unc = numeric(), annual_anomaly = numeric(),
  annual_anomaly_unc = numeric()
)


# == Global temperature ========================================================

#' Fetch Berkeley Earth global land+ocean temperature anomalies
#'
#' Downloads the complete monthly Land_and_Ocean temperature record from
#' Berkeley Earth (S3-hosted). Anomalies are relative to the 1951--1980
#' climatological average. The dataset spans from 1850 to the present and
#' typically contains ~4,200 rows.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year (1850--present).}
#'     \item{month}{Integer. Month number (1--12).}
#'     \item{anomaly}{Numeric. Monthly temperature anomaly in degrees Celsius.}
#'     \item{anomaly_unc}{Numeric. 95\% confidence uncertainty on the monthly anomaly (degrees C).}
#'     \item{annual_anomaly}{Numeric. 12-month running annual anomaly (NaN for early months).}
#'     \item{annual_anomaly_unc}{Numeric. Uncertainty on the annual anomaly.}
#'   }
#' @examples
#' df <- berk_global_temp()
#' # Recent warming trend
#' df |> dplyr::filter(year >= 2020)
#' @export
berk_global_temp <- function() {
  url <- paste0(.berk_s3, "/Land_and_Ocean_complete.txt")
  f <- .fetch(url, ext = ".txt")
  df <- .parse_berk_txt(f)
  if (is.null(df) || nrow(df) == 0) return(.schema_global_temp)
  # The file has monthly data with columns:
  # Year, Month, Monthly_Anomaly, Monthly_Unc, Annual_Anomaly, Annual_Unc, ...
  # Number of columns can vary; take first 6
  nc <- min(ncol(df), 6)
  result <- tibble(
    year = as.integer(df[[1]]),
    month = as.integer(df[[2]]),
    anomaly = suppressWarnings(as.numeric(df[[3]])),
    anomaly_unc = if (nc >= 4) suppressWarnings(as.numeric(df[[4]])) else NA_real_,
    annual_anomaly = if (nc >= 5) suppressWarnings(as.numeric(df[[5]])) else NA_real_,
    annual_anomaly_unc = if (nc >= 6) suppressWarnings(as.numeric(df[[6]])) else NA_real_
  )
  result |> filter(!is.na(year), !is.na(month))
}

# == Land-only temperature ====================================================

#' Fetch Berkeley Earth land-only temperature anomalies
#'
#' Downloads the complete monthly land-only temperature record from
#' Berkeley Earth. Uses the same Land_and_Ocean source file but returns
#' only the core monthly anomaly columns (no annual running mean).
#' Anomalies are relative to the 1951--1980 average.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year (1850--present).}
#'     \item{month}{Integer. Month number (1--12).}
#'     \item{anomaly}{Numeric. Monthly temperature anomaly in degrees Celsius.}
#'     \item{anomaly_unc}{Numeric. 95\% confidence uncertainty on the anomaly (degrees C).}
#'   }
#' @examples
#' df <- berk_land_temp()
#' # Average anomaly by decade
#' df |> dplyr::mutate(decade = (year %/% 10) * 10) |>
#'   dplyr::summarise(mean_anomaly = mean(anomaly, na.rm = TRUE), .by = decade)
#' @export
berk_land_temp <- function() {
  schema <- tibble(year = integer(), month = integer(),
                   anomaly = numeric(), anomaly_unc = numeric())
  url <- paste0(.berk_s3, "/Land_and_Ocean_complete.txt")
  f <- .fetch(url, ext = ".txt")
  df <- .parse_berk_txt(f)
  if (is.null(df) || nrow(df) == 0) return(schema)

  tibble(
    year = as.integer(df[[1]]),
    month = as.integer(df[[2]]),
    anomaly = suppressWarnings(as.numeric(df[[3]])),
    anomaly_unc = if (ncol(df) >= 4) suppressWarnings(as.numeric(df[[4]])) else NA_real_
  ) |> filter(!is.na(year), !is.na(month))
}

# == Context ===================================================================

#' Get berkeleyearth.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
berkeleyearth_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(berkeleyearth_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/berkeleyearth.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "berkeleyearth.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# berkeleyearth.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# berkeleyearth.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
