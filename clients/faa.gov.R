# faa.gov.R - Federal Aviation Administration data client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.faa_base <- "https://www.faa.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".html") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_perform(path = tmp)
  tmp
}

# Note: FAA mainly provides large ZIP chart files (VFR charts, enroute charts, etc.)
# and aeronautical data products. The most programmatically useful data is the
# System Activity Report and airport/facility data.

# The FAA NASR (National Airspace System Resources) 28-day subscription data
# has airport, navaid, and airspace data in fixed-width format.

.faa_nasr_base <- "https://nfdc.faa.gov/webContent/28DaySub"

# == Schemas ===================================================================

.schema_products <- tibble(
  name = character(), description = character(),
  url = character(), type = character()
)

# == Public functions ==========================================================

#' List available FAA aeronautical data products
#'
#' Returns a curated catalog of FAA digital aeronautical data products
#' including charts, procedures, airport data, and the aircraft registry.
#' Each row provides a download URL and format description.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Product name (e.g. "Chart Supplements", "Aircraft Registry").}
#'     \item{description}{Character. Brief description of the product.}
#'     \item{url}{Character. Direct download or landing page URL.}
#'     \item{type}{Character. File format: "PDF/ZIP", "ZIP".}
#'   }
#' @export
#' @examples
#' \dontrun{
#' faa_list()
#' }
faa_list <- function() {
  products <- list(
    list(name = "Chart Supplements",
         desc = "Airport data, NAVAIDs, communications, weather, airspace info (PDF)",
         url  = "https://www.faa.gov/air_traffic/flight_info/aeronav/digital_products/dafd/",
         type = "PDF/ZIP"),
    list(name = "Sectional VFR Charts",
         desc = "Georeferenced raster VFR sectional charts",
         url  = "https://www.faa.gov/air_traffic/flight_info/aeronav/digital_products/vfr/",
         type = "ZIP"),
    list(name = "Terminal Area Charts",
         desc = "Georeferenced raster VFR terminal area charts",
         url  = "https://www.faa.gov/air_traffic/flight_info/aeronav/digital_products/vfr/",
         type = "ZIP"),
    list(name = "Enroute Charts",
         desc = "Digital enroute IFR charts (low/high altitude)",
         url  = "https://www.faa.gov/air_traffic/flight_info/aeronav/digital_products/ifr/",
         type = "ZIP"),
    list(name = "Terminal Procedures (TPP)",
         desc = "Instrument approach, departure, STAR, and airport diagrams",
         url  = "https://www.faa.gov/air_traffic/flight_info/aeronav/digital_products/dtpp/",
         type = "ZIP"),
    list(name = "NASR Airport Data (28-day)",
         desc = "National Airspace System Resources - airports, navaids, airways",
         url  = "https://nfdc.faa.gov/webContent/28DaySub/28DaySubscription_Effective_2026-04-03.zip",
         type = "ZIP"),
    list(name = "Aircraft Registry",
         desc = "FAA aircraft registration database (downloadable)",
         url  = "https://registry.faa.gov/database/ReleasableAircraft.zip",
         type = "ZIP")
  )
  tibble(
    name        = vapply(products, `[[`, character(1), "name"),
    description = vapply(products, `[[`, character(1), "desc"),
    url         = vapply(products, `[[`, character(1), "url"),
    type        = vapply(products, `[[`, character(1), "type")
  )
}

#' Download and parse FAA aircraft registry data
#'
#' Downloads the ReleasableAircraft.zip from the FAA registry and parses
#' the MASTER.txt file (comma-delimited). The full file contains ~300K
#' registered aircraft. Use \code{max_rows} to limit for exploration.
#' Note: the download is ~80MB and may take 30+ seconds.
#'
#' @param max_rows Integer. Maximum rows to read from the master file
#'   (default 10000). Set higher for full dataset.
#' @return A tibble with columns including: N-NUMBER, SERIAL NUMBER,
#'   MFR MDL CODE, ENG MFR MDL, YEAR MFR, TYPE REGISTRANT, NAME,
#'   STREET, CITY, STATE, and more (30+ columns).
#' @export
#' @examples
#' \dontrun{
#' faa_aircraft(max_rows = 100)
#' }
faa_aircraft <- function(max_rows = 10000) {
  url <- "https://registry.faa.gov/database/ReleasableAircraft.zip"
  zip_path <- tryCatch(.fetch(url, ext = ".zip"), error = function(e) {
    warning("FAA Aircraft Registry unavailable (may be temporarily down): ", e$message)
    return(NULL)
  })
  if (is.null(zip_path)) return(tibble())
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  utils::unzip(zip_path, exdir = tmp_dir)

  # The master file is MASTER.txt (comma-delimited)
  master <- file.path(tmp_dir, "MASTER.txt")
  if (!file.exists(master)) {
    # Try to find it
    files <- list.files(tmp_dir, pattern = "MASTER", full.names = TRUE,
                        recursive = TRUE, ignore.case = TRUE)
    if (length(files) == 0) {
      warning("MASTER.txt not found in ZIP")
      return(tibble())
    }
    master <- files[1]
  }
  df <- utils::read.csv(master, stringsAsFactors = FALSE, check.names = FALSE,
                        nrows = max_rows)
  tibble::as_tibble(df)
}

#' Search FAA aircraft registry by owner name
#'
#' Downloads the aircraft registry and filters by owner/registrant name
#' using a case-insensitive regex pattern. Wraps \code{faa_aircraft()}
#' with a name filter.
#'
#' @param name Character. Owner name pattern to search for (case-insensitive
#'   regex). Examples: \code{"DELTA"}, \code{"UNITED"}, \code{"CESSNA"}.
#' @param max_rows Integer. Maximum rows to scan from the master file
#'   (default 100000). Increase if the target owner appears later in the file.
#' @return A tibble of matching aircraft registrations (same schema as
#'   \code{faa_aircraft()}).
#' @export
#' @examples
#' \dontrun{
#' faa_search("DELTA")
#' }
faa_search <- function(name, max_rows = 100000) {
  df <- tryCatch(faa_aircraft(max_rows = max_rows), error = function(e) tibble())
  if (nrow(df) == 0) return(tibble())

  name_cols <- grep("name|owner", names(df), ignore.case = TRUE, value = TRUE)
  if (length(name_cols) == 0) {
    warning("No name/owner column found")
    return(df)
  }
  name_col <- name_cols[1]
  df |> dplyr::filter(grepl(name, .data[[name_col]], ignore.case = TRUE))
}

#' Get faa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
faa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(faa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/faa.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "faa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# faa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# faa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
