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
#' @return tibble of FAA digital products with name, description, url, type
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
#' Returns the master aircraft registration file from the FAA.
#' @param max_rows Maximum rows to return (default 10000).
#' @return tibble of registered aircraft
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
#' @param name Owner name pattern (case-insensitive).
#' @param max_rows Maximum rows to scan (default 100000).
#' @return tibble of matching aircraft registrations
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

#' Return function signatures and documentation for LLM context
#'
#' @return Printed function listing (invisibly returns string)
faa_context <- function() {
  src_file <- tryCatch({
    f <- getSrcFilename(faa_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else NULL
  }, error = function(e) NULL)

  if (is.null(src_file)) {
    src_dir <- system.file("source", package = "faa.gov")
    if (src_dir != "") {
      src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
      if (length(src_files)) src_file <- src_files[1]
    }
  }
  if (is.null(src_file)) {
    msg <- paste(
      "# faa.gov R client",
      "# Functions: faa_list, faa_aircraft, faa_search, faa_context",
      "# FAA aeronautical data - aircraft registry, charts, NASR data",
      sep = "\n"
    )
    cat(msg, "\n"); return(invisible(msg))
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
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }
  out <- paste(c("# faa.gov R client", "# Federal Aviation Administration", "#",
                 "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
