# bsee.gov.R - Bureau of Safety & Environmental Enforcement data client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bsee_base <- "https://www.data.bsee.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".zip") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(seconds = 120) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_zip_csv <- function(url, pattern = "\\.csv$", delim = ",") {
  zip_path <- .fetch(url, ext = ".zip")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  utils::unzip(zip_path, exdir = tmp_dir)
  csvs <- list.files(tmp_dir, pattern = pattern, full.names = TRUE,
                     recursive = TRUE, ignore.case = TRUE)
  if (length(csvs) == 0) {
    # Try tab-delimited or dat files
    csvs <- list.files(tmp_dir, pattern = "\\.(txt|dat|csv)$", full.names = TRUE,
                       recursive = TRUE, ignore.case = TRUE)
  }
  if (length(csvs) == 0) {
    warning("No data files found in ZIP")
    return(tibble())
  }
  # Read the first/largest file
  target <- csvs[which.max(file.size(csvs))]
  df <- tryCatch(
    utils::read.csv(target, stringsAsFactors = FALSE, check.names = FALSE,
                    nrows = 10000),
    error = function(e) {
      tryCatch(
        utils::read.delim(target, stringsAsFactors = FALSE, check.names = FALSE,
                          nrows = 10000),
        error = function(e2) data.frame()
      )
    }
  )
  tibble::as_tibble(df)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  name = character(), description = character(),
  zip_url = character(), category = character()
)

# == Public functions ==========================================================

#' List available BSEE downloadable datasets
#'
#' @return tibble of datasets with name, description, zip_url, category
bsee_list <- function() {
  datasets <- list(
    list(name = "Boreholes",
         desc = "Wellbore header metadata for Gulf of America OCS wells",
         url  = "https://www.data.bsee.gov/Well/Files/BoreholeRawData.zip",
         cat  = "Wells"),
    list(name = "Production Data",
         desc = "Oil and gas production data from Gulf of America leases",
         url  = "https://www.data.bsee.gov/Production/Files/ProductionRawData.zip",
         cat  = "Production"),
    list(name = "Platform Structures",
         desc = "Platform structures with associated INC counts",
         url  = "https://www.data.bsee.gov/Platform/Files/PlatStrucRawData.zip",
         cat  = "Platforms"),
    list(name = "Pipeline Locations",
         desc = "Pipeline location data for Gulf of America",
         url  = "https://www.data.bsee.gov/Pipeline/Files/PipeLocRawData.zip",
         cat  = "Pipelines"),
    list(name = "Pipeline Permits",
         desc = "Pipeline permit data",
         url  = "https://www.data.bsee.gov/Pipeline/Files/PipePermRawData.zip",
         cat  = "Pipelines"),
    list(name = "ROW Descriptions",
         desc = "Right of Way descriptions",
         url  = "https://www.data.bsee.gov/Pipeline/Files/RowDescRawData.zip",
         cat  = "Pipelines"),
    list(name = "Exploration & Development Plans",
         desc = "Exploration and development plan data",
         url  = "https://www.data.bsee.gov/Plans/Files/PlansRawData.zip",
         cat  = "Plans"),
    list(name = "APD (Permit to Drill)",
         desc = "Application for permit to drill/bypass/sidetrack",
         url  = "https://www.data.bsee.gov/Well/Files/APDRawData.zip",
         cat  = "Wells"),
    list(name = "Platform GIS (Shapefile)",
         desc = "Platform point locations for Gulf of America (NAD 27)",
         url  = "https://www.data.bsee.gov/Mapping/Files/platform.zip",
         cat  = "GIS"),
    list(name = "Pipeline GIS (Shapefile)",
         desc = "Pipeline polyline locations for Gulf of America (NAD 27)",
         url  = "https://www.data.bsee.gov/Mapping/Files/ppl_arcs.zip",
         cat  = "GIS")
  )
  tibble(
    name        = vapply(datasets, `[[`, character(1), "name"),
    description = vapply(datasets, `[[`, character(1), "desc"),
    zip_url     = vapply(datasets, `[[`, character(1), "url"),
    category    = vapply(datasets, `[[`, character(1), "cat")
  )
}

#' Download and parse a BSEE ZIP dataset
#'
#' Downloads a ZIP file from BSEE, extracts, and reads the largest CSV/TXT file.
#' Returns the first 10,000 rows by default to keep memory manageable.
#'
#' @param zip_url URL of the ZIP file (from bsee_list()).
#' @return tibble of parsed data
bsee_data <- function(zip_url) {
  .fetch_zip_csv(zip_url)
}

#' Get BSEE borehole (well) data
#'
#' @return tibble of borehole records (first 10k rows)
bsee_boreholes <- function() {
  .fetch_zip_csv("https://www.data.bsee.gov/Well/Files/BoreholeRawData.zip")
}

#' Get BSEE platform structure data
#'
#' @return tibble of platform structure records (first 10k rows)
bsee_platforms <- function() {
  .fetch_zip_csv("https://www.data.bsee.gov/Platform/Files/PlatStrucRawData.zip")
}

#' Get BSEE production data
#'
#' @return tibble of production records (first 10k rows)
bsee_production <- function() {
  .fetch_zip_csv("https://www.data.bsee.gov/Production/Files/ProductionRawData.zip")
}

#' Return function signatures and documentation for LLM context
#'
#' @return Printed function listing (invisibly returns string)
bsee_context <- function() {
  src_file <- tryCatch({
    f <- getSrcFilename(bsee_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else NULL
  }, error = function(e) NULL)

  if (is.null(src_file)) {
    src_dir <- system.file("source", package = "bsee.gov")
    if (src_dir != "") {
      src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
      if (length(src_files)) src_file <- src_files[1]
    }
  }
  if (is.null(src_file)) {
    msg <- paste(
      "# bsee.gov R client",
      "# Functions: bsee_list, bsee_data, bsee_boreholes, bsee_platforms, bsee_production, bsee_context",
      "# Bureau of Safety & Environmental Enforcement - offshore energy data",
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
  out <- paste(c("# bsee.gov R client", "# Bureau of Safety & Environmental Enforcement", "#",
                 "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
