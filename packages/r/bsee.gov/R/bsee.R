# bsee.gov.R - Bureau of Safety & Environmental Enforcement data client


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
#' Returns a curated catalog of 10 downloadable ZIP datasets from the
#' Bureau of Safety and Environmental Enforcement (BSEE). Covers wells,
#' production, platforms, pipelines, exploration plans, and GIS data
#' for the Gulf of America Outer Continental Shelf.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Dataset name (e.g. "Boreholes", "Production Data").}
#'     \item{description}{Character. Brief description.}
#'     \item{zip_url}{Character. Direct URL to the ZIP download. Pass to \code{\link{bsee_data}}.}
#'     \item{category}{Character. Category grouping: "Wells", "Production", "Platforms", "Pipelines", "Plans", or "GIS".}
#'   }
#' @examples
#' bsee_list()
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
#' Downloads a ZIP file from BSEE, extracts it to a temporary directory,
#' and reads the largest CSV or TXT file found. Returns the first 10,000
#' rows to keep memory manageable. Use with URLs from \code{\link{bsee_list}}.
#'
#' @param zip_url Character. URL of the ZIP file from \code{bsee_list()$zip_url}.
#'   Example: \code{"https://www.data.bsee.gov/Well/Files/BoreholeRawData.zip"}
#' @return A tibble of parsed data (up to 10,000 rows). Columns depend on the
#'   dataset. For example, the Boreholes dataset returns 26 columns including
#'   API_WELL_NUMBER, WELL_NAME, COMPANY_NAME, WELL_SPUD_DATE, etc.
#' @examples
#' urls <- bsee_list()
#' bsee_data(urls$zip_url[1])
bsee_data <- function(zip_url) {
  .fetch_zip_csv(zip_url)
}

#' Get BSEE borehole (well) data
#'
#' Downloads and parses the BSEE Borehole Raw Data ZIP. Contains wellbore
#' header metadata for Gulf of America OCS wells. Returns the first
#' 10,000 rows.
#'
#' @return A tibble with ~26 columns including:
#'   \describe{
#'     \item{API_WELL_NUMBER}{Numeric. API well number.}
#'     \item{WELL_NAME}{Character. Well name.}
#'     \item{COMPANY_NAME}{Character. Operating company.}
#'     \item{WELL_SPUD_DATE}{Character. Spud date string.}
#'     \item{BH_TOTAL_MD}{Character. Total measured depth.}
#'     \item{WATER_DEPTH}{Integer. Water depth in feet.}
#'     \item{SURF_LATITUDE, SURF_LONGITUDE}{Numeric. Surface coordinates.}
#'     \item{BOTM_AREA_CODE}{Character. Bottom-hole area code (e.g. "GC").}
#'     \item{WELL_TYPE_CODE}{Character. Well type.}
#'     \item{BOREHOLE_STAT_CD}{Character. Borehole status code.}
#'   }
#' @examples
#' bsee_boreholes()
bsee_boreholes <- function() {
  .fetch_zip_csv("https://www.data.bsee.gov/Well/Files/BoreholeRawData.zip")
}

#' Get BSEE platform structure data
#'
#' Downloads and parses the BSEE Platform Structure Raw Data ZIP. Contains
#' platform structures with associated inspection (INC) counts for the
#' Gulf of America OCS. Returns the first 10,000 rows.
#'
#' @return A tibble with ~9 columns:
#'   \describe{
#'     \item{ID_NUMBER}{Character or numeric. Platform ID.}
#'     \item{INC_NUMBER}{Character or numeric. Inspection count.}
#'     \item{INC_ENFRC_CODE}{Character. Enforcement code.}
#'     \item{STRUCTURE_NUMBER}{Character. Structure number.}
#'     \item{STRUCTURE_NAME}{Character. Structure name.}
#'     \item{MMS_COMPANY_NUM}{Character. MMS company number.}
#'     \item{COMPANY_NAME}{Character. Operating company name.}
#'     \item{INC_ISSUE_DATE}{Character. Inspection issue date.}
#'     \item{INSP_DATE}{Character. Inspection date.}
#'   }
#' @examples
#' bsee_platforms()
bsee_platforms <- function() {
  .fetch_zip_csv("https://www.data.bsee.gov/Platform/Files/PlatStrucRawData.zip")
}

#' Get BSEE production data
#'
#' Downloads and parses the BSEE Production Raw Data ZIP. Contains oil
#' and gas production data from Gulf of America OCS leases, reported
#' monthly. Returns the first 10,000 rows.
#'
#' @return A tibble with ~10 columns:
#'   \describe{
#'     \item{LEASE_NUMBER}{Character. Lease number.}
#'     \item{PROD_MONTH}{Integer or character. Production month (1--12).}
#'     \item{PROD_YEAR}{Integer or character. Production year.}
#'     \item{LEASE_OIL_PROD}{Numeric. Oil production (barrels).}
#'     \item{LEASE_GWG_PROD}{Numeric. Gas well gas production (MCF).}
#'     \item{LEASE_CONDN_PROD}{Numeric. Condensate production (barrels).}
#'     \item{LEASE_OWG_PROD}{Numeric. Oil well gas production (MCF).}
#'     \item{LEASE_WTR_PROD}{Numeric. Water production (barrels).}
#'     \item{LEASE_PROD_COMP}{Integer. Number of producing completions.}
#'   }
#' @examples
#' bsee_production()
bsee_production <- function() {
  .fetch_zip_csv("https://www.data.bsee.gov/Production/Files/ProductionRawData.zip")
}

#' Get bsee.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bsee_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bsee_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bsee.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bsee.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bsee.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bsee.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
