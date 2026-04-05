# dot.gov.R - Department of Transportation data client


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dot_base <- "https://data.transportation.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), description = character(),
  category = character(), type = character(), url = character()
)

# == Public functions ==========================================================

#' Search DOT datasets on data.transportation.gov (Socrata catalog)
#'
#' Searches the U.S. Department of Transportation open data catalog hosted
#' on Socrata. Returns dataset metadata matching the query.
#'
#' @param query Character. Search query string (e.g., \code{"safety"},
#'   \code{"pipeline"}, \code{"freight"}). \code{NULL} returns all datasets.
#' @param limit Integer. Maximum results to return (default 20, max 100).
#' @param offset Integer. Starting offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata 4x4 dataset identifier (e.g., "9ivb-8ae9").}
#'     \item{name}{Character. Dataset title.}
#'     \item{description}{Character. Dataset description (truncated to 200 chars).}
#'     \item{category}{Character. Domain category (e.g., "Public Safety", "Pipelines").}
#'     \item{type}{Character. Resource type (e.g., "dataset").}
#'     \item{url}{Character. Direct URL to view the dataset on data.transportation.gov.}
#'   }
#' @examples
#' dot_search("safety", limit = 5)
#' dot_search("pipeline", limit = 10, offset = 20)
dot_search <- function(query = NULL, limit = 20, offset = 0) {
  url <- sprintf("%s/api/catalog/v1?limit=%d&offset=%d&only=datasets",
                 .dot_base, min(limit, 100), offset)
  if (!is.null(query) && nzchar(query)) {
    url <- paste0(url, "&q=", utils::URLencode(query, reserved = TRUE))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) return(.schema_datasets)

  res <- raw$results
  resource <- res$resource
  tibble(
    id          = resource$id %||% NA_character_,
    name        = resource$name %||% NA_character_,
    description = substr(resource$description %||% "", 1, 200),
    category    = res$classification$domain_category %||% NA_character_,
    type        = resource$type %||% NA_character_,
    url         = paste0(.dot_base, "/d/", resource$id %||% "")
  )
}

#' List popular DOT datasets with direct download info
#'
#' Returns a curated list of notable DOT datasets with pre-built
#' JSON and CSV download URLs for the Socrata SODA API.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Dataset title (e.g., "Motor Carrier Inspections").}
#'     \item{id}{Character. Socrata 4x4 dataset identifier.}
#'     \item{description}{Character. Brief description.}
#'     \item{category}{Character. Topic category (e.g., "Safety", "Pipelines",
#'       "Freight", "Traffic", "Environment", "ITS").}
#'     \item{json_url}{Character. Socrata SODA API JSON endpoint URL.}
#'     \item{csv_url}{Character. Socrata SODA API CSV endpoint URL.}
#'   }
#' @examples
#' dot_list()
dot_list <- function() {
  datasets <- list(
    list(name = "Motor Carrier Inspections",
         id   = "7qiy-7ws9",
         desc = "Roadside inspections of large trucks and buses",
         cat  = "Safety"),
    list(name = "PHMSA Pipeline Safety Program Data",
         id   = "dynj-dx5h",
         desc = "Safety program data for pipeline and LNG operators",
         cat  = "Pipelines"),
    list(name = "PHMSA SRCR & Integrity Notifications",
         id   = "76qv-bk88",
         desc = "Safety-related condition reports and integrity notifications",
         cat  = "Pipelines"),
    list(name = "Freight Analysis Framework",
         id   = "rhnj-cjkt",
         desc = "Comprehensive picture of freight movement among states",
         cat  = "Freight"),
    list(name = "DOT User Facilities & R&D Equipment",
         id   = "mcwr-f47u",
         desc = "DOT-funded user facilities and R&D equipment list",
         cat  = "Research"),
    list(name = "Monthly Traffic Volume Trends",
         id   = "xj3g-6eev",
         desc = "National vehicle miles traveled data by state",
         cat  = "Traffic"),
    list(name = "CMAQ Projects",
         id   = "rq7e-ds5e",
         desc = "Congestion Mitigation and Air Quality project obligations",
         cat  = "Environment"),
    list(name = "ITS Costs Database",
         id   = "its-costs",
         desc = "Cost estimates for Intelligent Transportation Systems",
         cat  = "ITS")
  )
  tibble(
    name        = vapply(datasets, `[[`, character(1), "name"),
    id          = vapply(datasets, `[[`, character(1), "id"),
    description = vapply(datasets, `[[`, character(1), "desc"),
    category    = vapply(datasets, `[[`, character(1), "cat"),
    json_url    = paste0(.dot_base, "/resource/",
                         vapply(datasets, `[[`, character(1), "id"), ".json"),
    csv_url     = paste0(.dot_base, "/resource/",
                         vapply(datasets, `[[`, character(1), "id"), ".csv")
  )
}

#' Download a DOT Socrata dataset as a tibble
#'
#' Accesses the data.transportation.gov Socrata SODA API for any tabular
#' dataset. Returns raw data as-is from the API. Columns vary by dataset.
#'
#' @param dataset_id Character. Socrata 4x4 ID (e.g., \code{"mcwr-f47u"},
#'   \code{"7qiy-7ws9"}). Obtain IDs from \code{dot_search()} or
#'   \code{dot_list()}.
#' @param limit Integer. Maximum rows to return (default 1000, max 50000).
#' @param offset Integer. Starting row for pagination (default 0).
#' @param where Character. SoQL WHERE clause for server-side filtering
#'   (e.g., \code{"year > 2020"}, \code{"state = 'CA'"}). \code{NULL}
#'   returns unfiltered data.
#' @return A tibble with columns determined by the dataset schema. All
#'   columns are returned as character by Socrata; convert as needed.
#' @examples
#' dot_data("mcwr-f47u", limit = 100)
#' dot_data("7qiy-7ws9", limit = 50, where = "year > 2020")
dot_data <- function(dataset_id, limit = 1000, offset = 0, where = NULL) {
  url <- sprintf("%s/resource/%s.json?$limit=%d&$offset=%d",
                 .dot_base, dataset_id, limit, offset)
  if (!is.null(where) && nzchar(where)) {
    url <- paste0(url, "&$where=", utils::URLencode(where, reserved = TRUE))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Failed to fetch dataset: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  tibble::as_tibble(raw)
}

#' Get PHMSA pipeline safety ZIP data
#'
#' Downloads the PHMSA Safety Program Data ZIP archive from phmsa.dot.gov,
#' extracts it, and reads the largest CSV/TXT file (limited to 10,000 rows).
#' Contains pipeline operator safety information.
#'
#' @return A tibble of pipeline safety program data. Columns vary but typically
#'   include operator information, inspection data, and incident records.
#'   Limited to 10,000 rows for performance.
#' @examples
#' dot_pipeline_safety()
dot_pipeline_safety <- function() {
  url <- "https://www.phmsa.dot.gov/sites/phmsa.dot.gov/files/data_statistics/pipeline/Safety_Program_Data.zip"
  zip_path <- .fetch(url, ext = ".zip")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  utils::unzip(zip_path, exdir = tmp_dir)
  csvs <- list.files(tmp_dir, pattern = "\\.(csv|txt)$", full.names = TRUE,
                     recursive = TRUE, ignore.case = TRUE)
  if (length(csvs) == 0) return(tibble())
  target <- csvs[which.max(file.size(csvs))]
  df <- tryCatch(
    utils::read.csv(target, stringsAsFactors = FALSE, check.names = FALSE, nrows = 10000),
    error = function(e) data.frame()
  )
  tibble::as_tibble(df)
}

#' Get dot.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
dot_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(dot_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/dot.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "dot.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# dot.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# dot.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
