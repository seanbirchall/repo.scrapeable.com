# dot.gov.R - Department of Transportation data client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

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
#' @param query Search query string.
#' @param limit Max results (default 20).
#' @param offset Starting offset (default 0).
#' @return tibble of matching datasets with id, name, description, category
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
#' @return tibble of curated DOT datasets
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
#' Accesses data.transportation.gov Socrata SODA API for tabular datasets.
#'
#' @param dataset_id Socrata 4x4 ID (e.g. "mcwr-f47u").
#' @param limit Max rows (default 1000).
#' @param offset Starting row (default 0).
#' @param where SoQL WHERE clause for filtering (optional).
#' @return tibble of data
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
#' @return tibble of pipeline safety program data (from ZIP download)
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

#' Return function signatures and documentation for LLM context
#'
#' @return Printed function listing (invisibly returns string)
dot_context <- function() {
  src_file <- tryCatch({
    f <- getSrcFilename(dot_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else NULL
  }, error = function(e) NULL)

  if (is.null(src_file)) {
    src_dir <- system.file("source", package = "dot.gov")
    if (src_dir != "") {
      src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
      if (length(src_files)) src_file <- src_files[1]
    }
  }
  if (is.null(src_file)) {
    msg <- paste(
      "# dot.gov R client",
      "# Functions: dot_search, dot_list, dot_data, dot_pipeline_safety, dot_context",
      "# DOT data via Socrata API + PHMSA pipeline data",
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
  out <- paste(c("# dot.gov R client", "# Department of Transportation", "#",
                 "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
