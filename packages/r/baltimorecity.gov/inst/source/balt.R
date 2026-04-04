# baltimorecity.gov.R - City of Baltimore Open Data client (ArcGIS Hub)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.balt_hub <- "https://data.baltimorecity.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.fetch_csv <- function(url) {
  tmp <- .fetch(url, ext = ".csv")
  utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), title = character(), type = character(),
  description = character(), tags = character(), csv_url = character()
)

# == Public functions ==========================================================

#' Search Baltimore open data datasets via ArcGIS Hub v3 API
#'
#' @param query Tag to filter by (e.g. "police", "housing", "education").
#' @param limit Max datasets to return (default 20).
#' @return tibble of matching datasets
balt_search <- function(query = NULL, limit = 20) {
  url <- sprintf("%s/api/v3/datasets?page[size]=%d", .balt_hub, min(limit, 100))
  if (!is.null(query) && nzchar(query)) {
    url <- paste0(url, "&filter[tags]=", utils::URLencode(query, reserved = TRUE))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(.schema_datasets)

  items <- raw$data
  attrs <- items$attributes
  tibble(
    id          = items$id %||% NA_character_,
    title       = attrs$name %||% NA_character_,
    type        = attrs$type %||% NA_character_,
    description = substr(attrs$description %||% "", 1, 200),
    tags        = vapply(attrs$tags %||% list(), function(t) paste(t, collapse = "; "), character(1)),
    csv_url     = NA_character_
  )
}

#' List popular Baltimore datasets with CSV download links
#'
#' @return tibble of curated datasets
balt_list <- function() {
  datasets <- list(
    list(name = "Baltimore City Schools",      item_id = "bf4ca51722c44e0593d8b9b375e8a509", layer = 0,
         desc = "Public school locations"),
    list(name = "BPD Arrests",                 item_id = "619ec10c14b346f784a5a07bad4c43cd", layer = 0,
         desc = "Baltimore Police Department arrests"),
    list(name = "Real Property Information",   item_id = "64110b108565433d8da40dd0e422064e", layer = 0,
         desc = "Real property information within parcels"),
    list(name = "Grocery Stores",              item_id = "85924b7086ef4506b4f2240d282a54c0", layer = 0,
         desc = "Grocery store locations"),
    list(name = "Vacant Building Rehabs",      item_id = "4db6d1e54e714a3e8125990a09d4623d", layer = 2,
         desc = "Vacant building rehab locations"),
    list(name = "Environmental Citations",     item_id = "87f2524498a64caca3cc7e37a3a355f7", layer = 0,
         desc = "Environmental citations for code violations"),
    list(name = "911 Behavioral Health",        item_id = "37599bb547784bdd9a359ccaff32eb76", layer = 0,
         desc = "911 behavioral health diversion data"),
    list(name = "Polling Places",              item_id = "9a4a40601b31473ca3fe822342428bca", layer = 7,
         desc = "Polling place locations"),
    list(name = "Census Tracts",               item_id = "f4382ed86362467cac404fe7a77bf3a5", layer = 0,
         desc = "Baltimore City qualified census tracts"),
    list(name = "MBE/WBE Certifications",      item_id = "40a8431fe43941aa9db08e72e0d206f2", layer = 0,
         desc = "Minority and women's business enterprises"),
    list(name = "Naloxone Administration",     item_id = "9f7e90ab7bfe44cb9d86596ee20f5f6d", layer = 0,
         desc = "BCFD clinician-administered naloxone"),
    list(name = "Housing Permits 2015-2018",   item_id = "adb8e3419a2c4aad9c0c8018357ded01", layer = 0,
         desc = "Housing and building permits"),
    list(name = "Open Checkbook FY2021",       item_id = "5c14e56f10de4a59a20b7fd38b27529a", layer = 0,
         desc = "City expenses with vendors FY2021")
  )

  tibble(
    name        = vapply(datasets, `[[`, character(1), "name"),
    item_id     = vapply(datasets, `[[`, character(1), "item_id"),
    layer       = vapply(datasets, `[[`, numeric(1),   "layer"),
    csv_url     = sprintf("%s/api/download/v1/items/%s/csv?layers=%d",
                          .balt_hub,
                          vapply(datasets, `[[`, character(1), "item_id"),
                          vapply(datasets, `[[`, numeric(1), "layer")),
    description = vapply(datasets, `[[`, character(1), "desc")
  )
}

#' Download a Baltimore dataset as a tibble
#'
#' @param item_id The ArcGIS item ID (from balt_list() or balt_search()).
#' @param layer Layer number (default 0).
#' @param max_rows Maximum rows to return (default 5000, NULL for all).
#' @return tibble of the dataset
balt_data <- function(item_id, layer = 0, max_rows = 5000) {
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d",
                 .balt_hub, item_id, layer)
  df <- tryCatch(.fetch_csv(url), error = function(e) {
    warning("Failed to download: ", e$message)
    return(data.frame())
  })
  if (nrow(df) == 0) return(tibble())
  out <- tibble::as_tibble(df)
  if (!is.null(max_rows) && nrow(out) > max_rows) out <- out[seq_len(max_rows), ]
  out
}

#' Get Baltimore schools data
#'
#' @return tibble of Baltimore City school locations
balt_schools <- function() {
  balt_data("bf4ca51722c44e0593d8b9b375e8a509", layer = 0, max_rows = NULL)
}

#' Get Baltimore police arrests data
#'
#' @param max_rows Maximum rows (default 1000).
#' @return tibble of BPD arrest records
balt_arrests <- function(max_rows = 1000) {
  balt_data("619ec10c14b346f784a5a07bad4c43cd", layer = 0, max_rows = max_rows)
}

#' Return function signatures and documentation for LLM context
#'
#' @return Printed function listing (invisibly returns string)
balt_context <- function() {
  src_file <- tryCatch({
    f <- getSrcFilename(balt_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else NULL
  }, error = function(e) NULL)

  if (is.null(src_file)) {
    src_dir <- system.file("source", package = "baltimorecity.gov")
    if (src_dir != "") {
      src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
      if (length(src_files)) src_file <- src_files[1]
    }
  }
  if (is.null(src_file)) {
    msg <- paste(
      "# baltimorecity.gov R client",
      "# Functions: balt_search, balt_list, balt_data, balt_schools, balt_arrests, balt_context",
      "# Baltimore open data via ArcGIS Hub CSV downloads",
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
  out <- paste(c("# baltimorecity.gov R client", "# City of Baltimore Open Data (ArcGIS Hub)", "#",
                 "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
