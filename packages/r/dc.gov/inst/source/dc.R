# dc.gov.R - District of Columbia Open Data client (ArcGIS Hub)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dc_hub <- "https://opendata.dc.gov"

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
  description = character(), tags = character(),
  url = character(), csv_url = character()
)

.schema_data <- tibble()

# == Public functions ==========================================================

#' Search DC open data datasets via ArcGIS Hub v3 API
#'
#' @param query Search term (e.g. "crime", "schools", "trees").
#' @param limit Max datasets to return (default 20).
#' @return tibble of matching datasets with id, title, description, tags, urls
dc_search <- function(query = NULL, limit = 20) {
  url <- sprintf("%s/api/v3/datasets?page[size]=%d", .dc_hub, min(limit, 100))
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
    url         = attrs$url %||% NA_character_,
    csv_url     = NA_character_
  )
}

#' List popular DC open data datasets
#'
#' Returns a curated set of well-known DC datasets with direct CSV download links.
#' @return tibble with dataset name, item_id, csv_url, description
dc_list <- function() {
  datasets <- list(
    list(name = "COVID-19 Testing Locations",  item_id = "531cce69336a4ba3ac0e67bfe419c16b", layer = 0,
         desc = "COVID-19 testing sites in the District of Columbia"),
    list(name = "Street Trees",                item_id = "f6c3c04113944f23a7993f2e603abaf2", layer = 23,
         desc = "Urban forestry street trees - ~200k public trees"),
    list(name = "Child Development Centers",   item_id = "48009ae8fbe54157b86586d3e4f10929", layer = 33,
         desc = "Child development center locations from OSSE"),
    list(name = "CCTV Street Cameras",         item_id = "2bb8375e31a94067a17911ea70f917ef", layer = 11,
         desc = "Closed circuit TV street camera locations"),
    list(name = "Street Lights",               item_id = "6cb6520725b0489d9a209a337818fad1", layer = 90,
         desc = "Street light assets from DDOT"),
    list(name = "Sidewalk Condition",           item_id = "c2a15ac84c2145589f0ec965cad23d23", layer = 0,
         desc = "Sidewalk condition assessment (index 0-11)"),
    list(name = "Well Permits",                item_id = "84fdf39aaa3a4e75ba9e7a167577daa8", layer = 41,
         desc = "DC well permits"),
    list(name = "Odor Control Plans",          item_id = "c9b01b572f6e40af984e7193484f8ea9", layer = 1,
         desc = "Odor control plans from DOEE"),
    list(name = "Vertical Deflections",        item_id = "fc8aab4c929740e1b9f0dec7215fc676", layer = 89,
         desc = "Speed humps and vertical deflections"),
    list(name = "Roadway Blocks",              item_id = "6fcba8618ae744949630da3ea12d90eb", layer = 163,
         desc = "Street blocks for DC roadway network")
  )

  tibble(
    name        = vapply(datasets, `[[`, character(1), "name"),
    item_id     = vapply(datasets, `[[`, character(1), "item_id"),
    layer       = vapply(datasets, `[[`, numeric(1),   "layer"),
    csv_url     = sprintf("%s/api/download/v1/items/%s/csv?layers=%d",
                          .dc_hub,
                          vapply(datasets, `[[`, character(1), "item_id"),
                          vapply(datasets, `[[`, numeric(1), "layer")),
    description = vapply(datasets, `[[`, character(1), "desc")
  )
}

#' Download a DC open data dataset as a tibble
#'
#' @param item_id The ArcGIS item ID (from dc_list() or dc_search()).
#' @param layer Layer number (default 0).
#' @param max_rows Maximum rows to return (default 5000, NULL for all).
#' @return tibble of the dataset
dc_data <- function(item_id, layer = 0, max_rows = 5000) {
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d",
                 .dc_hub, item_id, layer)
  df <- tryCatch(.fetch_csv(url), error = function(e) {
    warning("Failed to download: ", e$message)
    return(data.frame())
  })
  if (nrow(df) == 0) return(tibble())
  out <- tibble::as_tibble(df)
  if (!is.null(max_rows) && nrow(out) > max_rows) out <- out[seq_len(max_rows), ]
  out
}

#' Get DC crime data (2025 Part 1 offenses)
#'
#' @param max_rows Maximum rows (default 1000).
#' @return tibble of crime incidents
dc_crime <- function(max_rows = 1000) {
  # Crime data 2025 Part 1 offenses
  url <- paste0(.dc_hub,
    "/api/download/v1/items/92bcecf4355140a98b9bba3cd8ecdca2/csv?layers=0")
  df <- tryCatch(.fetch_csv(url), error = function(e) {
    warning("Crime data unavailable: ", e$message)
    return(data.frame())
  })
  if (nrow(df) == 0) return(tibble())
  out <- tibble::as_tibble(df)
  if (nrow(out) > max_rows) out <- out[seq_len(max_rows), ]
  out
}

#' Return function signatures and documentation for LLM context
#'
#' @return Printed function listing (invisibly returns string)
dc_context <- function() {
  .build_context("dc.gov")
}

.build_context <- function(pkg_name) {
  src_dir <- system.file("source", package = pkg_name)
  if (src_dir == "") {
    src_file <- NULL
    env <- environment(dc_context)
    if (!is.null(env)) {
      src_file <- tryCatch({
        f <- getSrcFilename(dc_context, full.names = TRUE)
        if (length(f) && nzchar(f)) f else NULL
      }, error = function(e) NULL)
    }
    if (is.null(src_file)) {
      src_file <- tryCatch(
        normalizePath(sys.frame(1)$ofile %||% "dc.gov.R"),
        error = function(e) NULL
      )
    }
    if (is.null(src_file)) {
      msg <- paste(
        "# dc.gov R client",
        "# Functions: dc_search, dc_list, dc_data, dc_crime, dc_context",
        "# DC open data via ArcGIS Hub CSV downloads",
        sep = "\n"
      )
      cat(msg, "\n"); return(invisible(msg))
    }
  } else {
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    src_file <- src_files[1]
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
  out <- paste(c("# dc.gov R client", "# District of Columbia Open Data (ArcGIS Hub)", "#",
                 "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
