# hartford.gov.R - City of Hartford CT open data client (ArcGIS Hub)
#
# Data source: data.hartford.gov (ArcGIS Hub)
# Datasets: ~105 (food licenses, service requests, zoning, buildings, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.htfd_base <- "https://data.hartford.gov"

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
  tmp <- tryCatch(.fetch(url, ext = ".csv"), error = function(e) NULL)
  if (is.null(tmp)) return(tibble())
  tryCatch(as_tibble(utils::read.csv(tmp, stringsAsFactors = FALSE)),
           error = function(e) tibble())
}

# == Known datasets (item_id, layer, name) =====================================

.htfd_datasets <- list(
  food_licenses    = list(id = "27139a92097e4e6a957646fb633e1e71", layer = 2, name = "Food Establishments Licenses"),
  service_requests = list(id = "2185af186dda46caa1e59323407d1daf", layer = 9, name = "Service Requests Current Year"),
  zoning           = list(id = "0b899d5a56bc4791a66688a366ce9ea6", layer = 4, name = "Zoning Districts"),
  city_property    = list(id = "8e4ef8c879594a6f8f01805e7d0b0235", layer = 0, name = "City Owned Property"),
  buildings        = list(id = "493ed7fa65414636bb497504d3db1c80", layer = 29, name = "Buildings"),
  parking_lots     = list(id = "5272d58a79c7452b94b0c27e4a50032d", layer = 27, name = "Parking Lots")
)

# == Data access ===============================================================

#' List available Hartford datasets
#'
#' @return tibble: key, name, item_id, layer
htfd_datasets <- function() {
  tibble(
    key     = names(.htfd_datasets),
    name    = vapply(.htfd_datasets, function(x) x$name, character(1)),
    item_id = vapply(.htfd_datasets, function(x) x$id, character(1)),
    layer   = vapply(.htfd_datasets, function(x) as.integer(x$layer), integer(1))
  )
}

#' Search Hartford open data portal
#'
#' @param q Search query
#' @return tibble: id, type, title, description, url
htfd_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .htfd_base, utils::URLencode(q))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(tibble())
  d <- raw$data
  tibble(
    id          = as.character(d$id),
    type        = as.character(d$type %||% NA_character_),
    title       = as.character(d$attributes$name %||% NA_character_),
    description = as.character(d$attributes$searchDescription %||% NA_character_),
    url         = as.character(d$attributes$url %||% NA_character_)
  )
}

#' Download a Hartford dataset as CSV
#'
#' @param key Dataset key from htfd_datasets(), or a raw item_id
#' @param layer Layer number (only needed if using raw item_id)
#' @return tibble with dataset records
htfd_data <- function(key, layer = NULL) {
  if (key %in% names(.htfd_datasets)) {
    ds <- .htfd_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d", .htfd_base, item_id, lyr)
  .fetch_csv(url)
}

#' Download Hartford dataset as GeoJSON
#'
#' @param key Dataset key or item_id
#' @param layer Layer number (only needed if using raw item_id)
#' @return list (parsed GeoJSON)
htfd_geojson <- function(key, layer = NULL) {
  if (key %in% names(.htfd_datasets)) {
    ds <- .htfd_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/geojson?layers=%d", .htfd_base, item_id, lyr)
  tryCatch(.fetch_json(url), error = function(e) NULL)
}

# == Context ===================================================================

#' Generate LLM-friendly context for hartford.gov
#'
#' @return Character string with full function signatures and bodies
htfd_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/hartford.gov.R"
  if (!file.exists(src_file)) { cat("# hartford.gov context - source not found\n"); return(invisible(NULL)) }
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    blocks[[length(blocks) + 1]] <- c(rox, lines[fi:end_line], "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
