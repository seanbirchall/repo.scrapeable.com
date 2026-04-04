# tempe.gov.R - City of Tempe AZ open data client (ArcGIS Hub)
#
# Data source: data.tempe.gov (ArcGIS Hub)
# Datasets: ~599 (police, crime, addresses, wastewater, fire, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.tempe_base <- "https://data.tempe.gov"

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

# == Known datasets ============================================================

.tempe_datasets <- list(
  arrests_90d    = list(id = "8931f2aae8f44e9786d6398c4bdc6151", layer = 0, name = "Police Arrests - Last 90 Days"),
  arrests_all    = list(id = "aeb0328cb1d6412b9f7a1fc65b056148", layer = 0, name = "Police Arrests - All Data"),
  offenses       = list(id = "1563be5b343b4f78b1163e97a9a503ad", layer = 0, name = "General Offenses"),
  addresses      = list(id = "9250a8606ddb4b0c869b58ed1525cd9e", layer = 0, name = "Addresses"),
  zip_codes      = list(id = "9e56f81e8af8469eb89a9d464d6ce59f", layer = 0, name = "Zip Code Boundaries"),
  covid_ww       = list(id = "c921c8af868f4e17b857c390543c5250", layer = 0, name = "Wastewater COVID-19 Results"),
  fire_survey    = list(id = "744963c36de8447195a0461b8beb435a", layer = 0, name = "Fire Services Survey")
)

# == Data access ===============================================================

#' List available Tempe datasets
#'
#' @return tibble: key, name, item_id, layer
tempe_datasets <- function() {
  tibble(
    key     = names(.tempe_datasets),
    name    = vapply(.tempe_datasets, function(x) x$name, character(1)),
    item_id = vapply(.tempe_datasets, function(x) x$id, character(1)),
    layer   = vapply(.tempe_datasets, function(x) as.integer(x$layer), integer(1))
  )
}

#' Search Tempe open data portal
#'
#' @param q Search query
#' @return tibble: id, type, title, description, url
tempe_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .tempe_base, utils::URLencode(q))
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

#' Download a Tempe dataset as CSV
#'
#' @param key Dataset key from tempe_datasets(), or a raw item_id
#' @param layer Layer number (only needed if using raw item_id)
#' @return tibble with dataset records
tempe_data <- function(key, layer = NULL) {
  if (key %in% names(.tempe_datasets)) {
    ds <- .tempe_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d", .tempe_base, item_id, lyr)
  .fetch_csv(url)
}

#' Get Tempe police arrest data (last 90 days)
#'
#' @return tibble with arrest records
tempe_arrests <- function() {
  tempe_data("arrests_90d")
}

#' Get Tempe general offense crime data
#'
#' @return tibble with offense records
tempe_offenses <- function() {
  tempe_data("offenses")
}

# == Context ===================================================================

#' Generate LLM-friendly context for tempe.gov
#'
#' @return Character string with full function signatures and bodies
tempe_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/tempe.gov.R"
  if (!file.exists(src_file)) { cat("# tempe.gov context - source not found\n"); return(invisible(NULL)) }
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
