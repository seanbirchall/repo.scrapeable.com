# siouxfalls.gov.R - City of Sioux Falls SD open data client (ArcGIS Hub)
#
# Data source: dataworks.siouxfalls.gov (ArcGIS Hub)
# Datasets: ~233 (crime, taxes, housing, library, fire, annexations, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sf_base <- "https://dataworks.siouxfalls.gov"

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

.sf_datasets <- list(
  entertainment_tax = list(id = "eb2ae9c0c34044f2968499bc7c39dd9e", layer = 6,  name = "Entertainment Tax Growth"),
  fire_travel       = list(id = "da04cf7da54147068575467512cd1140", layer = 2,  name = "Fire Travel Time"),
  violent_crimes    = list(id = "cd28866b6c56472cb9c1ee660ff535b2", layer = 12, name = "Violent Crimes by Year"),
  annexations       = list(id = "6b2dc2947c3749009feb92b819bcb002", layer = 17, name = "Annexations by Year"),
  library_stats     = list(id = "89bbb70a6afc4f018face386bb26f589", layer = 22, name = "Library Statistics"),
  sales_tax         = list(id = "c8f7902737b94322acc8c8d621127533", layer = 7,  name = "Sales Tax Growth")
)

# == Data access ===============================================================

#' List available Sioux Falls datasets
#'
#' @return tibble: key, name, item_id, layer
sf_datasets <- function() {
  tibble(
    key     = names(.sf_datasets),
    name    = vapply(.sf_datasets, function(x) x$name, character(1)),
    item_id = vapply(.sf_datasets, function(x) x$id, character(1)),
    layer   = vapply(.sf_datasets, function(x) as.integer(x$layer), integer(1))
  )
}

#' Search Sioux Falls DataWorks portal
#'
#' @param q Search query
#' @return tibble: id, type, title, description, url
sf_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .sf_base, utils::URLencode(q))
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

#' Download a Sioux Falls dataset as CSV
#'
#' @param key Dataset key from sf_datasets(), or a raw item_id
#' @param layer Layer number (only needed if using raw item_id)
#' @return tibble with dataset records
sf_data <- function(key, layer = NULL) {
  if (key %in% names(.sf_datasets)) {
    ds <- .sf_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d", .sf_base, item_id, lyr)
  .fetch_csv(url)
}

#' Get Sioux Falls violent crime statistics by year
#'
#' @return tibble with annual violent crime data
sf_violent_crimes <- function() {
  sf_data("violent_crimes")
}

#' Get Sioux Falls sales tax growth data
#'
#' @return tibble with sales tax growth data
sf_sales_tax <- function() {
  sf_data("sales_tax")
}

# == Context ===================================================================

#' Generate LLM-friendly context for siouxfalls.gov
#'
#' @return Character string with full function signatures and bodies
sf_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/siouxfalls.gov.R"
  if (!file.exists(src_file)) { cat("# siouxfalls.gov context - source not found\n"); return(invisible(NULL)) }
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
