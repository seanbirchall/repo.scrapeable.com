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
#' Returns a catalog of pre-configured datasets from the Sioux Falls
#' DataWorks portal (ArcGIS Hub). Each row includes a short key for use
#' with \code{\link{sf_data}}, along with the underlying ArcGIS item ID
#' and layer number.
#'
#' @return A tibble with one row per known dataset:
#' \describe{
#'   \item{key}{Character. Short key for use with \code{sf_data()} (e.g. "violent_crimes").}
#'   \item{name}{Character. Human-readable dataset name.}
#'   \item{item_id}{Character. ArcGIS Hub item ID (32-character hex).}
#'   \item{layer}{Integer. Layer number within the ArcGIS item.}
#' }
#' @export
#' @examples
#' \dontrun{
#' sf_datasets()
#' }
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
#' Full-text search across the Sioux Falls DataWorks ArcGIS Hub portal.
#' Returns dataset metadata for all matching items, including maps,
#' feature layers, and downloadable datasets.
#'
#' @param q Character. Search query (e.g. \code{"crime"}, \code{"housing"},
#'   \code{"fire"}). Defaults to \code{""} which returns all items.
#' @return A tibble with one row per matching item:
#' \describe{
#'   \item{id}{Character. ArcGIS item ID.}
#'   \item{type}{Character. Item type (e.g. "Feature Layer", "Web Map").}
#'   \item{title}{Character. Item title.}
#'   \item{description}{Character. Item description.}
#'   \item{url}{Character. URL to access the item.}
#' }
#' @export
#' @examples
#' \dontrun{
#' sf_search("crime")
#' sf_search("housing")
#' }
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
#' Fetches and parses a dataset from the Sioux Falls DataWorks portal.
#' Accepts either a pre-configured dataset key (from \code{\link{sf_datasets}})
#' or a raw ArcGIS item ID with layer number.
#'
#' @param key Character. Either a dataset key from \code{sf_datasets()} (e.g.
#'   \code{"violent_crimes"}, \code{"sales_tax"}) or a raw 32-character
#'   ArcGIS item ID.
#' @param layer Integer or NULL. Layer number; only needed when passing a raw
#'   item ID. Ignored when using a pre-configured key.
#' @return A tibble with dataset records. Columns vary by dataset.
#' @export
#' @examples
#' \dontrun{
#' sf_data("violent_crimes")
#' sf_data("sales_tax")
#' # Using raw item ID and layer
#' sf_data("cd28866b6c56472cb9c1ee660ff535b2", layer = 12)
#' }
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
#' Convenience function that downloads annual violent crime counts from
#' the Sioux Falls DataWorks portal. Shortcut for
#' \code{sf_data("violent_crimes")}.
#'
#' @return A tibble with annual violent crime statistics. Columns include
#'   OBJECTID, Value (crime rate per 1,000), DateRecorded, and ShortDate (year).
#' @export
#' @examples
#' \dontrun{
#' sf_violent_crimes()
#' }
sf_violent_crimes <- function() {
  sf_data("violent_crimes")
}

#' Get Sioux Falls sales tax growth data
#'
#' Convenience function that downloads sales tax growth data from the
#' Sioux Falls DataWorks portal. Shortcut for \code{sf_data("sales_tax")}.
#'
#' @return A tibble with sales tax growth data by year.
#' @export
#' @examples
#' \dontrun{
#' sf_sales_tax()
#' }
sf_sales_tax <- function() {
  sf_data("sales_tax")
}

# == Context ===================================================================

#' Get siouxfalls.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
sf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(sf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/siouxfalls.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "siouxfalls.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# siouxfalls.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# siouxfalls.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
