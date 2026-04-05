# dc.gov.R - District of Columbia Open Data client (ArcGIS Hub)


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
#' Searches the opendata.dc.gov portal for datasets matching a tag filter.
#'
#' @param query Character or NULL. Tag-based search term.
#'   Example: \code{"crime"}, \code{"schools"}, \code{"trees"}, \code{"health"}
#' @param limit Integer. Max datasets to return (default 20, max 100).
#' @return A tibble with 7 columns:
#'   \describe{
#'     \item{id}{Character. ArcGIS Hub dataset ID.}
#'     \item{title}{Character. Dataset name.}
#'     \item{type}{Character. Asset type (e.g. "Feature Layer").}
#'     \item{description}{Character. Description (truncated to 200 chars).}
#'     \item{tags}{Character. Semicolon-delimited tags.}
#'     \item{url}{Character. Dataset URL on opendata.dc.gov.}
#'     \item{csv_url}{Character. Direct CSV download URL (NA if unavailable).}
#'   }
#' @examples
#' dc_search("crime")
#' dc_search("schools", limit = 5)
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
#' Returns a curated set of well-known DC government datasets with direct
#' CSV download links. Covers infrastructure, public safety, and services.
#'
#' @return A tibble with 5 columns:
#'   \describe{
#'     \item{name}{Character. Dataset name (e.g. "Street Trees").}
#'     \item{item_id}{Character. ArcGIS item ID for use with \code{\link{dc_data}}.}
#'     \item{layer}{Numeric. ArcGIS layer number.}
#'     \item{csv_url}{Character. Direct CSV download URL.}
#'     \item{description}{Character. Brief dataset description.}
#'   }
#' @examples
#' dc_list()
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
#' Downloads a CSV export of any DC open data dataset by ArcGIS item ID
#' and layer number. Use \code{\link{dc_list}} to find item IDs and layers.
#'
#' @param item_id Character. ArcGIS item ID (from \code{\link{dc_list}} or
#'   \code{\link{dc_search}}). Example: \code{"f6c3c04113944f23a7993f2e603abaf2"}
#' @param layer Integer. ArcGIS layer number (default 0). Some items have
#'   multiple layers.
#' @param max_rows Integer or NULL. Maximum rows to return (default 5000).
#'   Set \code{NULL} for all rows.
#' @return A tibble. Columns vary by dataset. Common patterns include
#'   ID, NAME, ADDRESS, WARD, LATITUDE, LONGITUDE, OBJECTID.
#' @examples
#' dc_data("2bb8375e31a94067a17911ea70f917ef", layer = 11, max_rows = 10)
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

#' Get DC crime data (Part 1 offenses)
#'
#' Downloads crime incident data from the DC Metropolitan Police Department.
#' Part 1 offenses include homicide, robbery, assault, burglary, theft, etc.
#'
#' @param max_rows Integer. Maximum rows to return (default 1000).
#' @return A tibble with columns including:
#'   \describe{
#'     \item{DATEEND}{Character. End date of the report period.}
#'     \item{TIMESTART}{Integer. Start time (24h integer).}
#'     \item{TIMEEND}{Integer. End time (24h integer).}
#'     \item{ADDRESS}{Character. Block-level address.}
#'     \item{CODE_DEFINED}{Character. Offense category.}
#'     \item{LarcenyCode}{Character. Larceny subcategory (if applicable).}
#'     \item{Arrest}{Character. Arrest indicator.}
#'     \item{LAT}{Numeric. Latitude.}
#'     \item{LONG}{Numeric. Longitude.}
#'     \item{ObjectId}{Integer. Unique record ID.}
#'   }
#' @examples
#' dc_crime(max_rows = 20)
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

#' Get dc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
dc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(dc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/dc.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "dc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# dc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# dc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
