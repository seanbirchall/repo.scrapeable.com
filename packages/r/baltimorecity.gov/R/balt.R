# baltimorecity.gov.R - City of Baltimore Open Data client (ArcGIS Hub)


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
#' Searches the data.baltimorecity.gov ArcGIS Hub catalog by tag. Returns
#' dataset metadata including title, type, description, and tags. Note:
#' this searches by tag filter, not full-text; use terms like "police",
#' "housing", or "education".
#'
#' @param query Character or NULL. Tag to filter by (e.g. \code{"police"},
#'   \code{"housing"}, \code{"education"}, \code{"crime"}). If NULL, returns
#'   recent datasets without filtering.
#' @param limit Integer. Max datasets to return (default 20, max 100).
#'   Example: \code{limit = 5}
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. ArcGIS item ID (e.g. "5b53f40abbc54c559ca991c96816cc17_0").}
#'     \item{title}{Character. Dataset name (e.g. "SPD Persons Arrested").}
#'     \item{type}{Character. Layer type (e.g. "Table", "Feature Layer").}
#'     \item{description}{Character. Truncated HTML description (first 200 chars).}
#'     \item{tags}{Character. Semicolon-delimited tags (e.g. "Crime; Open Data").}
#'     \item{csv_url}{Character. Always NA; use \code{\link{balt_data}} to download.}
#'   }
#' @examples
#' balt_search("police", limit = 5)
#' balt_search("housing")
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
#' Returns a curated list of 13 popular Baltimore open data datasets with
#' pre-built CSV download URLs. Includes schools, arrests, property data,
#' grocery stores, environmental citations, and more. Use the \code{item_id}
#' and \code{layer} values with \code{\link{balt_data}} to download any
#' dataset as a tibble.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Dataset name (e.g. "BPD Arrests", "Baltimore City Schools").}
#'     \item{item_id}{Character. ArcGIS item ID for use with \code{balt_data()}.}
#'     \item{layer}{Numeric. Layer number within the item.}
#'     \item{csv_url}{Character. Direct CSV download URL.}
#'     \item{description}{Character. Short dataset description.}
#'   }
#' @examples
#' balt_list()
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
#' Downloads a CSV from the Baltimore ArcGIS Hub and parses it into a tibble.
#' Use \code{\link{balt_list}} to find available item IDs, or pass any
#' ArcGIS item ID discovered via \code{\link{balt_search}}.
#'
#' @param item_id Character. The ArcGIS item ID. Get these from
#'   \code{balt_list()$item_id} or \code{balt_search()$id}.
#'   Example: \code{"bf4ca51722c44e0593d8b9b375e8a509"} (Schools)
#' @param layer Integer. Layer number within the item (default 0).
#'   Most datasets use layer 0; see \code{balt_list()} for exceptions.
#' @param max_rows Integer or NULL. Maximum rows to return (default 5000).
#'   Pass \code{NULL} to return all rows.
#' @return A tibble whose columns depend on the dataset. For example,
#'   the arrests dataset returns ~20 columns including Latitude, Longitude,
#'   ArrestDateTime, ChargeDescription, Neighborhood, etc.
#' @examples
#' # Download schools data (155 rows)
#' balt_data("bf4ca51722c44e0593d8b9b375e8a509")
#'
#' # Download first 100 arrest records
#' balt_data("619ec10c14b346f784a5a07bad4c43cd", max_rows = 100)
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
#' Downloads all Baltimore City public school locations (~155 schools).
#' Shortcut for \code{balt_data("bf4ca51722c44e0593d8b9b375e8a509")}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{OBJECTID}{Integer. Row identifier.}
#'     \item{BLDG_NUM, PRG_NUM}{Integer. Building and program numbers.}
#'     \item{NAME}{Character. School name (e.g. "Cecil Elementary").}
#'     \item{ADDRESS}{Character. Street address.}
#'     \item{CLASS}{Character. Grade range (e.g. "PK-5", "PK-8").}
#'     \item{CATEGORY}{Character. School category code (e.g. "E", "EM", "M", "H").}
#'     \item{x, y}{Numeric. Projected coordinates.}
#'     \item{GRADES}{Character. Grade levels served.}
#'     \item{ZIPCODE}{Integer. ZIP code.}
#'     \item{TYPE}{Character. School type.}
#'   }
#' @examples
#' balt_schools()
balt_schools <- function() {
  balt_data("bf4ca51722c44e0593d8b9b375e8a509", layer = 0, max_rows = NULL)
}

#' Get Baltimore police arrests data
#'
#' Downloads Baltimore Police Department arrest records. Shortcut for
#' \code{balt_data("619ec10c14b346f784a5a07bad4c43cd")}. Each row is
#' one arrest with charge, demographic, and location information.
#'
#' @param max_rows Integer. Maximum rows to return (default 1000).
#'   Example: \code{max_rows = 500}
#' @return A tibble with ~20 columns including:
#'   \describe{
#'     \item{Latitude, Longitude}{Numeric. Coordinates of arrest.}
#'     \item{ArrestNumber}{Integer. Arrest identifier.}
#'     \item{Age}{Integer. Age of arrestee.}
#'     \item{Gender}{Character. Gender.}
#'     \item{Race}{Character. Race.}
#'     \item{ArrestDateTime}{Character. Date and time of arrest.}
#'     \item{ArrestLocation}{Character. Location of arrest.}
#'     \item{Charge}{Character. Charge code.}
#'     \item{ChargeDescription}{Character. Full charge description.}
#'     \item{District}{Character. Police district.}
#'     \item{Neighborhood}{Character. Neighborhood name.}
#'   }
#' @examples
#' balt_arrests(max_rows = 50)
balt_arrests <- function(max_rows = 1000) {
  balt_data("619ec10c14b346f784a5a07bad4c43cd", layer = 0, max_rows = max_rows)
}

#' Get baltimorecity.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
balt_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(balt_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/baltimorecity.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "baltimorecity.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# baltimorecity.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# baltimorecity.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
