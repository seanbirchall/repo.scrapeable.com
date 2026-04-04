# ferndalemi.gov.R - City of Ferndale MI open data client (ArcGIS Hub)
#
# Data source: data.ferndalemi.gov (ArcGIS Hub)
# Datasets: ~273 (bus routes, drug incidents, electoral districts, etc.)
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fern_base <- "https://data.ferndalemi.gov"

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

.fern_datasets <- list(
  bus_445        = list(id = "7079716c96b14dcab1c5d73d7fdc1274", layer = 0, name = "SMART Bus Route 445"),
  bus_475        = list(id = "7079716c96b14dcab1c5d73d7fdc1274", layer = 2, name = "SMART Bus Route 475"),
  bus_710        = list(id = "7079716c96b14dcab1c5d73d7fdc1274", layer = 3, name = "SMART Bus Route 710"),
  drug_incidents = list(id = "0511440139bd4437b5ffa10a41f05a68", layer = 0, name = "Drug Incidents & NARCAN 2007-2017"),
  congress_dist  = list(id = "c0d77fa257eb410c8aaa0be2d4917d51", layer = 0, name = "MI Congressional Districts 2010"),
  house_dist     = list(id = "2bb8d3e1f78c441abb8e03a16be6acd0", layer = 0, name = "MI House Districts 2010"),
  senate_dist    = list(id = "14890ed705184b6cacd5fde2647026f7", layer = 0, name = "MI Senate Districts 2010"),
  land_water     = list(id = "4f9202a507244b10bb12942d5f9f46c7", layer = 0, name = "Land & Water Fund Detroit 2016")
)

# == Data access ===============================================================

#' List available Ferndale datasets
#'
#' Returns a reference table of known datasets from the City of Ferndale, MI
#' open data portal (ArcGIS Hub). Each row can be passed to
#' \code{\link{fern_data}} to download the full dataset.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{key}{Character. Short key name (e.g. "bus_445", "drug_incidents").}
#'     \item{name}{Character. Human-readable dataset name.}
#'     \item{item_id}{Character. ArcGIS item ID hash.}
#'     \item{layer}{Integer. Layer index within the item.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fern_datasets()
#' }
fern_datasets <- function() {
  tibble(
    key     = names(.fern_datasets),
    name    = vapply(.fern_datasets, function(x) x$name, character(1)),
    item_id = vapply(.fern_datasets, function(x) x$id, character(1)),
    layer   = vapply(.fern_datasets, function(x) as.integer(x$layer), integer(1))
  )
}

#' Search Ferndale open data portal
#'
#' Full-text search across the City of Ferndale, MI ArcGIS Hub portal.
#' Returns matching datasets, maps, and applications.
#'
#' @param q Character. Search query string (e.g. "bus", "zoning", "crime").
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. ArcGIS item ID.}
#'     \item{type}{Character. Item type (e.g. "dataset").}
#'     \item{title}{Character. Dataset title.}
#'     \item{description}{Character. Short description.}
#'     \item{url}{Character. URL to dataset page.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fern_search("bus")
#' fern_search("zoning")
#' }
fern_search <- function(q = "") {
  url <- sprintf("%s/api/v3/search?q=%s", .fern_base, utils::URLencode(q))
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

#' Download a Ferndale dataset as CSV
#'
#' Download and parse a dataset from the Ferndale open data portal.
#' Accepts either a short key from \code{\link{fern_datasets}} or
#' a raw ArcGIS item ID.
#'
#' @param key Character. Dataset key from \code{fern_datasets()$key}
#'   (e.g. \code{"bus_445"}, \code{"drug_incidents"}), or a raw ArcGIS item ID.
#' @param layer Integer. Layer number within the item (only needed when
#'   passing a raw item ID; ignored when using a known key).
#' @return A tibble with dataset-specific columns. Column names and types
#'   vary by dataset.
#' @export
#' @examples
#' \dontrun{
#' fern_data("bus_445")
#' fern_data("drug_incidents")
#' }
fern_data <- function(key, layer = NULL) {
  if (key %in% names(.fern_datasets)) {
    ds <- .fern_datasets[[key]]
    item_id <- ds$id
    lyr <- ds$layer
  } else {
    item_id <- key
    lyr <- layer %||% 0
  }
  url <- sprintf("%s/api/download/v1/items/%s/csv?layers=%d", .fern_base, item_id, lyr)
  .fetch_csv(url)
}

#' Get Ferndale drug incident and NARCAN data
#'
#' Convenience function to download the Drug Incidents and NARCAN
#' dataset (2007-2017) from the Ferndale open data portal.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{Incident_Anonymous_Number}{Integer. Anonymized incident ID.}
#'     \item{Date_Key}{Numeric. Date as Unix timestamp (milliseconds).}
#'     \item{Description}{Character. Incident description (e.g. "DRUG OVERDOSE").}
#'     \item{Crime_Class}{Character. Crime classification code.}
#'     \item{Block_Number}{Integer. Street block number.}
#'     \item{Street_Direction}{Character. Street direction prefix.}
#'     \item{Street_Name}{Character. Street name.}
#'     \item{Street_Name_Suffix}{Character. Street suffix (e.g. "St", "Ave").}
#'     \item{City}{Character. City name.}
#'     \item{State}{Character. State code.}
#'     \item{Zip_Code}{Integer. ZIP code.}
#'     \item{Narcan_Kits_Used}{Integer. Number of NARCAN kits used.}
#'     \item{Narcan_Used}{Character. Whether NARCAN was administered.}
#'     \item{Latitude}{Numeric. Latitude coordinate.}
#'     \item{Longitude}{Numeric. Longitude coordinate.}
#'     \item{FID}{Integer. Feature ID.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fern_drug_incidents()
#' }
fern_drug_incidents <- function() {
  fern_data("drug_incidents")
}

# == Context ===================================================================

#' Get ferndalemi.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fern_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fern_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ferndalemi.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ferndalemi.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ferndalemi.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ferndalemi.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
