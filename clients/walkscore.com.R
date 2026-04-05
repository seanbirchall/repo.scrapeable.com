# walkscore.com.R
# Self-contained Walk Score API client.
# Returns walk, transit, and bike scores for any address/coordinates.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required (free signup at https://www.walkscore.com/professional/api-sign-up.php)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ws_base <- "https://api.walkscore.com"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

.ws_key <- function(api_key = NULL) {
  key <- api_key %||% Sys.getenv("WALKSCORE_API_KEY", "")
  if (key == "") stop("Walk Score API key required. Set WALKSCORE_API_KEY or pass api_key parameter.\nSign up at https://www.walkscore.com/professional/api-sign-up.php")
  key
}

# == Schemas ===================================================================

.schema_score <- tibble(
  address = character(),
  lat = numeric(),
  lon = numeric(),
  walkscore = integer(),
  walk_description = character(),
  transit_score = integer(),
  transit_description = character(),
  bike_score = integer(),
  bike_description = character(),
  tip = character()
)

# == Public functions ==========================================================

#' Get Walk Score, Transit Score, and Bike Score for an address
#'
#' Queries the Walk Score API to evaluate walkability, transit access,
#' and bikeability for a given location. Scores range from 0--100
#' where higher is better. Requires a free API key from Walk Score.
#'
#' @param address Character street address (e.g.,
#'   \code{"1600 Pennsylvania Ave NW Washington DC"}).
#' @param lat Numeric latitude in decimal degrees.
#' @param lon Numeric longitude in decimal degrees.
#' @param api_key Optional Walk Score API key. Defaults to
#'   the \code{WALKSCORE_API_KEY} environment variable. Sign up at
#'   \url{https://www.walkscore.com/professional/api-sign-up.php}.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{address}{Snapped/corrected address from Walk Score}
#'     \item{lat}{Numeric latitude}
#'     \item{lon}{Numeric longitude}
#'     \item{walkscore}{Integer 0--100 walkability score}
#'     \item{walk_description}{Walkability category (e.g., "Very Walkable")}
#'     \item{transit_score}{Integer 0--100 transit score, or NA}
#'     \item{transit_description}{Transit category description}
#'     \item{bike_score}{Integer 0--100 bike score, or NA}
#'     \item{bike_description}{Bikeability category description}
#'     \item{tip}{Walk Score tip/explanation text}
#'   }
#' @export
#' @family Walk Score functions
#' @seealso \code{\link{ws_scores_batch}} for scoring multiple addresses
#' @examples
#' \dontrun{
#' ws_score("1600 Pennsylvania Ave NW Washington DC", 38.8977, -77.0365)
#' }
ws_score <- function(address, lat, lon, api_key = NULL) {
  key <- .ws_key(api_key)
  url <- sprintf(
    "%s/score?format=json&address=%s&lat=%f&lon=%f&transit=1&bike=1&wsapikey=%s",
    .ws_base, utils::URLencode(address), lat, lon, key
  )
  raw <- .fetch_json(url)

  if (!is.null(raw$status) && raw$status != 1) {
    status_msg <- switch(as.character(raw$status),
      "2" = "Score being calculated, try again later",
      "30" = "Invalid API key",
      "31" = "API quota exceeded",
      "40" = "Your API key is invalid",
      "41" = "Your API quota has been exceeded",
      sprintf("API error status: %s", raw$status)
    )
    warning(status_msg)
    return(.schema_score)
  }

  transit <- raw$transit %||% list()
  bike <- raw$bike %||% list()

  tibble(
    address = as.character(raw$`snapped-address` %||% address),
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    walkscore = as.integer(raw$walkscore %||% NA),
    walk_description = as.character(raw$description %||% NA),
    transit_score = as.integer(transit$score %||% NA),
    transit_description = as.character(transit$description %||% NA),
    bike_score = as.integer(bike$score %||% NA),
    bike_description = as.character(bike$description %||% NA),
    tip = as.character(raw$tip %||% NA)
  )
}

#' Get Walk Scores for multiple addresses
#'
#' Convenience wrapper that calls \code{\link{ws_score}} for each
#' location and combines results into a single tibble. Failed lookups
#' return a row with NA scores rather than stopping execution.
#'
#' @param addresses Character vector of street addresses.
#' @param lats Numeric vector of latitudes (same length as \code{addresses}).
#' @param lons Numeric vector of longitudes (same length as \code{addresses}).
#' @param api_key Optional Walk Score API key (defaults to env var).
#' @return A tibble with one row per address and the same columns as
#'   \code{\link{ws_score}}.
#' @export
#' @family Walk Score functions
#' @seealso \code{\link{ws_score}} for a single address
#' @examples
#' \dontrun{
#' ws_scores_batch(
#'   addresses = c("Times Square NYC", "Golden Gate Park SF"),
#'   lats = c(40.7580, 37.7694),
#'   lons = c(-73.9855, -122.4862)
#' )
#' }
ws_scores_batch <- function(addresses, lats, lons, api_key = NULL) {
  results <- lapply(seq_along(addresses), function(i) {
    tryCatch(
      ws_score(addresses[i], lats[i], lons[i], api_key),
      error = function(e) {
        row <- .schema_score[1, ]
        row$address <- addresses[i]
        row$lat <- lats[i]
        row$lon <- lons[i]
        row
      }
    )
  })
  bind_rows(results)
}

#' Get walkscore.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ws_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ws_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/walkscore.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "walkscore.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# walkscore.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# walkscore.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
