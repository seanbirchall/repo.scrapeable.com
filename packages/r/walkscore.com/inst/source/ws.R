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
#' Returns walkability, transit access, and bikeability scores for
#' a given address and coordinates.
#'
#' @param address Street address (e.g., "1600 Pennsylvania Ave NW Washington DC")
#' @param lat Latitude
#' @param lon Longitude
#' @param api_key Walk Score API key. Defaults to WALKSCORE_API_KEY env var.
#' @return tibble: address, lat, lon, walkscore, walk_description,
#'   transit_score, transit_description, bike_score, bike_description, tip
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
#' Convenience wrapper that scores multiple locations at once.
#'
#' @param addresses Character vector of street addresses
#' @param lats Numeric vector of latitudes
#' @param lons Numeric vector of longitudes
#' @param api_key Walk Score API key
#' @return tibble: same columns as ws_score(), one row per address
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

#' Show context for the walkscore.com client
#'
#' @return Invisible string of context
ws_context <- function() {
  src_dir <- system.file("source", package = "walkscore.com")
  if (src_dir == "") {
    this_file <- sys.frame(1)$ofile %||%
      attr(body(ws_score), "srcfile")$filename %||%
      ""
    if (this_file != "" && file.exists(this_file)) {
      lines <- readLines(this_file, warn = FALSE)
    } else {
      cat("# walkscore.com - Walk Score API client\n")
      cat("# Source not found. Use ?ws_score for help.\n")
      return(invisible(""))
    }
  } else {
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) {
      cat("# No R source found.\n")
      return(invisible(""))
    }
    lines <- readLines(src_files[1], warn = FALSE)
  }

  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]
    k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) {
      k <- k + 1
      sig <- paste(sig, trimws(lines[k]))
    }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }
  out <- paste(c(
    "# walkscore.com - Walk Score API",
    "# Walk, Transit, and Bike scores for any address",
    "# Requires API key (WALKSCORE_API_KEY env var)",
    "#",
    "# == Functions ==",
    "#",
    unlist(blocks)
  ), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
