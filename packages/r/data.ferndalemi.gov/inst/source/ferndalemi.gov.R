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
#' @return tibble: key, name, item_id, layer
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
#' @param q Search query
#' @return tibble: id, type, title, description, url
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
#' @param key Dataset key from fern_datasets(), or a raw item_id
#' @param layer Layer number (only needed if using raw item_id)
#' @return tibble with dataset records
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
#' @return tibble with drug incident records (2007-2017)
fern_drug_incidents <- function() {
  fern_data("drug_incidents")
}

# == Context ===================================================================

#' Generate LLM-friendly context for ferndalemi.gov
#'
#' @return Character string with full function signatures and bodies
fern_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({ f <- sys.frame(0)$ofile; if (!is.null(f) && file.exists(f)) src_file <<- f },
             error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/ferndalemi.gov.R"
  if (!file.exists(src_file)) { cat("# ferndalemi.gov context - source not found\n"); return(invisible(NULL)) }
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
