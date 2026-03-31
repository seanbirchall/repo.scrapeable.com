# bts-gov.R
# Self-contained Bureau of Transportation Statistics client (Socrata SODA).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA at datahub.transportation.gov

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.bts_base <- "https://datahub.transportation.gov"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
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
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character()
)

.schema_records <- tibble()

# == Functions =================================================================

#' List BTS datasets
#'
#' @param limit Number of datasets (default 50)
#' @param query Optional name filter
#' @return tibble: id, name, category, description
bts_datasets <- function(limit = 50, query = NULL) {
  url <- sprintf("%s/api/views?limit=%d", .bts_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BTS API error: ", e$message); NULL
  })
  if (is.null(raw) || nrow(raw) == 0) return(.schema_datasets)

  result <- tibble(
    id          = as.character(raw$id),
    name        = as.character(raw$name),
    category    = as.character(raw$category %||% NA_character_),
    description = ifelse(nchar(raw$description %||% "") > 200,
                         paste0(substr(raw$description, 1, 200), "..."),
                         as.character(raw$description %||% ""))
  )
  if (!is.null(query)) result <- result |> filter(grepl(query, name, ignore.case = TRUE))
  result
}

#' Query a BTS dataset
#'
#' @param dataset_id Socrata dataset ID
#' @param where SoQL where clause
#' @param select SoQL select clause
#' @param group SoQL group by
#' @param order SoQL order
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset
#' @return tibble
bts_query <- function(dataset_id, where = NULL, select = NULL,
                      group = NULL, order = NULL,
                      limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .bts_base, dataset_id,
                 utils::URLencode(query, reserved = FALSE))

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("BTS query error: ", e$message); NULL
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_records)
  as_tibble(raw)
}

#' Fetch all records with auto-pagination
#'
#' @param dataset_id Socrata dataset ID
#' @param where Optional SoQL where clause
#' @param max_rows Maximum rows (default 10000)
#' @param page_size Rows per request (default 1000)
#' @return tibble
bts_fetch_all <- function(dataset_id, where = NULL, max_rows = 10000,
                          page_size = 1000) {
  results <- list(); offset <- 0
  while (offset < max_rows) {
    batch <- bts_query(dataset_id, where = where, limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  bind_rows(results)
}

#' Generate context
#' @return Character string (invisibly)
bts_context <- function() {
  .build_context("bts.gov", header_lines = c(
    "# bts.gov - Bureau of Transportation Statistics Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Socrata SODA API at datahub.transportation.gov"
  ))
}
