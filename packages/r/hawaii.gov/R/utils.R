#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_retry req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# hawaii.gov.R
# Self-contained Hawaii Open Data (CKAN) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: CKAN v3 at opendata.hawaii.gov

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hi_base <- "https://opendata.hawaii.gov"

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
    httr2::req_retry(max_tries = 2) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -- CKAN helpers --------------------------------------------------------------

._ckan_action <- function(action, params = list()) {
  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- if (nchar(query) > 0) {
    sprintf("%s/api/action/%s?%s", .hi_base, action, query)
  } else {
    sprintf("%s/api/action/%s", .hi_base, action)
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Hawaii CKAN error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || !isTRUE(raw$success)) {
    if (!is.null(raw)) warning("CKAN API error: ", raw$error$message %||% "unknown")
    return(NULL)
  }
  raw$result
}

._datastore_search <- function(resource_id, q = NULL, filters = NULL,
                               limit = 1000, offset = 0) {
  params <- list(resource_id = resource_id, limit = limit, offset = offset)
  if (!is.null(q)) params[["q"]] <- q
  if (!is.null(filters)) params[["filters"]] <- filters

  result <- ._ckan_action("datastore_search", params)
  if (is.null(result)) return(tibble())

  records <- result$records
  if (is.null(records) || length(records) == 0) return(tibble())
  df <- as_tibble(records)
  df[["_id"]] <- NULL
  df
}

._datastore_paginate <- function(resource_id, q = NULL, max_rows = 10000,
                                 page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._datastore_search(resource_id, q = q, limit = page_size,
                                offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

# == Schemas ===================================================================

.schema_packages <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), num_resources = integer(), notes = character()
)

.schema_records <- tibble()
