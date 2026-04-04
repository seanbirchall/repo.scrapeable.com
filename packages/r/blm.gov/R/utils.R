#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# blm.gov.R
# Self-contained Bureau of Land Management GIS client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public ArcGIS REST services)
# Docs: https://gis.blm.gov/arcgis/rest/services

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.blm_base <- "https://gis.blm.gov/arcgis/rest/services"

# -- Core ArcGIS query engine --------------------------------------------------

.blm_query <- function(service, layer = 0, where = "1=1", out_fields = "*",
                       result_count = 1000, result_offset = 0,
                       order_by = NULL) {
  url <- paste0(.blm_base, "/", service, "/FeatureServer/", layer, "/query")
  params <- list(
    where = where,
    outFields = out_fields,
    resultRecordCount = result_count,
    resultOffset = result_offset,
    f = "json"
  )
  if (!is.null(order_by)) params$orderByFields <- order_by

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

.blm_fetch <- function(service, layer = 0, where = "1=1", out_fields = "*",
                       max_results = 500, order_by = NULL) {
  all_rows <- list()
  offset <- 0
  page <- min(max_results, 1000)

  repeat {
    raw <- .blm_query(service, layer, where, out_fields,
                      result_count = page, result_offset = offset,
                      order_by = order_by)
    feats <- raw$features
    if (is.null(feats) || length(feats) == 0) break

    rows <- lapply(feats, function(f) {
      attrs <- f$attributes
      lapply(attrs, function(v) {
        if (is.null(v)) NA_character_ else as.character(v)
      })
    })
    all_rows <- c(all_rows, rows)
    offset <- offset + length(feats)

    if (length(all_rows) >= max_results) break
    if (!isTRUE(raw$exceededTransferLimit) && length(feats) < page) break
  }

  if (length(all_rows) == 0) return(tibble::tibble())
  if (length(all_rows) > max_results) all_rows <- all_rows[seq_len(max_results)]
  bind_rows(lapply(all_rows, tibble::as_tibble))
}

.blm_count <- function(service, layer = 0, where = "1=1") {
  url <- paste0(.blm_base, "/", service, "/FeatureServer/", layer, "/query")
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_url_query(where = where, returnCountOnly = "true", f = "json") |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(15) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  raw$count %||% 0L
}

# -- Context generator ---------------------------------------------------------

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
