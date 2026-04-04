#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_retry req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# va.gov.R
# Self-contained VA Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA at www.data.va.gov

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.va_base <- "https://www.data.va.gov"

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

# -- SODA query helper --------------------------------------------------------

._soda_query <- function(view_id, where = NULL, select = NULL, group = NULL,
                         order = NULL, limit = 1000, offset = 0, q = NULL) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  url <- sprintf("%s/resource/%s.json?%s", .va_base, view_id,
                 utils::URLencode(query, reserved = FALSE))
  tryCatch(.fetch_json(url), error = function(e) {
    warning("VA SODA query error: ", e$message)
    return(NULL)
  })
}

._soda_paginate <- function(view_id, where = NULL, select = NULL,
                            order = NULL, max_rows = 10000,
                            page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._soda_query(view_id, where = where, select = select,
                          order = order, limit = page_size, offset = offset)
    if (is.null(batch) || length(batch) == 0 ||
        (is.data.frame(batch) && nrow(batch) == 0)) break
    results[[length(results) + 1]] <- as_tibble(batch)
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), rows_updated_at = as.POSIXct(character())
)

.schema_records <- tibble()
