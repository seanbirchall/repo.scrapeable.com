#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# population-un-org.R
# Self-contained UN World Population Prospects client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.unpop_base <- "https://population.un.org/dataportalapi/api/v1"

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_indicators <- tibble(
  id = integer(), name = character(), short_name = character()
)

.schema_locations <- tibble(
  id = integer(), name = character(), iso3 = character(),
  type = character()
)

.schema_data <- tibble(
  location_id = integer(), location = character(),
  indicator_id = integer(), indicator = character(),
  year = integer(), value = numeric(), sex = character(),
  variant = character()
)

# == Private helpers ===========================================================

.unpop_fetch_paginated <- function(url, max_pages = 50) {
  all_data <- list()
  page <- 1
  while (page <= max_pages) {
    sep <- if (grepl("\\?", url)) "&" else "?"
    paged_url <- sprintf("%s%spageNumber=%d&pageSize=100", url, sep, page)
    raw <- tryCatch(.fetch_json(paged_url), error = function(e) NULL)
    if (is.null(raw)) break
    dat <- if (is.data.frame(raw)) raw else raw$data
    if (is.null(dat) || (is.data.frame(dat) && nrow(dat) == 0)) break
    if (!is.data.frame(dat)) break
    all_data[[page]] <- dat
    # Check if there are more pages
    total <- raw$totalPages %||% raw$pages %||% page
    if (page >= total) break
    page <- page + 1
  }
  if (length(all_data) == 0) return(NULL)
  bind_rows(all_data)
}

# == Public functions ==========================================================

