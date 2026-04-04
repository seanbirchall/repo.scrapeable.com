#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_retry req_url_query
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# socrata.com.R
# Self-contained Socrata Open Data Discovery & Access client.
# Uses the Socrata Discovery API (api.us.socrata.com) for catalog search
# and the SODA 2.1 API for fetching actual data from any Socrata domain.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required for public data. App tokens optional (increase rate limits).
# Rate limits: Without token ~1000/hr per IP. With token higher.

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.socrata_disco <- "https://api.us.socrata.com/api/catalog/v1"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.double(x)

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

# -- JSON fetch helper ---------------------------------------------------------

.socrata_fetch <- function(url, params = list(), simplify = TRUE) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = simplify, flatten = FALSE)
}

# -- Type date columns ---------------------------------------------------------

.type_dates <- function(df) {
  for (col in names(df)) {
    if (grepl("date|Date|updated|created|At$", col) && is.character(df[[col]])) {
      vals <- df[[col]]
      is_datelike <- grepl("^\\d{4}-\\d{2}-\\d{2}", vals) & !is.na(vals)
      if (sum(is_datelike, na.rm = TRUE) > length(vals) * 0.5) {
        df[[col]] <- as.Date(substr(vals, 1, 10))
      }
    }
  }
  df
}

utils::globalVariables(c("head"))
