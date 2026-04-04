#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_timeout req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# sba.gov.R
# Self-contained SBA (Small Business Administration) data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: CKAN v3 Action API at data.sba.gov

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sba_base <- "https://data.sba.gov/api/3/action"

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

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
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- CKAN URL builder ----------------------------------------------------------

.sba_url <- function(action, ...) {
  params <- list(...)
  params <- params[!vapply(params, is.null, logical(1))]
  if (length(params) == 0) return(paste0(.sba_base, "/", action))
  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  paste0(.sba_base, "/", action, "?", query)
}

.sba_result <- function(url) {
  raw <- .fetch_json(url)
  if (!isTRUE(raw$success)) {
    warning("SBA CKAN API error: ", raw$error$message %||% "unknown")
    return(NULL)
  }
  raw$result
}
