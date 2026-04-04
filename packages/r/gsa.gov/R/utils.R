#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_error req_url_query resp_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# gsa.gov
# Self-contained GSA API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required. DEMO_KEY works but limited to 10 req/hr.
#   Get a key at https://api.gsa.gov or https://api.data.gov/signup/

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.gsa_base <- "https://api.gsa.gov"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.double(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x)

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

.gsa_fetch <- function(path, params = list(), api_key = "DEMO_KEY") {
  params$api_key <- api_key
  url <- paste0(.gsa_base, "/", path)
  tmp <- tempfile(fileext = ".json")
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)

  status <- httr2::resp_status(resp)
  if (status == 429) {
    stop("GSA API rate limit exceeded. Get a key at https://api.data.gov/signup/",
         call. = FALSE)
  }
  if (status >= 400) {
    body <- tryCatch(jsonlite::fromJSON(tmp), error = function(e) list())
    msg <- body$error$message %||% paste("HTTP", status)
    stop(sprintf("GSA API error: %s", msg), call. = FALSE)
  }
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

utils::globalVariables(c("head"))
