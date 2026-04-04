#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# greatschools.org - GreatSchools API client
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required (GREATSCHOOLS_API_KEY env var)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gs_base <- "https://gs-api.greatschools.org"

`%||%` <- function(x, y) if (is.null(x)) y else x

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

.fetch <- function(url, ext = ".json", api_key = NULL) {
  tmp <- tempfile(fileext = ext)
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua)
  if (!is.null(api_key)) {
    req <- req |> httr2::req_headers(`x-api-key` = api_key)
  }
  req |> httr2::req_perform(path = tmp)
  tmp
}

.fetch_gs_json <- function(url, api_key) {
  jsonlite::fromJSON(.fetch(url, api_key = api_key))
}

.gs_key <- function(api_key = NULL) {
  key <- api_key %||% Sys.getenv("GREATSCHOOLS_API_KEY", "")
  if (key == "") stop("GreatSchools API key required. Set GREATSCHOOLS_API_KEY or pass api_key parameter.\nRegister at https://www.greatschools.org/api/registration.page")
  key
}
