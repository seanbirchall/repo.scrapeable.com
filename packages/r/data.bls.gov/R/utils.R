#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_body_json
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# bls-gov.R
# Self-contained Bureau of Labor Statistics API v2 client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: optional API key (registrationKey). Without: 25 series/query, 10yr range,
#   25 req/day. With: 50 series/query, 20yr range, 500 req/day.
#   Register at https://data.bls.gov/registrationEngine/
# Docs: https://www.bls.gov/developers/api_signature_v2.htm


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.bls_base <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

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

# -- Fetch helper --------------------------------------------------------------

.bls_post <- function(body, key = NULL) {
  if (!is.null(key)) body$registrationKey <- key
  tmp <- tempfile(fileext = ".json")
  httr2::request(.bls_base) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  if (raw$status != "REQUEST_SUCCEEDED")
    warning("BLS API: ", paste(raw$message, collapse = "; "), call. = FALSE)
  raw
}


# == Schemas ===================================================================

.schema_series <- tibble(
  series_id = character(), year = integer(), period = character(),
  period_name = character(), value = numeric(), date = as.Date(character())
)

.schema_catalog <- tibble(
  series_id = character(), series_title = character(),
  survey_name = character(), survey_abbreviation = character(),
  seasonal = character(), area_code = character(), area_name = character(),
  item_code = character(), item_name = character()
)

.schema_search <- tibble(
  series_id = character(), series_title = character(),
  survey_name = character()
)



utils::globalVariables(c("series_id"))
