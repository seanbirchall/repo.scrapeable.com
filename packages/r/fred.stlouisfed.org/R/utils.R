#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# fred.R
# Self-contained FRED (Federal Reserve Economic Data) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: required API key. Get one at https://fred.stlouisfed.org/docs/api/api_key.html
# Rate limits: 120 requests per minute.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fred_base <- "https://api.stlouisfed.org/fred"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- FRED URL builder ----------------------------------------------------------

.fred_url <- function(endpoint, api_key, ...) {
  params <- list(...)
  params <- params[!vapply(params, is.null, logical(1))]
  query <- paste(names(params), params, sep = "=", collapse = "&")
  sprintf("%s/%s?%s&api_key=%s&file_type=json", .fred_base, endpoint, query, api_key)
}

# == Schemas ===================================================================

.schema_series <- tibble(
  date = as.Date(character()), value = numeric(), code = character()
)

.schema_series_info <- tibble(
  id = character(), title = character(), frequency = character(),
  units = character(), seasonal_adjustment = character(),
  observation_start = as.Date(character()), observation_end = as.Date(character()),
  last_updated = character(), notes = character()
)

.schema_search <- tibble(
  id = character(), title = character(), frequency = character(),
  units = character(), seasonal_adjustment = character(),
  observation_start = as.Date(character()), observation_end = as.Date(character()),
  popularity = integer(), notes = character()
)

.schema_categories <- tibble(
  id = integer(), name = character(), parent_id = integer()
)

.schema_releases <- tibble(
  id = integer(), name = character(), press_release = logical(),
  link = character(), notes = character()
)


