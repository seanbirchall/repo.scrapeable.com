#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# waterdata-usgs-gov.R
# Self-contained USGS Water Services client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.water_base <- "https://waterservices.usgs.gov/nwis"

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
.fetch_json_nested <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_realtime <- tibble(
  site = character(), parameter = character(), datetime = as.POSIXct(character()),
  value = numeric(), qualifier = character(), site_name = character()
)

.schema_daily <- tibble(
  site = character(), parameter = character(), date = as.Date(character()),
  value = numeric(), qualifier = character(), site_name = character()
)

.schema_sites <- tibble(
  site_no = character(), site_name = character(), site_type = character(),
  lat = numeric(), lon = numeric(), state_cd = character(),
  county_cd = character(), huc_cd = character()
)

# == Private helpers ===========================================================

.parse_iv_response <- function(raw) {
  ts <- raw$value$timeSeries
  if (is.null(ts) || length(ts) == 0) return(.schema_realtime)

  rows <- lapply(ts, function(entry) {
    site_code <- entry$sourceInfo$siteCode[[1]]$value
    site_name <- entry$sourceInfo$siteName
    param_code <- entry$variable$variableCode[[1]]$value
    vals <- entry$values[[1]]$value
    if (is.null(vals) || length(vals) == 0) return(NULL)
    datetimes <- vapply(vals, function(v) v$dateTime, character(1))
    values <- vapply(vals, function(v) v$value, character(1))
    qualifiers <- vapply(vals, function(v) {
      q <- v$qualifiers
      if (is.null(q)) NA_character_ else paste(q, collapse = ",")
    }, character(1))
    tibble(
      site = site_code,
      parameter = param_code,
      datetime = as.POSIXct(datetimes, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
      value = suppressWarnings(as.numeric(values)),
      qualifier = qualifiers,
      site_name = site_name
    )
  })
  bind_rows(rows)
}

.parse_dv_response <- function(raw) {
  ts <- raw$value$timeSeries
  if (is.null(ts) || length(ts) == 0) return(.schema_daily)

  rows <- lapply(ts, function(entry) {
    site_code <- entry$sourceInfo$siteCode[[1]]$value
    site_name <- entry$sourceInfo$siteName
    param_code <- entry$variable$variableCode[[1]]$value
    vals <- entry$values[[1]]$value
    if (is.null(vals) || length(vals) == 0) return(NULL)
    datetimes <- vapply(vals, function(v) v$dateTime, character(1))
    values <- vapply(vals, function(v) v$value, character(1))
    qualifiers <- vapply(vals, function(v) {
      q <- v$qualifiers
      if (is.null(q)) NA_character_ else paste(q, collapse = ",")
    }, character(1))
    tibble(
      site = site_code,
      parameter = param_code,
      date = as.Date(datetimes),
      value = suppressWarnings(as.numeric(values)),
      qualifier = qualifiers,
      site_name = site_name
    )
  })
  bind_rows(rows)
}

# == Public functions ==========================================================

