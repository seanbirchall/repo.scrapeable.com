#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# swpc-noaa-gov.R
# Self-contained NOAA Space Weather Prediction Center client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.swpc_base <- "https://services.swpc.noaa.gov/products"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- Parse array-of-arrays format (first row = header) ------------------------

.parse_swpc_array <- function(raw) {
  if (length(raw) < 2) return(tibble())
  headers <- unlist(raw[[1]])
  rows <- raw[-1]
  mat <- do.call(rbind, lapply(rows, function(r) unlist(r)))
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- headers
  as_tibble(df)
}

# == Schemas ===================================================================

.schema_plasma <- tibble(
  time_tag = as.POSIXct(character()), density = numeric(),
  speed = numeric(), temperature = numeric()
)

.schema_mag <- tibble(
  time_tag = as.POSIXct(character()), bx_gsm = numeric(),
  by_gsm = numeric(), bz_gsm = numeric(),
  lon_gsm = numeric(), lat_gsm = numeric(), bt = numeric()
)

.schema_kp <- tibble(
  time_tag = as.POSIXct(character()), kp = numeric(),
  a_running = numeric(), station_count = integer()
)

.schema_alerts <- tibble(
  product_id = character(), issue_datetime = as.POSIXct(character()),
  message = character()
)
