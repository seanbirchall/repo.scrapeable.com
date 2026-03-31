#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# rba-gov-au.R
# Self-contained Reserve Bank of Australia (RBA) CSV client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rba_base <- "https://www.rba.gov.au/statistics/tables/csv"

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

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.parse_rba_csv <- function(file) {
  lines <- readLines(file, warn = FALSE)
  # RBA CSVs have metadata header rows; find the actual data header
  # Usually "Series ID" row or the first row that looks like column headers
  header_idx <- which(grepl("^Series ID", lines))
  if (length(header_idx) == 0) {
    # Try to find first line with multiple commas (data header)
    header_idx <- which(grepl("^Title,", lines) | grepl("^\"Title\"", lines))
  }
  if (length(header_idx) == 0) {
    # Fallback: skip lines starting with common metadata prefixes
    skip <- sum(grepl("^(Title|Description|Frequency|Type|Units|Source|Publication)", lines[1:min(20, length(lines))]))
    if (skip == 0) skip <- 0
    return(utils::read.csv(file, stringsAsFactors = FALSE, skip = skip))
  }
  # Read from header_idx
  data_lines <- lines[header_idx[1]:length(lines)]
  tc <- textConnection(data_lines)
  on.exit(close(tc))
  utils::read.csv(tc, stringsAsFactors = FALSE)
}

# == Schemas ===================================================================

.schema_table <- tibble(
  date = as.Date(character()), series_id = character(),
  series_name = character(), value = numeric()
)
