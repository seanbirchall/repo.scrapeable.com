#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# dol.R
# Self-contained Department of Labor foreign labor disclosure client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2, readxl
# Auth: none
# Data source: https://www.dol.gov/agencies/eta/foreign-labor/performance
# Updated quarterly. XLSX files for PERM, H-1B, H-2A, H-2B, CW-1, PW programs.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dol_page <- "https://www.dol.gov/agencies/eta/foreign-labor/performance"

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

.fetch_html <- function(url) xml2::read_html(.fetch(url, ".html"))

# -- Program classifier -------------------------------------------------------

.classify_program <- function(filename) {
  f <- toupper(filename)
  case_when(
    grepl("PERM", f)        ~ "PERM",
    grepl("H.?2A|_2A", f)   ~ "H-2A",
    grepl("H.?2B|_2B", f)   ~ "H-2B",
    grepl("LCA|H.?1B", f)   ~ "H-1B",
    grepl("PW", f)           ~ "PW",
    grepl("CW", f)           ~ "CW-1",
    TRUE                     ~ "OTHER"
  )
}

# == Schemas ===================================================================

.schema_files <- tibble(
  url = character(), filename = character(), program = character()
)


