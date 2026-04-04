#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# eeoc.gov.R
# Self-contained EEOC workforce data client.
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Docs: https://www.eeoc.gov/data

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.eeoc_base <- "https://www.eeoc.gov"

.eeo4_url <- "https://www.eeoc.gov/sites/default/files/2022-09/puf_eeo4_2005_2021.csv"

.eeo1_years <- c(
  "2021" = "https://www.eeoc.gov/sites/default/files/2023-10/EEO-1%20Component%201%20Public%20Use%20File.xlsx",
  "2020" = "https://www.eeoc.gov/sites/default/files/2023-10/EEO-1%20Component%201%20Public%20Use%20File.xlsx",
  "2019" = "https://www.eeoc.gov/sites/default/files/2023-10/EEO-1%20Component%201%20Public%20Use%20File.xlsx",
  "2018" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202018%20PUF.xlsx",
  "2017" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202017%20PUF.xlsx",
  "2016" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202016%20PUF.xlsx",
  "2015" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202015%20PUF.xlsx",
  "2014" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202014%20PUF.xlsx"
)

.eeoc_download <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(120) |>
    httr2::req_perform(path = tmp)
  tmp
}

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
