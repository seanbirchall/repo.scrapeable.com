#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# msci-com.R
# Self-contained MSCI index data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (undocumented API)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.msci_base <- "https://app2.msci.com/products/service/index/indexmaster"

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

# -- Known MSCI index codes ----------------------------------------------------

.msci_codes <- tibble::tibble(
  code    = c("990100", "891800", "990300", "139261", "106235",
              "990200", "891600", "891400", "891200",
              "100800", "990500", "990700", "990900", "891000"),
  name    = c("MSCI World", "MSCI ACWI", "MSCI Emerging Markets",
              "MSCI EAFE", "MSCI Europe",
              "MSCI World ex USA", "MSCI ACWI ex USA",
              "MSCI EM Asia", "MSCI EM Latin America",
              "MSCI Japan", "MSCI Pacific", "MSCI Pacific ex Japan",
              "MSCI Kokusai", "MSCI EM Europe Middle East & Africa")
)

# == Schemas ===================================================================

.schema_levels <- tibble::tibble(
  date = as.Date(character()), level = numeric(),
  index_code = character(), index_variant = character(),
  currency = character()
)

.schema_index_codes <- tibble::tibble(
  code = character(), name = character()
)
