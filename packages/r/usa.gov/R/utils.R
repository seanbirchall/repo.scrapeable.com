#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# usa.gov.R
# Self-contained usa.gov client.
# Wraps Federal Register executive orders API + analytics.usa.gov JSON API
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (all public data)

# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

.usag_ua <- "usag-r-client/0.1 (support@scrapeable.com)"

.usag_fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .usag_ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

.usag_fr_base <- "https://www.federalregister.gov/api/v1"
.usag_analytics_base <- "https://analytics.usa.gov/data/live"

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

# == Schemas ===================================================================

.schema_eo <- tibble(
  eo_number = integer(),
  title = character(),
  signing_date = as.Date(character()),
  publication_date = as.Date(character()),
  citation = character(),
  document_number = character(),
  president = character(),
  html_url = character(),
  pdf_url = character()
)

.schema_top_pages <- tibble(
  page_title = character(),
  active_users = integer()
)

.schema_top_domains <- tibble(
  domain = character(),
  visits = numeric()
)

.schema_all_domains <- tibble(
  domain = character(),
  visits = numeric(),
  pageviews = numeric(),
  users = numeric(),
  pageviews_per_session = numeric(),
  avg_session_duration = numeric()
)

.schema_realtime <- tibble(
  active_users = integer(),
  pageviews = integer(),
  taken_at = character()
)

# == Private helpers ===========================================================

.usag_presidents <- c(
  "william-j-clinton", "george-w-bush", "barack-obama",
  "donald-trump", "joe-biden"
)

.parse_eo_results <- function(results, president = NA_character_) {
  if (is.null(results) || length(results) == 0) return(.schema_eo)
  if (!is.data.frame(results)) return(.schema_eo)
  if (nrow(results) == 0) return(.schema_eo)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      eo_number = as.integer(if ("executive_order_number" %in% nms) executive_order_number else NA_integer_),
      title = as.character(if ("title" %in% nms) title else NA_character_),
      signing_date = as.Date(if ("signing_date" %in% nms) signing_date else NA_character_),
      publication_date = as.Date(if ("publication_date" %in% nms) publication_date else NA_character_),
      citation = as.character(if ("citation" %in% nms) citation else NA_character_),
      document_number = as.character(if ("document_number" %in% nms) document_number else NA_character_),
      president = president,
      html_url = as.character(if ("html_url" %in% nms) html_url else NA_character_),
      pdf_url = as.character(if ("pdf_url" %in% nms) pdf_url else NA_character_)
    )
}
