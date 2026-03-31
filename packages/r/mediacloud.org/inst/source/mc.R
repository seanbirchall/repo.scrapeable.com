# mediacloud.R
# Self-contained Media Cloud API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for search overview
# Rate limits: not documented, be respectful

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.mc_base <- "https://search.mediacloud.org/api"

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_search <- tibble(
  query = character(), date_range = character(),
  total = integer(), matches_per_day = character()
)

# == Search ====================================================================

#' Search Media Cloud for news coverage overview
#'
#' @param query Search query (e.g. "climate change", "elections")
#' @param date_range Date range as "YYYY-MM-DD..YYYY-MM-DD"
#'   (e.g. "2025-01-01..2025-03-31")
#' @return tibble: query, date_range, total, matches_per_day
mc_search <- function(query, date_range) {
  url <- paste0(.mc_base, "/search/overview?q=", utils::URLencode(query),
                "&dt=", date_range)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_search)

  total <- as.integer(raw$total %||% raw$`relevant` %||% NA_integer_)
  mpd <- if (!is.null(raw$dailyCounts) || !is.null(raw$`matches_per_day`)) {
    counts <- raw$dailyCounts %||% raw$`matches_per_day`
    if (is.data.frame(counts)) {
      paste(apply(counts, 1, function(r) paste(r, collapse = ":")), collapse = "; ")
    } else if (is.list(counts)) {
      paste(names(counts), unlist(counts), sep = ":", collapse = "; ")
    } else as.character(counts)
  } else NA_character_

  tibble(
    query = query,
    date_range = date_range,
    total = total,
    matches_per_day = mpd
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the Media Cloud package
#'
#' @return Character string (invisibly), also printed
mc_context <- function() {
  .build_context("mediacloud.org", header_lines = c(
    "# mediacloud.org - Media Cloud News Search API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required for search overview",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Media Cloud tracks news coverage across global media.",
    "# Use date_range format: 'YYYY-MM-DD..YYYY-MM-DD'"
  ))
}
