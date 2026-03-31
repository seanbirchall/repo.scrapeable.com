# courtlistener.R
# Self-contained CourtListener API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for basic search
# Rate limits: be respectful

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cl_base <- "https://www.courtlistener.com/api/rest/v4"

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
  id = integer(), caseName = character(), court = character(),
  dateFiled = as.Date(character()), snippet = character(),
  url = character()
)

.schema_opinion <- tibble(
  id = integer(), caseName = character(), court = character(),
  dateFiled = as.Date(character()), snippet = character(),
  url = character(), plain_text = character()
)

# == Search ====================================================================

#' Search CourtListener legal opinions and dockets
#'
#' @param query Search query string (e.g. "privacy", "first amendment")
#' @param type Result type: "o" = opinions, "r" = RECAP, "d" = dockets,
#'   "p" = people (default "o")
#' @param page_size Number of results (default 10)
#' @param page Page number (default 1)
#' @return tibble: id, caseName, court, dateFiled, snippet, url
cl_search <- function(query, type = "o", page_size = 10, page = 1) {
  url <- paste0(.cl_base, "/search/?q=", utils::URLencode(query),
                "&type=", type, "&page_size=", page_size, "&page=", page)
  raw <- .fetch_json(url)
  d <- raw$results
  if (is.null(d) || length(d) == 0) return(.schema_search)

  as_tibble(d) |>
    transmute(
      id = as.integer(id),
      caseName = as.character(caseName),
      court = as.character(if ("court" %in% names(d)) court else NA_character_),
      dateFiled = tryCatch(as.Date(dateFiled), error = function(e) as.Date(NA)),
      snippet = as.character(if ("snippet" %in% names(d)) snippet else NA_character_),
      url = paste0("https://www.courtlistener.com", as.character(absolute_url))
    )
}

# == Opinion detail ============================================================

#' Fetch a CourtListener opinion cluster by ID
#'
#' @param id Opinion cluster ID (integer)
#' @return tibble: one row with id, caseName, court, dateFiled, snippet, url, plain_text
cl_opinion <- function(id) {
  url <- paste0(.cl_base, "/clusters/", id, "/")
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$id)) return(.schema_opinion)

  tibble(
    id = as.integer(raw$id),
    caseName = as.character(raw$case_name %||% NA_character_),
    court = as.character(raw$court %||% NA_character_),
    dateFiled = tryCatch(as.Date(raw$date_filed), error = function(e) as.Date(NA)),
    snippet = as.character(raw$syllabus %||% NA_character_),
    url = paste0("https://www.courtlistener.com", as.character(raw$absolute_url %||% "")),
    plain_text = NA_character_
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the CourtListener package
#'
#' @return Character string (invisibly), also printed
cl_context <- function() {
  .build_context("courtlistener.com", header_lines = c(
    "# courtlistener.com - CourtListener Legal API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required for basic search",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Search types: o=opinions, r=RECAP, d=dockets, p=people",
    "# CourtListener provides free access to US court opinions."
  ))
}
