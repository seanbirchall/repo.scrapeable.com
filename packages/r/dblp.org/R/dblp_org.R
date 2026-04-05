# dblp.org.R - Self-contained dblp.org client



# dblp-org.R
# Self-contained DBLP computer science bibliography API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: be polite, no official limit


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dblp_base <- "https://dblp.org"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)


# == Schemas ===================================================================

.schema_publications <- tibble(
  key = character(), title = character(), venue = character(),
  year = integer(), type = character(), doi = character(),
  url = character(), authors = character()
)

.schema_authors <- tibble(
  name = character(), pid = character(), url = character(), notes = character()
)

# == Public functions ==========================================================

#' Search DBLP publications
#'
#' Searches the DBLP computer science bibliography for publications matching
#' a query. DBLP indexes over 7 million publications from journals,
#' conferences, and workshops across computer science.
#'
#' @param query Character. Search query string (e.g. "machine learning",
#'   "graph neural networks", "Hinton dropout").
#' @param hits Integer. Maximum results to return (default 30, max 1000).
#' @param offset Integer. Starting position for pagination (default 0).
#' @return A tibble with columns:
#' \describe{
#'   \item{key}{Character. DBLP publication key (e.g. "conf/ukci/NaikN23a", "phd/hal/Simonian24").}
#'   \item{title}{Character. Publication title.}
#'   \item{venue}{Character. Conference acronym or journal name (e.g. "UKCI", "AMCIS"). May be NA.}
#'   \item{year}{Integer. Publication year.}
#'   \item{type}{Character. Publication type: "Conference and Workshop Papers", "Books and Theses", "Journal Articles", "Informal and Other Publications".}
#'   \item{doi}{Character. DOI identifier (e.g. "10.1109/..."). May be NA.}
#'   \item{url}{Character. DBLP URL for the publication record.}
#'   \item{authors}{Character. Semicolon-separated author names (e.g. "Dishit Naik; Nitin Naik").}
#' }
#' @export
#' @examples
#' \dontrun{
#' dblp_publications("machine learning", hits = 10)
#' dblp_publications("transformer attention", hits = 20, offset = 0)
#' }
dblp_publications <- function(query, hits = 30, offset = 0) {
  url <- sprintf("%s/search/publ/api?q=%s&format=json&h=%d&f=%d",
                 .dblp_base, utils::URLencode(query), hits, offset)
  raw <- .fetch_json(url)
  hit_list <- raw$result$hits$hit
  if (is.null(hit_list) || length(hit_list) == 0) return(.schema_publications)

  rows <- lapply(hit_list, function(h) {
    info <- h$info
    authors_raw <- info$authors$author
    if (is.null(authors_raw)) {
      auth_str <- NA_character_
    } else if (is.list(authors_raw) && !is.null(authors_raw$text)) {
      auth_str <- authors_raw$text
    } else {
      auth_str <- paste(vapply(authors_raw, function(a) a$text %||% NA_character_, character(1)), collapse = "; ")
    }
    tibble(
      key = as.character(info$key %||% NA),
      title = as.character(info$title %||% NA),
      venue = as.character(info$venue %||% NA),
      year = as.integer(info$year %||% NA),
      type = as.character(info$type %||% NA),
      doi = as.character(info$doi %||% NA),
      url = as.character(info$url %||% NA),
      authors = auth_str
    )
  })
  bind_rows(rows)
}

#' Search DBLP authors
#'
#' Searches the DBLP author index. Returns matching author profiles with
#' their DBLP person identifiers (PIDs).
#'
#' @param query Character. Author name search query (e.g. "Hinton",
#'   "Yann LeCun", "Turing").
#' @param hits Integer. Maximum results to return (default 30).
#' @return A tibble with columns:
#' \describe{
#'   \item{name}{Character. Author name as indexed in DBLP (e.g. "Geoffrey E. Hinton", "Alan Hinton").}
#'   \item{pid}{Character. DBLP person URL (e.g. "https://dblp.org/pid/152/3980").}
#'   \item{url}{Character. DBLP URL for the author record.}
#'   \item{notes}{Character. Additional notes or affiliations. Often NA.}
#' }
#' @export
#' @examples
#' \dontrun{
#' dblp_authors("Hinton", hits = 5)
#' dblp_authors("Yann LeCun")
#' }
dblp_authors <- function(query, hits = 30) {
  url <- sprintf("%s/search/author/api?q=%s&format=json&h=%d",
                 .dblp_base, utils::URLencode(query), hits)
  raw <- .fetch_json(url)
  hit_list <- raw$result$hits$hit
  if (is.null(hit_list) || length(hit_list) == 0) return(.schema_authors)

  rows <- lapply(hit_list, function(h) {
    info <- h$info
    tibble(
      name = as.character(info$author %||% NA),
      pid = as.character(info$url %||% NA),
      url = as.character(h$url %||% NA),
      notes = as.character(info$notes$note$text %||% NA)
    )
  })
  bind_rows(rows)
}

#' Search DBLP venues (conferences/journals)
#'
#' Searches the DBLP venue index for conferences, journals, and workshop
#' series matching a query.
#'
#' @param query Character. Venue name search query (e.g. "neural",
#'   "machine learning", "SIGMOD").
#' @param hits Integer. Maximum results to return (default 30).
#' @return A tibble with columns:
#' \describe{
#'   \item{name}{Character. Full venue name (e.g. "Artificial Neural Networks (ANN)", "Advances in Artificial Neural Systems").}
#'   \item{acronym}{Character. Venue abbreviation (e.g. "ANN", "ANNES"). May be NA.}
#'   \item{type}{Character. Venue type: "Journal", "Conference or Workshop".}
#'   \item{url}{Character. DBLP URL for the venue page.}
#' }
#' @export
#' @examples
#' \dontrun{
#' dblp_venues("neural", hits = 5)
#' dblp_venues("database")
#' }
dblp_venues <- function(query, hits = 30) {
  url <- sprintf("%s/search/venue/api?q=%s&format=json&h=%d",
                 .dblp_base, utils::URLencode(query), hits)
  raw <- .fetch_json(url)
  hit_list <- raw$result$hits$hit
  if (is.null(hit_list) || length(hit_list) == 0) {
    return(tibble(name = character(), acronym = character(),
                  type = character(), url = character()))
  }
  rows <- lapply(hit_list, function(h) {
    info <- h$info
    tibble(
      name = as.character(info$venue %||% NA),
      acronym = as.character(info$acronym %||% NA),
      type = as.character(info$type %||% NA),
      url = as.character(info$url %||% NA)
    )
  })
  bind_rows(rows)
}
`%||%` <- function(x, y) if (is.null(x)) y else x

# == Context ===================================================================

#' Get dblp.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
dblp_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(dblp_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/dblp.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "dblp.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# dblp.org context - source not found\n"); return(invisible("")) }

  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_idx <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_idx) {
    fn <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn, ".")) next
    j <- fi - 1; rs <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rs <- j; j <- j - 1 }
    rox <- if (rs < fi) lines[rs:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("[[:space:]]*[{][[:space:]]*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, paste0("  Run `", fn, "` to view source or `?", fn, "` for help."), "")
  }
  out <- paste(c("# dblp.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
