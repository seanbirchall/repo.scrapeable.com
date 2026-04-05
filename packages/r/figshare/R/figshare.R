# figshare.R
# Self-contained Figshare API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public data
# Rate limits: not documented, be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.figshare_base <- "https://api.figshare.com/v2"

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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

.post_json <- function(url, body) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}

# == Schemas ===================================================================

.schema_articles <- tibble(
  id = integer(), title = character(), doi = character(),
  url = character(), published_date = as.Date(character()),
  defined_type_name = character()
)

.schema_article_detail <- tibble(
  id = integer(), title = character(), doi = character(),
  url = character(), published_date = as.Date(character()),
  description = character(), defined_type_name = character(),
  license = character(), authors = character(), tags = character(),
  citation = character()
)

# == Article search ============================================================

#' Search Figshare articles
#'
#' Search the Figshare repository for publicly available articles, datasets,
#' figures, and other research outputs. Use \code{figshare_article()} to get
#' full details for a specific result.
#'
#' @param query Search query string (e.g. \code{"climate"}, \code{"genomics"},
#'   \code{"machine learning"}).
#' @param page_size Number of results per page (default 10, max 1000).
#' @param page Page number for pagination (default 1).
#' @return A tibble with one row per article:
#'   \describe{
#'     \item{id}{\code{integer} -- Figshare article ID (pass to \code{figshare_article()}).}
#'     \item{title}{\code{character} -- Article title.}
#'     \item{doi}{\code{character} -- Digital Object Identifier.}
#'     \item{url}{\code{character} -- Figshare URL.}
#'     \item{published_date}{\code{Date} -- Publication date.}
#'     \item{defined_type_name}{\code{character} -- Item type (e.g. \code{"dataset"}, \code{"figure"}, \code{"journal contribution"}).}
#'   }
#' @examples
#' \dontrun{
#' figshare_search("climate", page_size = 20)
#' figshare_search("genomics", page = 2)
#' }
#' @export
figshare_search <- function(query, page_size = 10, page = 1) {
  body <- list(search_for = query, page_size = page_size, page = page)
  raw <- .post_json(paste0(.figshare_base, "/articles/search"), body)
  if (is.null(raw) || length(raw) == 0) return(.schema_articles)

  as_tibble(raw) |>
    transmute(
      id = as.integer(id),
      title = as.character(title),
      doi = as.character(doi),
      url = as.character(url),
      published_date = tryCatch(as.Date(published_date), error = function(e) as.Date(NA)),
      defined_type_name = as.character(if ("defined_type_name" %in% names(raw)) defined_type_name else NA_character_)
    )
}


# == Article detail ============================================================

#' Fetch a single Figshare article by ID
#'
#' Retrieve full metadata for one Figshare article including description,
#' authors, tags, license, and citation string. Complements the lighter
#' \code{figshare_search()} results.
#'
#' @param id Figshare article ID (integer). Obtain from the \code{id} column
#'   of \code{figshare_search()} results.
#' @return A single-row tibble:
#'   \describe{
#'     \item{id}{\code{integer} -- Figshare article ID.}
#'     \item{title}{\code{character} -- Article title.}
#'     \item{doi}{\code{character} -- Digital Object Identifier.}
#'     \item{url}{\code{character} -- Figshare URL.}
#'     \item{published_date}{\code{Date} -- Publication date.}
#'     \item{description}{\code{character} -- Full description / abstract.}
#'     \item{defined_type_name}{\code{character} -- Item type.}
#'     \item{license}{\code{character} -- License name (e.g. \code{"CC BY 4.0"}).}
#'     \item{authors}{\code{character} -- Author names, semicolon-separated.}
#'     \item{tags}{\code{character} -- Tags, semicolon-separated.}
#'     \item{citation}{\code{character} -- Formatted citation string.}
#'   }
#' @examples
#' \dontrun{
#' results <- figshare_search("climate")
#' figshare_article(results$id[1])
#' }
#' @export
figshare_article <- function(id) {
  url <- paste0(.figshare_base, "/articles/", id)
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$id)) return(.schema_article_detail)

  auth_str <- if (!is.null(raw$authors) && is.data.frame(raw$authors)) {
    paste(raw$authors$full_name, collapse = "; ")
  } else NA_character_

  tag_str <- if (!is.null(raw$tags)) {
    paste(raw$tags, collapse = "; ")
  } else NA_character_

  lic_str <- if (!is.null(raw$license) && is.list(raw$license)) {
    as.character(raw$license$name %||% NA_character_)
  } else NA_character_

  tibble(
    id = as.integer(raw$id),
    title = as.character(raw$title %||% NA_character_),
    doi = as.character(raw$doi %||% NA_character_),
    url = as.character(raw$url %||% NA_character_),
    published_date = tryCatch(as.Date(raw$published_date), error = function(e) as.Date(NA)),
    description = as.character(raw$description %||% NA_character_),
    defined_type_name = as.character(raw$defined_type_name %||% NA_character_),
    license = lic_str,
    authors = auth_str,
    tags = tag_str,
    citation = as.character(raw$citation %||% NA_character_)
  )
}


# == Context (LLM injection) ==================================================

#' Get Figshare client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' figshare_context()
#' }
#' @export
figshare_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(figshare_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/figshare.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "figshare")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# figshare context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# figshare", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
