# courtlistener.com.R - Self-contained courtlistener.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# courtlistener.R
# Self-contained CourtListener API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for basic search
# Rate limits: be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cl_base <- "https://www.courtlistener.com/api/rest/v4"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_search <- tibble(
  cluster_id = integer(), caseName = character(), court = character(),
  dateFiled = as.Date(character()), snippet = character(),
  url = character()
)

.schema_opinion <- tibble(
  cluster_id = integer(), caseName = character(), court = character(),
  dateFiled = as.Date(character()), snippet = character(),
  url = character(), plain_text = character()
)

# == Search ====================================================================

#' Search CourtListener legal opinions and dockets
#'
#' Performs a full-text search across U.S. federal and state court records
#' using the CourtListener REST API v4. Supports searching opinions,
#' RECAP dockets, dockets, and judicial personnel. No authentication is
#' required for basic search.
#'
#' @param query Character. Search query string (e.g. \code{"privacy"},
#'   \code{"first amendment"}, \code{"patent infringement"}).
#' @param type Character. Result type code: \code{"o"} = opinions (default),
#'   \code{"r"} = RECAP archive, \code{"d"} = dockets, \code{"p"} = people
#'   (judges and attorneys).
#' @param page_size Integer. Number of results per page (default 10).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with one row per result and columns:
#' \describe{
#'   \item{cluster_id}{Integer. Opinion cluster identifier.}
#'   \item{caseName}{Character. Name of the case.}
#'   \item{court}{Character. Court identifier slug, or \code{NA}.}
#'   \item{dateFiled}{Date. Date the opinion was filed, or \code{NA}.}
#'   \item{snippet}{Character. Syllabus or text snippet, or \code{NA}.}
#'   \item{url}{Character. Full URL to the opinion on CourtListener.}
#' }
#' @examples
#' \dontrun{
#' cl_search("privacy")
#' cl_search("first amendment", type = "o", page_size = 20)
#' }
#' @export
cl_search <- function(query, type = "o", page_size = 10, page = 1) {
  url <- paste0(.cl_base, "/search/?q=", utils::URLencode(query),
                "&type=", type, "&page_size=", page_size, "&page=", page)
  raw <- .fetch_json(url)
  d <- raw$results
  if (is.null(d) || length(d) == 0) return(.schema_search)

  as_tibble(d) |>
    transmute(
      cluster_id = as.integer(cluster_id),
      caseName = as.character(caseName),
      court = as.character(if ("court" %in% names(d)) court else NA_character_),
      dateFiled = tryCatch(as.Date(dateFiled), error = function(e) as.Date(NA)),
      snippet = as.character(if ("syllabus" %in% names(d)) syllabus else NA_character_),
      url = paste0("https://www.courtlistener.com", as.character(absolute_url))
    )
}

# == Opinion detail ============================================================

#' Fetch a CourtListener opinion cluster by ID
#'
#' Retrieves metadata for a single opinion cluster from the CourtListener
#' REST API v4. The \code{plain_text} column is currently \code{NA} because
#' fetching full opinion text requires authentication.
#'
#' @param id Integer. Opinion cluster ID (visible in search results or
#'   CourtListener URLs).
#' @return A one-row tibble with columns:
#' \describe{
#'   \item{cluster_id}{Integer. Opinion cluster identifier.}
#'   \item{caseName}{Character. Case name, or \code{NA}.}
#'   \item{court}{Character. Court identifier slug, or \code{NA}.}
#'   \item{dateFiled}{Date. Filing date, or \code{NA}.}
#'   \item{snippet}{Character. Syllabus text, or \code{NA}.}
#'   \item{url}{Character. Full CourtListener URL.}
#'   \item{plain_text}{Character. Always \code{NA} (auth required for full text).}
#' }
#' @examples
#' \dontrun{
#' cl_opinion(2245941)
#' }
#' @export
cl_opinion <- function(id) {
  url <- paste0(.cl_base, "/clusters/", id, "/")
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$id)) return(.schema_opinion)

  tibble(
    cluster_id = as.integer(raw$id),
    caseName = as.character(raw$case_name %||% NA_character_),
    court = as.character(raw$court %||% NA_character_),
    dateFiled = tryCatch(as.Date(raw$date_filed), error = function(e) as.Date(NA)),
    snippet = as.character(raw$syllabus %||% NA_character_),
    url = paste0("https://www.courtlistener.com", as.character(raw$absolute_url %||% "")),
    plain_text = NA_character_
  )
}

# == Courts ====================================================================

#' List courts available on CourtListener
#'
#' Retrieves a paginated list of courts tracked by CourtListener, including
#' federal courts (SCOTUS, circuit, district), state courts, military courts,
#' and administrative bodies. Useful as a lookup for filtering search results.
#'
#' @param page_size Integer. Number of courts per page (default 20).
#' @return A tibble with one row per court and columns:
#' \describe{
#'   \item{id}{Character. Court identifier slug (e.g. \code{"scotus"}, \code{"ca9"}).}
#'   \item{full_name}{Character. Full court name.}
#'   \item{short_name}{Character. Abbreviated court name.}
#'   \item{jurisdiction}{Character. Jurisdiction code (e.g. \code{"F"} = federal,
#'     \code{"FS"} = federal special, \code{"MA"} = military appellate).}
#'   \item{url}{Character. API URL for the court resource.}
#' }
#' @examples
#' \dontrun{
#' cl_courts()
#' cl_courts(page_size = 100)
#' }
#' @export
cl_courts <- function(page_size = 20) {
  schema <- tibble(id = character(), full_name = character(),
                   short_name = character(), jurisdiction = character(),
                   url = character())
  url <- sprintf("%s/courts/?page_size=%d", .cl_base, as.integer(page_size))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$results)) return(schema)
  d <- raw$results
  if (length(d) == 0 || nrow(d) == 0) return(schema)

  as_tibble(d) |>
    transmute(
      id = as.character(if ("id" %in% names(d)) id else NA_character_),
      full_name = as.character(if ("full_name" %in% names(d)) full_name else NA_character_),
      short_name = as.character(if ("short_name" %in% names(d)) short_name else NA_character_),
      jurisdiction = as.character(if ("jurisdiction" %in% names(d)) jurisdiction else NA_character_),
      url = as.character(if ("url" %in% names(d)) url else NA_character_)
    )
}

# == Context ===================================================================

#' Get courtlistener.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/courtlistener.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "courtlistener.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# courtlistener.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# courtlistener.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
