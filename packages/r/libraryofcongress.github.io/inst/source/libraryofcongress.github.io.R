# libraryofcongress.github.io.R - Self-contained libraryofcongress.github.io client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

`%||%` <- function(a, b) if (is.null(a)) b else a

.ua <- "support@scrapeable.com"
.loc_base <- "https://www.loc.gov"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

#' Search Library of Congress digital collections
#'
#' Searches across Library of Congress digital collections using the loc.gov
#' JSON API. Returns item metadata including titles, dates, and format types.
#' Can optionally restrict search to a specific collection.
#'
#' @param query Search term string (e.g. "civil war", "baseball", "jazz").
#' @param collection Optional collection name to restrict search. Common values:
#'   "newspapers", "photos", "maps", "manuscripts", "films", "audio",
#'   "books", "legislation". When NULL (default), searches all collections.
#' @return A tibble with up to 25 results and columns:
#'   \describe{
#'     \item{id}{LOC item URL/identifier}
#'     \item{title}{Item title}
#'     \item{date}{Date string associated with the item}
#'     \item{type}{Original format type (e.g. "photo, print, drawing")}
#'   }
#' @examples
#' loc_search("baseball")
#' loc_search("jazz", collection = "audio")
#' @seealso [loc_collections()], [libraryofcongress_context()]
#' @source <https://www.loc.gov/apis/>
loc_search <- function(query, collection = NULL) {
  if (!is.null(collection)) {
    url <- sprintf("%s/%s/?q=%s&fo=json&c=25", .loc_base, collection, URLencode(query, TRUE))
  } else {
    url <- sprintf("%s/search/?q=%s&fo=json&c=25", .loc_base, URLencode(query, TRUE))
  }
  raw <- .fetch_json(url)
  results <- raw$results
  if (length(results) == 0) return(tibble::tibble(id = character(), title = character()))
  tibble::tibble(
    id = vapply(results, function(x) x$id %||% NA_character_, character(1)),
    title = vapply(results, function(x) {
      if (is.list(x$title) || length(x$title) > 1) x$title[[1]] else x$title %||% NA_character_
    }, character(1)),
    date = vapply(results, function(x) x$date %||% NA_character_, character(1)),
    type = vapply(results, function(x) {
      if (is.list(x$original_format)) paste(unlist(x$original_format), collapse = ", ") else x$original_format %||% NA_character_
    }, character(1))
  )
}

#' List Library of Congress digital collections
#'
#' Returns a catalog of available digital collections from the Library of
#' Congress, including collection titles, descriptions, and item counts.
#' Use collection IDs or names with \code{\link{loc_search}} to filter
#' search results.
#'
#' @return A tibble with up to 50 rows and columns:
#'   \describe{
#'     \item{id}{Collection URL/identifier}
#'     \item{title}{Collection title}
#'     \item{description}{Brief description of the collection}
#'     \item{count}{Number of items in the collection (integer)}
#'   }
#' @examples
#' loc_collections()
#' @seealso [loc_search()], [libraryofcongress_context()]
#' @source <https://www.loc.gov/apis/>
loc_collections <- function() {
  schema <- tibble(id = character(), title = character(),
                   description = character(), count = integer())
  url <- sprintf("%s/collections/?fo=json&c=50", .loc_base)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$results) || length(raw$results) == 0) return(schema)
  results <- raw$results
  tibble(
    id = vapply(results, function(x) as.character(x$id %||% NA), character(1)),
    title = vapply(results, function(x) {
      t <- x$title
      if (is.list(t) || length(t) > 1) as.character(t[[1]]) else as.character(t %||% NA)
    }, character(1)),
    description = vapply(results, function(x) {
      d <- x$description
      if (is.list(d) || length(d) > 1) as.character(d[[1]]) else as.character(d %||% NA)
    }, character(1)),
    count = vapply(results, function(x) as.integer(x$count %||% NA), integer(1))
  )
}

# == Context ===================================================================

#' Get libraryofcongress.github.io client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the Library of Congress client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' libraryofcongress_context()
#' @export
libraryofcongress_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(libraryofcongress_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/libraryofcongress.github.io.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "libraryofcongress.github.io")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# libraryofcongress.github.io context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# libraryofcongress.github.io", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
