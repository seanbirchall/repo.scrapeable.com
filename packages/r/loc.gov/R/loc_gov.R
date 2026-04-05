# loc.gov.R
# Self-contained Library of Congress client.
# Covers: Chronicling America (historic newspapers) and LOC collections/search.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required



# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.ua <- "support@scrapeable.com"
.chron_base <- "https://www.loc.gov"
.loc_base <- "https://www.loc.gov"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))
.fetch_json_nested <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)


# == Schemas ===================================================================

.schema_pages <- tibble(
  title = character(), date = as.Date(character()),
  newspaper = character(), id = character(),
  image_url = character(), url = character()
)

.schema_newspapers <- tibble(
  lccn = character(), title = character(), place = character(),
  start_year = character(), end_year = character(), url = character()
)




#' Search historic newspaper pages on Chronicling America
#'
#' Full-text search across digitized newspaper pages from the Library of
#' Congress Chronicling America collection via the loc.gov JSON API.
#' Results are paginated at roughly 25-40 items per page.
#'
#' @param text Character. Search text (e.g. \code{"baseball"},
#'   \code{"suffrage"}, \code{"earthquake"}).
#' @param page Integer. Page number of results (default \code{1}).
#'   Each page returns up to 40 results.
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Headline or page title.}
#'     \item{date}{Date. Publication date of the newspaper page.}
#'     \item{newspaper}{Character. Name(s) of the source newspaper,
#'       semicolon-separated when multiple.}
#'     \item{id}{Character. LOC resource identifier / URL path.}
#'     \item{image_url}{Character. URL(s) to page scan images,
#'       semicolon-separated.}
#'     \item{url}{Character. Permalink to the resource on loc.gov.}
#'   }
#' @examples
#' chron_search("prohibition")
#' chron_search("gold rush", page = 2)
chron_search <- function(text, page = 1) {
  url <- sprintf(
    "%s/collections/chronicling-america/?q=%s&sp=%d&fo=json",
    .chron_base, utils::URLencode(text, reserved = TRUE), as.integer(page)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_pages)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_pages)

  rows <- lapply(results, function(r) {
    tibble(
      title = as.character(r$title %||% NA_character_),
      date = tryCatch(as.Date(r$date %||% NA_character_), error = function(e) as.Date(NA)),
      newspaper = as.character(if (!is.null(r$partof)) paste(sapply(r$partof, function(x) if(is.list(x)) x$title %||% x else x), collapse="; ") else NA_character_),
      id = as.character(r$id %||% NA_character_),
      image_url = as.character(if (!is.null(r$image_url)) paste(r$image_url, collapse="; ") else NA_character_),
      url = as.character(r$url %||% r$id %||% NA_character_)
    )
  })
  bind_rows(rows)
}


#' List newspapers in Chronicling America
#'
#' Returns a tibble of digitized newspapers available in the Library of
#' Congress Chronicling America collection, optionally filtered by state.
#' Up to 100 results are returned per call.
#'
#' @param state Character. Optional state name or abbreviation to filter
#'   results (e.g. \code{"New York"}, \code{"California"}). \code{NULL}
#'   (default) returns newspapers from all states.
#' @return A tibble with columns:
#'   \describe{
#'     \item{lccn}{Character. Library of Congress Control Number.}
#'     \item{title}{Character. Newspaper title.}
#'     \item{place}{Character. Publication location(s),
#'       semicolon-separated.}
#'     \item{start_year}{Character. Earliest year of coverage.}
#'     \item{end_year}{Character. Latest year of coverage.}
#'     \item{url}{Character. Permalink to the newspaper on loc.gov.}
#'   }
#' @examples
#' chron_newspapers()
#' chron_newspapers(state = "California")
chron_newspapers <- function(state = NULL) {
  url <- sprintf("%s/collections/chronicling-america/?fo=json&fa=original-format:newspaper", .chron_base)
  if (!is.null(state)) url <- paste0(url, "&fa=location:", utils::URLencode(state))
  url <- paste0(url, "&c=100")

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_newspapers)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_newspapers)

  rows <- lapply(results, function(r) {
    # Extract LCCN from aka list
    lccn <- NA_character_
    if (!is.null(r$aka)) {
      lccn_match <- grep("lccn.loc.gov", unlist(r$aka), value = TRUE)
      if (length(lccn_match) > 0) lccn <- sub(".*/", "", lccn_match[1])
    }

    dates <- r$dates %||% list()
    tibble(
      lccn = as.character(lccn),
      title = as.character(r$title %||% NA_character_),
      place = as.character(if (!is.null(r$location)) paste(unlist(r$location), collapse="; ") else NA_character_),
      start_year = as.character(if (length(dates) > 0) dates[[1]] else NA_character_),
      end_year = as.character(if (length(dates) > 1) dates[[length(dates)]] else NA_character_),
      url = as.character(r$id %||% r$url %||% NA_character_)
    )
  })
  bind_rows(rows)
}


#' Search Chronicling America by date range and state
#'
#' An advanced search that adds state and date-range filters on top of
#' the full-text search provided by \code{\link{chron_search}}.
#'
#' @param text Character. Full-text search query
#'   (e.g. \code{"civil war"}, \code{"influenza"}).
#' @param state Character. State name to restrict results
#'   (e.g. \code{"New York"}, \code{"Virginia"}). \code{NULL} (default)
#'   searches all states.
#' @param date_from Character. Start date in \code{"YYYY-MM-DD"} format
#'   (e.g. \code{"1900-01-01"}). Used together with \code{date_to}.
#' @param date_to Character. End date in \code{"YYYY-MM-DD"} format
#'   (e.g. \code{"1920-12-31"}). Used together with \code{date_from}.
#' @param page Integer. Page number (default \code{1}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Headline or page title.}
#'     \item{date}{Date. Publication date.}
#'     \item{newspaper}{Character. Source newspaper name(s).}
#'     \item{id}{Character. LOC resource identifier.}
#'     \item{image_url}{Character. Page scan image URL(s).}
#'     \item{url}{Character. Permalink on loc.gov.}
#'   }
#' @examples
#' chron_search_advanced("suffrage", state = "New York",
#'                       date_from = "1910-01-01", date_to = "1920-12-31")
chron_search_advanced <- function(text, state = NULL, date_from = NULL,
                                  date_to = NULL, page = 1) {
  url <- sprintf(
    "%s/collections/chronicling-america/?q=%s&sp=%d&fo=json",
    .chron_base, utils::URLencode(text, reserved = TRUE), as.integer(page)
  )
  if (!is.null(state)) url <- paste0(url, "&fa=state:", utils::URLencode(state))
  if (!is.null(date_from) && !is.null(date_to)) {
    url <- paste0(url, "&dates=", date_from, "/", date_to)
  }

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_pages)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_pages)

  rows <- lapply(results, function(r) {
    tibble(
      title = as.character(r$title %||% NA_character_),
      date = tryCatch(as.Date(r$date %||% NA_character_), error = function(e) as.Date(NA)),
      newspaper = as.character(if (!is.null(r$partof)) paste(sapply(r$partof, function(x) if(is.list(x)) x$title %||% x else x), collapse="; ") else NA_character_),
      id = as.character(r$id %||% NA_character_),
      image_url = as.character(if (!is.null(r$image_url)) paste(r$image_url, collapse="; ") else NA_character_),
      url = as.character(r$url %||% r$id %||% NA_character_)
    )
  })
  bind_rows(rows)
}


#' Search the Library of Congress
#'
#' Full-text search across all LOC digital collections (or a specific
#' collection) via the loc.gov JSON API. Returns up to 25 results
#' per call.
#'
#' @param query Character. Search term (e.g. \code{"jazz"},
#'   \code{"Abraham Lincoln"}, \code{"map of virginia"}).
#' @param collection Character. Optional collection slug to restrict
#'   results, e.g. \code{"newspapers"}, \code{"photos"},
#'   \code{"maps"}, \code{"manuscripts"}. \code{NULL} (default)
#'   searches across all collections.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. LOC resource identifier / URL.}
#'     \item{title}{Character. Item title.}
#'     \item{date}{Character. Date string (format varies by item).}
#'     \item{type}{Character. Original format(s), comma-separated
#'       (e.g. \code{"photo, print, drawing"}).}
#'   }
#' @examples
#' loc_search("jazz")
#' loc_search("civil war", collection = "photos")
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
#' Returns a catalog of all digital collections available on loc.gov,
#' including collection names, descriptions, and item counts. Use the
#' collection slug from the \code{id} column with \code{\link{loc_search}}
#' to search within a specific collection.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Collection identifier / URL path.}
#'     \item{title}{Character. Collection name
#'       (e.g. \code{"Baseball Cards"}).}
#'     \item{description}{Character. Brief collection description.}
#'     \item{count}{Integer. Number of items in the collection.}
#'   }
#' @examples
#' loc_collections()
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

#' Get loc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
loc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(loc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/loc.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "loc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# loc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# loc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
