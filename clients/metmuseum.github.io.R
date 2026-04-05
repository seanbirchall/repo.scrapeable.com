# metmuseum.github.io.R - Self-contained metmuseum.github.io client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://collectionapi.metmuseum.org/public/collection/v1"

#' Search Met Museum artworks
#'
#' Searches the Metropolitan Museum of Art's Open Access collection API.
#' First retrieves matching object IDs, then fetches detail records for
#' each (up to \code{limit}). Note: each detail fetch is a separate API
#' call, so larger limits are slower.
#'
#' @param query Search term string (e.g. "sunflower", "impressionism",
#'   "Egyptian cat").
#' @param limit Maximum number of results to fetch full details for
#'   (default 20, integer). The search may match thousands of objects but
#'   only the first \code{limit} are returned with metadata.
#' @return A tibble with columns:
#'   \describe{
#'     \item{object_id}{Met Museum object ID (integer)}
#'     \item{title}{Artwork title}
#'     \item{artist}{Artist display name}
#'     \item{department}{Met Museum department (e.g. "Asian Art", "Paintings")}
#'     \item{medium}{Medium/materials description}
#'   }
#' @examples
#' met_search("sunflower", limit = 5)
#' @seealso [met_object()], [met_context()]
#' @source <https://metmuseum.github.io>
met_search <- function(query, limit = 20L) {
  url <- sprintf("%s/search?q=%s", .base, URLencode(query, TRUE))
  raw <- .fetch_json(url)
  ids <- unlist(raw$objectIDs)
  if (length(ids) == 0) return(tibble::tibble(object_id = integer(), title = character()))
  ids <- ids[seq_len(min(limit, length(ids)))]
  rows <- lapply(ids, function(id) {
    tryCatch({
      obj <- .fetch_json(sprintf("%s/objects/%d", .base, id))
      tibble::tibble(object_id = obj$objectID %||% NA_integer_,
                     title = obj$title %||% NA_character_,
                     artist = obj$artistDisplayName %||% NA_character_,
                     department = obj$department %||% NA_character_,
                     medium = obj$medium %||% NA_character_)
    }, error = function(e) NULL)
  })
  dplyr::bind_rows(rows[!vapply(rows, is.null, logical(1))])
}

#' Get a specific Met Museum object
#'
#' Fetches complete metadata for a single artwork from the Metropolitan
#' Museum of Art's Open Access API. Returns title, artist, department,
#' medium, date, culture, period, and a link to the primary image.
#'
#' @param object_id Met Museum object ID (integer). Object IDs are returned
#'   by \code{\link{met_search}} or can be found on the Met's website URLs.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{object_id}{Object ID (integer)}
#'     \item{title}{Artwork title}
#'     \item{artist}{Artist display name}
#'     \item{department}{Met Museum department}
#'     \item{medium}{Medium/materials}
#'     \item{date}{Date or date range string (e.g. "ca. 1889")}
#'     \item{culture}{Culture or origin (e.g. "Japan", "French")}
#'     \item{period}{Historical period (e.g. "Edo period")}
#'     \item{image_url}{URL to the primary image (may be empty for restricted works)}
#'   }
#' @examples
#' met_object(45734)
#' @seealso [met_search()], [met_context()]
#' @source <https://metmuseum.github.io>
met_object <- function(object_id) {
  url <- sprintf("%s/objects/%d", .base, as.integer(object_id))
  raw <- .fetch_json(url)
  tibble::tibble(
    object_id = raw$objectID %||% NA_integer_,
    title = raw$title %||% NA_character_,
    artist = raw$artistDisplayName %||% NA_character_,
    department = raw$department %||% NA_character_,
    medium = raw$medium %||% NA_character_,
    date = raw$objectDate %||% NA_character_,
    culture = raw$culture %||% NA_character_,
    period = raw$period %||% NA_character_,
    image_url = raw$primaryImage %||% NA_character_
  )
}

#' Get metmuseum.github.io client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the Met Museum client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' met_context()
#' @export
met_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(met_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/metmuseum.github.io.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "metmuseum.github.io")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# metmuseum.github.io context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# metmuseum.github.io", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
