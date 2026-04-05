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
#' Searches the Metropolitan Museum of Art collection via the public
#' Collection API. First retrieves matching object IDs, then fetches
#' individual object details for up to \code{limit} results.
#'
#' @param query Character. Search term or phrase
#'   (e.g. \code{"sunflower"}, \code{"impressionist landscape"},
#'   \code{"Egyptian sculpture"}).
#' @param limit Integer. Maximum number of objects to fetch full
#'   details for (default \code{20}). The API may return thousands
#'   of matching IDs; only the first \code{limit} are resolved.
#' @return A tibble with columns:
#'   \describe{
#'     \item{object_id}{Integer. Met Museum object identifier.}
#'     \item{title}{Character. Artwork title.}
#'     \item{artist}{Character. Artist display name, or \code{NA}.}
#'     \item{department}{Character. Curatorial department
#'       (e.g. \code{"European Paintings"},
#'       \code{"Modern and Contemporary Art"}).}
#'     \item{medium}{Character. Materials and technique
#'       (e.g. \code{"Oil on canvas"}).}
#'   }
#' @examples
#' met_search("sunflower", limit = 5)
#' met_search("Egyptian sculpture", limit = 10)
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
#' Fetches full metadata for a single artwork by its object ID from
#' the Metropolitan Museum of Art Collection API.
#'
#' @param object_id Integer or numeric. The Met Museum object
#'   identifier (e.g. \code{436524} for Van Gogh's Sunflowers,
#'   \code{437112} for Monet's Bouquet of Sunflowers). Object IDs
#'   can be found via \code{\link{met_search}}.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{object_id}{Integer. Met object identifier.}
#'     \item{title}{Character. Artwork title.}
#'     \item{artist}{Character. Artist display name.}
#'     \item{department}{Character. Curatorial department.}
#'     \item{medium}{Character. Materials and technique.}
#'     \item{date}{Character. Date or date range of creation
#'       (e.g. \code{"1887"}, \code{"ca. 1500"}).}
#'     \item{culture}{Character. Culture or origin, or empty string.}
#'     \item{period}{Character. Art-historical period, or empty string.}
#'     \item{image_url}{Character. URL to the primary image, or
#'       \code{NA} if not available.}
#'   }
#' @examples
#' met_object(436524)
#' met_object(437112)
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

#' Get metmuseum.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
met_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(met_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/metmuseum.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "metmuseum.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# metmuseum.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# metmuseum.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
