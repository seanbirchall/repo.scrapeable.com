# ntrs.nasa.gov.R - Self-contained ntrs.nasa.gov client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# ntrs-nasa-gov.R
# Self-contained NASA Technical Reports Server (NTRS) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (POST-based search API)
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ntrs_base <- "https://ntrs.nasa.gov/api"

`%||%` <- function(x, y) if (is.null(x)) y else x

.ntrs_post <- function(endpoint, body) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(paste0(.ntrs_base, endpoint)) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
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

.schema_citations <- tibble(
  id = character(), title = character(), sti_type = character(),
  distribution = character(), created = as.Date(character()),
  authors = character()
)

# == Public functions ==========================================================

#' Search the NASA Technical Reports Server (NTRS)
#'
#' Full-text search across NASA technical reports, conference papers,
#' journal reprints, and other STI (Scientific and Technical Information)
#' documents.
#'
#' @details
#' NTRS indexes over 500 000 NASA-authored or NASA-funded documents
#' dating back to the agency's founding. Results are sorted by relevance.
#' The \code{page} parameter is zero-indexed, so \code{page = 0} returns
#' the first \code{page_size} results, \code{page = 1} the next batch, etc.
#'
#' @param query Character. Free-text search query (e.g. \code{"mars rover"},
#'   \code{"aerodynamics turbulence"}).
#' @param page_size Integer. Results per page (default 25, max 100).
#' @param page Integer. Zero-indexed page number (default 0).
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. NTRS document identifier.}
#'   \item{title}{Character. Document title.}
#'   \item{sti_type}{Character. STI type: \code{"TECHNICAL_REPORT"},
#'     \code{"CONFERENCE_PAPER"}, \code{"REPRINT"}, etc.}
#'   \item{distribution}{Character. Distribution level (e.g. \code{"PUBLIC"}).}
#'   \item{created}{Date. Record creation date.}
#'   \item{authors}{Character. Semicolon-separated author names.}
#' }
#' @export
#' @seealso \code{\link{ntrs_citation}}, \code{\link{ntrs_by_center}},
#'   \code{\link{ntrs_recent}}
#' @examples
#' ntrs_search("mars rover", page_size = 5)
#' ntrs_search("climate change", page_size = 10, page = 1)
ntrs_search <- function(query, page_size = 25, page = 0) {
  body <- list(
    query = query,
    page = list(size = page_size, from = page * page_size)
  )
  raw <- .ntrs_post("/citations/search", body)
  df <- raw$results
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(.schema_citations)

  auth_strs <- vapply(seq_len(nrow(df)), function(i) {
    auths <- df$authorAffiliations[[i]]
    if (is.null(auths) || !is.data.frame(auths) || nrow(auths) == 0) return(NA_character_)
    paste(vapply(seq_len(nrow(auths)), function(j) {
      tryCatch(auths$meta$author$name[j] %||% NA_character_,
               error = function(e) NA_character_)
    }, character(1)), collapse = "; ")
  }, character(1))

  tibble(
    id = as.character(df$id),
    title = as.character(df$title),
    sti_type = as.character(df$stiType),
    distribution = as.character(df$distribution),
    created = tryCatch(as.Date(substr(df$created, 1, 10)),
                       error = function(e) rep(as.Date(NA), nrow(df))),
    authors = auth_strs
  )
}

#' Fetch a single NTRS citation by ID
#'
#' Retrieve the full citation record for one NASA Technical Report,
#' including title, type, distribution status, creation date, and
#' author names.
#'
#' @details
#' The citation ID is the long numeric string returned in the \code{id}
#' column of \code{\link{ntrs_search}}. The returned tibble has the same
#' schema as search results, making it easy to \code{bind_rows}.
#'
#' @param id Character or numeric. NTRS citation identifier.
#' @return A single-row tibble with columns: \code{id}, \code{title},
#'   \code{sti_type}, \code{distribution}, \code{created}, \code{authors}.
#' @export
#' @seealso \code{\link{ntrs_search}}
#' @examples
#' # Fetch a specific citation
#' results <- ntrs_search("hubble", page_size = 1)
#' if (nrow(results) > 0) ntrs_citation(results$id[1])
ntrs_citation <- function(id) {
  url <- sprintf("%s/citations/%s", .ntrs_base, as.character(id))
  raw <- .fetch_json(url)
  if (is.null(raw)) return(.schema_citations)

  auths <- raw$authorAffiliations
  auth_str <- if (!is.null(auths) && length(auths) > 0) {
    if (is.data.frame(auths)) {
      paste(vapply(seq_len(nrow(auths)), function(i) {
        tryCatch(auths$meta[[i]]$author$name %||% NA_character_,
                 error = function(e) NA_character_)
      }, character(1)), collapse = "; ")
    } else {
      paste(vapply(auths, function(a) a$meta$author$name %||% NA_character_, character(1)), collapse = "; ")
    }
  } else NA_character_

  tibble(
    id = as.character(raw$id %||% NA),
    title = as.character(raw$title %||% NA),
    sti_type = as.character(raw$stiType %||% NA),
    distribution = as.character(raw$distribution %||% NA),
    created = tryCatch(as.Date(substr(raw$created %||% "", 1, 10)),
                       error = function(e) as.Date(NA)),
    authors = auth_str
  )
}

#' Search NASA Technical Reports by center
#'
#' Filter NTRS documents by the originating NASA center or facility.
#'
#' @details
#' Common NASA center codes include:
#' \itemize{
#'   \item \code{"JPL"} -- Jet Propulsion Laboratory
#'   \item \code{"GSFC"} -- Goddard Space Flight Center
#'   \item \code{"ARC"} -- Ames Research Center
#'   \item \code{"MSFC"} -- Marshall Space Flight Center
#'   \item \code{"JSC"} -- Johnson Space Center
#'   \item \code{"LaRC"} -- Langley Research Center
#'   \item \code{"KSC"} -- Kennedy Space Center
#'   \item \code{"GRC"} -- Glenn Research Center
#' }
#'
#' @param center Character. NASA center code (see Details).
#' @param page_size Integer. Results per page (default 25).
#' @return A tibble with columns: \code{id}, \code{title}, \code{sti_type},
#'   \code{distribution}, \code{created}, \code{authors}.
#' @export
#' @seealso \code{\link{ntrs_search}}, \code{\link{ntrs_recent}}
#' @examples
#' ntrs_by_center("JPL", page_size = 5)
ntrs_by_center <- function(center, page_size = 25) {
  body <- list(
    center = center,
    page = list(size = page_size, from = 0)
  )
  raw <- tryCatch(.ntrs_post("/citations/search", body), error = function(e) NULL)
  if (is.null(raw)) return(.schema_citations)
  df <- raw$results
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(.schema_citations)

  auth_strs <- vapply(seq_len(nrow(df)), function(i) {
    auths <- df$authorAffiliations[[i]]
    if (is.null(auths) || !is.data.frame(auths) || nrow(auths) == 0) return(NA_character_)
    paste(vapply(seq_len(nrow(auths)), function(j) {
      tryCatch(auths$meta$author$name[j] %||% NA_character_,
               error = function(e) NA_character_)
    }, character(1)), collapse = "; ")
  }, character(1))

  tibble(
    id = as.character(df$id),
    title = as.character(df$title),
    sti_type = as.character(df$stiType),
    distribution = as.character(df$distribution),
    created = tryCatch(as.Date(substr(df$created, 1, 10)),
                       error = function(e) rep(as.Date(NA), nrow(df))),
    authors = auth_strs
  )
}

#' Get recently added NASA Technical Reports
#'
#' Retrieve the most recently created NTRS records, optionally
#' filtered by STI document type.
#'
#' @details
#' Supported STI type values include \code{"TECHNICAL_REPORT"},
#' \code{"CONFERENCE_PAPER"}, \code{"REPRINT"}, \code{"ACCEPTED_MANUSCRIPT"},
#' \code{"TECHNICAL_MEMORANDUM"}, and \code{"CONTRACTOR_OR_GRANTEE_REPORT"}.
#' When \code{type} is \code{NULL}, all types are returned.
#'
#' @param page_size Integer. Number of recent reports to return (default 25).
#' @param type Character or \code{NULL}. Optional STI type filter (see Details).
#' @return A tibble with columns: \code{id}, \code{title}, \code{sti_type},
#'   \code{distribution}, \code{created}, \code{authors}.
#' @export
#' @seealso \code{\link{ntrs_search}}, \code{\link{ntrs_by_center}}
#' @examples
#' ntrs_recent(page_size = 5)
#' ntrs_recent(page_size = 10, type = "TECHNICAL_REPORT")
ntrs_recent <- function(page_size = 25, type = NULL) {
  body <- list(
    page = list(size = page_size, from = 0)
  )
  if (!is.null(type)) body$stiType <- type
  raw <- tryCatch(.ntrs_post("/citations/search", body), error = function(e) NULL)
  if (is.null(raw)) return(.schema_citations)
  df <- raw$results
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(.schema_citations)

  auth_strs <- vapply(seq_len(nrow(df)), function(i) {
    auths <- df$authorAffiliations[[i]]
    if (is.null(auths) || !is.data.frame(auths) || nrow(auths) == 0) return(NA_character_)
    paste(vapply(seq_len(nrow(auths)), function(j) {
      tryCatch(auths$meta$author$name[j] %||% NA_character_,
               error = function(e) NA_character_)
    }, character(1)), collapse = "; ")
  }, character(1))

  tibble(
    id = as.character(df$id),
    title = as.character(df$title),
    sti_type = as.character(df$stiType),
    distribution = as.character(df$distribution),
    created = tryCatch(as.Date(substr(df$created, 1, 10)),
                       error = function(e) rep(as.Date(NA), nrow(df))),
    authors = auth_strs
  )
}

# == Context ===================================================================

#' Get ntrs.nasa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nasa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nasa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ntrs.nasa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ntrs.nasa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ntrs.nasa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ntrs.nasa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
