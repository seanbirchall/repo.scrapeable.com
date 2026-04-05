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
#' Searches the CourtListener database of U.S. court opinions, dockets,
#' RECAP archive filings, and judicial personnel. Returns paginated results
#' with case metadata and text snippets matching the query.
#'
#' @param query Character. Search query string supporting full-text search
#'   (e.g., \code{"privacy"}, \code{"first amendment"}, \code{"qualified immunity"}).
#' @param type Character. Result type to search. One of:
#'   \describe{
#'     \item{\code{"o"}}{Opinions (default) -- judicial opinions and orders}
#'     \item{\code{"r"}}{RECAP -- PACER filings from the RECAP archive}
#'     \item{\code{"d"}}{Dockets -- case docket entries}
#'     \item{\code{"p"}}{People -- judges and other legal personnel}
#'   }
#' @param page_size Integer. Number of results per page (default 10, max 20).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{\code{integer} -- Opinion cluster ID (use with \code{cl_opinion()})}
#'     \item{caseName}{\code{character} -- Short case name (e.g., "Roe v. Wade")}
#'     \item{court}{\code{character} -- Court identifier (e.g., "scotus", "ca9")}
#'     \item{dateFiled}{\code{Date} -- Date the opinion was filed}
#'     \item{snippet}{\code{character} -- Text snippet with search term highlighted}
#'     \item{url}{\code{character} -- Full URL to the opinion on CourtListener}
#'   }
#' @examples
#' cl_search("privacy")
#' cl_search("first amendment", type = "o", page_size = 5)
#' @export
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
#' Retrieves detailed metadata for a single opinion cluster from the
#' CourtListener API. Use cluster IDs obtained from \code{cl_search()}.
#'
#' @param id Integer. Opinion cluster ID (obtained from the \code{id} column
#'   of \code{cl_search()} results).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{\code{integer} -- Cluster ID}
#'     \item{caseName}{\code{character} -- Case name}
#'     \item{court}{\code{character} -- Court URL path}
#'     \item{dateFiled}{\code{Date} -- Filing date}
#'     \item{snippet}{\code{character} -- Case syllabus (if available)}
#'     \item{url}{\code{character} -- Full CourtListener URL}
#'     \item{plain_text}{\code{character} -- Plain text of the opinion (NA if not fetched)}
#'   }
#' @examples
#' cl_opinion(2812209)
#' @export
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

#' Get courtlistener client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/courtlistener.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "courtlistener")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# courtlistener context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# courtlistener", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
