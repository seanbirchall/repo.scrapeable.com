# api.openalex.org.R - Self-contained api.openalex.org client



# api-openalex-org.R
# Self-contained OpenAlex API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (polite pool via email in User-Agent)
# Rate limits: 10 req/sec without key, 100K/day


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.oalex_base <- "https://api.openalex.org"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = paste0(.ua, " (mailto:", .ua, ")")) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_works <- tibble(
  id = character(), doi = character(), title = character(),
  publication_year = integer(), cited_by_count = integer(),
  type = character(), journal = character(), authors = character(),
  open_access = logical()
)

.schema_authors <- tibble(
  id = character(), display_name = character(), works_count = integer(),
  cited_by_count = integer(), institution = character(),
  orcid = character()
)

.schema_institutions <- tibble(
  id = character(), display_name = character(), type = character(),
  country_code = character(), works_count = integer(),
  cited_by_count = integer()
)

# == Public functions ==========================================================


#' Search OpenAlex for scholarly works
#'
#' Query the OpenAlex catalog of scholarly works (papers, books, datasets,
#' etc.) using full-text search with optional filters.
#'
#' @details
#' OpenAlex indexes over 250 million works with rich metadata including
#' citation counts, author affiliations, and open-access status. The
#' \code{filter} parameter accepts OpenAlex filter syntax, e.g.
#' \code{"publication_year:2024"}, \code{"is_oa:true"},
#' \code{"type:journal-article"}. Multiple filters can be combined with
#' commas: \code{"publication_year:2024,is_oa:true"}.
#'
#' @param search Character. Full-text search query.
#' @param per_page Integer. Results per page (default 10, max 200).
#' @param page Integer. Page number (default 1).
#' @param filter Character or \code{NULL}. OpenAlex filter string (see Details).
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. OpenAlex work ID (URL form).}
#'   \item{doi}{Character. DOI URL, or \code{NA}.}
#'   \item{title}{Character. Work title.}
#'   \item{publication_year}{Integer. Year of publication.}
#'   \item{cited_by_count}{Integer. Total citation count.}
#'   \item{type}{Character. Work type (e.g. \code{"journal-article"}).}
#'   \item{journal}{Character. Source / journal name.}
#'   \item{authors}{Character. Semicolon-separated author names.}
#'   \item{open_access}{Logical. Whether the work is open access.}
#' }
#' @export
#' @seealso \code{\link{oalex_authors}}, \code{\link{oalex_institutions}}
#' @examples
#' oalex_works("machine learning", per_page = 5)
#' oalex_works("CRISPR", filter = "publication_year:2024", per_page = 5)
oalex_works <- function(search, per_page = 10, page = 1, filter = NULL) {
  url <- sprintf(
    "%s/works?search=%s&per_page=%d&page=%d&mailto=%s",
    .oalex_base, utils::URLencode(search, reserved = TRUE),
    as.integer(per_page), as.integer(page), .ua
  )
  if (!is.null(filter)) url <- paste0(url, "&filter=", utils::URLencode(filter))

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_works)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_works)
  if (!is.data.frame(results)) return(.schema_works)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      id = as.character(if ("id" %in% nms) id else NA_character_),
      doi = as.character(if ("doi" %in% nms) doi else NA_character_),
      title = as.character(if ("display_name" %in% nms) display_name else if ("title" %in% nms) title else NA_character_),
      publication_year = as.integer(if ("publication_year" %in% nms) publication_year else NA_integer_),
      cited_by_count = as.integer(if ("cited_by_count" %in% nms) cited_by_count else NA_integer_),
      type = as.character(if ("type" %in% nms) type else NA_character_),
      journal = as.character(if ("primary_location" %in% nms) {
        pl <- primary_location
        if (is.data.frame(pl) && "source" %in% names(pl)) {
          src <- pl$source
          if (is.data.frame(src) && "display_name" %in% names(src)) src$display_name
          else NA_character_
        } else NA_character_
      } else NA_character_),
      authors = as.character(if ("authorships" %in% nms) sapply(seq_len(n()), function(i) {
        x <- authorships[[i]]
        if (is.data.frame(x) && "author" %in% names(x)) {
          au <- x$author
          if (is.data.frame(au) && "display_name" %in% names(au)) paste(au$display_name, collapse = "; ")
          else NA_character_
        } else NA_character_
      }) else NA_character_),
      open_access = as.logical(if ("open_access" %in% nms) {
        oa <- open_access
        if (is.data.frame(oa) && "is_oa" %in% names(oa)) oa$is_oa
        else NA
      } else NA)
    )
}

#' Search OpenAlex for authors
#'
#' Find researchers by name in the OpenAlex author index, which covers
#' over 200 million author profiles disambiguated by the OpenAlex
#' algorithm.
#'
#' @details
#' Author records link to their published works, citation counts, and
#' last known institutional affiliation. The \code{orcid} column
#' contains the ORCID URL when available (e.g.
#' \code{"https://orcid.org/0000-0001-2345-6789"}).
#'
#' @param search Character. Author name query (e.g. \code{"Albert Einstein"},
#'   \code{"Fei-Fei Li"}).
#' @param per_page Integer. Number of results (default 10, max 200).
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. OpenAlex author ID.}
#'   \item{display_name}{Character. Author display name.}
#'   \item{works_count}{Integer. Number of indexed works.}
#'   \item{cited_by_count}{Integer. Total citations received.}
#'   \item{institution}{Character. Last known institution name.}
#'   \item{orcid}{Character. ORCID URL, or \code{NA}.}
#' }
#' @export
#' @seealso \code{\link{oalex_works}}, \code{\link{oalex_institutions}}
#' @examples
#' oalex_authors("Yoshua Bengio", per_page = 3)
oalex_authors <- function(search, per_page = 10) {
  url <- sprintf(
    "%s/authors?search=%s&per_page=%d&mailto=%s",
    .oalex_base, utils::URLencode(search, reserved = TRUE),
    as.integer(per_page), .ua
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_authors)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_authors)
  if (!is.data.frame(results)) return(.schema_authors)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      id = as.character(if ("id" %in% nms) id else NA_character_),
      display_name = as.character(if ("display_name" %in% nms) display_name else NA_character_),
      works_count = as.integer(if ("works_count" %in% nms) works_count else NA_integer_),
      cited_by_count = as.integer(if ("cited_by_count" %in% nms) cited_by_count else NA_integer_),
      institution = as.character(if ("last_known_institutions" %in% nms) sapply(last_known_institutions, function(x) {
        if (is.data.frame(x) && nrow(x) > 0) x$display_name[1] %||% NA_character_
        else NA_character_
      }) else NA_character_),
      orcid = as.character(if ("orcid" %in% nms) orcid else NA_character_)
    )
}

#' Search OpenAlex for institutions
#'
#' Search the OpenAlex institution index by name. Includes universities,
#' research institutes, government agencies, and other organizations.
#'
#' @details
#' Institution types include \code{"education"}, \code{"healthcare"},
#' \code{"company"}, \code{"government"}, \code{"nonprofit"}, and
#' \code{"facility"}. The \code{country_code} is a two-letter ISO
#' 3166-1 alpha-2 code.
#'
#' @param search Character. Institution name query (e.g. \code{"MIT"},
#'   \code{"Max Planck"}).
#' @param per_page Integer. Number of results (default 10, max 200).
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Character. OpenAlex institution ID.}
#'   \item{display_name}{Character. Institution name.}
#'   \item{type}{Character. Institution type (see Details).}
#'   \item{country_code}{Character. ISO country code.}
#'   \item{works_count}{Integer. Number of indexed works.}
#'   \item{cited_by_count}{Integer. Total citations.}
#' }
#' @export
#' @seealso \code{\link{oalex_works}}, \code{\link{oalex_authors}}
#' @examples
#' oalex_institutions("Stanford", per_page = 3)
oalex_institutions <- function(search, per_page = 10) {
  url <- sprintf(
    "%s/institutions?search=%s&per_page=%d&mailto=%s",
    .oalex_base, utils::URLencode(search, reserved = TRUE),
    as.integer(per_page), .ua
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_institutions)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_institutions)
  if (!is.data.frame(results)) return(.schema_institutions)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      id = as.character(if ("id" %in% nms) id else NA_character_),
      display_name = as.character(if ("display_name" %in% nms) display_name else NA_character_),
      type = as.character(if ("type" %in% nms) type else NA_character_),
      country_code = as.character(if ("country_code" %in% nms) country_code else NA_character_),
      works_count = as.integer(if ("works_count" %in% nms) works_count else NA_integer_),
      cited_by_count = as.integer(if ("cited_by_count" %in% nms) cited_by_count else NA_integer_)
    )
}

# == Context ===================================================================

#' Get openalex.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
oalex_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(oalex_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/openalex.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "openalex.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# openalex.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# openalex.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
