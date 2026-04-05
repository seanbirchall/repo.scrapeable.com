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


#' @title Search OpenAlex for scholarly works
#'
#' Full-text search across the OpenAlex database of 250M+ scholarly works.
#' Returns bibliographic metadata with citation counts, author lists, and
#' open access status. OpenAlex is a free and open replacement for Microsoft
#' Academic Graph. Supports filtering by year, type, open access status, etc.
#'
#' @param search Search query string. Matches titles, abstracts, and full text.
#'   Examples: \code{"deep learning"}, \code{"CRISPR gene editing"},
#'   \code{"climate change adaptation"}.
#' @param per_page Number of results per page. Default \code{10}. Maximum \code{200}.
#' @param page Page number (1-indexed). Default \code{1}. Use with \code{per_page}
#'   to paginate.
#' @param filter Optional OpenAlex filter string. Examples:
#'   \code{"publication_year:2024"}, \code{"is_oa:true"},
#'   \code{"type:article"}, \code{"cited_by_count:>100"}.
#'   Multiple filters can be combined with commas. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{id} (character): OpenAlex ID URL, e.g. \code{"https://openalex.org/W2919115771"}
#'     \item \code{doi} (character): DOI URL, e.g. \code{"https://doi.org/10.1038/nature14539"}
#'     \item \code{title} (character): Work title
#'     \item \code{publication_year} (integer): Year of publication
#'     \item \code{cited_by_count} (integer): Total citation count
#'     \item \code{type} (character): Work type, e.g. \code{"article"}, \code{"review"}, \code{"book-chapter"}
#'     \item \code{journal} (character): Journal/source name (may be \code{NA} for conference papers)
#'     \item \code{authors} (character): Semicolon-separated author names
#'     \item \code{open_access} (logical): Whether the work is open access
#'   }
#' @examples
#' \dontrun{
#' # Search for deep learning papers
#' oalex_works("deep learning", per_page = 20)
#'
#' # Only open-access articles from 2024
#' oalex_works("climate change", filter = "publication_year:2024,is_oa:true")
#'
#' # Highly cited papers
#' oalex_works("transformer architecture", filter = "cited_by_count:>100")
#' }
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

#' @title Search OpenAlex for authors
#'
#' Searches the OpenAlex author database by name. Returns author profiles
#' with publication counts, citation metrics, institutional affiliation,
#' and ORCID identifiers. Note that common names may return multiple
#' disambiguated author profiles.
#'
#' @param search Author name to search for. Examples: \code{"Yoshua Bengio"},
#'   \code{"Jennifer Doudna"}, \code{"Albert Einstein"}.
#' @param per_page Number of results to return. Default \code{10}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{id} (character): OpenAlex author ID URL
#'     \item \code{display_name} (character): Author's display name
#'     \item \code{works_count} (integer): Total number of published works
#'     \item \code{cited_by_count} (integer): Total citation count across all works
#'     \item \code{institution} (character): Last known institutional affiliation (may be \code{NA})
#'     \item \code{orcid} (character): ORCID URL if available (may be \code{NA})
#'   }
#' @examples
#' \dontrun{
#' # Find Yoshua Bengio
#' oalex_authors("Yoshua Bengio")
#'
#' # Search for a common name (may return multiple profiles)
#' oalex_authors("John Smith", per_page = 20)
#' }
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

#' @title Search OpenAlex for institutions
#'
#' Searches the OpenAlex institution database by name. Returns institution
#' profiles with type classification, country, publication counts, and
#' citation metrics. Includes universities, research labs, hospitals,
#' government agencies, and other research organizations.
#'
#' @param search Institution name to search for. Examples: \code{"MIT"},
#'   \code{"Harvard University"}, \code{"Max Planck"}.
#' @param per_page Number of results to return. Default \code{10}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{id} (character): OpenAlex institution ID URL
#'     \item \code{display_name} (character): Institution name, e.g. \code{"Massachusetts Institute of Technology"}
#'     \item \code{type} (character): Institution type, e.g. \code{"education"}, \code{"facility"}, \code{"company"}
#'     \item \code{country_code} (character): Two-letter ISO country code, e.g. \code{"US"}, \code{"GB"}
#'     \item \code{works_count} (integer): Total number of works affiliated with this institution
#'     \item \code{cited_by_count} (integer): Total citations to works from this institution
#'   }
#' @examples
#' \dontrun{
#' # Search for MIT
#' oalex_institutions("MIT")
#'
#' # Find Max Planck institutes
#' oalex_institutions("Max Planck", per_page = 20)
#'
#' # Search for a specific university
#' oalex_institutions("Stanford University")
#' }
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

#' Get api.openalex.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.openalex.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.openalex.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.openalex.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.openalex.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
