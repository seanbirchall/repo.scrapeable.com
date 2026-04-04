# api.crossref.org.R - Self-contained api.crossref.org client



# api-crossref-org.R
# Self-contained Crossref API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data, polite pool via email in User-Agent)
# Rate limits: ~50 req/sec with polite pool


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.cr_base <- "https://api.crossref.org"

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
  doi = character(), title = character(), type = character(),
  container_title = character(), published_date = character(),
  authors = character(), publisher = character(),
  reference_count = integer(), is_referenced_by_count = integer(),
  url = character()
)

.schema_journals <- tibble(
  issn = character(), title = character(), publisher = character(),
  subjects = character(), total_dois = integer()
)

# == Private helpers ===========================================================

.parse_works <- function(items) {
  if (is.null(items) || length(items) == 0) return(.schema_works)
  if (!is.data.frame(items)) return(.schema_works)
  if (nrow(items) == 0) return(.schema_works)

  nms <- names(items)

  as_tibble(items) |>
    transmute(
      doi = as.character(if ("DOI" %in% nms) DOI else NA_character_),
      title = as.character(if ("title" %in% nms) sapply(title, function(x) paste(x, collapse = " ")) else NA_character_),
      type = as.character(if ("type" %in% nms) type else NA_character_),
      container_title = as.character(if ("container-title" %in% nms) sapply(`container-title`, function(x) paste(x, collapse = "; ")) else NA_character_),
      published_date = as.character(if ("created" %in% nms) {
        if (is.data.frame(created)) {
          sapply(created$`date-parts`, function(dp) tryCatch(as.character(dp[1,1]), error = function(e) NA_character_))
        } else NA_character_
      } else NA_character_),
      authors = as.character(if ("author" %in% nms) sapply(author, function(x) {
        if (is.data.frame(x)) paste(paste(x$given %||% "", x$family %||% ""), collapse = "; ")
        else NA_character_
      }) else NA_character_),
      publisher = as.character(if ("publisher" %in% nms) publisher else NA_character_),
      reference_count = as.integer(if ("reference-count" %in% nms) `reference-count` else NA_integer_),
      is_referenced_by_count = as.integer(if ("is-referenced-by-count" %in% nms) `is-referenced-by-count` else NA_integer_),
      url = as.character(if ("URL" %in% nms) URL else NA_character_)
    )
}

# == Public functions ==========================================================


#' Search Crossref for scholarly works (articles, books, etc.)
#'
#' Queries the Crossref REST API for scholarly metadata including journal
#' articles, books, conference papers, and datasets. Uses the polite pool
#' for higher rate limits. Supports filters for narrowing by type, date, etc.
#'
#' @param query Character. Free-text search query (e.g., \code{"machine learning"},
#'   \code{"CRISPR gene editing"}).
#' @param rows Integer. Number of results to return (default 10, max 1000).
#' @param offset Integer. Starting offset for pagination (default 0).
#'   Use \code{offset = 10} to get results 11--20 when \code{rows = 10}.
#' @param filter Character or \code{NULL}. Crossref filter string for narrowing results.
#'   Examples: \code{"type:journal-article"}, \code{"from-pub-date:2023"},
#'   \code{"has-orcid:true"}, \code{"is-update:true"}.
#'   Multiple filters separated by commas.
#' @return A tibble with columns:
#'   \describe{
#'     \item{doi}{\code{character} -- Digital Object Identifier (e.g., "10.1038/nature12373")}
#'     \item{title}{\code{character} -- Work title}
#'     \item{type}{\code{character} -- Work type (e.g., "journal-article", "book-chapter", "monograph")}
#'     \item{container_title}{\code{character} -- Journal or book title}
#'     \item{published_date}{\code{character} -- Publication year}
#'     \item{authors}{\code{character} -- Semicolon-separated author names}
#'     \item{publisher}{\code{character} -- Publisher name}
#'     \item{reference_count}{\code{integer} -- Number of references cited}
#'     \item{is_referenced_by_count}{\code{integer} -- Citation count}
#'     \item{url}{\code{character} -- DOI URL}
#'   }
#' @examples
#' cr_works("machine learning", rows = 5)
#' cr_works("climate change", filter = "type:journal-article", rows = 10)
#' @export
cr_works <- function(query, rows = 10, offset = 0, filter = NULL) {
  url <- sprintf(
    "%s/works?query=%s&rows=%d&offset=%d",
    .cr_base, utils::URLencode(query, reserved = TRUE),
    as.integer(rows), as.integer(offset)
  )
  if (!is.null(filter)) url <- paste0(url, "&filter=", filter)

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_works)

  items <- raw$message$items
  .parse_works(items)
}

#' Search Crossref journals
#'
#' Searches the Crossref journal registry by title. Returns journal metadata
#' including ISSN, publisher, subject areas, and total DOI count.
#'
#' @param query Character. Search query for journal titles (e.g., \code{"nature"},
#'   \code{"bioinformatics"}, \code{"lancet"}).
#' @param rows Integer. Number of results to return (default 10).
#' @return A tibble with columns:
#'   \describe{
#'     \item{issn}{\code{character} -- Semicolon-separated ISSN(s)}
#'     \item{title}{\code{character} -- Journal title}
#'     \item{publisher}{\code{character} -- Publisher name}
#'     \item{subjects}{\code{character} -- Semicolon-separated subject areas (may be NA)}
#'     \item{total_dois}{\code{integer} -- Total number of DOIs registered in this journal}
#'   }
#' @examples
#' cr_journals("nature", rows = 5)
#' cr_journals("bioinformatics")
#' @export
cr_journals <- function(query, rows = 10) {
  url <- sprintf(
    "%s/journals?query=%s&rows=%d",
    .cr_base, utils::URLencode(query, reserved = TRUE), as.integer(rows)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_journals)

  items <- raw$message$items
  if (is.null(items) || length(items) == 0) return(.schema_journals)
  if (!is.data.frame(items)) return(.schema_journals)

  nms <- names(items)
  as_tibble(items) |>
    transmute(
      issn = as.character(if ("ISSN" %in% nms) sapply(ISSN, paste, collapse = "; ") else NA_character_),
      title = as.character(if ("title" %in% nms) title else NA_character_),
      publisher = as.character(if ("publisher" %in% nms) publisher else NA_character_),
      subjects = as.character(if ("subjects" %in% nms) sapply(subjects, function(x) {
        if (is.data.frame(x)) paste(x$name, collapse = "; ") else NA_character_
      }) else NA_character_),
      total_dois = as.integer(if ("counts" %in% nms && is.data.frame(counts)) counts$`total-dois` else NA_integer_)
    )
}

#' Get metadata for a single work by DOI
#'
#' Fetches full bibliographic metadata for one scholarly work identified
#' by its DOI. Returns the same column schema as \code{cr_works()}.
#'
#' @param doi Character. DOI string (e.g., \code{"10.1038/nature12373"},
#'   \code{"10.1126/science.aax1566"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{doi}{\code{character} -- Digital Object Identifier}
#'     \item{title}{\code{character} -- Work title}
#'     \item{type}{\code{character} -- Work type (e.g., "journal-article")}
#'     \item{container_title}{\code{character} -- Journal or book title}
#'     \item{published_date}{\code{character} -- Publication year}
#'     \item{authors}{\code{character} -- Semicolon-separated author names}
#'     \item{publisher}{\code{character} -- Publisher name}
#'     \item{reference_count}{\code{integer} -- Number of references cited}
#'     \item{is_referenced_by_count}{\code{integer} -- Citation count}
#'     \item{url}{\code{character} -- DOI URL}
#'   }
#' @examples
#' cr_work("10.1038/nature12373")
#' @export
cr_work <- function(doi) {
  url <- sprintf("%s/works/%s", .cr_base, utils::URLencode(doi, reserved = TRUE))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_works)
  item <- raw$message
  if (is.null(item)) return(.schema_works)
  # Wrap single item in a data.frame-like structure for .parse_works
  df <- tryCatch(as.data.frame(item, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df)) {
    tibble(
      doi = item$DOI %||% NA_character_,
      title = if (!is.null(item$title)) paste(item$title, collapse = " ") else NA_character_,
      type = item$type %||% NA_character_,
      container_title = if (!is.null(item$`container-title`)) paste(item$`container-title`, collapse = "; ") else NA_character_,
      published_date = tryCatch(as.character(item$created$`date-parts`[[1]][1]), error = function(e) NA_character_),
      authors = if (!is.null(item$author) && is.data.frame(item$author)) paste(paste(item$author$given %||% "", item$author$family %||% ""), collapse = "; ") else NA_character_,
      publisher = item$publisher %||% NA_character_,
      reference_count = as.integer(item$`reference-count` %||% NA),
      is_referenced_by_count = as.integer(item$`is-referenced-by-count` %||% NA),
      url = item$URL %||% NA_character_
    )
  } else {
    .parse_works(df)
  }
}

#' Search Crossref funders
#'
#' Searches the Crossref Funder Registry for research funding organizations.
#' Returns funder identifiers that can be used to filter works by funder.
#'
#' @param query Character. Search query for funder names (e.g., \code{"NIH"},
#'   \code{"NSF"}, \code{"Wellcome Trust"}).
#' @param rows Integer. Number of results to return (default 10).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{\code{character} -- Funder DOI (e.g., "100000002")}
#'     \item{name}{\code{character} -- Primary funder name}
#'     \item{location}{\code{character} -- Country (e.g., "United States")}
#'     \item{alt_names}{\code{character} -- Semicolon-separated alternative names and acronyms}
#'     \item{uri}{\code{character} -- Funder Registry URI}
#'   }
#' @examples
#' cr_funders("NIH")
#' cr_funders("European Commission", rows = 5)
#' @export
cr_funders <- function(query, rows = 10) {
  url <- sprintf("%s/funders?query=%s&rows=%d",
                 .cr_base, utils::URLencode(query, reserved = TRUE), as.integer(rows))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(tibble(id = character(), name = character(), location = character()))
  items <- raw$message$items
  if (is.null(items) || length(items) == 0) return(tibble(id = character(), name = character()))

  if (is.data.frame(items)) {
    nms <- names(items)
    as_tibble(items) |>
      transmute(
        id = as.character(if ("id" %in% nms) id else NA_character_),
        name = as.character(if ("name" %in% nms) name else NA_character_),
        location = as.character(if ("location" %in% nms) location else NA_character_),
        alt_names = as.character(if ("alt-names" %in% nms) sapply(`alt-names`, function(x) paste(unlist(x), collapse = "; ")) else NA_character_),
        uri = as.character(if ("uri" %in% nms) uri else NA_character_)
      )
  } else {
    tibble(
      id = vapply(items, function(x) x$id %||% NA_character_, character(1)),
      name = vapply(items, function(x) x$name %||% NA_character_, character(1)),
      location = vapply(items, function(x) x$location %||% NA_character_, character(1)),
      alt_names = vapply(items, function(x) if (!is.null(x$`alt-names`)) paste(unlist(x$`alt-names`), collapse = "; ") else NA_character_, character(1)),
      uri = vapply(items, function(x) x$uri %||% NA_character_, character(1))
    )
  }
}

# == Context ===================================================================

#' Get crossref.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
cr_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(cr_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/crossref.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "crossref.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# crossref.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# crossref.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
