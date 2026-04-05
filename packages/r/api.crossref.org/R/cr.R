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


#' @title Search Crossref for scholarly works
#'
#' Full-text search across the Crossref metadata database of scholarly works
#' including journal articles, books, book chapters, conference papers, and
#' preprints. Returns parsed bibliographic metadata with citation counts.
#' Use \code{cr_work()} to fetch a single work by DOI.
#'
#' @param query Search query string. Matches against titles, authors, abstracts,
#'   and full-text. Examples: \code{"machine learning"}, \code{"CRISPR gene editing"}.
#' @param rows Number of results to return. Default \code{10}. Maximum \code{1000}.
#' @param offset Starting offset for pagination. Default \code{0}. Use with
#'   \code{rows} to page through results.
#' @param filter Optional Crossref filter string. Examples:
#'   \code{"type:journal-article"}, \code{"from-pub-date:2020"},
#'   \code{"has-orcid:true"}. See Crossref API docs for full filter syntax.
#'   Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{doi} (character): Digital Object Identifier, e.g. \code{"10.1038/nature12373"}
#'     \item \code{title} (character): Work title
#'     \item \code{type} (character): Work type, e.g. \code{"journal-article"}, \code{"book-chapter"}, \code{"monograph"}
#'     \item \code{container_title} (character): Journal or book title containing the work
#'     \item \code{published_date} (character): Publication year
#'     \item \code{authors} (character): Semicolon-separated author names (Given Family format)
#'     \item \code{publisher} (character): Publisher name
#'     \item \code{reference_count} (integer): Number of references cited by this work
#'     \item \code{is_referenced_by_count} (integer): Number of times this work is cited
#'     \item \code{url} (character): DOI URL (e.g. \code{"https://doi.org/10.1038/..."})
#'   }
#' @examples
#' \dontrun{
#' # Search for machine learning papers
#' cr_works("machine learning", rows = 20)
#'
#' # Only journal articles from 2023+
#' cr_works("climate change", filter = "type:journal-article,from-pub-date:2023")
#'
#' # Paginate through results
#' page1 <- cr_works("CRISPR", rows = 50, offset = 0)
#' page2 <- cr_works("CRISPR", rows = 50, offset = 50)
#' }
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

#' @title Search Crossref journals
#'
#' Searches the Crossref journal registry by title. Returns journal metadata
#' including ISSNs, publisher, subject areas, and total number of DOIs
#' registered. Use the ISSN from results to filter \code{cr_works()} queries.
#'
#' @param query Search query string for journal titles. Examples: \code{"nature"},
#'   \code{"machine learning"}, \code{"cell biology"}.
#' @param rows Number of results to return. Default \code{10}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{issn} (character): ISSN(s), semicolon-separated if multiple (print and electronic)
#'     \item \code{title} (character): Journal title
#'     \item \code{publisher} (character): Publisher name
#'     \item \code{subjects} (character): Semicolon-separated subject areas (may be \code{NA})
#'     \item \code{total_dois} (integer): Total number of DOIs registered for this journal
#'   }
#' @examples
#' \dontrun{
#' # Search for Nature journals
#' cr_journals("nature", rows = 20)
#'
#' # Find machine learning journals
#' cr_journals("machine learning")
#' }
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

#' @title Get metadata for a single work by DOI
#'
#' Fetches full bibliographic metadata for a single scholarly work identified
#' by its DOI. Returns the same column structure as \code{cr_works()} but
#' always a single row. Use this when you have a specific DOI and need its
#' metadata (title, authors, citation count, etc.).
#'
#' @param doi DOI string. Examples: \code{"10.1038/nature12373"},
#'   \code{"10.1126/science.aax2342"}. Do not include the \code{"https://doi.org/"}
#'   prefix.
#' @return A tibble with one row and columns:
#'   \itemize{
#'     \item \code{doi} (character): The DOI
#'     \item \code{title} (character): Work title
#'     \item \code{type} (character): Work type (e.g. \code{"journal-article"})
#'     \item \code{container_title} (character): Journal or book title
#'     \item \code{published_date} (character): Publication year
#'     \item \code{authors} (character): Semicolon-separated author names
#'     \item \code{publisher} (character): Publisher name
#'     \item \code{reference_count} (integer): Number of references cited
#'     \item \code{is_referenced_by_count} (integer): Citation count
#'     \item \code{url} (character): Full DOI URL
#'   }
#' @examples
#' \dontrun{
#' # Look up a Nature paper
#' cr_work("10.1038/nature12373")
#'
#' # Look up a Science paper
#' cr_work("10.1126/science.aax2342")
#' }
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

#' @title Search Crossref funders
#'
#' Searches the Crossref Funder Registry (based on the Open Funder Registry)
#' by name. Returns funder identifiers, locations, and alternative names.
#' Useful for finding standardized funder IDs for grant acknowledgment searches.
#'
#' @param query Search query for funder names. Examples: \code{"NIH"},
#'   \code{"National Science Foundation"}, \code{"Wellcome Trust"}.
#' @param rows Number of results to return. Default \code{10}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{id} (character): Crossref funder ID (e.g. \code{"100006108"})
#'     \item \code{name} (character): Official funder name
#'     \item \code{location} (character): Country, e.g. \code{"United States"}, \code{"United Kingdom"}
#'     \item \code{alt_names} (character): Semicolon-separated alternative names and abbreviations
#'     \item \code{uri} (character): Funder DOI URI (e.g. \code{"https://doi.org/10.13039/..."})
#'   }
#' @examples
#' \dontrun{
#' # Search for NIH-related funders
#' cr_funders("NIH")
#'
#' # Find the NSF
#' cr_funders("National Science Foundation")
#'
#' # Search European funders
#' cr_funders("European Research Council")
#' }
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

#' Get api.crossref.org client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.crossref.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.crossref.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.crossref.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.crossref.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
