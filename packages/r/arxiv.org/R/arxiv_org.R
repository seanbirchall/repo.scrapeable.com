# arxiv.org.R - Self-contained arxiv.org client



# arxiv-org.R
# Self-contained arXiv API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2
# Auth: none (public data)
# Rate limits: 1 request per 3 seconds recommended


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.arxiv_base <- "http://export.arxiv.org/api/query"

.fetch <- function(url, ext = ".xml") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url, ext = ".json"))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_papers <- tibble(
  id = character(), title = character(), summary = character(),
  authors = character(), published = as.Date(character()),
  updated = as.Date(character()), categories = character(),
  pdf_url = character(), doi = character()
)

# == Public functions ==========================================================


#' Search arXiv for papers
#'
#' Queries the arXiv API using the Atom feed format. Supports field-specific
#' searches via prefix syntax. Returns parsed metadata for each matching paper
#' including title, authors, abstract, categories, and PDF link.
#'
#' @param query Character. Search query string with optional field prefixes.
#'   Supported prefixes: \code{"all:"} (all fields), \code{"ti:"} (title),
#'   \code{"au:"} (author), \code{"abs:"} (abstract), \code{"cat:"} (category),
#'   \code{"id:"} (arXiv ID). Boolean operators \code{AND}, \code{OR},
#'   \code{ANDNOT} are supported. Examples: \code{"all:quantum computing"},
#'   \code{"ti:transformer AND cat:cs.CL"}, \code{"au:Hinton"}.
#' @param max_results Integer. Maximum number of results (default 10, max 2000).
#'   The arXiv API recommends keeping this under 1000 for performance.
#' @param start Integer. Starting index for pagination (default 0). Use with
#'   \code{max_results} to page through large result sets.
#' @param sort_by Character. Sort order. One of \code{"relevance"} (default),
#'   \code{"lastUpdatedDate"}, or \code{"submittedDate"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. arXiv paper ID (e.g. "2301.12345").}
#'     \item{title}{Character. Paper title (whitespace-normalized).}
#'     \item{summary}{Character. Full abstract text.}
#'     \item{authors}{Character. Semicolon-separated author names
#'       (e.g. "Seyon Sivarajah; Ross Duncan").}
#'     \item{published}{Date. Original submission date.}
#'     \item{updated}{Date. Most recent update date.}
#'     \item{categories}{Character. Semicolon-separated arXiv categories
#'       (e.g. "quant-ph; cs.AI").}
#'     \item{pdf_url}{Character. Direct link to PDF.}
#'     \item{doi}{Character. DOI if available, otherwise \code{NA}.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' arxiv_search("all:quantum computing", max_results = 5)
#' arxiv_search("ti:transformer AND cat:cs.CL")
#' arxiv_search("au:Hinton", sort_by = "submittedDate")
#' }
arxiv_search <- function(query, max_results = 10, start = 0,
                         sort_by = "relevance") {
  url <- sprintf(
    "%s?search_query=%s&start=%d&max_results=%d&sortBy=%s&sortOrder=descending",
    .arxiv_base, utils::URLencode(query, reserved = TRUE),
    as.integer(start), as.integer(max_results), sort_by
  )

  tmp <- .fetch(url, ext = ".xml")
  doc <- xml2::read_xml(tmp)
  ns <- xml2::xml_ns(doc)

  entries <- xml2::xml_find_all(doc, ".//d1:entry", ns)
  if (length(entries) == 0) return(.schema_papers)

  rows <- lapply(entries, function(entry) {
    id_raw <- xml2::xml_text(xml2::xml_find_first(entry, "d1:id", ns))
    id_clean <- sub("http://arxiv.org/abs/", "", id_raw)
    id_clean <- sub("v\\d+$", "", id_clean)

    title <- xml2::xml_text(xml2::xml_find_first(entry, "d1:title", ns))
    title <- gsub("\\s+", " ", trimws(title))

    summary <- xml2::xml_text(xml2::xml_find_first(entry, "d1:summary", ns))
    summary <- gsub("\\s+", " ", trimws(summary))

    author_nodes <- xml2::xml_find_all(entry, "d1:author/d1:name", ns)
    authors <- paste(xml2::xml_text(author_nodes), collapse = "; ")

    published <- tryCatch(
      as.Date(xml2::xml_text(xml2::xml_find_first(entry, "d1:published", ns))),
      error = function(e) as.Date(NA)
    )
    updated <- tryCatch(
      as.Date(xml2::xml_text(xml2::xml_find_first(entry, "d1:updated", ns))),
      error = function(e) as.Date(NA)
    )

    cat_nodes <- xml2::xml_find_all(entry, "d1:category", ns)
    categories <- paste(xml2::xml_attr(cat_nodes, "term"), collapse = "; ")

    links <- xml2::xml_find_all(entry, "d1:link", ns)
    link_titles <- xml2::xml_attr(links, "title")
    pdf_idx <- which(!is.na(link_titles) & link_titles == "pdf")
    pdf_url <- if (length(pdf_idx) > 0) xml2::xml_attr(links[[pdf_idx[1]]], "href") else NA_character_

    doi_node <- xml2::xml_find_first(entry, "arxiv:doi", ns)
    doi <- if (!inherits(doi_node, "xml_missing") && !is.na(doi_node)) xml2::xml_text(doi_node) else NA_character_

    tibble(
      id = id_clean, title = title, summary = summary, authors = authors,
      published = published, updated = updated, categories = categories,
      pdf_url = pdf_url, doi = doi
    )
  })

  bind_rows(rows)
}

#' Get a single arXiv paper by ID
#'
#' Retrieves full metadata for a single arXiv paper identified by its ID.
#' Wraps \code{arxiv_search()} with an \code{id:} prefix query.
#'
#' @param arxiv_id Character. arXiv paper ID. Accepts new-style IDs
#'   (e.g. \code{"2301.12345"}) or old-style category-prefixed IDs
#'   (e.g. \code{"cs/0601001"}).
#' @return A tibble with one row and columns: \code{id}, \code{title},
#'   \code{summary}, \code{authors}, \code{published}, \code{updated},
#'   \code{categories}, \code{pdf_url}, \code{doi}. See
#'   \code{arxiv_search()} for column descriptions.
#' @export
#' @examples
#' \dontrun{
#' arxiv_paper("2301.12345")
#' arxiv_paper("1706.03762")
#' }
arxiv_paper <- function(arxiv_id) {
  arxiv_search(paste0("id:", arxiv_id), max_results = 1)
}

#' Get recent papers from an arXiv category
#'
#' Fetches the most recently submitted papers in a given arXiv category,
#' sorted by submission date descending. Wraps \code{arxiv_search()} with
#' a \code{cat:} prefix and \code{sort_by = "submittedDate"}.
#'
#' @param category Character. arXiv category code. Examples: \code{"cs.AI"}
#'   (Artificial Intelligence), \code{"stat.ML"} (Machine Learning),
#'   \code{"physics.gen-ph"} (General Physics), \code{"q-bio.BM"}
#'   (Biomolecules), \code{"econ.GN"} (General Economics), \code{"math.AG"}
#'   (Algebraic Geometry). See \url{https://arxiv.org/category_taxonomy}.
#' @param max_results Integer. Number of papers to return (default 25, max 2000).
#' @return A tibble with columns: \code{id}, \code{title}, \code{summary},
#'   \code{authors}, \code{published}, \code{updated}, \code{categories},
#'   \code{pdf_url}, \code{doi}. See \code{arxiv_search()} for details.
#' @export
#' @examples
#' \dontrun{
#' arxiv_recent("cs.AI", max_results = 10)
#' arxiv_recent("stat.ML")
#' }
arxiv_recent <- function(category, max_results = 25) {
  arxiv_search(paste0("cat:", category), max_results = max_results,
               sort_by = "submittedDate")
}

#' Search arXiv by author name
#'
#' Searches for papers by a specific author, sorted by submission date
#' descending. Wraps \code{arxiv_search()} with an \code{au:} prefix.
#'
#' @param author Character. Author name or partial name. Examples:
#'   \code{"Hinton"}, \code{"Geoffrey Hinton"}, \code{"Bengio"},
#'   \code{"LeCun"}.
#' @param max_results Integer. Number of results to return (default 25).
#' @return A tibble with columns: \code{id}, \code{title}, \code{summary},
#'   \code{authors}, \code{published}, \code{updated}, \code{categories},
#'   \code{pdf_url}, \code{doi}. See \code{arxiv_search()} for details.
#' @export
#' @examples
#' \dontrun{
#' arxiv_author("Hinton", max_results = 5)
#' arxiv_author("Yann LeCun")
#' }
arxiv_author <- function(author, max_results = 25) {
  arxiv_search(paste0("au:", author), max_results = max_results,
               sort_by = "submittedDate")
}

# == Context ===================================================================

#' Get arxiv.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
arxiv_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(arxiv_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/arxiv.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "arxiv.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# arxiv.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# arxiv.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
