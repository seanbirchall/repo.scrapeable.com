# arxiv-org.R
# Self-contained arXiv API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2
# Auth: none (public data)
# Rate limits: 1 request per 3 seconds recommended

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.arxiv_base <- "http://export.arxiv.org/api/query"

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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
#' Queries the arXiv API using Atom feed format. Supports field-specific
#' searches: all, ti (title), au (author), abs (abstract), cat (category).
#'
#' @param query Search query. Use "all:term" for general search, "ti:term"
#'   for title, "au:name" for author, "cat:cs.AI" for category.
#' @param max_results Maximum number of results. Default 10, max 2000.
#' @param start Starting index for pagination. Default 0.
#' @param sort_by Sort by "relevance", "lastUpdatedDate", or "submittedDate".
#'   Default "relevance".
#' @return tibble: id, title, summary, authors, published, updated,
#'   categories, pdf_url, doi
#' @export
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

#' Print arXiv context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
arxiv_context <- function() {
  .build_context(
    pkg_name = "arxiv.org",
    header_lines = c(
      "# Package: arxiv.org",
      "# arXiv.org API - preprint search",
      "# Auth: none",
      "# Rate limits: 1 request per 3 seconds recommended",
      "#",
      "# Query prefixes: all: (any field), ti: (title), au: (author),",
      "#   abs: (abstract), cat: (category e.g. cs.AI, math.CO)",
      "# Boolean: AND, OR, ANDNOT between terms",
      "#",
      "# Popular categories: cs.AI, cs.LG, cs.CL, stat.ML, math.CO,",
      "#   physics, q-bio, q-fin, econ"
    )
  )
}
