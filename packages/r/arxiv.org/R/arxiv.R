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
