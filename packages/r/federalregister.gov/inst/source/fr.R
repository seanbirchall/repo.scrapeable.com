# federalregister.gov.R
# Self-contained Federal Register API client for R.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required. Public API.
# Docs: https://www.federalregister.gov/developers/api/v1
# Rate limits: not documented, be respectful.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.fr_base <- "https://www.federalregister.gov/api/v1"

.fr_fields <- paste0("fields%5B%5D=", c(
  "title", "type", "abstract", "document_number", "html_url", "pdf_url",
  "publication_date", "agencies", "citation", "signing_date", "start_page",
  "end_page", "executive_order_number", "subtype", "action", "dates",
  "docket_ids", "regulation_id_numbers", "excerpts"
), collapse = "&")

# -- Core fetch engine ---------------------------------------------------------

.fr_get <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

.fr_parse_doc <- function(r) {
  agencies <- paste(
    sapply(r$agencies %||% list(), function(a) a$name %||% a$raw_name %||% ""),
    collapse = "; "
  )
  tibble(
    document_number = as.character(r$document_number %||% NA),
    title           = as.character(r$title %||% NA),
    type            = as.character(r$type %||% NA),
    subtype         = as.character(r$subtype %||% NA),
    abstract        = as.character(r$abstract %||% NA),
    publication_date = as.Date(r$publication_date %||% NA),
    signing_date    = as.Date(r$signing_date %||% NA),
    citation        = as.character(r$citation %||% NA),
    start_page      = as.integer(r$start_page %||% NA),
    end_page        = as.integer(r$end_page %||% NA),
    executive_order_number = as.character(r$executive_order_number %||% NA),
    action          = as.character(r$action %||% NA),
    dates           = as.character(r$dates %||% NA),
    agencies        = agencies,
    docket_ids      = paste(unlist(r$docket_ids %||% list()), collapse = "; "),
    regulation_id_numbers = paste(unlist(r$regulation_id_numbers %||% list()), collapse = "; "),
    html_url        = as.character(r$html_url %||% NA),
    pdf_url         = as.character(r$pdf_url %||% NA)
  )
}

# -- Paginated fetch -----------------------------------------------------------

.fr_fetch_pages <- function(url, max_pages = 5) {
  all_rows <- list()
  current_url <- url
  page <- 0

  repeat {
    page <- page + 1
    if (page > max_pages) break

    raw <- tryCatch(.fr_get(current_url), error = function(e) {
      warning("FR API error: ", conditionMessage(e))
      return(NULL)
    })
    if (is.null(raw)) break

    results <- raw$results %||% list()
    if (length(results) == 0) break

    rows <- bind_rows(lapply(results, .fr_parse_doc))
    all_rows[[page]] <- rows

    next_url <- raw$next_page_url
    if (is.null(next_url) || !nzchar(next_url)) break
    current_url <- next_url
  }

  if (length(all_rows) == 0) {
    return(tibble(
      document_number = character(), title = character(), type = character(),
      subtype = character(), abstract = character(),
      publication_date = as.Date(character()), signing_date = as.Date(character()),
      citation = character(), start_page = integer(), end_page = integer(),
      executive_order_number = character(), action = character(),
      dates = character(), agencies = character(), docket_ids = character(),
      regulation_id_numbers = character(), html_url = character(),
      pdf_url = character()
    ))
  }
  bind_rows(all_rows)
}

# == Schema ====================================================================

.schema_fr_doc <- tibble(
  document_number = character(), title = character(), type = character(),
  subtype = character(), abstract = character(),
  publication_date = as.Date(character()), signing_date = as.Date(character()),
  citation = character(), start_page = integer(), end_page = integer(),
  executive_order_number = character(), action = character(),
  dates = character(), agencies = character(), docket_ids = character(),
  regulation_id_numbers = character(), html_url = character(),
  pdf_url = character()
)

.schema_fr_agency <- tibble(
  id = integer(), name = character(), short_name = character(),
  slug = character(), parent_id = integer(), description = character(),
  url = character(), agency_url = character(),
  recent_articles_url = character()
)

# == Public functions ==========================================================

#' Search Federal Register documents
#'
#' Search the Federal Register by keyword, document type, agency, date range,
#' and other criteria. Returns structured metadata for matching documents.
#'
#' @param term Search term (e.g. "climate change", "immigration")
#' @param type Document type: "RULE", "PRORULE", "NOTICE", "PRESDOCU", or NULL for all
#' @param agency_ids Integer vector of agency IDs to filter by
#' @param publication_date_gte Start date (YYYY-MM-DD) for publication date range
#' @param publication_date_lte End date (YYYY-MM-DD) for publication date range
#' @param per_page Results per page (default 20, max 200)
#' @param page Page number (default 1)
#' @param order Sort order: "relevance", "newest", "oldest", "executive_order" (default "newest")
#' @param max_pages Max pages to fetch (default 1; set higher for pagination)
#' @return tibble of Federal Register documents
fr_search <- function(term = NULL, type = NULL, agency_ids = NULL,
                      publication_date_gte = NULL, publication_date_lte = NULL,
                      per_page = 20, page = 1, order = "newest",
                      max_pages = 1) {
  params <- list()
  if (!is.null(term)) params[["conditions[term]"]] <- term
  if (!is.null(type)) params[["conditions[type]"]] <- type
  if (!is.null(publication_date_gte))
    params[["conditions[publication_date][gte]"]] <- publication_date_gte
  if (!is.null(publication_date_lte))
    params[["conditions[publication_date][lte]"]] <- publication_date_lte
  if (!is.null(agency_ids)) {
    for (aid in agency_ids) {
      params[[paste0("conditions[agency_ids][]")]] <- aid
    }
  }
  params[["per_page"]] <- min(per_page, 200)
  params[["page"]] <- page
  params[["order"]] <- order

  query <- paste(
    c(paste(names(params), sapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE)),
            sep = "="),
      .fr_fields),
    collapse = "&"
  )
  url <- paste0(.fr_base, "/documents.json?", query)
  .fr_fetch_pages(url, max_pages = max_pages)
}

#' Get a single Federal Register document by document number
#'
#' @param document_number The FR document number (e.g. "2026-06601")
#' @return tibble with one row of document metadata
fr_document <- function(document_number) {
  url <- paste0(.fr_base, "/documents/", document_number, ".json?", .fr_fields)
  raw <- tryCatch(.fr_get(url), error = function(e) {
    warning("FR API error: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(raw)) return(.schema_fr_doc[0, ])
  .fr_parse_doc(raw)
}

#' Get multiple Federal Register documents by document numbers
#'
#' @param document_numbers Character vector of document numbers
#' @return tibble of documents
fr_documents <- function(document_numbers) {
  nums <- paste(document_numbers, collapse = ",")
  url <- paste0(.fr_base, "/documents/", nums, ".json?", .fr_fields)
  raw <- tryCatch(.fr_get(url), error = function(e) {
    warning("FR API error: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(raw)) return(.schema_fr_doc[0, ])
  results <- raw$results %||% raw
  if (!is.list(results[[1]])) results <- list(results)
  bind_rows(lapply(results, .fr_parse_doc))
}

#' List all Federal Register agencies
#'
#' @return tibble: id, name, short_name, slug, parent_id, description, url, agency_url
fr_agencies <- function() {
  url <- paste0(.fr_base, "/agencies.json")
  raw <- tryCatch(.fr_get(url), error = function(e) {
    warning("FR API error: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_fr_agency)

  bind_rows(lapply(raw, function(a) {
    tibble(
      id          = as.integer(a$id %||% NA),
      name        = as.character(a$name %||% NA),
      short_name  = as.character(a$short_name %||% NA),
      slug        = as.character(a$slug %||% NA),
      parent_id   = as.integer(a$parent_id %||% NA),
      description = as.character(a$description %||% NA),
      url         = as.character(a$url %||% NA),
      agency_url  = as.character(a$agency_url %||% NA),
      recent_articles_url = as.character(a$recent_articles_url %||% NA)
    )
  }))
}

#' Get a single agency by slug or ID
#'
#' @param slug Agency slug (e.g. "environmental-protection-agency") or integer ID
#' @return tibble with one row of agency metadata
fr_agency <- function(slug) {
  url <- paste0(.fr_base, "/agencies/", slug, ".json")
  raw <- tryCatch(.fr_get(url), error = function(e) {
    warning("FR API error: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(raw)) return(.schema_fr_agency[0, ])
  tibble(
    id          = as.integer(raw$id %||% NA),
    name        = as.character(raw$name %||% NA),
    short_name  = as.character(raw$short_name %||% NA),
    slug        = as.character(raw$slug %||% NA),
    parent_id   = as.integer(raw$parent_id %||% NA),
    description = as.character(raw$description %||% NA),
    url         = as.character(raw$url %||% NA),
    agency_url  = as.character(raw$agency_url %||% NA),
    recent_articles_url = as.character(raw$recent_articles_url %||% NA)
  )
}

#' Search for Executive Orders
#'
#' Convenience wrapper for fr_search with type = "PRESDOCU" and
#' subtype filtering for executive orders.
#'
#' @param term Optional search term
#' @param president President slug (e.g. "donald-trump", "barack-obama", "joe-biden")
#' @param year Optional year to filter by
#' @param per_page Results per page (default 20, max 200)
#' @param max_pages Max pages to fetch (default 1)
#' @return tibble of executive orders
fr_executive_orders <- function(term = NULL, president = NULL, year = NULL,
                                per_page = 20, max_pages = 1) {
  params <- list()
  if (!is.null(term)) params[["conditions[term]"]] <- term
  params[["conditions[type]"]] <- "PRESDOCU"
  params[["conditions[presidential_document_type]"]] <- "executive_order"
  params[["conditions[correction]"]] <- "0"
  if (!is.null(president)) params[["conditions[president]"]] <- president
  if (!is.null(year)) params[["conditions[signing_date][year]"]] <- year
  params[["per_page"]] <- min(per_page, 200)
  params[["order"]] <- "executive_order"

  query <- paste(
    c(paste(names(params), sapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE)),
            sep = "="),
      .fr_fields),
    collapse = "&"
  )
  url <- paste0(.fr_base, "/documents.json?", query)
  .fr_fetch_pages(url, max_pages = max_pages)
}

#' Get current public inspection documents
#'
#' Returns documents currently available for public inspection before
#' official publication in the Federal Register.
#'
#' @return tibble: document_number, title, type, agencies, filing_type, filed_at,
#'   num_pages, html_url, pdf_url
fr_public_inspection <- function() {
  url <- paste0(.fr_base, "/public-inspection-documents/current.json")
  raw <- tryCatch(.fr_get(url), error = function(e) {
    warning("FR API error: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(raw)) {
    return(tibble(
      document_number = character(), title = character(), type = character(),
      agencies = character(), filing_type = character(), filed_at = character(),
      num_pages = integer(), html_url = character(), pdf_url = character()
    ))
  }

  results <- raw$results %||% list()
  if (length(results) == 0) {
    return(tibble(
      document_number = character(), title = character(), type = character(),
      agencies = character(), filing_type = character(), filed_at = character(),
      num_pages = integer(), html_url = character(), pdf_url = character()
    ))
  }

  bind_rows(lapply(results, function(r) {
    agencies <- paste(
      sapply(r$agencies %||% list(), function(a) a$name %||% a$raw_name %||% ""),
      collapse = "; "
    )
    tibble(
      document_number = as.character(r$document_number %||% NA),
      title           = as.character(r$title %||% NA),
      type            = as.character(r$type %||% NA),
      agencies        = agencies,
      filing_type     = as.character(r$filing_type %||% NA),
      filed_at        = as.character(r$filed_at %||% NA),
      num_pages       = as.integer(r$num_pages %||% NA),
      html_url        = as.character(r$html_url %||% NA),
      pdf_url         = as.character(r$pdf_url %||% NA)
    )
  }))
}

#' Search Federal Register rules (final rules)
#'
#' Convenience wrapper for searching final rules (regulations).
#'
#' @param term Search term
#' @param agency_ids Integer vector of agency IDs
#' @param publication_date_gte Start date (YYYY-MM-DD)
#' @param publication_date_lte End date (YYYY-MM-DD)
#' @param per_page Results per page (default 20)
#' @param max_pages Max pages to fetch (default 1)
#' @return tibble of final rules
fr_rules <- function(term = NULL, agency_ids = NULL,
                     publication_date_gte = NULL, publication_date_lte = NULL,
                     per_page = 20, max_pages = 1) {
  fr_search(term = term, type = "RULE", agency_ids = agency_ids,
            publication_date_gte = publication_date_gte,
            publication_date_lte = publication_date_lte,
            per_page = per_page, max_pages = max_pages)
}

#' Search Federal Register proposed rules
#'
#' Convenience wrapper for searching proposed rules.
#'
#' @param term Search term
#' @param agency_ids Integer vector of agency IDs
#' @param publication_date_gte Start date (YYYY-MM-DD)
#' @param publication_date_lte End date (YYYY-MM-DD)
#' @param per_page Results per page (default 20)
#' @param max_pages Max pages to fetch (default 1)
#' @return tibble of proposed rules
fr_proposed_rules <- function(term = NULL, agency_ids = NULL,
                              publication_date_gte = NULL,
                              publication_date_lte = NULL,
                              per_page = 20, max_pages = 1) {
  fr_search(term = term, type = "PRORULE", agency_ids = agency_ids,
            publication_date_gte = publication_date_gte,
            publication_date_lte = publication_date_lte,
            per_page = per_page, max_pages = max_pages)
}

# == Context ===================================================================

#' Generate LLM-friendly context for federalregister.gov
#'
#' @return Character string with full function signatures and bodies
fr_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/federalregister.gov.R"
  if (!file.exists(src_file)) {
    cat("# federalregister.gov context - source not found\n")
    return(invisible("# federalregister.gov context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth <= 0 && k > fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- paste(c(rox, body), collapse = "\n")
  }
  txt <- paste(c(
    "# federalregister.gov (Federal Register) R client",
    "# Functions:", paste("#  -", sapply(blocks, function(b) sub(" <- function.*", "", strsplit(b, "\n")[[1]][grep("<- function", strsplit(b, "\n")[[1]])[1]])), collapse = "\n"),
    "", paste(blocks, collapse = "\n\n")
  ), collapse = "\n")
  cat(txt)
  invisible(txt)
}
