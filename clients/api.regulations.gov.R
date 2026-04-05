# api.regulations.gov.R
# Self-contained api.regulations.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# regulations-gov.R
# Self-contained Regulations.gov API v4 client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required. DEMO_KEY works for testing (limited rate).
#   Get a key at https://api.data.gov/signup/
# Rate limits: DEMO_KEY = 30/hr. Real key = 1000/hr.
# Docs: https://open.gsa.gov/api/regulationsgov/


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.regs_base <- "https://api.regulations.gov/v4"
# -- Fetch helper (JSON:API format) --------------------------------------------

.regs_get <- function(endpoint, params = list(), api_key = "DEMO_KEY",
                      max_results = NULL) {
  params$api_key <- api_key
  if (is.null(params[["page[size]"]])) params[["page[size]"]] <- min(max_results %||% 250, 250)
  if (is.null(params[["page[number]"]])) params[["page[number]"]] <- 1

  all_data <- list()
  repeat {
    query <- paste(names(params),
                   vapply(params, function(v) utils::URLencode(as.character(v), reserved = FALSE),
                          character(1)),
                   sep = "=", collapse = "&")
    url <- paste0(.regs_base, "/", endpoint, "?", query)

    tmp <- tempfile(fileext = ".json")
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_retry(max_tries = 2, backoff = function(...) 10) |>
      httr2::req_perform(path = tmp)
    raw <- jsonlite::fromJSON(tmp)

    data <- raw$data
    if (is.null(data) || length(data) == 0 || nrow(data) == 0) break

    # Flatten JSON:API structure: merge id + attributes
    df <- as_tibble(data$attributes)
    df$id <- data$id
    df$type <- data$type
    all_data[[length(all_data) + 1]] <- df

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break

    has_next <- raw$meta$hasNextPage %||% FALSE
    if (!has_next) break
    params[["page[number]"]] <- params[["page[number]"]] + 1
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)

  # Type date columns
  for (col in names(result)) {
    if (grepl("date|Date", col) && is.character(result[[col]])) {
      vals <- result[[col]]
      is_datelike <- grepl("^\\d{4}-\\d{2}-\\d{2}", vals) & !is.na(vals)
      if (sum(is_datelike) > length(vals) * 0.5)
        result[[col]] <- as.Date(substr(vals, 1, 10))
    }
  }
  result
}



# == Documents =================================================================

#' Search federal regulatory documents
#'
#' Searches proposed rules, final rules, notices, and other documents
#' published in the Federal Register via the Regulations.gov API v4.
#' Each row is one document from the federal rulemaking process.
#' Supports filtering by agency, document type, date range, and docket.
#' Use \code{regs_document()} for full details on a single document,
#' or \code{regs_comments()} to find public comments on a document.
#' Handles pagination automatically up to \code{max_results}.
#'
#' @param query Character or NULL. Free-text search term, e.g.
#'   \code{"clean water"}, \code{"emissions standards"},
#'   \code{"pharmaceutical pricing"}. Default NULL (no text filter).
#' @param agency Character or NULL. Federal agency acronym, e.g.
#'   \code{"EPA"} (Environmental Protection Agency), \code{"FDA"} (Food
#'   and Drug Administration), \code{"DOL"} (Dept. of Labor),
#'   \code{"SEC"} (Securities and Exchange Commission). Default NULL.
#' @param document_type Character or NULL. One of \code{"Rule"},
#'   \code{"Proposed Rule"}, \code{"Notice"}, or \code{"Other"}.
#'   Default NULL (all types).
#' @param posted_date_from Character or NULL. Start date in
#'   \code{"YYYY-MM-DD"} format. Default NULL.
#' @param posted_date_to Character or NULL. End date in
#'   \code{"YYYY-MM-DD"} format. Default NULL.
#' @param docket_id Character or NULL. Docket ID to filter to, e.g.
#'   \code{"EPA-HQ-OAR-2021-0208"}. Default NULL.
#' @param api_key Character. API key from \url{https://api.data.gov/signup/}.
#'   Default \code{"DEMO_KEY"} (30 requests/hr limit).
#' @param max_results Integer. Maximum documents to return, default
#'   \code{100}. Pagination is handled automatically.
#' @return A tibble with one row per document. Common columns include:
#'   \describe{
#'     \item{id}{\code{character} -- Document ID (e.g. "EPA-HQ-OAR-2021-0208-0001")}
#'     \item{title}{\code{character} -- Document title}
#'     \item{agencyId}{\code{character} -- Agency acronym}
#'     \item{documentType}{\code{character} -- "Rule", "Proposed Rule", "Notice", or "Other"}
#'     \item{postedDate}{\code{Date} -- Date posted to Regulations.gov}
#'     \item{commentEndDate}{\code{Date} -- Comment period deadline, or NA}
#'     \item{frDocNum}{\code{character} -- Federal Register document number}
#'     \item{type}{\code{character} -- JSON:API resource type}
#'   }
#'   Additional attribute columns may be present depending on the document.
#' @examples
#' \dontrun{
#' # Search EPA documents about clean water
#' regs_documents(query = "clean water", agency = "EPA", max_results = 20)
#'
#' # Recent proposed rules from FDA
#' regs_documents(agency = "FDA", document_type = "Proposed Rule",
#'                posted_date_from = "2025-01-01", max_results = 50)
#'
#' # All documents in a specific docket
#' regs_documents(docket_id = "EPA-HQ-OAR-2021-0208")
#' }
regs_documents <- function(query = NULL, agency = NULL, document_type = NULL,
                           posted_date_from = NULL, posted_date_to = NULL,
                           docket_id = NULL, api_key = "DEMO_KEY",
                           max_results = 100) {
  params <- list()
  if (!is.null(query))            params[["filter[searchTerm]"]] <- query
  if (!is.null(agency))           params[["filter[agencyId]"]] <- agency
  if (!is.null(document_type))    params[["filter[documentType]"]] <- document_type
  if (!is.null(posted_date_from)) params[["filter[postedDate][ge]"]] <- posted_date_from
  if (!is.null(posted_date_to))   params[["filter[postedDate][le]"]] <- posted_date_to
  if (!is.null(docket_id))        params[["filter[docketId]"]] <- docket_id
  .regs_get("documents", params, api_key, max_results)
}

#' Fetch a single regulatory document by ID
#'
#' Returns full metadata for one document, including fields not present
#' in search results (e.g. full abstract, CFR references, RIN). Use
#' \code{regs_documents()} to discover document IDs.
#'
#' @param document_id Character. Document ID from \code{regs_documents()},
#'   e.g. \code{"EPA-HQ-OAR-2021-0208-0001"}.
#' @param api_key Character. API key, default \code{"DEMO_KEY"}.
#' @return A single-row tibble with all document attributes. Includes
#'   the same columns as \code{regs_documents()} plus additional detail
#'   fields like \code{abstract}, \code{rin}, \code{cfrPart}, etc.
#' @examples
#' \dontrun{
#' # Get full details for a specific document
#' regs_document("EPA-HQ-OAR-2021-0208-0001")
#'
#' # Retrieve and inspect abstract
#' doc <- regs_document("FDA-2023-N-0001-0001")
#' doc$abstract
#' }
regs_document <- function(document_id, api_key = "DEMO_KEY") {
  url <- sprintf("%s/documents/%s?api_key=%s", .regs_base, document_id, api_key)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp)
  if (is.null(raw$data)) return(tibble())
  df <- as_tibble(as.list(raw$data$attributes))
  df$id <- raw$data$id
  df
}


# == Comments ==================================================================

#' Search public comments on regulations
#'
#' Searches public comments submitted during notice-and-comment
#' rulemaking. Filter by agency, specific document, date range, or
#' keyword. Comments are the primary channel for public input on
#' federal regulations. Use \code{regs_comment()} to fetch full
#' comment text for a specific comment ID.
#'
#' @param query Character or NULL. Free-text search within comment
#'   content, e.g. \code{"safety concerns"}, \code{"small business"}.
#'   Default NULL.
#' @param agency Character or NULL. Agency acronym filter, e.g.
#'   \code{"EPA"}, \code{"FDA"}, \code{"FCC"}. Default NULL.
#' @param document_id Character or NULL. Filter to comments on a
#'   specific document ID (from \code{regs_documents()$id}).
#'   Default NULL.
#' @param posted_date_from Character or NULL. Start date
#'   \code{"YYYY-MM-DD"}. Default NULL.
#' @param posted_date_to Character or NULL. End date
#'   \code{"YYYY-MM-DD"}. Default NULL.
#' @param api_key Character. API key, default \code{"DEMO_KEY"}.
#' @param max_results Integer. Maximum comments to return, default
#'   \code{100}. Pagination is automatic.
#' @return A tibble with one row per comment. Common columns include:
#'   \describe{
#'     \item{id}{\code{character} -- Comment ID}
#'     \item{title}{\code{character} -- Comment title / subject line}
#'     \item{agencyId}{\code{character} -- Agency acronym}
#'     \item{postedDate}{\code{Date} -- Date the comment was posted}
#'     \item{type}{\code{character} -- JSON:API resource type}
#'   }
#'   Additional columns vary by comment.
#' @examples
#' \dontrun{
#' # EPA comments mentioning "clean water"
#' regs_comments(query = "clean water", agency = "EPA", max_results = 20)
#'
#' # All comments on a specific proposed rule
#' regs_comments(document_id = "EPA-HQ-OAR-2021-0208-0001", max_results = 50)
#'
#' # Recent FCC comments
#' regs_comments(agency = "FCC", posted_date_from = "2025-01-01")
#' }
regs_comments <- function(query = NULL, agency = NULL, document_id = NULL,
                          posted_date_from = NULL, posted_date_to = NULL,
                          api_key = "DEMO_KEY", max_results = 100) {
  params <- list()
  if (!is.null(query))            params[["filter[searchTerm]"]] <- query
  if (!is.null(agency))           params[["filter[agencyId]"]] <- agency
  if (!is.null(document_id))      params[["filter[commentOnId]"]] <- document_id
  if (!is.null(posted_date_from)) params[["filter[postedDate][ge]"]] <- posted_date_from
  if (!is.null(posted_date_to))   params[["filter[postedDate][le]"]] <- posted_date_to
  .regs_get("comments", params, api_key, max_results)
}

#' Fetch a single public comment by ID
#'
#' Returns full details for one comment, including the complete comment
#' text which is not always present in search results. Use
#' \code{regs_comments()} to discover comment IDs.
#'
#' @param comment_id Character. Comment ID from \code{regs_comments()$id}.
#' @param api_key Character. API key, default \code{"DEMO_KEY"}.
#' @return A single-row tibble with all comment attributes, including
#'   \code{comment} (full text), \code{firstName}, \code{lastName},
#'   \code{organization}, \code{postedDate}, and other fields.
#' @examples
#' \dontrun{
#' # Fetch full comment text
#' c <- regs_comment("EPA-HQ-OAR-2021-0208-0500")
#' cat(c$comment)
#' }
regs_comment <- function(comment_id, api_key = "DEMO_KEY") {
  url <- sprintf("%s/comments/%s?api_key=%s", .regs_base, comment_id, api_key)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp)
  if (is.null(raw$data)) return(tibble())
  df <- as_tibble(as.list(raw$data$attributes))
  df$id <- raw$data$id
  df
}


# == Dockets ===================================================================

#' Search regulatory dockets
#'
#' A docket is a folder-like container grouping all documents and
#' public comments for a single rulemaking proceeding. Use this
#' function to discover dockets by agency or keyword, then drill
#' into specific dockets with \code{regs_docket()} or find their
#' documents with \code{regs_documents(docket_id = ...)}.
#'
#' @param query Character or NULL. Free-text search, e.g.
#'   \code{"emissions"}, \code{"pharmaceutical"}. Default NULL.
#' @param agency Character or NULL. Agency acronym filter, e.g.
#'   \code{"EPA"}, \code{"FDA"}, \code{"DOT"}. Default NULL.
#' @param docket_type Character or NULL. One of \code{"Rulemaking"}
#'   or \code{"Nonrulemaking"}. Default NULL (both types).
#' @param api_key Character. API key, default \code{"DEMO_KEY"}.
#' @param max_results Integer. Maximum dockets to return, default
#'   \code{50}. Pagination is automatic.
#' @return A tibble with one row per docket. Common columns include:
#'   \describe{
#'     \item{id}{\code{character} -- Docket ID (e.g. "EPA-HQ-OAR-2021-0208")}
#'     \item{title}{\code{character} -- Docket title}
#'     \item{agencyId}{\code{character} -- Agency acronym}
#'     \item{docketType}{\code{character} -- "Rulemaking" or "Nonrulemaking"}
#'     \item{type}{\code{character} -- JSON:API resource type}
#'   }
#'   Additional attribute columns may be present.
#' @examples
#' \dontrun{
#' # Search EPA dockets about emissions
#' regs_dockets(query = "emissions", agency = "EPA", max_results = 10)
#'
#' # Find FDA rulemaking dockets
#' regs_dockets(agency = "FDA", docket_type = "Rulemaking")
#'
#' # Browse recent DOT dockets
#' regs_dockets(agency = "DOT", max_results = 20)
#' }
regs_dockets <- function(query = NULL, agency = NULL, docket_type = NULL,
                         api_key = "DEMO_KEY", max_results = 50) {
  params <- list()
  if (!is.null(query))       params[["filter[searchTerm]"]] <- query
  if (!is.null(agency))      params[["filter[agencyId]"]] <- agency
  if (!is.null(docket_type)) params[["filter[docketType]"]] <- docket_type
  .regs_get("dockets", params, api_key, max_results)
}

#' Fetch a single regulatory docket by ID
#'
#' Returns full metadata for one docket, which groups all documents
#' and comments for a rulemaking. Includes fields not in search
#' results like category, subtype, and program information.
#'
#' @param docket_id Character. Docket ID, e.g.
#'   \code{"EPA-HQ-OAR-2021-0208"}. Find IDs via \code{regs_dockets()}.
#' @param api_key Character. API key, default \code{"DEMO_KEY"}.
#' @return A single-row tibble with all docket attributes including
#'   \code{title}, \code{agencyId}, \code{docketType}, \code{category},
#'   \code{subType}, and other metadata fields.
#' @examples
#' \dontrun{
#' # Get full docket details
#' regs_docket("EPA-HQ-OAR-2021-0208")
#'
#' # Check docket type and category
#' d <- regs_docket("FDA-2023-N-0001")
#' d$docketType
#' }
regs_docket <- function(docket_id, api_key = "DEMO_KEY") {
  url <- sprintf("%s/dockets/%s?api_key=%s", .regs_base, docket_id, api_key)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp)
  if (is.null(raw$data)) return(tibble())
  df <- as_tibble(as.list(raw$data$attributes))
  df$id <- raw$data$id
  df
}


# == Context ===================================================================

#' Get api.regulations.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
regs_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(regs_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.regulations.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.regulations.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.regulations.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.regulations.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
