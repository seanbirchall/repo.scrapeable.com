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
  if (is.null(params[["page[size]"]])) params[["page[size]"]] <- max(5, min(max_results %||% 250, 250))
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
#' published in the Federal Register via the Regulations.gov API (v4).
#' Supports filtering by agency, document type, date range, and docket.
#' Handles pagination automatically. Requires an API key (the default
#' \code{DEMO_KEY} is rate-limited to 30 requests/hour).
#'
#' @param query Character or \code{NULL}. Free-text search term
#'   (e.g., \code{"clean water"}, \code{"emissions"}).
#' @param agency Character or \code{NULL}. Agency acronym filter
#'   (e.g., \code{"EPA"}, \code{"FDA"}, \code{"DOL"}, \code{"SEC"}).
#' @param document_type Character or \code{NULL}. One of \code{"Rule"},
#'   \code{"Proposed Rule"}, \code{"Notice"}, or \code{"Other"}.
#' @param posted_date_from Character or \code{NULL}. Start date in
#'   \code{"YYYY-MM-DD"} format.
#' @param posted_date_to Character or \code{NULL}. End date in
#'   \code{"YYYY-MM-DD"} format.
#' @param docket_id Character or \code{NULL}. Docket ID filter
#'   (e.g., \code{"EPA-HQ-OAR-2021-0208"}).
#' @param api_key Character. API key (default \code{"DEMO_KEY"}).
#'   Register at \url{https://api.data.gov/signup/} for higher limits.
#' @param max_results Integer. Maximum results to return (default 100).
#'   Minimum page size is 5.
#' @return A tibble with columns including \code{id}, \code{title},
#'   \code{agencyId}, \code{documentType}, \code{postedDate},
#'   \code{commentEndDate}, \code{frDocNum}, and others.
#'   Date columns are auto-converted to Date class.
#' @examples
#' regs_documents(query = "water", agency = "EPA", max_results = 10)
#' @export
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
#' Retrieves full details for a specific document from Regulations.gov,
#' including the full title, abstract, agency, dates, and Federal Register
#' document number.
#'
#' @param document_id Character. Document ID as returned in the \code{id}
#'   column of \code{regs_documents()}.
#' @param api_key Character. API key (default \code{"DEMO_KEY"}).
#' @return A tibble with one row containing all available document attributes.
#' @seealso \code{\link{regs_documents}} to search for document IDs.
#' @export
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
#' Searches the public comment database on Regulations.gov. Comments are
#' submitted by individuals, organizations, and agencies during the
#' notice-and-comment rulemaking process. Supports filtering by search text,
#' agency, specific document, and date range.
#'
#' @param query Character or \code{NULL}. Free-text search term.
#' @param agency Character or \code{NULL}. Agency acronym filter
#'   (e.g., \code{"EPA"}, \code{"FDA"}).
#' @param document_id Character or \code{NULL}. Restrict to comments on a
#'   specific document ID.
#' @param posted_date_from Character or \code{NULL}. Start date
#'   (\code{"YYYY-MM-DD"}).
#' @param posted_date_to Character or \code{NULL}. End date
#'   (\code{"YYYY-MM-DD"}).
#' @param api_key Character. API key (default \code{"DEMO_KEY"}).
#' @param max_results Integer. Maximum results to return (default 100).
#' @return A tibble with columns including \code{id}, \code{title},
#'   \code{agencyId}, \code{postedDate}, and comment attributes.
#' @examples
#' regs_comments(query = "pollution", agency = "EPA", max_results = 10)
#' @export
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

#' Fetch a single comment by ID
#'
#' Retrieves full details for a specific public comment, including the
#' comment text body, submitter information, and associated document.
#'
#' @param comment_id Character. Comment ID as returned in the \code{id}
#'   column of \code{regs_comments()}.
#' @param api_key Character. API key (default \code{"DEMO_KEY"}).
#' @return A tibble with one row containing all available comment attributes
#'   including the comment text.
#' @seealso \code{\link{regs_comments}} to search for comment IDs.
#' @export
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
#' Searches for dockets on Regulations.gov. A docket is a folder that groups
#' all documents, supporting materials, and public comments related to a
#' single rulemaking or non-rulemaking action.
#'
#' @param query Character or \code{NULL}. Free-text search term.
#' @param agency Character or \code{NULL}. Agency acronym filter
#'   (e.g., \code{"EPA"}).
#' @param docket_type Character or \code{NULL}. Either \code{"Rulemaking"}
#'   or \code{"Nonrulemaking"}.
#' @param api_key Character. API key (default \code{"DEMO_KEY"}).
#' @param max_results Integer. Maximum results to return (default 50).
#' @return A tibble with columns including \code{id}, \code{title},
#'   \code{agencyId}, \code{docketType}, and others.
#' @examples
#' regs_dockets(query = "air quality", agency = "EPA", max_results = 5)
#' @export
regs_dockets <- function(query = NULL, agency = NULL, docket_type = NULL,
                         api_key = "DEMO_KEY", max_results = 50) {
  params <- list()
  if (!is.null(query))       params[["filter[searchTerm]"]] <- query
  if (!is.null(agency))      params[["filter[agencyId]"]] <- agency
  if (!is.null(docket_type)) params[["filter[docketType]"]] <- docket_type
  .regs_get("dockets", params, api_key, max_results)
}

#' Fetch a single docket by ID
#'
#' Retrieves full metadata for a specific regulatory docket, including
#' title, agency, type, and related identifiers.
#'
#' @param docket_id Character. Docket ID (e.g.,
#'   \code{"EPA-HQ-OAR-2021-0208"}).
#' @param api_key Character. API key (default \code{"DEMO_KEY"}).
#' @return A tibble with one row containing all available docket attributes.
#' @seealso \code{\link{regs_dockets}} to search for docket IDs.
#' @export
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

#' Get regulations.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/regulations.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "regulations.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# regulations.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# regulations.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
