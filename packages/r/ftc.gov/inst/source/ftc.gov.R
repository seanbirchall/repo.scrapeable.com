# ftc.gov.R
# Self-contained Federal Trade Commission API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required. Register at https://api.ftc.gov
#   DEMO_KEY works for testing (10 req/min).
# Docs: https://api.ftc.gov
#
# Available endpoint: HSR Early Termination Notices (JSON:API)
# ~27K records of Hart-Scott-Rodino merger review early terminations.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.ftc_base <- "https://api.ftc.gov/v0"

# -- Core JSON:API fetch engine ------------------------------------------------

.ftc_get <- function(endpoint, api_key = NULL, page_size = 50,
                     page_offset = 0, sort = NULL, filter = NULL) {
  key <- api_key %||% Sys.getenv("FTC_API_KEY", "DEMO_KEY")
  url <- paste0(.ftc_base, "/", endpoint)

  params <- list(
    api_key = key,
    `page[limit]` = min(page_size, 50),
    `page[offset]` = page_offset
  )
  if (!is.null(sort)) params[["sort[sort-field][path]"]] <- sort

  req <- httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30)

  tmp <- tempfile(fileext = ".json")
  resp <- httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# Parse HSR records from JSON:API response
.ftc_parse_hsr <- function(data_list) {
  if (length(data_list) == 0) return(tibble())
  bind_rows(lapply(data_list, function(rec) {
    a <- rec$attributes
    tibble(
      id               = rec$id %||% NA_character_,
      title            = a$title %||% NA_character_,
      transaction_number = a$`transaction-number` %||% NA_character_,
      acquiring_party  = a$`acquiring-party` %||% NA_character_,
      acquired_party   = a$`acquired-party` %||% NA_character_,
      acquired_entities = paste(a$`acquired-entities` %||% list(), collapse = "; "),
      date             = as.Date(a$date %||% NA_character_),
      created          = a$created %||% NA_character_,
      updated          = a$updated %||% NA_character_
    )
  }))
}

# Paginated fetch
.ftc_fetch_all <- function(endpoint, api_key = NULL, max_results = 100,
                           sort = NULL) {
  all_data <- list()
  offset <- 0
  page_size <- min(max_results, 50)

  repeat {
    raw <- .ftc_get(endpoint, api_key = api_key, page_size = page_size,
                    page_offset = offset, sort = sort)
    recs <- raw$data
    if (is.null(recs) || length(recs) == 0) break

    all_data <- c(all_data, recs)
    offset <- offset + length(recs)

    total <- raw$meta$count %||% 0
    if (offset >= total) break
    if (length(all_data) >= max_results) break
    if (length(recs) < page_size) break
    Sys.sleep(0.5)
  }

  if (length(all_data) > max_results) all_data <- all_data[seq_len(max_results)]
  all_data
}

# -- Context generator ---------------------------------------------------------

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

# == Public functions ==========================================================

#' List available FTC API datasets
#'
#' Returns a single-row tibble describing the available FTC API endpoints,
#' their record counts, and authentication requirements.
#'
#' @return A tibble with columns: endpoint, description, total_records, auth,
#'   rate_limit.
#' @examples
#' ftc_list()
#' @export
ftc_list <- function() {
  tibble(
    endpoint = "hsr-early-termination-notices",
    description = "Hart-Scott-Rodino Early Termination Notices",
    total_records = "~27,000",
    auth = "API key (DEMO_KEY for testing, register at api.ftc.gov)",
    rate_limit = "10 requests/minute with DEMO_KEY"
  )
}

#' Search FTC HSR Early Termination Notices
#'
#' Search Hart-Scott-Rodino merger review early termination notices.
#' These are public records of transactions where the FTC/DOJ granted
#' early termination of the HSR waiting period. Approximately 27,000
#' records are available. Results are fetched via pagination and then
#' filtered client-side by query.
#'
#' @param query Character. Optional search text to match against the title
#'   field (acquiring and acquired party names). Case-insensitive regex.
#' @param max_results Integer. Maximum results to fetch before filtering
#'   (default 50).
#' @param api_key Character. FTC API key. Defaults to DEMO_KEY or the
#'   \code{FTC_API_KEY} environment variable.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Record UUID}
#'     \item{title}{Transaction title with party names}
#'     \item{transaction_number}{HSR transaction number}
#'     \item{acquiring_party}{Name of the acquiring party}
#'     \item{acquired_party}{Name of the acquired party}
#'     \item{acquired_entities}{Semicolon-separated acquired entities}
#'     \item{date}{Notice date}
#'     \item{created, updated}{Timestamps}
#'   }
#' @examples
#' # Recent HSR notices (no filter)
#' ftc_search(max_results = 5)
#'
#' # Search for a specific company
#' ftc_search("Microsoft", max_results = 100)
#' @export
ftc_search <- function(query = NULL, max_results = 50, api_key = NULL) {
  data <- .ftc_fetch_all("hsr-early-termination-notices", api_key = api_key,
                         max_results = max_results)
  result <- .ftc_parse_hsr(data)

  if (!is.null(query) && nrow(result) > 0) {
    result <- result |>
      filter(grepl(!!query, .data$title, ignore.case = TRUE))
  }
  result
}

#' Get recent FTC HSR Early Termination Notices
#'
#' Returns the most recent HSR early termination notices, ordered by
#' recency. Quick way to see the latest merger filings granted early
#' termination.
#'
#' @param n Integer. Number of recent notices to return (default 20, max 50).
#' @param api_key Character. FTC API key (defaults to DEMO_KEY).
#' @return A tibble with the same columns as \code{\link{ftc_search}}.
#' @examples
#' ftc_hsr_recent(n = 5)
#' @export
ftc_hsr_recent <- function(n = 20, api_key = NULL) {
  raw <- .ftc_get("hsr-early-termination-notices", api_key = api_key,
                  page_size = min(n, 50))
  .ftc_parse_hsr(raw$data) |> head(n)
}

#' Get FTC HSR notice by transaction number
#'
#' Looks up a specific HSR early termination notice by its transaction
#' number. Fetches a batch of records and filters client-side.
#'
#' @param transaction_number Character. The HSR transaction number
#'   (e.g. "20261054").
#' @param api_key Character. FTC API key (defaults to DEMO_KEY).
#' @return A tibble (usually 1 row) with the same columns as
#'   \code{\link{ftc_search}}.
#' @examples
#' ftc_hsr_transaction("20261054")
#' @export
ftc_hsr_transaction <- function(transaction_number, api_key = NULL) {
  # Fetch a batch and filter by transaction number
  data <- .ftc_fetch_all("hsr-early-termination-notices", api_key = api_key,
                         max_results = 200)
  result <- .ftc_parse_hsr(data)
  result |> filter(.data$transaction_number == !!transaction_number)
}

#' Get FTC HSR notice count
#'
#' Returns the total number of HSR early termination notices in the FTC
#' database. Useful for monitoring growth or verifying pagination.
#'
#' @param api_key Character. FTC API key (defaults to DEMO_KEY).
#' @return Integer total count of HSR early termination notices.
#' @examples
#' ftc_hsr_count()
#' @export
ftc_hsr_count <- function(api_key = NULL) {
  raw <- .ftc_get("hsr-early-termination-notices", api_key = api_key,
                  page_size = 1)
  raw$meta$count %||% 0L
}

#' Get ftc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ftc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ftc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ftc.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ftc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ftc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ftc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
