# usa.gov.R - Self-contained usa.gov client
# Wraps Federal Register executive orders API + analytics.usa.gov JSON API
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (all public data)
# Prefix: usag_



# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

.usag_ua <- "usag-r-client/0.1 (support@scrapeable.com)"

.usag_fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .usag_ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE)
}

.usag_fr_base <- "https://www.federalregister.gov/api/v1"
.usag_analytics_base <- "https://analytics.usa.gov/data/live"

# == Schemas ===================================================================

.schema_eo <- tibble(
  eo_number = integer(),
  title = character(),
  signing_date = as.Date(character()),
  publication_date = as.Date(character()),

  citation = character(),
  document_number = character(),
  president = character(),
  html_url = character(),
  pdf_url = character()
)

.schema_top_pages <- tibble(
  page_title = character(),
  active_users = integer()
)

.schema_top_domains <- tibble(
  domain = character(),
  visits = numeric()
)

.schema_all_domains <- tibble(
  domain = character(),
  visits = numeric(),
  pageviews = numeric(),
  users = numeric(),
  pageviews_per_session = numeric(),
  avg_session_duration = numeric()
)

.schema_realtime <- tibble(
  active_users = integer(),
  pageviews = integer(),
  taken_at = character()
)

# == Private helpers ===========================================================

.usag_presidents <- c(
  "william-j-clinton", "george-w-bush", "barack-obama",
  "donald-trump", "joe-biden"
)

.parse_eo_results <- function(results, president = NA_character_) {
  if (is.null(results) || length(results) == 0) return(.schema_eo)
  if (!is.data.frame(results)) return(.schema_eo)
  if (nrow(results) == 0) return(.schema_eo)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      eo_number = as.integer(if ("executive_order_number" %in% nms) executive_order_number else NA_integer_),
      title = as.character(if ("title" %in% nms) title else NA_character_),
      signing_date = as.Date(if ("signing_date" %in% nms) signing_date else NA_character_),
      publication_date = as.Date(if ("publication_date" %in% nms) publication_date else NA_character_),
      citation = as.character(if ("citation" %in% nms) citation else NA_character_),
      document_number = as.character(if ("document_number" %in% nms) document_number else NA_character_),
      president = president,
      html_url = as.character(if ("html_url" %in% nms) html_url else NA_character_),
      pdf_url = as.character(if ("pdf_url" %in% nms) pdf_url else NA_character_)
    )
}

# == Public functions ==========================================================

#' List executive orders from the Federal Register
#'
#' Queries the Federal Register API for presidential executive orders.
#' Supports filtering by president and signing year. Returns metadata
#' including EO number, title, signing/publication dates, and links
#' to the full text (HTML and PDF).
#'
#' @param president Character. President slug for filtering. Valid values:
#'   "donald-trump", "joe-biden", "barack-obama", "george-w-bush",
#'   "william-j-clinton". Default \code{NULL} returns all presidents.
#' @param per_page Integer. Results per page (max 1000, default 100).
#' @param page Integer. Page number for pagination (default 1).
#' @param year Integer. Optional four-digit signing year filter (e.g. 2024).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{eo_number}{Integer. Executive order number.}
#'     \item{title}{Character. Title of the executive order.}
#'     \item{signing_date}{Date. Date the president signed the order.}
#'     \item{publication_date}{Date. Date published in the Federal Register.}
#'     \item{citation}{Character. Federal Register citation (e.g. "89 FR 12345").}
#'     \item{document_number}{Character. Federal Register document number.}
#'     \item{president}{Character. President slug or NA.}
#'     \item{html_url}{Character. URL to the full HTML text.}
#'     \item{pdf_url}{Character. URL to the official PDF.}
#'   }
#'   The result also carries attributes \code{count} (total matching results)
#'   and \code{total_pages} (number of available pages).
#'
#' @examples
#' \dontrun{
#' usag_executive_orders(president = "joe-biden", per_page = 10)
#' usag_executive_orders(year = 2024)
#' }
#'
#' @seealso [usag_all_executive_orders()], [usag_search()]
#' @export
usag_executive_orders <- function(president = NULL, per_page = 100, page = 1, year = NULL) {
  fields <- c("title", "executive_order_number", "signing_date",
               "publication_date", "citation", "document_number",
               "html_url", "pdf_url")
  field_params <- paste0("fields%5B%5D=", fields, collapse = "&")

  url <- sprintf(
    "%s/documents.json?conditions%%5Bpresidential_document_type%%5D=executive_order&conditions%%5Btype%%5D=PRESDOCU&%s&per_page=%d&page=%d&order=executive_order",
    .usag_fr_base, field_params, as.integer(min(per_page, 1000)), as.integer(page)
  )

  if (!is.null(president)) {
    url <- paste0(url, "&conditions%5Bpresident%5D=", utils::URLencode(president))
  }

  if (!is.null(year)) {
    url <- paste0(url, "&conditions%5Bsigning_date%5D%5Byear%5D=", as.integer(year))
  }

  raw <- tryCatch(.usag_fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_eo)

  pres_label <- president %||% NA_character_
  out <- .parse_eo_results(raw$results, president = pres_label)

  attr(out, "count") <- raw$count %||% NA_integer_
 attr(out, "total_pages") <- raw$total_pages %||% NA_integer_
  out
}

#' Get all executive orders for a president (auto-paginates)
#'
#' Fetches the complete set of executive orders for a given president
#' by automatically paginating through all available results. May take
#' several seconds for presidents with many orders.
#'
#' @param president Character. President slug (required). One of:
#'   "donald-trump", "joe-biden", "barack-obama", "george-w-bush",
#'   "william-j-clinton".
#' @param year Integer. Optional four-digit signing year filter.
#'
#' @return A tibble with the same columns as [usag_executive_orders()].
#'
#' @examples
#' \dontrun{
#' usag_all_executive_orders("barack-obama")
#' usag_all_executive_orders("donald-trump", year = 2017)
#' }
#'
#' @seealso [usag_executive_orders()]
#' @export
usag_all_executive_orders <- function(president, year = NULL) {
  page <- 1
  all_results <- list()

  repeat {
    chunk <- usag_executive_orders(president = president, per_page = 1000, page = page, year = year)
    if (nrow(chunk) == 0) break
    all_results[[page]] <- chunk
    total_pages <- attr(chunk, "total_pages") %||% 1
    if (page >= total_pages) break
    page <- page + 1
  }

  if (length(all_results) == 0) return(.schema_eo)
  bind_rows(all_results)
}

#' Real-time analytics for all U.S. government websites
#'
#' Returns a single-row snapshot of current traffic across all .gov
#' websites, powered by the Digital Analytics Program (DAP) at
#' analytics.usa.gov.
#'
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{active_users}{Integer. Number of users currently active across
#'       all .gov sites.}
#'     \item{pageviews}{Integer. Current pageview count.}
#'     \item{taken_at}{Character. ISO 8601 timestamp of the snapshot.}
#'   }
#'
#' @examples
#' \dontrun{
#' usag_realtime()
#' }
#'
#' @export
usag_realtime <- function() {
  url <- sprintf("%s/realtime.json", .usag_analytics_base)
  raw <- tryCatch(.usag_fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data)) return(.schema_realtime)

  d <- raw$data
  if (is.data.frame(d)) {
    au <- as.integer(d$activeUsers[1] %||% NA)
    pv <- as.integer(d$pageviews[1] %||% NA)
  } else if (is.list(d) && length(d) > 0) {
    au <- as.integer(d[[1]]$activeUsers %||% d[[1]][["activeUsers"]] %||% NA)
    pv <- as.integer(d[[1]]$pageviews %||% d[[1]][["pageviews"]] %||% NA)
  } else {
    return(.schema_realtime)
  }

  tibble(
    active_users = au,
    pageviews = pv,
    taken_at = as.character(raw$taken_at %||% NA_character_)
  )
}

#' Top pages being viewed right now on government websites
#'
#' Returns the most-visited .gov pages at this moment, as reported
#' by the Digital Analytics Program (DAP).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{page_title}{Character. Title of the page.}
#'     \item{active_users}{Integer. Number of users currently viewing this page.}
#'   }
#'
#' @examples
#' \dontrun{
#' usag_top_pages()
#' }
#'
#' @export
usag_top_pages <- function() {
  url <- sprintf("%s/top-pages-realtime.json", .usag_analytics_base)
  raw <- tryCatch(.usag_fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(.schema_top_pages)

  items <- raw$data
  tibble(
    page_title = as.character(items$page_title %||% NA_character_),
    active_users = as.integer(items$activeUsers %||% NA_integer_)
  )
}

#' Top government domains by visits (30-day window)
#'
#' Returns the most-visited .gov domains over the past 30 days,
#' ranked by total visits.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{domain}{Character. Government domain name.}
#'     \item{visits}{Numeric. Total visits in the 30-day window.}
#'   }
#'
#' @examples
#' \dontrun{
#' usag_top_domains()
#' }
#'
#' @export
usag_top_domains <- function() {
  url <- sprintf("%s/top-domains-30-days.json", .usag_analytics_base)
  raw <- tryCatch(.usag_fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(.schema_top_domains)

  items <- raw$data
  tibble(
    domain = as.character(items$domain %||% NA_character_),
    visits = as.numeric(items$visits %||% NA_real_)
  )
}

#' All government domains with traffic stats (30-day window)
#'
#' Returns detailed traffic metrics for all .gov domains over the past
#' 30 days, including visits, pageviews, unique users, and engagement.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{domain}{Character. Government domain name.}
#'     \item{visits}{Numeric. Total visits.}
#'     \item{pageviews}{Numeric. Total pageviews.}
#'     \item{users}{Numeric. Unique users.}
#'     \item{pageviews_per_session}{Numeric. Average pages per session.}
#'     \item{avg_session_duration}{Numeric. Average session duration in seconds.}
#'   }
#'
#' @examples
#' \dontrun{
#' usag_all_domains()
#' }
#'
#' @export
usag_all_domains <- function() {
  url <- sprintf("%s/all-domains-30-days.json", .usag_analytics_base)
  raw <- tryCatch(.usag_fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) return(.schema_all_domains)

  items <- raw$data
  tibble(
    domain = as.character(items$domain %||% NA_character_),
    visits = as.numeric(items$visits %||% NA_real_),
    pageviews = as.numeric(items$pageviews %||% NA_real_),
    users = as.numeric(items$users %||% NA_real_),
    pageviews_per_session = as.numeric(items$pageviews_per_session %||% NA_real_),
    avg_session_duration = as.numeric(items$avg_session_duration %||% NA_real_)
  )
}

#' Government sites with visit counts
#'
#' Returns all tracked .gov sites with their visit counts from the
#' Digital Analytics Program.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{domain}{Character. Government site domain.}
#'     \item{visits}{Numeric. Total visits.}
#'   }
#'
#' @examples
#' \dontrun{
#' usag_sites()
#' }
#'
#' @export
usag_sites <- function() {
  url <- sprintf("%s/sites.json", .usag_analytics_base)
  raw <- tryCatch(.usag_fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$data) || length(raw$data) == 0) {
    return(tibble(domain = character(), visits = numeric()))
  }

  items <- raw$data
  tibble(
    domain = as.character(items$domain %||% NA_character_),
    visits = as.numeric(items$visits %||% NA_real_)
  )
}

#' List available data categories
#'
#' Returns a catalog of all data sources available through this client,
#' including Federal Register executive orders and analytics.usa.gov
#' traffic data endpoints.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{category}{Character. Short identifier for the data category.}
#'     \item{description}{Character. Human-readable description.}
#'     \item{endpoint}{Character. API endpoint URL.}
#'   }
#'
#' @examples
#' usag_list()
#'
#' @export
usag_list <- function() {
  tibble(
    category = c("executive_orders", "realtime", "top_pages", "top_domains",
                  "all_domains", "sites"),
    description = c(
      "Executive orders from Federal Register (all presidents since Clinton)",
      "Real-time active users and pageviews across all .gov sites",
      "Top 30 pages being viewed right now",
      "Top 30 government domains by visits (30 days)",
      "All government domains with traffic metrics (30 days)",
      "All government sites with visit counts"
    ),
    endpoint = c(
      "federalregister.gov/api/v1/documents",
      "analytics.usa.gov/data/live/realtime.json",
      "analytics.usa.gov/data/live/top-pages-realtime.json",
      "analytics.usa.gov/data/live/top-domains-30-days.json",
      "analytics.usa.gov/data/live/all-domains-30-days.json",
      "analytics.usa.gov/data/live/sites.json"
    )
  )
}

#' Search executive orders by keyword
#'
#' Performs a full-text search across all executive orders in the Federal
#' Register, ranked by relevance. Optionally filter by president.
#'
#' @param query Character. Search query string (e.g. "climate",
#'   "immigration", "tariff").
#' @param president Character. Optional president slug to narrow results.
#' @param per_page Integer. Results per page (default 20).
#'
#' @return A tibble with the same columns as [usag_executive_orders()]:
#'   eo_number, title, signing_date, publication_date, citation,
#'   document_number, president, html_url, pdf_url.
#'
#' @examples
#' \dontrun{
#' usag_search("climate")
#' usag_search("immigration", president = "barack-obama")
#' }
#'
#' @seealso [usag_executive_orders()]
#' @export
usag_search <- function(query, president = NULL, per_page = 20) {
  fields <- c("title", "executive_order_number", "signing_date",
               "publication_date", "citation", "document_number",
               "html_url", "pdf_url")
  field_params <- paste0("fields%5B%5D=", fields, collapse = "&")

  url <- sprintf(
    "%s/documents.json?conditions%%5Bpresidential_document_type%%5D=executive_order&conditions%%5Btype%%5D=PRESDOCU&conditions%%5Bterm%%5D=%s&%s&per_page=%d&order=relevance",
    .usag_fr_base,
    utils::URLencode(query, reserved = TRUE),
    field_params,
    as.integer(per_page)
  )

  if (!is.null(president)) {
    url <- paste0(url, "&conditions%5Bpresident%5D=", utils::URLencode(president))
  }

  raw <- tryCatch(.usag_fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_eo)

  pres_label <- president %||% NA_character_
  .parse_eo_results(raw$results, president = pres_label)
}

#' Get usa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
usag_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(usag_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/usa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "usa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# usa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# usa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
