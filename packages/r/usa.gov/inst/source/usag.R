# usa.gov.R - Self-contained usa.gov client
# Wraps Federal Register executive orders API + analytics.usa.gov JSON API
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (all public data)
# Prefix: usag_

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


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
#' @param president President slug: "donald-trump", "joe-biden", "barack-obama",
#'   "george-w-bush", "william-j-clinton". Default NULL returns all.
#' @param per_page Results per page (max 1000). Default 100.
#' @param page Page number. Default 1.
#' @param year Optional signing year filter (integer).
#' @return tibble: eo_number, title, signing_date, publication_date,
#'   citation, document_number, president, html_url, pdf_url
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
#' @param president President slug (required)
#' @param year Optional signing year filter
#' @return tibble: all executive orders for the given president
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
#' @return tibble with one row: active_users, pageviews, taken_at
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
#' @return tibble: page_title, active_users
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
#' @return tibble: domain, visits
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
#' @return tibble: domain, visits, pageviews, users, pageviews_per_session, avg_session_duration
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
#' @return tibble: domain, visits
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
#' @return tibble: category, description, endpoint
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
#' @param query Search query string
#' @param president Optional president slug to narrow results
#' @param per_page Results per page. Default 20.
#' @return tibble: eo_number, title, signing_date, publication_date,
#'   citation, document_number, president, html_url, pdf_url
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

#' Generate LLM-friendly context for usa.gov
#'
#' @return Character string with full function signatures and bodies
usag_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/usa.gov.R"
  if (!file.exists(src_file)) {
    cat("# usa.gov context - source not found\n")
    return(invisible("# usa.gov context - source not found"))
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
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
