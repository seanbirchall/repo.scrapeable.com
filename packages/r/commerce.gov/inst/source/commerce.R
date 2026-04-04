# commerce.gov.R - Self-contained Department of Commerce API client
#
# Data sources:
#   - Enterprise Data Inventory (Project Open Data catalog, 1964 datasets)
#   - CIO Governance Board Membership List
#   - IT Cost Savings & Avoidance
#   - Bureau IT Leadership Directory
#
# Note: USPTO patent text endpoints (Socrata) are defunct (404).
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.base_catalog   <- "https://www.commerce.gov/sites/default/files/data.json"
.base_governance <- "https://www.commerce.gov/sites/default/files/2018-11/governanceboards.json"
.base_savings    <- "https://www.commerce.gov/sites/default/files/costsavings.json"
.base_leadership <- "https://www.commerce.gov/sites/default/files/bureaudirectory.json"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(seconds = 120) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

.safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.list(x)) return(paste(unlist(x), collapse = "; "))
  as.character(x)[1]
}

.safe_date <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(as.Date(NA))
  # Try ISO date first

  d <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (!is.na(d)) return(d)
  # Try MM/DD/YYYY

  tryCatch(as.Date(x, format = "%m/%d/%Y"), error = function(e) as.Date(NA))
}

.extract_publisher <- function(pub) {
  if (is.null(pub)) return(NA_character_)
  .safe_chr(pub$name)
}

.extract_distribution_urls <- function(dist) {
  if (is.null(dist) || length(dist) == 0) return(NA_character_)
  urls <- vapply(dist, function(d) {
    .safe_chr(d$downloadURL %||% d$accessURL)
  }, character(1))
  paste(urls[!is.na(urls)], collapse = "; ")
}

.extract_distribution_formats <- function(dist) {
  if (is.null(dist) || length(dist) == 0) return(NA_character_)
  fmts <- vapply(dist, function(d) .safe_chr(d$format %||% d$mediaType), character(1))
  paste(unique(fmts[!is.na(fmts)]), collapse = "; ")
}

# == Schemas ===================================================================

.schema_catalog <- tibble(
  title       = character(),
  description = character(),
  publisher   = character(),
  keywords    = character(),
  modified    = as.Date(character()),
  identifier  = character(),
  access_level = character(),
  format      = character(),
  access_url  = character(),
  distribution_urls = character(),
  landing_page = character(),
  bureau_code = character(),
  license     = character(),
  temporal    = character(),
  spatial     = character()
)

.schema_governance <- tibble(
  board_name   = character(),
  bureau_code  = character(),
  program_code = character(),
  description  = character()
)

.schema_savings <- tibble(
  strategy_id    = integer(),
  strategy_title = character(),
  decision_date  = as.Date(character()),
  omb_initiative = character(),
  amount_type    = character(),
  fy2012 = numeric(), fy2013 = numeric(), fy2014 = numeric(),
  fy2015 = numeric(), fy2016 = numeric(), fy2017 = numeric(),
  fy2018 = numeric(), fy2019 = numeric(), fy2020 = numeric()
)

.schema_leadership <- tibble(
  bureau_code     = character(),
  first_name      = character(),
  last_name       = character(),
  employment_type = character(),
  appointment     = character(),
  responsibilities = character(),
  rating_official  = character(),
  review_official  = character(),
  key_bureau_cio   = logical()
)

# == Internal parsers ==========================================================

.parse_catalog <- function(raw) {
  datasets <- raw$dataset
  if (is.null(datasets) || length(datasets) == 0) return(.schema_catalog)

  rows <- lapply(datasets, function(d) {
    tibble(
      title       = .safe_chr(d$title),
      description = .safe_chr(d$description),
      publisher   = .extract_publisher(d$publisher),
      keywords    = .safe_chr(d$keyword),
      modified    = .safe_date(.safe_chr(d$modified)),
      identifier  = .safe_chr(d$identifier),
      access_level = .safe_chr(d$accessLevel),
      format      = .safe_chr(d$format) %||% .extract_distribution_formats(d$distribution),
      access_url  = .safe_chr(d$accessURL),
      distribution_urls = .extract_distribution_urls(d$distribution),
      landing_page = .safe_chr(d$landingPage),
      bureau_code = .safe_chr(d$bureauCode),
      license     = .safe_chr(d$license),
      temporal    = .safe_chr(d$temporal),
      spatial     = .safe_chr(d$spatial)
    )
  })
  bind_rows(rows)
}

.parse_governance <- function(raw) {
  boards <- raw$boards
  if (is.null(boards) || length(boards) == 0) return(.schema_governance)

  rows <- lapply(boards, function(b) {
    tibble(
      board_name   = .safe_chr(b$governanceBoardName),
      bureau_code  = .safe_chr(b$bureauCode),
      program_code = .safe_chr(b$programCodeFPI),
      description  = .safe_chr(b$cioInvolvementDescription)
    )
  })
  bind_rows(rows)
}

.parse_savings <- function(raw) {
  strategies <- raw$strategies
  if (is.null(strategies) || length(strategies) == 0) return(.schema_savings)

  rows <- lapply(strategies, function(s) {
    .fy <- function(year) {
      fy <- s[[paste0("fy", year)]]
      if (is.null(fy)) return(NA_real_)
      as.numeric(fy$amount %||% NA)
    }
    tibble(
      strategy_id    = as.integer(s$strategyId %||% NA),
      strategy_title = .safe_chr(s$strategyTitle),
      decision_date  = .safe_date(.safe_chr(s$decisionDate)),
      omb_initiative = .safe_chr(s$ombInitiative),
      amount_type    = .safe_chr(s$amountType),
      fy2012 = .fy(2012), fy2013 = .fy(2013), fy2014 = .fy(2014),
      fy2015 = .fy(2015), fy2016 = .fy(2016), fy2017 = .fy(2017),
      fy2018 = .fy(2018), fy2019 = .fy(2019), fy2020 = .fy(2020)
    )
  })
  bind_rows(rows)
}

.parse_leadership <- function(raw) {
  leaders <- raw$leaders
  if (is.null(leaders) || length(leaders) == 0) return(.schema_leadership)

  rows <- lapply(leaders, function(l) {
    tibble(
      bureau_code     = .safe_chr(l$bureauCode),
      first_name      = .safe_chr(l$firstName),
      last_name       = .safe_chr(l$lastName),
      employment_type = .safe_chr(l$employmentType),
      appointment     = .safe_chr(l$typeOfAppointment),
      responsibilities = .safe_chr(l$otherResponsibilities),
      rating_official  = .safe_chr(l$evaluationRatingOfficialTitle),
      review_official  = .safe_chr(l$evaluationReviewingOfficialTitle),
      key_bureau_cio   = identical(tolower(.safe_chr(l$keyBureauCIO)), "yes")
    )
  })
  bind_rows(rows)
}

# == Public functions ==========================================================

#' Commerce Enterprise Data Inventory (Project Open Data catalog)
#'
#' Fetches and parses the Department of Commerce data.json catalog containing
#' ~1964 dataset entries. Returns a tibble with one row per dataset.
#'
#' @return tibble with columns: title, description, publisher, keywords,
#'   modified, identifier, access_level, format, access_url,
#'   distribution_urls, landing_page, bureau_code, license, temporal, spatial
#' @export
doc_catalog <- function() {
  raw <- tryCatch(.fetch_json(.base_catalog), error = function(e) {
    warning("Failed to fetch Commerce data catalog: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(raw)) return(.schema_catalog)
  .parse_catalog(raw)
}

#' Search the Commerce data catalog by keyword
#'
#' Searches across title, description, keywords, and publisher fields
#' of the Enterprise Data Inventory. Case-insensitive.
#'
#' @param query Character string to search for
#' @param field Optional: restrict search to "title", "description",
#'   "keywords", or "publisher". Default searches all.
#' @return tibble (same schema as doc_catalog)
#' @export
doc_search <- function(query, field = NULL) {
  stopifnot(is.character(query), length(query) == 1, nzchar(query))
  catalog <- doc_catalog()
  if (nrow(catalog) == 0) return(catalog)

  if (!is.null(field)) {
    stopifnot(field %in% c("title", "description", "keywords", "publisher"))
    matches <- grepl(query, catalog[[field]], ignore.case = TRUE)
  } else {
    matches <- grepl(query, catalog$title, ignore.case = TRUE) |
      grepl(query, catalog$description, ignore.case = TRUE) |
      grepl(query, catalog$keywords, ignore.case = TRUE) |
      grepl(query, catalog$publisher, ignore.case = TRUE)
  }
  catalog[matches, , drop = FALSE]
}

#' CIO Governance Board memberships
#'
#' Fetches the Department of Commerce CIO governance board membership list.
#'
#' @return tibble with columns: board_name, bureau_code, program_code, description
#' @export
doc_governance <- function() {
  raw <- tryCatch(.fetch_json(.base_governance), error = function(e) {
    warning("Failed to fetch governance boards: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(raw)) return(.schema_governance)
  .parse_governance(raw)
}

#' IT Cost Savings and Avoidance strategies
#'
#' Fetches the Department of Commerce IT cost savings/avoidance data.
#' Amounts are in millions of dollars.
#'
#' @return tibble with columns: strategy_id, strategy_title, decision_date,
#'   omb_initiative, amount_type, fy2012 through fy2020
#' @export
doc_cost_savings <- function() {
  raw <- tryCatch(.fetch_json(.base_savings), error = function(e) {
    warning("Failed to fetch cost savings data: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(raw)) return(.schema_savings)
  .parse_savings(raw)
}

#' Bureau IT Leadership Directory
#'
#' Fetches the Department of Commerce bureau IT leadership directory.
#'
#' @return tibble with columns: bureau_code, first_name, last_name,
#'   employment_type, appointment, responsibilities, rating_official,
#'   review_official, key_bureau_cio
#' @export
doc_leadership <- function() {
  raw <- tryCatch(.fetch_json(.base_leadership), error = function(e) {
    warning("Failed to fetch leadership directory: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(raw)) return(.schema_leadership)
  .parse_leadership(raw)
}

#' List all available Commerce datasets (summary view)
#'
#' Returns a compact tibble of all datasets in the Enterprise Data Inventory,
#' with just the key identification and access fields.
#'
#' @return tibble with columns: title, publisher, format, access_url, modified
#' @export
doc_list <- function() {
  catalog <- doc_catalog()
  if (nrow(catalog) == 0) {
    return(tibble(
      title = character(), publisher = character(),
      format = character(), access_url = character(),
      modified = as.Date(character())
    ))
  }
  catalog |>
    select(title, publisher, format, access_url, modified) |>
    arrange(desc(modified))
}

#' Return context about the commerce.gov client functions
#'
#' Reads the source file and returns all function bodies for
#' introspection by LLMs or other tools.
#'
#' @return Character string of the full source file
#' @export
doc_context <- function() {
  src <- readLines(sys.frame(environment(doc_context))$ofile %||% {
    f <- getSrcFilename(doc_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else "clients/commerce.gov.R"
  })
  paste(src, collapse = "\n")
}
