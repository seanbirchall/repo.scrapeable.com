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
#' ~1,964 dataset entries from bureaus including USPTO, NOAA, Census, NIST,
#' and BEA. Returns a tibble with one row per dataset. Note: the full catalog
#' download may take 10--30 seconds.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Dataset title (e.g. "Asset Inventory").}
#'     \item{description}{Character. Full description text.}
#'     \item{publisher}{Character. Publishing bureau (e.g. "U.S. Patent and Trademark Office").}
#'     \item{keywords}{Character. Semicolon-delimited keywords.}
#'     \item{modified}{Date. Last modification date.}
#'     \item{identifier}{Character. Dataset identifier (e.g. "DOC-0715").}
#'     \item{access_level}{Character. "public", "restricted-public", or "non-public".}
#'     \item{format}{Character. Data format (e.g. "json", "csv", "api", "tsv").}
#'     \item{access_url}{Character. Direct access URL, if available.}
#'     \item{distribution_urls}{Character. Semicolon-delimited download URLs.}
#'     \item{landing_page}{Character. Human-readable landing page URL.}
#'     \item{bureau_code}{Character. OMB bureau code.}
#'     \item{license}{Character. License URL or text.}
#'     \item{temporal, spatial}{Character. Temporal coverage and spatial extent.}
#'   }
#' @examples
#' doc_catalog()
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
#' of the Enterprise Data Inventory. Case-insensitive regex matching.
#' Downloads the full catalog first, then filters client-side.
#'
#' @param query Character. Search string (regex supported).
#'   Example: \code{"weather"}, \code{"patent"}, \code{"census"}
#' @param field Character or NULL. Restrict search to a single field:
#'   \code{"title"}, \code{"description"}, \code{"keywords"}, or
#'   \code{"publisher"}. Default NULL searches all four fields.
#'   Example: \code{field = "title"}
#' @return A tibble with the same 15 columns as \code{\link{doc_catalog}}.
#'   Only matching rows are returned.
#' @examples
#' doc_search("weather", field = "title")
#' doc_search("patent")
#' doc_search("NOAA", field = "publisher")
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
#' Fetches the Department of Commerce CIO governance board membership list
#' from a static JSON file (2018 vintage).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{board_name}{Character. Governance board name.}
#'     \item{bureau_code}{Character. OMB bureau code.}
#'     \item{program_code}{Character. FPI program code.}
#'     \item{description}{Character. CIO involvement description.}
#'   }
#' @examples
#' doc_governance()
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
#' Fetches the Department of Commerce IT cost savings and avoidance data.
#' Each row is one strategy with fiscal year amounts (FY2012--FY2020).
#' Amounts are in millions of dollars.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{strategy_id}{Integer. Strategy identifier.}
#'     \item{strategy_title}{Character. Title of the savings strategy.}
#'     \item{decision_date}{Date. Date the strategy was decided.}
#'     \item{omb_initiative}{Character. Related OMB initiative.}
#'     \item{amount_type}{Character. Type of amount (savings vs avoidance).}
#'     \item{fy2012 ... fy2020}{Numeric. Dollar amounts per fiscal year (millions).}
#'   }
#' @examples
#' doc_cost_savings()
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
#' Lists CIO and IT leadership personnel across Commerce bureaus.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{bureau_code}{Character. OMB bureau code.}
#'     \item{first_name, last_name}{Character. Leader's name.}
#'     \item{employment_type}{Character. Employment classification.}
#'     \item{appointment}{Character. Type of appointment.}
#'     \item{responsibilities}{Character. Other responsibilities.}
#'     \item{rating_official}{Character. Rating official title.}
#'     \item{review_official}{Character. Reviewing official title.}
#'     \item{key_bureau_cio}{Logical. TRUE if designated key bureau CIO.}
#'   }
#' @examples
#' doc_leadership()
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
#' Returns a compact tibble of all ~1,964 datasets in the Enterprise Data
#' Inventory, sorted by modification date descending (most recent first).
#' A lightweight alternative to \code{\link{doc_catalog}} with fewer columns.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{Character. Dataset title (e.g. "Asset Inventory").}
#'     \item{publisher}{Character. Publishing bureau (e.g. "U.S. Patent and Trademark Office").}
#'     \item{format}{Character. Data format (e.g. "json", "csv", "api").}
#'     \item{access_url}{Character. Direct access URL, if available.}
#'     \item{modified}{Date. Last modification date.}
#'   }
#' @examples
#' doc_list()
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

#' Get commerce.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
doc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(doc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/commerce.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "commerce.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# commerce.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# commerce.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
