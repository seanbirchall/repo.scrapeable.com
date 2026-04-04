

# treasurydirect-gov.R
# Self-contained U.S. Treasury Direct API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown; be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.treas_base <- "https://www.treasurydirect.gov"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyDataFrame = TRUE)

.parse_date <- function(x) {

  if (is.null(x)) return(as.Date(NA))
  x <- as.character(x)
  x <- sub("T.*", "", x)
  x <- trimws(x)
  # Handle "Month Day, Year EDT" format
  parsed <- suppressWarnings(as.Date(x, format = "%B %d, %Y"))
  if (all(is.na(parsed))) parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  parsed
}


# == Schemas ===================================================================

.schema_securities <- tibble::tibble(
  cusip = character(), security_type = character(), security_term = character(),
  issue_date = as.Date(character()), maturity_date = as.Date(character()),
  interest_rate = numeric(), auction_date = as.Date(character()),
  offering_amount = numeric(), high_yield = character(),
  bid_to_cover_ratio = character(), reopening = character()
)

.schema_debt <- tibble::tibble(
  effective_date = as.Date(character()), public_debt = numeric(),
  government_holdings = numeric(), total_debt = numeric()
)


# == Public functions ==========================================================

#' List upcoming Treasury security auctions
#'
#' @param limit Maximum number of results (default 50, max 250)
#' @return tibble: cusip, security_type, security_term, issue_date,
#'   maturity_date, interest_rate, auction_date, offering_amount,
#'   high_yield, bid_to_cover_ratio, reopening
#' @export
treas_upcoming <- function(limit = 50) {
  url <- sprintf("%s/TA_WS/securities/announced?format=json", .treas_base)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0 || nrow(raw) == 0) return(.schema_securities)
  raw <- utils::head(raw, limit)

  tibble::tibble(
    cusip             = as.character(raw$cusip %||% NA),
    security_type     = as.character(raw$securityType %||% NA),
    security_term     = as.character(raw$securityTerm %||% NA),
    issue_date        = .parse_date(raw$issueDate),
    maturity_date     = .parse_date(raw$maturityDate),
    interest_rate     = suppressWarnings(as.numeric(raw$interestRate)),
    auction_date      = .parse_date(raw$auctionDate),
    offering_amount   = suppressWarnings(as.numeric(raw$offeringAmount)),
    high_yield        = as.character(raw$highYield %||% NA),
    bid_to_cover_ratio = as.character(raw$bidToCoverRatio %||% NA),
    reopening         = as.character(raw$reopening %||% NA)
  )
}

#' List recently auctioned Treasury securities
#'
#' @param days Number of days to look back (default 0 = today, 7 = last week)
#' @param limit Maximum number of results (default 50, max 250)
#' @return tibble: cusip, security_type, security_term, issue_date,
#'   maturity_date, interest_rate, auction_date, offering_amount,
#'   high_yield, bid_to_cover_ratio, reopening
#' @export
treas_auctioned <- function(days = 7, limit = 50) {
  url <- sprintf("%s/TA_WS/securities/auctioned?format=json&day=%d",
                 .treas_base, as.integer(days))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0 || nrow(raw) == 0) return(.schema_securities)
  raw <- utils::head(raw, limit)

  tibble::tibble(
    cusip             = as.character(raw$cusip %||% NA),
    security_type     = as.character(raw$securityType %||% NA),
    security_term     = as.character(raw$securityTerm %||% NA),
    issue_date        = .parse_date(raw$issueDate),
    maturity_date     = .parse_date(raw$maturityDate),
    interest_rate     = suppressWarnings(as.numeric(raw$interestRate)),
    auction_date      = .parse_date(raw$auctionDate),
    offering_amount   = suppressWarnings(as.numeric(raw$offeringAmount)),
    high_yield        = as.character(raw$highYield %||% NA),
    bid_to_cover_ratio = as.character(raw$bidToCoverRatio %||% NA),
    reopening         = as.character(raw$reopening %||% NA)
  )
}

#' Search Treasury securities by type and date range
#'
#' @param type Security type: "Bill", "Note", "Bond", "TIPS", "FRN", "CMB"
#' @param start_date Start date (YYYY-MM-DD string or Date)
#' @param end_date End date (YYYY-MM-DD string or Date)
#' @param limit Maximum number of results (default 100)
#' @return tibble: cusip, security_type, security_term, issue_date,
#'   maturity_date, interest_rate, auction_date, offering_amount,
#'   high_yield, bid_to_cover_ratio, reopening
#' @export
treas_search <- function(type = "Bond", start_date = NULL, end_date = NULL,
                         limit = 100) {
  url <- sprintf("%s/TA_WS/securities/search?format=json&type=%s",
                 .treas_base, utils::URLencode(type))
  if (!is.null(start_date)) url <- paste0(url, "&startDate=", as.character(start_date))
  if (!is.null(end_date))   url <- paste0(url, "&endDate=", as.character(end_date))

  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0 || nrow(raw) == 0) return(.schema_securities)
  raw <- utils::head(raw, limit)

  tibble::tibble(
    cusip             = as.character(raw$cusip %||% NA),
    security_type     = as.character(raw$securityType %||% NA),
    security_term     = as.character(raw$securityTerm %||% NA),
    issue_date        = .parse_date(raw$issueDate),
    maturity_date     = .parse_date(raw$maturityDate),
    interest_rate     = suppressWarnings(as.numeric(raw$interestRate)),
    auction_date      = .parse_date(raw$auctionDate),
    offering_amount   = suppressWarnings(as.numeric(raw$offeringAmount)),
    high_yield        = as.character(raw$highYield %||% NA),
    bid_to_cover_ratio = as.character(raw$bidToCoverRatio %||% NA),
    reopening         = as.character(raw$reopening %||% NA)
  )
}

#' Get current U.S. national debt totals
#'
#' @return tibble: effective_date, public_debt, government_holdings, total_debt
#' @export
treas_debt_current <- function() {
  url <- sprintf("%s/NP_WS/debt/current?format=json", .treas_base)
  raw <- .fetch_json(url)
  if (is.null(raw)) return(.schema_debt)

  tibble::tibble(
    effective_date      = .parse_date(raw$effectiveDate),
    public_debt         = as.numeric(raw$publicDebt),
    government_holdings = as.numeric(raw$governmentHoldings),
    total_debt          = as.numeric(raw$totalDebt)
  )
}

#' Get historical U.S. national debt totals for a date range
#'
#' @param start_date Start date (YYYY-MM-DD string or Date)
#' @param end_date End date (YYYY-MM-DD string or Date)
#' @return tibble: effective_date, public_debt, government_holdings, total_debt
#' @export
treas_debt_history <- function(start_date, end_date) {
  url <- sprintf("%s/NP_WS/debt/search?startdate=%s&enddate=%s&format=json",
                 .treas_base, as.character(start_date), as.character(end_date))
  raw <- .fetch_json(url)
  entries <- raw$entries
  if (is.null(entries) || length(entries) == 0) return(.schema_debt)

  tibble::tibble(
    effective_date      = .parse_date(entries$effectiveDate),
    public_debt         = as.numeric(entries$publicDebt),
    government_holdings = as.numeric(entries$governmentHoldings),
    total_debt          = as.numeric(entries$totalDebt)
  )
}

#' Get details for a specific Treasury security by CUSIP
#'
#' @param cusip CUSIP identifier (e.g., "912810UR7")
#' @return tibble: single row with security details
#' @export
treas_security <- function(cusip) {
  url <- sprintf("%s/TA_WS/securities/%s?format=json",
                 .treas_base, utils::URLencode(as.character(cusip)))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_securities)

  # May be a list or data.frame depending on response

  if (is.data.frame(raw)) {
    raw <- utils::head(raw, 1)
    tibble::tibble(
      cusip             = as.character(raw$cusip %||% NA),
      security_type     = as.character(raw$securityType %||% NA),
      security_term     = as.character(raw$securityTerm %||% NA),
      issue_date        = .parse_date(raw$issueDate),
      maturity_date     = .parse_date(raw$maturityDate),
      interest_rate     = suppressWarnings(as.numeric(raw$interestRate)),
      auction_date      = .parse_date(raw$auctionDate),
      offering_amount   = suppressWarnings(as.numeric(raw$offeringAmount)),
      high_yield        = as.character(raw$highYield %||% NA),
      bid_to_cover_ratio = as.character(raw$bidToCoverRatio %||% NA),
      reopening         = as.character(raw$reopening %||% NA)
    )
  } else {
    tibble::tibble(
      cusip             = as.character(raw$cusip %||% NA),
      security_type     = as.character(raw$securityType %||% NA),
      security_term     = as.character(raw$securityTerm %||% NA),
      issue_date        = .parse_date(raw$issueDate),
      maturity_date     = .parse_date(raw$maturityDate),
      interest_rate     = suppressWarnings(as.numeric(raw$interestRate)),
      auction_date      = .parse_date(raw$auctionDate),
      offering_amount   = suppressWarnings(as.numeric(raw$offeringAmount)),
      high_yield        = as.character(raw$highYield %||% NA),
      bid_to_cover_ratio = as.character(raw$bidToCoverRatio %||% NA),
      reopening         = as.character(raw$reopening %||% NA)
    )
  }
}


# == Context ===================================================================

#' Generate LLM-friendly context for treasurydirect.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
treas_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) {
    src_file <- system.file("source", "treas.R", package = "treasurydirect.gov")
  }
  if (is.null(src_file) || !nzchar(src_file) || !file.exists(src_file)) {
    cat("# treasurydirect.gov context - source not found\n")
    return(invisible("# treasurydirect.gov context - source not found"))
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
