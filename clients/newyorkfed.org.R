# newyorkfed.org.R - Self-contained newyorkfed.org client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# newyorkfed-org.R
# Self-contained New York Fed Markets API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nyfed_base <- "https://markets.newyorkfed.org/api"

`%||%` <- function(a, b) if (is.null(a)) b else a
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- Date formatter ------------------------------------------------------------

.nyfed_date <- function(d) {
  if (is.null(d)) return(NULL)
  format(as.Date(d), "%Y-%m-%d")
}

# == Schemas ===================================================================

.schema_sofr <- tibble(
  date = as.Date(character()), rate = numeric(),
  percentile_1 = numeric(), percentile_25 = numeric(),
  percentile_75 = numeric(), percentile_99 = numeric(),
  volume = numeric()
)

.schema_effr <- tibble(
  date = as.Date(character()), rate = numeric(),
  percentile_1 = numeric(), percentile_25 = numeric(),
  percentile_75 = numeric(), percentile_99 = numeric(),
  volume = numeric()
)

.schema_treasury <- tibble(
  date = as.Date(character()), maturity = character(),
  rate = numeric()
)

# == SOFR (Secured Overnight Financing Rate) ===================================


#' Fetch SOFR (Secured Overnight Financing Rate) data
#'
#' Retrieves the Secured Overnight Financing Rate from the New York Fed
#' Markets API. SOFR is a broad measure of the cost of borrowing cash
#' overnight collateralized by Treasury securities.
#'
#' @param start Date or character. Start date in \code{"YYYY-MM-DD"} format
#'   (default: 30 days ago).
#' @param end Date or character. End date in \code{"YYYY-MM-DD"} format
#'   (default: today).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- effective date of the rate}
#'     \item{rate}{numeric -- SOFR rate in percent}
#'     \item{percentile_1}{numeric -- 1st percentile rate}
#'     \item{percentile_25}{numeric -- 25th percentile rate}
#'     \item{percentile_75}{numeric -- 75th percentile rate}
#'     \item{percentile_99}{numeric -- 99th percentile rate}
#'     \item{volume}{numeric -- transaction volume in billions USD}
#'   }
#' @export
#' @examples
#' \dontrun{
#' nyfed_sofr()
#' nyfed_sofr(start = "2024-01-01", end = "2024-01-31")
#' }
nyfed_sofr <- function(start = NULL, end = NULL) {
  if (is.null(start)) start <- Sys.Date() - 30
  if (is.null(end)) end <- Sys.Date()
  url <- sprintf(
    "%s/rates/secured/all/search.json?startDate=%s&endDate=%s&type=rate",
    .nyfed_base, .nyfed_date(start), .nyfed_date(end)
  )
  raw <- .fetch_json(url)
  rates <- raw$refRates
  if (is.null(rates) || length(rates) == 0) return(.schema_sofr)
  if (is.data.frame(rates) && nrow(rates) == 0) return(.schema_sofr)

  # Filter to SOFR only
  if (is.data.frame(rates)) {
    sofr <- rates[rates$type == "SOFR", , drop = FALSE]
    if (nrow(sofr) == 0) return(.schema_sofr)
    as_tibble(sofr) |>
      transmute(
        date = as.Date(effectiveDate),
        rate = as.numeric(percentRate %||% NA),
        percentile_1 = as.numeric(if ("percentPercentile1" %in% names(sofr)) percentPercentile1 else NA),
        percentile_25 = as.numeric(if ("percentPercentile25" %in% names(sofr)) percentPercentile25 else NA),
        percentile_75 = as.numeric(if ("percentPercentile75" %in% names(sofr)) percentPercentile75 else NA),
        percentile_99 = as.numeric(if ("percentPercentile99" %in% names(sofr)) percentPercentile99 else NA),
        volume = as.numeric(if ("volumeInBillions" %in% names(sofr)) volumeInBillions else NA)
      )
  } else {
    .schema_sofr
  }
}

# == EFFR (Effective Federal Funds Rate) =======================================

#' Fetch EFFR (Effective Federal Funds Rate) data
#'
#' Retrieves the Effective Federal Funds Rate from the New York Fed
#' Markets API. EFFR is the volume-weighted median rate of overnight
#' federal funds transactions.
#'
#' @param start Date or character. Start date in \code{"YYYY-MM-DD"} format
#'   (default: 30 days ago).
#' @param end Date or character. End date in \code{"YYYY-MM-DD"} format
#'   (default: today).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- effective date of the rate}
#'     \item{rate}{numeric -- EFFR rate in percent}
#'     \item{percentile_1}{numeric -- 1st percentile rate}
#'     \item{percentile_25}{numeric -- 25th percentile rate}
#'     \item{percentile_75}{numeric -- 75th percentile rate}
#'     \item{percentile_99}{numeric -- 99th percentile rate}
#'     \item{volume}{numeric -- transaction volume in billions USD}
#'   }
#' @export
#' @examples
#' \dontrun{
#' nyfed_effr()
#' nyfed_effr(start = "2024-01-01", end = "2024-06-30")
#' }
nyfed_effr <- function(start = NULL, end = NULL) {
  if (is.null(start)) start <- Sys.Date() - 30
  if (is.null(end)) end <- Sys.Date()
  url <- sprintf(
    "%s/rates/unsecured/all/search.json?startDate=%s&endDate=%s&type=rate",
    .nyfed_base, .nyfed_date(start), .nyfed_date(end)
  )
  raw <- .fetch_json(url)
  rates <- raw$refRates
  if (is.null(rates) || length(rates) == 0) return(.schema_effr)
  if (is.data.frame(rates) && nrow(rates) == 0) return(.schema_effr)

  if (is.data.frame(rates)) {
    effr <- rates[rates$type == "EFFR", , drop = FALSE]
    if (nrow(effr) == 0) return(.schema_effr)
    as_tibble(effr) |>
      transmute(
        date = as.Date(effectiveDate),
        rate = as.numeric(percentRate %||% NA),
        percentile_1 = as.numeric(if ("percentPercentile1" %in% names(effr)) percentPercentile1 else NA),
        percentile_25 = as.numeric(if ("percentPercentile25" %in% names(effr)) percentPercentile25 else NA),
        percentile_75 = as.numeric(if ("percentPercentile75" %in% names(effr)) percentPercentile75 else NA),
        percentile_99 = as.numeric(if ("percentPercentile99" %in% names(effr)) percentPercentile99 else NA),
        volume = as.numeric(if ("volumeInBillions" %in% names(effr)) volumeInBillions else NA)
      )
  } else {
    .schema_effr
  }
}

# == Treasury securities =======================================================

#' Fetch Treasury securities operations data
#'
#' Retrieves results from the New York Fed's Treasury securities open market
#' operations, including outright purchases and sales.
#'
#' @param start Date or character. Start date in \code{"YYYY-MM-DD"} format
#'   (default: 30 days ago).
#' @param end Date or character. End date in \code{"YYYY-MM-DD"} format
#'   (default: today).
#' @param operation Character. Operation type: \code{"all"} (default),
#'   \code{"purchases"}, or \code{"sales"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- operation or auction date}
#'     \item{maturity}{character -- security type or maturity description}
#'     \item{rate}{numeric -- high rate from the operation}
#'   }
#' @export
#' @examples
#' \dontrun{
#' nyfed_treasury()
#' nyfed_treasury(operation = "purchases")
#' }
nyfed_treasury <- function(start = NULL, end = NULL, operation = "all") {
  if (is.null(start)) start <- Sys.Date() - 30
  if (is.null(end)) end <- Sys.Date()
  url <- sprintf(
    "%s/tsy/%s/results/search.json?startDate=%s&endDate=%s",
    .nyfed_base, operation, .nyfed_date(start), .nyfed_date(end)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_treasury)

  # The treasury ops response has nested structure
  auctions <- raw$treasury$auctions %||% raw$auctions %||% NULL
  if (is.null(auctions) || length(auctions) == 0) return(.schema_treasury)
  if (is.data.frame(auctions) && nrow(auctions) == 0) return(.schema_treasury)

  if (is.data.frame(auctions)) {
    as_tibble(auctions) |>
      transmute(
        date = tryCatch(as.Date(operationDate %||% auctionDate %||% NA), error = function(e) NA_real_),
        maturity = as.character(if ("securityType" %in% names(auctions)) securityType else NA),
        rate = as.numeric(if ("highRate" %in% names(auctions)) highRate else if ("rate" %in% names(auctions)) rate else NA)
      )
  } else {
    .schema_treasury
  }
}

# == Context ===================================================================

#' Get newyorkfed.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nyfed_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nyfed_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/newyorkfed.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "newyorkfed.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# newyorkfed.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# newyorkfed.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
