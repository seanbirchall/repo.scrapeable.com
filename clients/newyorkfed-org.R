# newyorkfed-org.R
# Self-contained New York Fed Markets API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nyfed_base <- "https://markets.newyorkfed.org/api"

`%||%` <- function(a, b) if (is.null(a)) b else a

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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
#' @param start Start date (Date or "YYYY-MM-DD"), default last 30 days
#' @param end End date (Date or "YYYY-MM-DD"), default today
#' @return tibble: date, rate, percentile_1, percentile_25,
#'   percentile_75, percentile_99, volume
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
#' @param start Start date (Date or "YYYY-MM-DD"), default last 30 days
#' @param end End date (Date or "YYYY-MM-DD"), default today
#' @return tibble: date, rate, percentile_1, percentile_25,
#'   percentile_75, percentile_99, volume
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
#' Retrieves results from the NY Fed's Treasury securities operations
#' (e.g., outright purchases, sales).
#'
#' @param start Start date (Date or "YYYY-MM-DD"), default last 30 days
#' @param end End date (Date or "YYYY-MM-DD"), default today
#' @param operation Operation type: "all", "purchases", "sales" (default "all")
#' @return tibble: date, maturity, rate
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

#' Generate LLM-friendly context for the newyorkfed.org package
#'
#' @return Character string (invisibly), also printed
nyfed_context <- function() {
  .build_context("newyorkfed.org", header_lines = c(
    "# newyorkfed.org - New York Fed Markets Data API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API base: https://markets.newyorkfed.org/api",
    "# All functions return tibbles with typed columns.",
    "# Key rates: SOFR (secured), EFFR (unsecured/fed funds)"
  ))
}
