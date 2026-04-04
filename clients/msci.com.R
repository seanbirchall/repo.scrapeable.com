# msci.com.R - Self-contained msci.com client

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)



# msci-com.R
# Self-contained MSCI index data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (undocumented API)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.msci_base <- "https://app2.msci.com/products/service/index/indexmaster"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- Known MSCI index codes ----------------------------------------------------

.msci_codes <- tibble::tibble(
  code    = c("990100", "891800", "990300", "139261", "106235",
              "990200", "891600", "891400", "891200",
              "100800", "990500", "990700", "990900", "891000"),
  name    = c("MSCI World", "MSCI ACWI", "MSCI Emerging Markets",
              "MSCI EAFE", "MSCI Europe",
              "MSCI World ex USA", "MSCI ACWI ex USA",
              "MSCI EM Asia", "MSCI EM Latin America",
              "MSCI Japan", "MSCI Pacific", "MSCI Pacific ex Japan",
              "MSCI Kokusai", "MSCI EM Europe Middle East & Africa")
)

# == Schemas ===================================================================

.schema_levels <- tibble::tibble(
  date = as.Date(character()), level = numeric(),
  index_code = character(), index_variant = character(),
  currency = character()
)

.schema_index_codes <- tibble::tibble(
  code = character(), name = character()
)

# == Index level data ==========================================================

#' Fetch MSCI index daily level data
#'
#' Returns end-of-day index levels for one or more MSCI indexes. Data is
#' sourced from MSCI's public index data service. No authentication is
#' required. Supports price, net total return, and gross total return
#' variants in multiple currencies.
#'
#' @param codes Character vector. One or more MSCI index codes (e.g.,
#'   \code{"990100"} for MSCI World, \code{"891800"} for MSCI ACWI).
#'   Use \code{\link{msci_indexes}} to see all available codes and names.
#' @param start Character or \code{NULL}. Start date in \code{"YYYYMMDD"}
#'   format (default: one year ago).
#' @param end Character or \code{NULL}. End date in \code{"YYYYMMDD"}
#'   format (default: today).
#' @param variant Character. Index return variant:
#'   \itemize{
#'     \item \code{"STRD"} -- Standard (price return, default)
#'     \item \code{"NETR"} -- Net total return (dividends reinvested, net of tax)
#'     \item \code{"GRTR"} -- Gross total return (dividends reinvested, no tax)
#'   }
#' @param currency Character. Currency for level values (default
#'   \code{"USD"}). Also supports \code{"EUR"}, \code{"GBP"},
#'   \code{"JPY"}, and others.
#' @param frequency Character. Data frequency: \code{"DAILY"} (default) or
#'   \code{"END_OF_MONTH"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{level}{Numeric. End-of-day index level.}
#'     \item{index_code}{Character. MSCI index code.}
#'     \item{index_variant}{Character. Return variant used.}
#'     \item{currency}{Character. Currency code.}
#'   }
#' @examples
#' msci_levels("990100")
#' msci_levels(c("990100", "990300"), variant = "NETR")
msci_levels <- function(codes, start = NULL, end = NULL,
                        variant = "STRD", currency = "USD",
                        frequency = "DAILY") {
  if (is.null(start)) start <- format(Sys.Date() - 365, "%Y%m%d")
  if (is.null(end))   end   <- format(Sys.Date(), "%Y%m%d")

  results <- lapply(codes, function(code) {
    url <- sprintf(
      "%s/getLevelDataForGraph?currency_symbol=%s&index_variant=%s&start_date=%s&end_date=%s&data_frequency=%s&index_codes=%s",
      .msci_base, currency, variant, start, end, frequency, code
    )

    raw <- tryCatch(.fetch_json(url), error = function(e) {
      warning("MSCI API request failed for code ", code, ": ", e$message)
      return(NULL)
    })

    if (is.null(raw)) return(NULL)

    lvls <- raw$indexes$INDEX_LEVELS
    if (is.null(lvls) || length(lvls) == 0) return(NULL)

    idx_code <- raw$msci_index_code
    if (is.null(idx_code)) idx_code <- code

    if (is.data.frame(lvls)) {
      tibble::tibble(
        date          = as.Date(as.character(lvls$calc_date), format = "%Y%m%d"),
        level         = as.numeric(lvls$level_eod),
        index_code    = as.character(idx_code),
        index_variant = variant,
        currency      = currency
      )
    } else {
      NULL
    }
  })

  result <- bind_rows(results)
  if (nrow(result) == 0) return(.schema_levels)
  result
}


#' Fetch MSCI index monthly level data
#'
#' Convenience wrapper around \code{\link{msci_levels}} that sets
#' \code{frequency = "END_OF_MONTH"} and defaults to a 5-year lookback.
#' Returns one observation per month per index.
#'
#' @param codes Character vector. MSCI index codes (see
#'   \code{\link{msci_indexes}}).
#' @param start Character or \code{NULL}. Start date in \code{"YYYYMMDD"}
#'   format (default: 5 years ago).
#' @param end Character or \code{NULL}. End date in \code{"YYYYMMDD"}
#'   format (default: today).
#' @param variant Character. Return variant: \code{"STRD"} (default),
#'   \code{"NETR"}, or \code{"GRTR"}.
#' @param currency Character. Currency code (default \code{"USD"}).
#' @return A tibble with columns: \code{date} (Date), \code{level}
#'   (Numeric), \code{index_code} (Character), \code{index_variant}
#'   (Character), \code{currency} (Character).
#' @seealso \code{\link{msci_levels}} for daily data.
#' @examples
#' msci_monthly("990100")
msci_monthly <- function(codes, start = NULL, end = NULL,
                         variant = "STRD", currency = "USD") {
  if (is.null(start)) start <- format(Sys.Date() - 365 * 5, "%Y%m%d")
  if (is.null(end))   end   <- format(Sys.Date(), "%Y%m%d")
  msci_levels(codes, start = start, end = end,
              variant = variant, currency = currency, frequency = "END_OF_MONTH")
}


#' Compare MSCI index performance over a date range
#'
#' Fetches level data for multiple MSCI indexes and computes the total
#' return percentage over the period. Useful for benchmarking and
#' side-by-side performance comparison.
#'
#' @param codes Character vector. MSCI index codes to compare (see
#'   \code{\link{msci_indexes}}).
#' @param start Character or \code{NULL}. Start date in \code{"YYYYMMDD"}
#'   format (default: one year ago).
#' @param end Character or \code{NULL}. End date in \code{"YYYYMMDD"}
#'   format (default: today).
#' @param variant Character. Return variant (default \code{"NETR"}, net
#'   total return). See \code{\link{msci_levels}} for options.
#' @param currency Character. Currency code (default \code{"USD"}).
#' @return A tibble with one row per index and columns:
#'   \describe{
#'     \item{index_code}{Character. MSCI index code.}
#'     \item{name}{Character. Human-readable index name (from
#'       \code{\link{msci_indexes}}), or \code{NA} if not in the
#'       reference table.}
#'     \item{start_level}{Numeric. Index level on the first available date.}
#'     \item{end_level}{Numeric. Index level on the last available date.}
#'     \item{return_pct}{Numeric. Period return as a percentage
#'       (e.g., \code{12.5} means +12.5\%).}
#'     \item{start_date}{Date. First observation date.}
#'     \item{end_date}{Date. Last observation date.}
#'   }
#' @examples
#' msci_performance(c("990100", "990300"))
msci_performance <- function(codes, start = NULL, end = NULL,
                             variant = "NETR", currency = "USD") {
  levels <- msci_levels(codes, start = start, end = end,
                        variant = variant, currency = currency)
  if (nrow(levels) == 0) {
    return(tibble::tibble(
      index_code = character(), name = character(),
      start_level = numeric(), end_level = numeric(), return_pct = numeric(),
      start_date = as.Date(character()), end_date = as.Date(character())
    ))
  }

  levels |>
    group_by(index_code) |>
    summarise(
      start_level = first(level),
      end_level   = last(level),
      return_pct  = (last(level) / first(level) - 1) * 100,
      start_date  = min(date),
      end_date    = max(date),
      .groups     = "drop"
    ) |>
    left_join(.msci_codes, by = c("index_code" = "code")) |>
    select(index_code, name, start_level, end_level, return_pct, start_date, end_date)
}


# == Index reference ===========================================================

#' List known MSCI index codes
#'
#' Returns a curated reference table of commonly used MSCI index codes
#' and their human-readable names. Since the MSCI API does not provide
#' a discovery endpoint, these codes are maintained from the MSCI
#' website. Pass these codes to \code{\link{msci_levels}},
#' \code{\link{msci_monthly}}, or \code{\link{msci_performance}}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{Character. Numeric MSCI index code (e.g., \code{"990100"}).}
#'     \item{name}{Character. Index name (e.g., \code{"MSCI World"}).}
#'   }
#' @examples
#' msci_indexes()
msci_indexes <- function() {
  .msci_codes
}


# == Context ===================================================================

#' Get msci.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
msci_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(msci_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/msci.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "msci.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# msci.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# msci.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
