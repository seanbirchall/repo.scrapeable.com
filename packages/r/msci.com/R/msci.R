# == Index level data ==========================================================

#' Fetch MSCI index daily level data
#'
#' Returns end-of-day index levels for one or more MSCI indexes.
#' Data is sourced from MSCI's public index data service.
#'
#' @param codes Character vector of MSCI index codes (e.g. "990100" for MSCI World).
#'   Use msci_indexes() to see available codes.
#' @param start Start date in "YYYYMMDD" format (default: one year ago)
#' @param end End date in "YYYYMMDD" format (default: today)
#' @param variant Index variant: "STRD" (price), "NETR" (net total return),
#'   "GRTR" (gross total return). Default "STRD".
#' @param currency Currency code (default "USD"). Also supports "EUR", "GBP", "JPY".
#' @param frequency Data frequency: "DAILY" or "END_OF_MONTH". Default "DAILY".
#' @return tibble: date (Date), level (numeric), index_code (character),
#'   index_variant (character), currency (character)
#' @export
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
#' Convenience wrapper for msci_levels() with monthly frequency.
#'
#' @param codes Character vector of MSCI index codes
#' @param start Start date in "YYYYMMDD" format (default: 5 years ago)
#' @param end End date in "YYYYMMDD" format (default: today)
#' @param variant Index variant: "STRD", "NETR", or "GRTR". Default "STRD".
#' @param currency Currency code (default "USD")
#' @return tibble: date, level, index_code, index_variant, currency
#' @export
msci_monthly <- function(codes, start = NULL, end = NULL,
                         variant = "STRD", currency = "USD") {
  if (is.null(start)) start <- format(Sys.Date() - 365 * 5, "%Y%m%d")
  if (is.null(end))   end   <- format(Sys.Date(), "%Y%m%d")
  msci_levels(codes, start = start, end = end,
              variant = variant, currency = currency, frequency = "END_OF_MONTH")
}


#' Fetch MSCI index performance comparison
#'
#' Fetches level data for multiple indexes and computes returns over the period.
#' Useful for comparing index performance side by side.
#'
#' @param codes Character vector of MSCI index codes
#' @param start Start date in "YYYYMMDD" format (default: one year ago)
#' @param end End date in "YYYYMMDD" format (default: today)
#' @param variant Index variant: "STRD", "NETR", or "GRTR". Default "NETR".
#' @param currency Currency code (default "USD")
#' @return tibble: index_code (character), name (character),
#'   start_level (numeric), end_level (numeric), return_pct (numeric),
#'   start_date (Date), end_date (Date)
#' @export
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
#' Returns a reference table of commonly used MSCI index codes.
#' The MSCI API does not have a discovery endpoint, so these codes
#' are curated from the MSCI website.
#'
#' @return tibble: code (character), name (character)
#' @export
msci_indexes <- function() {
  .msci_codes
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the msci package
#'
#' @return Character string (invisibly), also printed
#' @export
msci_context <- function() {
  .build_context("msci.com", header_lines = c(
    "# msci.com - MSCI Index Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: unknown (undocumented API)",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Common index codes:",
    "#   990100 = MSCI World      891800 = MSCI ACWI",
    "#   990300 = MSCI EM          139261 = MSCI EAFE",
    "#   106235 = MSCI Europe      100800 = MSCI Japan",
    "#",
    "# Index variants: STRD (price), NETR (net return), GRTR (gross return)",
    "# Date format: YYYYMMDD (e.g. '20250101')"
  ))
}
