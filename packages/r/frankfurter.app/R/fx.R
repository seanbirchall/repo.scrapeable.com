# == Latest rates ==============================================================

#' Fetch latest exchange rates
#'
#' @param from Base currency code (default "USD")
#' @param to Comma-separated target currencies (e.g. "EUR,GBP,JPY").
#'   If NULL, returns all available currencies.
#' @return tibble: date (Date), base (character), currency (character), rate (numeric)
#' @export
fx_latest <- function(from = "USD", to = NULL) {
  url <- sprintf("%s/latest?from=%s", .fx_base, from)
  if (!is.null(to)) url <- paste0(url, "&to=", to)
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$rates)) return(.schema_rates)

  rates <- raw$rates
  tibble(
    date = as.Date(rep(raw$date, length(rates))),
    base = rep(raw$base, length(rates)),
    currency = names(rates),
    rate = as.numeric(unlist(rates))
  )
}

#' Fetch historical exchange rates for a single date
#'
#' @param date Date string in "YYYY-MM-DD" format
#' @param from Base currency code (default "USD")
#' @param to Comma-separated target currencies (optional)
#' @return tibble: date (Date), base (character), currency (character), rate (numeric)
#' @export
fx_historical <- function(date, from = "USD", to = NULL) {
  url <- sprintf("%s/%s?from=%s", .fx_base, date, from)
  if (!is.null(to)) url <- paste0(url, "&to=", to)
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$rates)) return(.schema_rates)

  rates <- raw$rates
  tibble(
    date = as.Date(rep(raw$date, length(rates))),
    base = rep(raw$base, length(rates)),
    currency = names(rates),
    rate = as.numeric(unlist(rates))
  )
}

#' Fetch exchange rate time series between two dates
#'
#' @param start Start date in "YYYY-MM-DD" format
#' @param end End date in "YYYY-MM-DD" format
#' @param from Base currency code (default "USD")
#' @param to Comma-separated target currencies (e.g. "EUR,GBP")
#' @return tibble: date (Date), base (character), currency (character), rate (numeric)
#' @export
fx_timeseries <- function(start, end, from = "USD", to = NULL) {
  url <- sprintf("%s/%s..%s?from=%s", .fx_base, start, end, from)
  if (!is.null(to)) url <- paste0(url, "&to=", to)
  raw <- .fetch_json(url)
  if (is.null(raw) || is.null(raw$rates)) return(.schema_rates)

  rows <- lapply(names(raw$rates), function(d) {
    r <- raw$rates[[d]]
    tibble(
      date = as.Date(d),
      base = raw$base,
      currency = names(r),
      rate = as.numeric(unlist(r))
    )
  })
  bind_rows(rows) |> arrange(date, currency)
}

#' Fetch available currencies
#'
#' @return tibble: code (character), name (character)
#' @export
fx_currencies <- function() {
  raw <- .fetch_json(sprintf("%s/currencies", .fx_base))
  if (is.null(raw) || length(raw) == 0) return(.schema_currencies)

  tibble(
    code = names(raw),
    name = as.character(unlist(raw))
  )
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the frankfurter package
#'
#' @return Character string (invisibly), also printed
#' @export
fx_context <- function() {
  .build_context("frankfurter.app", header_lines = c(
    "# frankfurter.app - Exchange Rate Client for R (ECB data)",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: none known",
    "# Source: European Central Bank reference rates",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Common currency codes: USD, EUR, GBP, JPY, CHF, CAD, AUD, CNY, INR"
  ))
}
