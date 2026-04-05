# frankfurter.app.R - Self-contained frankfurter.app client



# frankfurter-app.R
# Self-contained Frankfurter exchange rate client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none known
# Source: European Central Bank via frankfurter.app


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fx_base <- "https://api.frankfurter.app"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_rates <- tibble(
  date = as.Date(character()), base = character(),
  currency = character(), rate = numeric()
)

.schema_currencies <- tibble(
  code = character(), name = character()
)


# == Latest rates ==============================================================

#' Fetch latest exchange rates from the European Central Bank
#'
#' Retrieves the most recent exchange rates published by the ECB via the
#' Frankfurter API. Rates are updated around 16:00 CET on working days.
#'
#' @param from Base currency code (default "USD"). Use \code{\link{fx_currencies}}
#'   to see all supported codes.
#' @param to Comma-separated target currencies (e.g. "EUR,GBP,JPY").
#'   If NULL, returns all available currencies.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date the rate was published}
#'     \item{base}{Base currency code}
#'     \item{currency}{Target currency code}
#'     \item{rate}{Exchange rate (1 unit of base = rate units of target)}
#'   }
#' @examples
#' # Latest USD rates for EUR, GBP, JPY
#' fx_latest("USD", "EUR,GBP,JPY")
#'
#' # All available rates from EUR
#' fx_latest("EUR")
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
#' Retrieves ECB reference rates for a specific past date. The API returns the
#' closest available rate if the requested date falls on a weekend or holiday.
#'
#' @param date Date string in "YYYY-MM-DD" format, or a Date object.
#' @param from Base currency code (default "USD").
#' @param to Comma-separated target currencies (optional). If NULL, returns
#'   all available currencies.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date of the rate (may differ from input if weekend/holiday)}
#'     \item{base}{Base currency code}
#'     \item{currency}{Target currency code}
#'     \item{rate}{Exchange rate}
#'   }
#' @examples
#' # EUR/GBP rate on a specific date
#' fx_historical("2024-01-15", from = "EUR", to = "GBP")
#'
#' # All USD rates on a past date
#' fx_historical("2023-06-01")
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
#' Retrieves daily ECB reference rates for every working day in the specified
#' date range. Useful for charting currency trends or computing returns.
#'
#' @param start Start date in "YYYY-MM-DD" format or a Date object.
#' @param end End date in "YYYY-MM-DD" format or a Date object.
#' @param from Base currency code (default "USD").
#' @param to Comma-separated target currencies (e.g. "EUR,GBP"). Required
#'   for reasonable result sizes over long date ranges.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date of the rate}
#'     \item{base}{Base currency code}
#'     \item{currency}{Target currency code}
#'     \item{rate}{Exchange rate}
#'   }
#' @examples
#' # USD to EUR daily rates for January 2024
#' fx_timeseries("2024-01-01", "2024-01-31", from = "USD", to = "EUR")
#'
#' # Multiple currencies over a week
#' fx_timeseries("2024-06-01", "2024-06-07", to = "EUR,GBP,JPY")
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
#' Lists all currency codes supported by the Frankfurter API (sourced from
#' the European Central Bank). Typically ~30 currencies.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{ISO 4217 currency code (e.g. "USD", "EUR")}
#'     \item{name}{Full currency name (e.g. "US Dollar")}
#'   }
#' @examples
#' fx_currencies()
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

#' Get frankfurter.app client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fx_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fx_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/frankfurter.app.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "frankfurter.app")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# frankfurter.app context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# frankfurter.app", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
