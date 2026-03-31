# frankfurter-app.R
# Self-contained Frankfurter exchange rate client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none known
# Source: European Central Bank via frankfurter.app

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fx_base <- "https://api.frankfurter.app"

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

# == Schemas ===================================================================

.schema_rates <- tibble(
  date = as.Date(character()), base = character(),
  currency = character(), rate = numeric()
)

.schema_currencies <- tibble(
  code = character(), name = character()
)

# == Latest rates ==============================================================

#' Fetch latest exchange rates
#'
#' @param from Base currency code (default "USD")
#' @param to Comma-separated target currencies (e.g. "EUR,GBP,JPY").
#'   If NULL, returns all available currencies.
#' @return tibble: date (Date), base (character), currency (character), rate (numeric)
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
