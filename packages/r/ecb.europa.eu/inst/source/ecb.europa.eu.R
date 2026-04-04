# ecb.europa.eu.R - Self-contained ecb.europa.eu client

library(httr2)
library(dplyr)
library(tibble)



# ecb-europa.R
# Self-contained European Central Bank Statistical Data Warehouse client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required (public SDMX REST API)
# Rate limits: undocumented, be polite
# Docs: https://data.ecb.europa.eu/help/api/overview
#
# Uses SDMX key format: {freq}.{dimension1}.{dimension2}...
# Example: EXR/D.USD.EUR.SP00.A = daily EUR/USD exchange rate


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.ecb_base <- "https://data-api.ecb.europa.eu/service"
# -- SDMX CSV fetch engine ----------------------------------------------------

.ecb_fetch <- function(dataflow, key, params = list()) {
  params$format <- "csvdata"
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/data/%s/%s?%s", .ecb_base, dataflow, key, query)

  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  df <- read.csv(tmp, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(tibble())

  as_tibble(df) |>
    mutate(
      TIME_PERIOD = tryCatch(as.Date(TIME_PERIOD), error = function(e) {
        # Handle monthly (2024-03), quarterly (2024-Q1), annual (2024)
        tp <- TIME_PERIOD
        tp <- gsub("-Q1$", "-01-01", tp)
        tp <- gsub("-Q2$", "-04-01", tp)
        tp <- gsub("-Q3$", "-07-01", tp)
        tp <- gsub("-Q4$", "-10-01", tp)
        tp <- ifelse(grepl("^\\d{4}$", tp), paste0(tp, "-01-01"), tp)
        tp <- ifelse(grepl("^\\d{4}-\\d{2}$", tp), paste0(tp, "-01"), tp)
        as.Date(tp)
      }),
      OBS_VALUE = as.numeric(OBS_VALUE)
    )
}


# == Schemas ===================================================================

.schema_ecb <- tibble(
  date = as.Date(character()), value = numeric(), key = character(),
  title = character()
)



# == Core data fetching ========================================================

#' Fetch any ECB SDMX dataset
#'
#' Universal engine using the SDMX REST API. Returns CSV data parsed
#' into a tibble.
#'
#' @param dataflow SDMX dataflow ID (e.g. "EXR", "FM", "BSI", "MIR")
#' @param key SDMX key (e.g. "D.USD.EUR.SP00.A" for daily EUR/USD).
#'   Use "." to wildcard a dimension.
#' @param start Start date (YYYY-MM-DD)
#' @param end End date
#' @param last_n Last N observations only
#' @return tibble: full SDMX CSV columns including KEY, TIME_PERIOD,
#'   OBS_VALUE, TITLE, plus all dimension columns
ecb_get <- function(dataflow, key, start = NULL, end = NULL, last_n = NULL) {
  params <- list()
  if (!is.null(start))  params$startPeriod <- as.character(start)
  if (!is.null(end))    params$endPeriod <- as.character(end)
  if (!is.null(last_n)) params$lastNObservations <- last_n
  .ecb_fetch(dataflow, key, params)
}


# == Exchange rates ============================================================

#' ECB reference exchange rates
#'
#' Daily EUR exchange rates against major currencies.
#'
#' @param currency 3-letter currency code (e.g. "USD", "GBP", "JPY", "CHF").
#'   Use "." for all currencies.
#' @param start Start date
#' @param end End date
#' @param last_n Last N observations
#' @return tibble: date, value, currency, title
ecb_exchange_rate <- function(currency = "USD", start = NULL, end = NULL,
                              last_n = NULL) {
  key <- sprintf("D.%s.EUR.SP00.A", toupper(currency))
  df <- ecb_get("EXR", key, start, end, last_n)
  if (nrow(df) == 0) return(.schema_ecb)

  df |>
    transmute(
      date     = TIME_PERIOD,
      value    = OBS_VALUE,
      currency = CURRENCY,
      title    = TITLE
    )
}

#' Exchange rates for multiple currencies
#'
#' @param currencies Character vector of currency codes
#' @param last_n Last N observations per currency (default 30)
#' @return tibble: date, value, currency, title
ecb_exchange_rates <- function(currencies = c("USD", "GBP", "JPY", "CHF", "CNY"),
                               last_n = 30) {
  # ECB supports + for multiple currencies in the key
  key <- sprintf("D.%s.EUR.SP00.A", paste(currencies, collapse = "+"))
  df <- ecb_get("EXR", key, last_n = last_n)
  if (nrow(df) == 0) return(.schema_ecb)

  df |>
    transmute(
      date     = TIME_PERIOD,
      value    = OBS_VALUE,
      currency = CURRENCY,
      title    = TITLE
    )
}


# == Interest rates ============================================================

#' ECB key interest rates (main refinancing operations, deposit facility, etc.)
#'
#' @param last_n Last N observations (default 20)
#' @return tibble: date, value, key, title
ecb_key_rates <- function(last_n = 20) {
  # Main refinancing rate
  df1 <- tryCatch(ecb_get("FM", "B.U2.EUR.4F.KR.MRR_FR.LEV", last_n = last_n),
                  error = function(e) tibble())
  # Deposit facility rate
  df2 <- tryCatch(ecb_get("FM", "B.U2.EUR.4F.KR.DFR.LEV", last_n = last_n),
                  error = function(e) tibble())
  # Marginal lending facility
  df3 <- tryCatch(ecb_get("FM", "B.U2.EUR.4F.KR.MLFR.LEV", last_n = last_n),
                  error = function(e) tibble())

  bind_rows(df1, df2, df3) |>
    transmute(
      date  = TIME_PERIOD,
      value = OBS_VALUE,
      key   = KEY,
      title = if ("TITLE" %in% names(pick(everything()))) TITLE else NA_character_
    ) |>
    filter(!is.na(value))
}

#' Euro area government bond yields
#'
#' @param maturity Maturity: "1Y", "2Y", "5Y", "10Y", "30Y"
#' @param last_n Last N observations (default 60)
#' @return tibble: date, value, key, title
ecb_bond_yield <- function(maturity = "10Y", last_n = 60) {
  # Euro area yield curve
  mat_map <- c("1Y" = "1Y", "2Y" = "2Y", "5Y" = "5Y",
               "10Y" = "10Y", "30Y" = "30Y")
  m <- mat_map[toupper(maturity)]
  if (is.na(m)) stop("Maturity must be one of: 1Y, 2Y, 5Y, 10Y, 30Y", call. = FALSE)

  key <- sprintf("B.U2.EUR.4F.G_N_A.SV_C_YM.SR_%s", m)
  df <- ecb_get("YC", key, last_n = last_n)
  if (nrow(df) == 0) return(.schema_ecb)

  df |>
    transmute(
      date  = TIME_PERIOD,
      value = OBS_VALUE,
      key   = KEY,
      title = if ("TITLE" %in% names(pick(everything()))) TITLE else NA_character_
    )
}


# == Money supply ==============================================================

#' Euro area monetary aggregates (M1, M2, M3)
#'
#' @param aggregate "M1", "M2", or "M3" (default "M3")
#' @param last_n Last N observations (default 60)
#' @return tibble: date, value, key, title
ecb_money_supply <- function(aggregate = "M3", last_n = 60) {
  agg_map <- c("M1" = "M10", "M2" = "M20", "M3" = "M30")
  a <- agg_map[toupper(aggregate)]
  if (is.na(a)) stop("Aggregate must be M1, M2, or M3", call. = FALSE)

  key <- sprintf("M.U2.Y.V.%s.X.1.U2.2300.Z01.E", a)
  df <- ecb_get("BSI", key, last_n = last_n)
  if (nrow(df) == 0) return(.schema_ecb)

  df |>
    transmute(
      date  = TIME_PERIOD,
      value = OBS_VALUE,
      key   = KEY,
      title = if ("TITLE" %in% names(pick(everything()))) TITLE else NA_character_
    )
}


# == Inflation =================================================================

#' Euro area HICP inflation
#'
#' Harmonised Index of Consumer Prices (HICP) — the ECB's primary
#' inflation measure.
#'
#' @param component "000000" (all items, default), "010000" (food),
#'   "040000" (housing), "070000" (transport), "090000" (education)
#' @param last_n Last N observations (default 60)
#' @return tibble: date, value, key, title
ecb_inflation <- function(component = "000000", last_n = 60) {
  key <- sprintf("M.U2.N.%s.4.ANR", component)
  df <- ecb_get("ICP", key, last_n = last_n)
  if (nrow(df) == 0) return(.schema_ecb)

  df |>
    transmute(
      date  = TIME_PERIOD,
      value = OBS_VALUE,
      key   = KEY,
      title = if ("TITLE" %in% names(pick(everything()))) TITLE else NA_character_
    )
}


# == Context ===================================================================

#' Get ecb.europa.eu client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ecb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ecb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ecb.europa.eu.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ecb.europa.eu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ecb.europa.eu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ecb.europa.eu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
