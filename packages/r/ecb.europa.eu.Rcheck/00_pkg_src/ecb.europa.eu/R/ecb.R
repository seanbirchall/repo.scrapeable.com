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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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

#' Generate LLM-friendly context for the ecb.europa.eu package
#'
#' @return Character string (invisibly), also printed
#' @export
ecb_context <- function() {
  .build_context("ecb.europa.eu", header_lines = c(
    "# ecb.europa.eu - European Central Bank Statistical Data Client for R",
    "# Dependencies: httr2, dplyr, tibble",
    "# Auth: none (public SDMX REST API)",
    "# All functions return tibbles.",
    "#",
    "# SDMX key format: {freq}.{dim1}.{dim2}... (use . to wildcard)",
    "# Key dataflows:",
    "#   EXR = Exchange rates",
    "#   FM  = Financial market data (key rates)",
    "#   BSI = Balance sheet items (money supply)",
    "#   ICP = HICP inflation",
    "#   YC  = Yield curve",
    "#",
    "# Currency codes: USD, GBP, JPY, CHF, CNY, AUD, CAD, etc."
  ))
}
