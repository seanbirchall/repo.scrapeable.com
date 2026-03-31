
# == Exchange rates ============================================================

#' Fetch exchange rates from Norges Bank SDMX API
#'
#' @param currency ISO currency code (e.g. "USD", "EUR", "GBP", "SEK").
#'   Use "all" for all currencies.
#' @param start Start period ("YYYY-MM" or "YYYY-MM-DD")
#' @param end End period ("YYYY-MM" or "YYYY-MM-DD")
#' @param frequency Frequency code: "B" (business daily, default), "M" (monthly), "A" (annual)
#' @return tibble: date (Date), currency (character), value (numeric),
#'   frequency (character), unit_mult (character), decimals (integer)
#' @export
nb_exchange_rates <- function(currency = "USD", start = "2024-01",
                              end = format(Sys.Date(), "%Y-%m"),
                              frequency = "B") {
  cur_key <- if (tolower(currency) == "all") ".." else paste0(".", currency, ".")
  key <- paste0(frequency, cur_key, "NOK.SP")
  url <- sprintf("%s/EXR/%s?format=sdmx-csv&startPeriod=%s&endPeriod=%s",
                 .nb_base, key, start, end)
  df <- tryCatch(.fetch_csv(url), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_exchange_rates)
  df <- as_tibble(df)
  # SDMX-CSV has standard column names
  tp_col <- intersect(c("TIME_PERIOD", "TIME"), names(df))
  ov_col <- intersect(c("OBS_VALUE", "VALUE"), names(df))
  cur_col <- intersect(c("BASE_CUR", "CURRENCY", "UNIT_MEASURE"), names(df))
  if (length(tp_col) == 0 || length(ov_col) == 0) return(.schema_exchange_rates)
  tibble(
    date = as.Date(tryCatch(as.Date(df[[tp_col[1]]]), error = function(e) {
      as.Date(paste0(df[[tp_col[1]]], "-01"))
    })),
    currency = if (length(cur_col) > 0) sub(":.*", "", as.character(df[[cur_col[1]]])) else currency,
    value = suppressWarnings(as.numeric(df[[ov_col[1]]])),
    frequency = if ("FREQ" %in% names(df)) sub(":.*", "", as.character(df[["FREQ"]])) else frequency,
    unit_mult = if ("UNIT_MULT" %in% names(df)) as.character(df[["UNIT_MULT"]]) else NA_character_,
    decimals = if ("DECIMALS" %in% names(df)) as.integer(df[["DECIMALS"]]) else NA_integer_
  )
}

# == Context ===================================================================

#' Show Norges Bank API context for LLMs
#'
#' Displays package overview, common currency codes, and function signatures.
#' @return Invisibly returns the context string
#' @export
nb_context <- function() {
  .build_context(
    "norges.bank.no",
    header_lines = c(
      "# norges.bank.no",
      "# Norges Bank (Norwegian central bank) SDMX API client",
      "# Auth: none required",
      "# Format: SDMX-CSV",
      "#",
      "# Common currencies: USD, EUR, GBP, SEK, DKK, JPY, CHF",
      "# Frequencies: B (business daily), M (monthly), A (annual)"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
