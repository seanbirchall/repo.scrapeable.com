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

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.ecb_base <- "https://data-api.ecb.europa.eu/service"

# -- Context generator ---------------------------------------------------------

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

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
#' Universal engine for the ECB Statistical Data Warehouse SDMX REST API.
#' Returns CSV data parsed into a typed tibble. This is the low-level
#' workhorse; prefer the convenience wrappers for common series.
#'
#' @param dataflow Character. SDMX dataflow ID. Common values:
#'   \code{"EXR"} (exchange rates), \code{"FM"} (financial market),
#'   \code{"BSI"} (balance sheet items / money supply), \code{"ICP"} (HICP inflation),
#'   \code{"YC"} (yield curve), \code{"MIR"} (interest rates on loans/deposits).
#' @param key Character. SDMX key with dot-separated dimensions
#'   (e.g. \code{"D.USD.EUR.SP00.A"} for daily EUR/USD). Use \code{"."}
#'   to wildcard a dimension and \code{"+"} for multiple values.
#' @param start Character or Date. Start date (\code{"YYYY-MM-DD"} or \code{"YYYY-MM"} or \code{"YYYY"}).
#' @param end Character or Date. End date.
#' @param last_n Integer. Return only the last N observations (overrides start/end).
#' @return A tibble with all SDMX CSV columns including
#'   \code{KEY} (character), \code{TIME_PERIOD} (Date), \code{OBS_VALUE} (numeric),
#'   \code{TITLE} (character), plus all dimension columns from the dataflow.
#' @examples
#' ecb_get("EXR", "D.USD.EUR.SP00.A", last_n = 5)
#' ecb_get("FM", "B.U2.EUR.4F.KR.MRR_FR.LEV", last_n = 10)
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
#' Daily EUR exchange rates against major currencies, published by the
#' European Central Bank. Rates express how many units of the foreign
#' currency equal one euro.
#'
#' @param currency Character. 3-letter ISO currency code (e.g. \code{"USD"},
#'   \code{"GBP"}, \code{"JPY"}, \code{"CHF"}, \code{"CNY"}).
#'   Use \code{"."} for all available currencies.
#' @param start Character or Date. Start date (e.g. \code{"2024-01-01"}).
#' @param end Character or Date. End date.
#' @param last_n Integer. Return only the last N observations.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- observation date}
#'     \item{value}{numeric -- exchange rate (foreign currency per 1 EUR)}
#'     \item{currency}{character -- 3-letter currency code}
#'     \item{title}{character -- series description}
#'   }
#' @examples
#' ecb_exchange_rate("USD", last_n = 10)
#' ecb_exchange_rate("GBP", start = "2024-01-01")
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
#' Fetches daily EUR exchange rates for several currencies in a single
#' request using the ECB SDMX \code{"+"} multi-value syntax.
#'
#' @param currencies Character vector. ISO currency codes (default
#'   \code{c("USD", "GBP", "JPY", "CHF", "CNY")}).
#' @param last_n Integer. Last N observations per currency (default 30).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- observation date}
#'     \item{value}{numeric -- exchange rate (foreign currency per 1 EUR)}
#'     \item{currency}{character -- 3-letter currency code}
#'     \item{title}{character -- series description}
#'   }
#' @examples
#' ecb_exchange_rates(c("USD", "GBP"), last_n = 5)
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

#' ECB key interest rates
#'
#' Fetches the three main ECB policy rates: the main refinancing
#' operations rate (MRR), the deposit facility rate (DFR), and the
#' marginal lending facility rate (MLFR).
#'
#' @param last_n Integer. Last N observations per rate series (default 20).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- effective date of rate change}
#'     \item{value}{numeric -- interest rate in percent}
#'     \item{key}{character -- SDMX series key identifying the rate}
#'     \item{title}{character -- human-readable series title (e.g.
#'       \code{"Main refinancing operations - fixed rate tenders (fixed rate)..."})}
#'   }
#' @examples
#' ecb_key_rates(last_n = 10)
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
#' AAA-rated euro area government bond yield curve spot rates at
#' selected maturities, published by the ECB.
#'
#' @param maturity Character. Bond maturity: \code{"1Y"}, \code{"2Y"},
#'   \code{"5Y"}, \code{"10Y"}, or \code{"30Y"}. Default \code{"10Y"}.
#' @param last_n Integer. Last N observations (default 60).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- observation date}
#'     \item{value}{numeric -- yield in percent}
#'     \item{key}{character -- SDMX series key}
#'     \item{title}{character -- series title (e.g. \code{"AAA yield curve - 10-year spot rate"})}
#'   }
#' @examples
#' ecb_bond_yield("10Y", last_n = 30)
#' ecb_bond_yield("2Y", last_n = 10)
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
#' Monthly outstanding amounts (stocks) of euro area monetary
#' aggregates from the ECB Balance Sheet Items (BSI) dataset.
#'
#' @param aggregate Character. Monetary aggregate: \code{"M1"} (narrow money),
#'   \code{"M2"} (intermediate), or \code{"M3"} (broad). Default \code{"M3"}.
#' @param last_n Integer. Last N observations (default 60).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- monthly observation date}
#'     \item{value}{numeric -- outstanding amount in millions of EUR}
#'     \item{key}{character -- SDMX series key}
#'     \item{title}{character -- series title (e.g. \code{"Monetary aggregate M3, Stocks"})}
#'   }
#' @examples
#' ecb_money_supply("M3", last_n = 12)
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
#' Monthly Harmonised Index of Consumer Prices (HICP) annual rate
#' of change -- the ECB's primary inflation measure for the euro area.
#'
#' @param component Character. COICOP component code:
#'   \code{"000000"} (all items, default), \code{"010000"} (food),
#'   \code{"040000"} (housing), \code{"070000"} (transport),
#'   \code{"090000"} (education).
#' @param last_n Integer. Last N observations (default 60).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date -- monthly observation date}
#'     \item{value}{numeric -- annual rate of change in percent}
#'     \item{key}{character -- SDMX series key}
#'     \item{title}{character -- series title (e.g. \code{"HICP - Overall index"})}
#'   }
#' @examples
#' ecb_inflation(last_n = 12)
#' ecb_inflation("010000", last_n = 6)
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

#' Get ecb-europa client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ecb-europa.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ecb-europa")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ecb-europa context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ecb-europa", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
