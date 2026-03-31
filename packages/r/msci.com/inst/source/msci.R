# msci-com.R
# Self-contained MSCI index data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (undocumented API)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.msci_base <- "https://app2.msci.com/products/service/index/indexmaster"

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

# -- Known MSCI index codes ----------------------------------------------------

.msci_codes <- tibble(
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

.schema_levels <- tibble(
  date = as.Date(character()), level = numeric(),
  index_code = character(), index_variant = character(),
  currency = character()
)

.schema_index_codes <- tibble(
  code = character(), name = character()
)

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
      tibble(
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
msci_performance <- function(codes, start = NULL, end = NULL,
                             variant = "NETR", currency = "USD") {
  levels <- msci_levels(codes, start = start, end = end,
                        variant = variant, currency = currency)
  if (nrow(levels) == 0) {
    return(tibble(
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
msci_indexes <- function() {
  .msci_codes
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the msci package
#'
#' @return Character string (invisibly), also printed
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
