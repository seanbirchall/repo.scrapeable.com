# norges-bank-no.R
# Self-contained Norges Bank (Norwegian central bank) SDMX API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous
# Format: SDMX-CSV

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nb_base <- "https://data.norges-bank.no/api/data"

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

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_csv <- function(url) {
  f <- .fetch(url, ext = ".csv")
  lines <- readLines(f, warn = FALSE)
  # SDMX-CSV headers include descriptions like "FREQ:Frequency" — strip to bare key
  if (length(lines) > 0) {
    header <- strsplit(lines[1], ",")[[1]]
    header <- sub(":.*", "", header)
    lines[1] <- paste(header, collapse = ",")
  }
  tc <- textConnection(lines)
  on.exit(close(tc))
  utils::read.csv(tc, stringsAsFactors = FALSE)
}

# == Schemas ===================================================================

.schema_exchange_rates <- tibble(
  date = as.Date(character()), currency = character(),
  value = numeric(), frequency = character(),
  unit_mult = character(), decimals = integer()
)

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
