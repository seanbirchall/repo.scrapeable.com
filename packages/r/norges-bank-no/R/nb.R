# norges-bank-no.R
# Self-contained Norges Bank (Norwegian central bank) SDMX API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous
# Format: SDMX-CSV


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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' Retrieves foreign exchange rates against the Norwegian krone (NOK) from
#' the Norges Bank (Norwegian central bank) SDMX data service. Returns
#' time series of exchange rates at daily, monthly, or annual frequency.
#'
#' @param currency ISO 4217 currency code (e.g. "USD", "EUR", "GBP", "SEK",
#'   "JPY"). Use "all" to retrieve all available currencies at once.
#' @param start Start period as "YYYY-MM" or "YYYY-MM-DD" string
#'   (default "2024-01").
#' @param end End period as "YYYY-MM" or "YYYY-MM-DD" string (default:
#'   current month).
#' @param frequency Frequency code: "B" for business daily (default),
#'   "M" for monthly average, "A" for annual average.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Observation date (Date)}
#'     \item{currency}{ISO currency code}
#'     \item{value}{Exchange rate: units of NOK per 1 unit of foreign currency}
#'     \item{frequency}{Frequency code}
#'     \item{unit_mult}{Unit multiplier description}
#'     \item{decimals}{Number of decimal places (integer)}
#'   }
#' @examples
#' nb_exchange_rates("USD", start = "2025-01", end = "2025-03")
#' nb_exchange_rates("EUR", frequency = "M")
#' @seealso [nb_context()]
#' @source <https://data.norges-bank.no>
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

#' Get norges-bank-no client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the Norges Bank SDMX-CSV client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' nb_context()
#' @export
nb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/norges-bank-no.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "norges-bank-no")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# norges-bank-no context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# norges-bank-no", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
