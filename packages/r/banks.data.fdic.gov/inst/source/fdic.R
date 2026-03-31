# banks-data-fdic-gov.R
# Self-contained FDIC BankFind Suite API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fdic_base <- "https://banks.data.fdic.gov/api"

`%||%` <- function(a, b) if (is.null(a)) b else a

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

.schema_institutions <- tibble(
  cert = integer(), name = character(), city = character(),
  state = character(), zip = character(), active = integer(),
  class = character(), total_assets = numeric()
)

.schema_financials <- tibble(
  cert = integer(), repdte = character(), asset = numeric(),
  dep = numeric(), netinc = numeric(), roa = numeric(),
  roe = numeric(), equity = numeric()
)

.schema_search <- tibble(
  cert = integer(), name = character(), city = character(),
  state = character(), active = integer()
)

# == Institutions ==============================================================

#' Fetch FDIC-insured institution data
#'
#' Query the FDIC BankFind institutions endpoint. Returns bank
#' details including name, location, and asset size.
#'
#' @param state Two-letter state code (e.g. "NY", "CA")
#' @param active Active status: 1 = active, 0 = inactive (default 1)
#' @param limit Max results (default 100, max 10000)
#' @param offset Pagination offset (default 0)
#' @return tibble: cert, name, city, state, zip, active, class, total_assets
fdic_institutions <- function(state = NULL, active = 1, limit = 100,
                              offset = 0) {
  filters <- character()
  if (!is.null(state)) filters <- c(filters, sprintf("STALP:%s", state))
  if (!is.null(active)) filters <- c(filters, sprintf("ACTIVE:%d", as.integer(active)))
  filter_str <- paste(filters, collapse = " AND ")

  url <- sprintf(
    "%s/institutions?filters=%s&fields=CERT,INSTNAME,CITY,STALP,ZIP,ACTIVE,INSTCAT,ASSET&sort_by=ASSET&sort_order=DESC&limit=%d&offset=%d",
    .fdic_base, utils::URLencode(filter_str), as.integer(limit), as.integer(offset)
  )
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_institutions)

  rows <- lapply(d, function(x) {
    r <- x$data %||% x
    tibble(
      cert = as.integer(r$CERT %||% NA),
      name = as.character(r$INSTNAME %||% NA),
      city = as.character(r$CITY %||% NA),
      state = as.character(r$STALP %||% NA),
      zip = as.character(r$ZIP %||% NA),
      active = as.integer(r$ACTIVE %||% NA),
      class = as.character(r$INSTCAT %||% NA),
      total_assets = as.numeric(r$ASSET %||% NA)
    )
  })
  bind_rows(rows)
}

# == Financials ================================================================

#' Fetch FDIC financial data for a bank
#'
#' Returns quarterly financial data (Call Reports) for an FDIC-insured
#' institution identified by certificate number.
#'
#' @param cert FDIC certificate number (e.g. 3850 for Goldman Sachs)
#' @param limit Max results (default 20)
#' @param offset Pagination offset (default 0)
#' @return tibble: cert, repdte, asset, dep, netinc, roa, roe, equity
fdic_financials <- function(cert, limit = 20, offset = 0) {
  url <- sprintf(
    "%s/financials?filters=CERT:%d&fields=CERT,REPDTE,ASSET,DEP,NETINC,ROA,ROE,EQ&sort_by=REPDTE&sort_order=DESC&limit=%d&offset=%d",
    .fdic_base, as.integer(cert), as.integer(limit), as.integer(offset)
  )
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_financials)

  rows <- lapply(d, function(x) {
    r <- x$data %||% x
    tibble(
      cert = as.integer(r$CERT %||% NA),
      repdte = as.character(r$REPDTE %||% NA),
      asset = as.numeric(r$ASSET %||% NA),
      dep = as.numeric(r$DEP %||% NA),
      netinc = as.numeric(r$NETINC %||% NA),
      roa = as.numeric(r$ROA %||% NA),
      roe = as.numeric(r$ROE %||% NA),
      equity = as.numeric(r$EQ %||% NA)
    )
  })
  bind_rows(rows)
}

# == Search ====================================================================

#' Search FDIC institutions by name
#'
#' @param name Bank name or partial name (e.g. "Goldman", "Chase")
#' @param limit Max results (default 25)
#' @return tibble: cert, name, city, state, active
fdic_search <- function(name, limit = 25) {
  url <- sprintf(
    "%s/institutions?search=INSTNAME:%s&fields=CERT,INSTNAME,CITY,STALP,ACTIVE&limit=%d",
    .fdic_base, utils::URLencode(name), as.integer(limit)
  )
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_search)

  rows <- lapply(d, function(x) {
    r <- x$data %||% x
    tibble(
      cert = as.integer(r$CERT %||% NA),
      name = as.character(r$INSTNAME %||% NA),
      city = as.character(r$CITY %||% NA),
      state = as.character(r$STALP %||% NA),
      active = as.integer(r$ACTIVE %||% NA)
    )
  })
  bind_rows(rows)
}

# == Context ===================================================================

#' Generate LLM-friendly context for the banks.data.fdic.gov package
#'
#' @return Character string (invisibly), also printed
fdic_context <- function() {
  .build_context("banks.data.fdic.gov", header_lines = c(
    "# banks.data.fdic.gov - FDIC BankFind Suite API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# API base: https://banks.data.fdic.gov/api",
    "# All functions return tibbles with typed columns.",
    "# Key identifiers: CERT (certificate number) uniquely identifies each bank.",
    "# Popular CERTs: 3850 (Goldman Sachs), 628 (JPMorgan Chase), 3510 (Citibank)"
  ))
}
