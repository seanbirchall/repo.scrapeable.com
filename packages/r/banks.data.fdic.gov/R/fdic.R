# banks.data.fdic.gov.R - Self-contained banks.data.fdic.gov client



# banks-data-fdic-gov.R
# Self-contained FDIC BankFind Suite API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown (be polite)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fdic_base <- "https://banks.data.fdic.gov/api"

`%||%` <- function(a, b) if (is.null(a)) b else a
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
#' Queries the FDIC BankFind Suite API for insured financial institutions.
#' Results are sorted by total assets descending. Supports filtering by state
#' and active status with pagination.
#'
#' @param state Character or NULL. Two-letter US state code to filter by.
#'   Examples: \code{"NY"}, \code{"CA"}, \code{"TX"}, \code{"IL"}.
#'   Default \code{NULL} returns all states.
#' @param active Integer or NULL. Active status filter: \code{1} = active
#'   (default), \code{0} = inactive/closed. Use \code{NULL} for both.
#' @param limit Integer. Maximum number of results (default 100, max 10000).
#' @param offset Integer. Pagination offset (default 0). Use with \code{limit}
#'   to page through results.
#' @return A tibble with columns:
#'   \describe{
#'     \item{cert}{Integer. FDIC certificate number (unique bank identifier).
#'       Pass to \code{fdic_financials()} for financial data.}
#'     \item{name}{Character. Institution name (e.g. "JPMorgan Chase Bank").}
#'     \item{city}{Character. City of main office.}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{zip}{Character. ZIP code.}
#'     \item{active}{Integer. 1 = active, 0 = inactive.}
#'     \item{class}{Character. Institution category/class.}
#'     \item{total_assets}{Numeric. Total assets in thousands of dollars.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Largest active banks in New York
#' fdic_institutions(state = "NY", limit = 10)
#'
#' # All active banks (first page)
#' fdic_institutions(limit = 50)
#' }
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
#' Retrieves quarterly Call Report financial data for an FDIC-insured
#' institution identified by its certificate number. Results are sorted by
#' report date descending (most recent first). Use \code{fdic_search()} or
#' \code{fdic_institutions()} to find certificate numbers.
#'
#' @param cert Integer. FDIC certificate number. Examples: \code{628}
#'   (JPMorgan Chase), \code{3850} (Goldman Sachs), \code{7213} (Citibank).
#'   Get these from \code{fdic_search()} or \code{fdic_institutions()}.
#' @param limit Integer. Maximum number of quarterly reports (default 20).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{cert}{Integer. FDIC certificate number.}
#'     \item{repdte}{Character. Report date in \code{"YYYYMMDD"} format
#'       (e.g. "20231231" for Q4 2023).}
#'     \item{asset}{Numeric. Total assets (thousands of dollars).}
#'     \item{dep}{Numeric. Total deposits (thousands of dollars).}
#'     \item{netinc}{Numeric. Net income (thousands of dollars).}
#'     \item{roa}{Numeric. Return on assets (percentage).}
#'     \item{roe}{Numeric. Return on equity (percentage).}
#'     \item{equity}{Numeric. Total equity capital (thousands of dollars).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # JPMorgan Chase financials
#' fdic_financials(628, limit = 8)
#'
#' # Goldman Sachs last 4 quarters
#' fdic_financials(3850, limit = 4)
#' }
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
#' Searches the FDIC BankFind Suite for institutions matching a name query.
#' Returns basic identification data. Use the \code{cert} column to look up
#' detailed financials via \code{fdic_financials()}.
#'
#' @param name Character. Bank name or partial name to search for. Examples:
#'   \code{"Goldman"}, \code{"Chase"}, \code{"Wells Fargo"},
#'   \code{"Bank of America"}.
#' @param limit Integer. Maximum number of results (default 25).
#' @return A tibble with columns:
#'   \describe{
#'     \item{cert}{Integer. FDIC certificate number. Pass to
#'       \code{fdic_financials()}.}
#'     \item{name}{Character. Full institution name.}
#'     \item{city}{Character. City of main office.}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{active}{Integer. 1 = active, 0 = inactive.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fdic_search("Chase")
#' fdic_search("Goldman")
#' fdic_search("Wells Fargo")
#' }
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

#' Get banks.data.fdic.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fdic_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fdic_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/banks.data.fdic.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "banks.data.fdic.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# banks.data.fdic.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# banks.data.fdic.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
