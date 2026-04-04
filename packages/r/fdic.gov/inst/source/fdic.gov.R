# fdic.gov.R
# Self-contained FDIC BankFind Suite API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required — public API.
# Docs: https://banks.data.fdic.gov/docs/

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.fdic_base <- "https://banks.data.fdic.gov/api"

# Core fetch engine — follows redirects to api.fdic.gov/banks/
.fdic_get <- function(endpoint, filters = NULL, fields = NULL, sort_by = NULL,
                      sort_order = "DESC", limit = 100, offset = 0) {
  params <- list()
  if (!is.null(filters) && nchar(filters) > 0) params$filters <- filters
  if (!is.null(fields))  params$fields <- fields
  if (!is.null(sort_by)) params$sort_by <- sort_by
  params$sort_order <- sort_order
  params$limit <- min(limit, 10000)
  if (offset > 0) params$offset <- offset

  query <- paste(names(params),
                 sapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE)),
                 sep = "=", collapse = "&")
  url <- paste0(.fdic_base, "/", endpoint, "?", query)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)

  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  if (!is.null(raw$error)) {
    stop("FDIC API error: ", raw$error$message %||% "unknown", call. = FALSE)
  }
  raw
}

# Parse FDIC response data into tibble
.fdic_parse <- function(raw, fields_map = NULL) {
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(tibble())
  bind_rows(lapply(data, function(item) {
    d <- item$data
    flat <- lapply(d, function(v) {
      if (is.null(v)) NA_character_ else as.character(v)
    })
    as_tibble(flat)
  }))
}

# Paginated fetch
.fdic_fetch_all <- function(endpoint, filters = NULL, fields = NULL,
                            sort_by = NULL, sort_order = "DESC",
                            max_results = 1000) {
  all_rows <- list()
  offset <- 0
  page_size <- min(max_results, 10000)

  repeat {
    raw <- .fdic_get(endpoint, filters = filters, fields = fields,
                     sort_by = sort_by, sort_order = sort_order,
                     limit = page_size, offset = offset)
    total <- raw$meta$total %||% 0
    batch <- .fdic_parse(raw)
    if (nrow(batch) == 0) break

    all_rows[[length(all_rows) + 1]] <- batch
    offset <- offset + nrow(batch)

    if (offset >= total) break
    if (offset >= max_results) break
  }

  if (length(all_rows) == 0) return(tibble())
  result <- bind_rows(all_rows)
  if (nrow(result) > max_results) result <- result[seq_len(max_results), ]
  result
}

# == Institutions ==============================================================

.inst_fields <- paste0(
  "CERT,NAME,CITY,STNAME,STALP,ZIP,ASSET,DEP,NETINC,REPDTE,",
  "ACTIVE,CHARTER,SPECGRP,ESTYMD,INSDATE,NAMEHCR,WEBADDR,RISESSION"
)

#' List FDIC-insured institutions
#'
#' Retrieves a list of FDIC-insured banks and savings institutions from the
#' BankFind Suite API. Results are sorted by total assets descending by default.
#' Use \code{\link{fdic_search}} for name-based filtering.
#'
#' @param state Character. Two-letter USPS state abbreviation to filter by
#'   (e.g., \code{"CA"}, \code{"NY"}, \code{"TX"}). \code{NULL} (default)
#'   returns institutions from all states.
#' @param active Logical. If \code{TRUE} (default), return only currently
#'   active institutions. If \code{FALSE}, include closed/inactive as well.
#' @param sort_by Character. FDIC field name to sort results by. Common
#'   values: \code{"ASSET"} (default, total assets), \code{"DEP"} (deposits),
#'   \code{"NAME"} (institution name), \code{"REPDTE"} (report date).
#' @param max_results Integer. Maximum number of rows to return (default 500).
#'   The API supports up to 10,000 per page; pagination handles larger requests.
#' @return A tibble with columns:
#'   \describe{
#'     \item{CERT}{Integer. FDIC certificate number (unique identifier).}
#'     \item{NAME}{Character. Institution name.}
#'     \item{CITY}{Character. City of main office.}
#'     \item{STNAME}{Character. Full state name.}
#'     \item{STALP}{Character. Two-letter state abbreviation.}
#'     \item{ZIP}{Character. ZIP code.}
#'     \item{ASSET}{Numeric. Total assets in thousands of dollars.}
#'     \item{DEP}{Numeric. Total deposits in thousands of dollars.}
#'     \item{NETINC}{Numeric. Net income in thousands of dollars.}
#'     \item{REPDTE}{Character. Report date (YYYYMMDD format).}
#'     \item{ACTIVE}{Integer. Active status (1 = active, 0 = inactive).}
#'     \item{CHARTER}{Character. Charter type code.}
#'     \item{SPECGRP}{Character. Specialization group.}
#'     \item{ESTYMD}{Character. Establishment date.}
#'     \item{INSDATE}{Character. Insurance date.}
#'     \item{NAMEHCR}{Character. Holding company name.}
#'     \item{WEBADDR}{Character. Institution website URL.}
#'   }
#' @export
#' @family FDIC functions
#' @seealso \code{\link{fdic_search}} for name-based search,
#'   \code{\link{fdic_institution}} for a single institution
#' @examples
#' \dontrun{
#' # Largest banks in California
#' fdic_list(state = "CA", max_results = 10)
#'
#' # All active institutions sorted by deposits
#' fdic_list(sort_by = "DEP", max_results = 20)
#' }
fdic_list <- function(state = NULL, active = TRUE, sort_by = "ASSET",
                      max_results = 500) {
  filt_parts <- character()
  if (active) filt_parts <- c(filt_parts, "ACTIVE:1")
  if (!is.null(state)) filt_parts <- c(filt_parts, paste0("STALP:", state))
  filters <- if (length(filt_parts) > 0) paste(filt_parts, collapse = " AND ") else NULL

  .fdic_fetch_all("institutions", filters = filters, fields = .inst_fields,
                   sort_by = sort_by, sort_order = "DESC",
                   max_results = max_results) |>
    mutate(
      CERT   = as.integer(CERT),
      ASSET  = as.numeric(ASSET),
      DEP    = as.numeric(DEP),
      NETINC = suppressWarnings(as.numeric(NETINC)),
      ACTIVE = as.integer(ACTIVE)
    )
}

#' Search FDIC institutions by name, state, and asset filters
#'
#' Since the FDIC BankFind API does not support free-text name search
#' server-side, this function fetches a broad result set and filters
#' locally by name pattern. Combine with state and minimum-asset filters
#' for faster, more targeted results.
#'
#' @param name Character. Regex pattern to match against institution name
#'   (case-insensitive, partial match). For example, \code{"chase"},
#'   \code{"wells fargo"}, \code{"community"}. \code{NULL} (default) returns
#'   all institutions matching other filters.
#' @param state Character. Two-letter USPS state abbreviation (e.g.,
#'   \code{"CA"}, \code{"NY"}). \code{NULL} (default) searches all states.
#' @param min_assets Numeric. Minimum total assets in thousands of dollars.
#'   For example, \code{1000000} filters to banks with at least \$1B in assets.
#'   \code{NULL} (default) applies no asset filter.
#' @param active Logical. If \code{TRUE} (default), only active institutions.
#' @param max_results Integer. Maximum rows to return (default 200).
#' @return A tibble with the same columns as \code{\link{fdic_list}}:
#'   \code{CERT}, \code{NAME}, \code{CITY}, \code{STNAME}, \code{STALP},
#'   \code{ZIP}, \code{ASSET}, \code{DEP}, \code{NETINC}, \code{REPDTE},
#'   \code{ACTIVE}, \code{CHARTER}, \code{SPECGRP}, \code{ESTYMD},
#'   \code{INSDATE}, \code{NAMEHCR}, \code{WEBADDR}.
#' @export
#' @family FDIC functions
#' @seealso \code{\link{fdic_list}} for unfiltered listing
#' @examples
#' \dontrun{
#' # Find banks with "community" in their name
#' fdic_search(name = "community", state = "TX")
#'
#' # Large banks (over $10B assets)
#' fdic_search(min_assets = 10000000)
#' }
fdic_search <- function(name = NULL, state = NULL, min_assets = NULL,
                        active = TRUE, max_results = 200) {
  filt_parts <- character()
  if (active) filt_parts <- c(filt_parts, "ACTIVE:1")
  if (!is.null(state)) filt_parts <- c(filt_parts, paste0("STALP:", state))
  if (!is.null(min_assets)) {
    filt_parts <- c(filt_parts, paste0("ASSET:[", min_assets, " TO *]"))
  }
  filters <- if (length(filt_parts) > 0) paste(filt_parts, collapse = " AND ") else NULL

  # Fetch a larger set to allow local name filtering
  fetch_n <- if (!is.null(name)) max_results * 10 else max_results
  result <- .fdic_fetch_all("institutions", filters = filters,
                            fields = .inst_fields,
                            sort_by = "ASSET", sort_order = "DESC",
                            max_results = fetch_n)
  if (nrow(result) == 0) return(result)

  result <- result |>
    mutate(
      CERT   = as.integer(CERT),
      ASSET  = as.numeric(ASSET),
      DEP    = as.numeric(DEP),
      NETINC = suppressWarnings(as.numeric(NETINC)),
      ACTIVE = as.integer(ACTIVE)
    )

  if (!is.null(name)) {
    result <- result |> filter(grepl(name, NAME, ignore.case = TRUE))
  }

  if (nrow(result) > max_results) result <- result[seq_len(max_results), ]
  result
}

# == Institution detail ========================================================

#' Get detailed info for a single FDIC-insured institution
#'
#' Fetches the full record for a single institution identified by its FDIC
#' certificate number. Returns all available fields (not just the subset
#' returned by \code{\link{fdic_list}}).
#'
#' @param cert Integer. FDIC certificate number. Find certificate numbers via
#'   \code{\link{fdic_list}} or \code{\link{fdic_search}} (the \code{CERT}
#'   column). For example, \code{3514} (JPMorgan Chase), \code{7213} (Citibank).
#' @return A tibble with one row containing all available FDIC fields.
#'   Key columns include \code{CERT}, \code{NAME}, \code{CITY}, \code{STALP},
#'   \code{ASSET}, \code{DEP}, and many more depending on API availability.
#' @export
#' @family FDIC functions
#' @seealso \code{\link{fdic_financials}} for quarterly financial data
#' @examples
#' \dontrun{
#' # JPMorgan Chase
#' fdic_institution(3514)
#' }
fdic_institution <- function(cert) {
  raw <- .fdic_get(paste0("institutions"), filters = paste0("CERT:", cert),
                   limit = 1)
  .fdic_parse(raw) |>
    mutate(
      CERT  = as.integer(CERT),
      ASSET = suppressWarnings(as.numeric(ASSET)),
      DEP   = suppressWarnings(as.numeric(DEP))
    )
}

# == Financials ================================================================

.fin_fields <- paste0(
  "CERT,REPDTE,ASSET,DEP,NETINC,EQ,LNLSNET,SC,INTINC,EINTEXP,ROA,ROE,",
  "NITEFAAV,ELNATR,NCLNLS,NUMEMP"
)

#' Fetch quarterly financial data for an institution
#'
#' Downloads quarterly Call Report financial data for a single
#' FDIC-insured institution. Data includes assets, deposits, net income,
#' equity, loans, securities, interest income/expense, and performance
#' ratios (ROA, ROE).
#'
#' @param cert Integer. FDIC certificate number (e.g., \code{3514} for
#'   JPMorgan Chase). Find via \code{\link{fdic_list}}.
#' @param max_results Integer. Maximum quarterly reports to return
#'   (default 40, roughly 10 years of data).
#' @return A tibble with columns:
#'   \describe{
#'     \item{CERT}{Integer. FDIC certificate number.}
#'     \item{REPDTE}{Character. Report date (YYYYMMDD).}
#'     \item{ASSET}{Numeric. Total assets (thousands of dollars).}
#'     \item{DEP}{Numeric. Total deposits (thousands).}
#'     \item{NETINC}{Numeric. Net income (thousands).}
#'     \item{EQ}{Numeric. Total equity capital (thousands).}
#'     \item{LNLSNET}{Numeric. Net loans and leases (thousands).}
#'     \item{SC}{Numeric. Total securities (thousands).}
#'     \item{INTINC}{Numeric. Total interest income (thousands).}
#'     \item{EINTEXP}{Numeric. Total interest expense (thousands).}
#'     \item{ROA}{Character. Return on assets (percentage).}
#'     \item{ROE}{Character. Return on equity (percentage).}
#'     \item{NUMEMP}{Integer. Number of employees.}
#'   }
#' @export
#' @family FDIC functions
#' @seealso \code{\link{fdic_institution}} for institution metadata
#' @examples
#' \dontrun{
#' # Last 10 years of financials for JPMorgan Chase
#' fdic_financials(3514)
#'
#' # Last 2 years (8 quarters)
#' fdic_financials(3514, max_results = 8)
#' }
fdic_financials <- function(cert, max_results = 40) {
  .fdic_fetch_all("financials",
                   filters = paste0("CERT:", cert),
                   fields = .fin_fields,
                   sort_by = "REPDTE", sort_order = "DESC",
                   max_results = max_results) |>
    mutate(
      CERT    = as.integer(CERT),
      ASSET   = as.numeric(ASSET),
      DEP     = as.numeric(DEP),
      NETINC  = suppressWarnings(as.numeric(NETINC)),
      EQ      = suppressWarnings(as.numeric(EQ)),
      LNLSNET = suppressWarnings(as.numeric(LNLSNET)),
      SC      = suppressWarnings(as.numeric(SC)),
      INTINC  = suppressWarnings(as.numeric(INTINC)),
      EINTEXP = suppressWarnings(as.numeric(EINTEXP)),
      NUMEMP  = suppressWarnings(as.integer(NUMEMP))
    )
}

# == Failures ==================================================================

.fail_fields <- paste0(
  "CERT,NAME,CITY,PSTALP,FAILDATE,FAILYR,SAVR,RESTYPE,RESTYPE1,",
  "COST,QBFASSET,QBFDEP"
)

#' List FDIC bank failures
#'
#' Fetches records of failed FDIC-insured institutions. Each record includes
#' the institution name, location, failure date, resolution type, and
#' estimated cost. Useful for research on bank stability and crisis history.
#'
#' @param state Character. Two-letter USPS state abbreviation
#'   (e.g., \code{"IL"}, \code{"CA"}). \code{NULL} (default) returns
#'   failures from all states.
#' @param start_year Integer. Earliest failure year to include
#'   (e.g., \code{2008}). \code{NULL} (default) has no lower bound.
#' @param end_year Integer. Latest failure year to include
#'   (e.g., \code{2012}). \code{NULL} (default) has no upper bound.
#' @param max_results Integer. Maximum rows to return (default 500).
#' @return A tibble with columns:
#'   \describe{
#'     \item{CERT}{Integer. FDIC certificate number.}
#'     \item{NAME}{Character. Institution name at time of failure.}
#'     \item{CITY}{Character. City of main office.}
#'     \item{PSTALP}{Character. Two-letter state abbreviation.}
#'     \item{FAILDATE}{Character. Failure date (MM/DD/YYYY).}
#'     \item{FAILYR}{Integer. Failure year.}
#'     \item{SAVR}{Character. Savings type (e.g., \code{"DIF"}).}
#'     \item{RESTYPE}{Character. Resolution type (e.g., \code{"FAILURE"}).}
#'     \item{RESTYPE1}{Character. Detailed resolution type.}
#'     \item{COST}{Numeric. Estimated loss to the DIF (thousands of dollars).}
#'     \item{QBFASSET}{Numeric. Total assets at failure (thousands).}
#'     \item{QBFDEP}{Numeric. Total deposits at failure (thousands).}
#'   }
#' @export
#' @family FDIC functions
#' @examples
#' \dontrun{
#' # Failures during 2008-2012 financial crisis
#' fdic_failures(start_year = 2008, end_year = 2012)
#'
#' # Recent failures in Illinois
#' fdic_failures(state = "IL", start_year = 2023)
#' }
fdic_failures <- function(state = NULL, start_year = NULL, end_year = NULL,
                          max_results = 500) {
  filt_parts <- character()
  if (!is.null(state)) filt_parts <- c(filt_parts, paste0("PSTALP:", state))
  if (!is.null(start_year) && !is.null(end_year)) {
    filt_parts <- c(filt_parts,
                    paste0("FAILYR:[", start_year, " TO ", end_year, "]"))
  } else if (!is.null(start_year)) {
    filt_parts <- c(filt_parts,
                    paste0("FAILYR:[", start_year, " TO *]"))
  } else if (!is.null(end_year)) {
    filt_parts <- c(filt_parts,
                    paste0("FAILYR:[* TO ", end_year, "]"))
  }
  filters <- if (length(filt_parts) > 0) paste(filt_parts, collapse = " AND ") else NULL

  .fdic_fetch_all("failures", filters = filters, fields = .fail_fields,
                   sort_by = "FAILDATE", sort_order = "DESC",
                   max_results = max_results) |>
    mutate(
      CERT     = suppressWarnings(as.integer(CERT)),
      FAILYR   = as.integer(FAILYR),
      COST     = suppressWarnings(as.numeric(COST)),
      QBFASSET = suppressWarnings(as.numeric(QBFASSET)),
      QBFDEP   = suppressWarnings(as.numeric(QBFDEP))
    )
}

# == History ===================================================================

#' Fetch bank event history (mergers, name changes, relocations)
#'
#' Returns the historical event log for a single FDIC-insured institution,
#' including mergers, acquisitions, name changes, and address relocations.
#' Results are sorted by effective date descending (most recent first).
#'
#' @param cert Integer. FDIC certificate number (e.g., \code{3514}).
#' @param max_results Integer. Maximum event records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{CERT}{Integer. FDIC certificate number.}
#'     \item{INSTNAME}{Character. Institution name at the time of the event.}
#'     \item{CHANGECODE}{Character. Event type code.}
#'     \item{CHANGECODE_DESC}{Character. Human-readable event description
#'       (e.g., \code{"Name Change"}, \code{"Merger"}).}
#'     \item{EFFDATE}{Character. Effective date of the event.}
#'     \item{PROCDATE}{Character. Processing date.}
#'     \item{CITY}{Character. City at the time of the event.}
#'     \item{STALP}{Character. State abbreviation.}
#'   }
#' @export
#' @family FDIC functions
#' @seealso \code{\link{fdic_institution}} for current institution data
#' @examples
#' \dontrun{
#' fdic_history(3514)
#' }
fdic_history <- function(cert, max_results = 100) {
  .fdic_fetch_all("history",
                   filters = paste0("CERT:", cert),
                   fields = "CERT,INSTNAME,CHANGECODE,CHANGECODE_DESC,EFFDATE,PROCDATE,CITY,STALP",
                   sort_by = "EFFDATE", sort_order = "DESC",
                   max_results = max_results) |>
    mutate(CERT = as.integer(CERT))
}

# == Summary / Aggregate =======================================================

#' Fetch aggregate banking industry summary statistics
#'
#' Returns annual aggregate data for the banking industry, optionally
#' filtered by state and institution type. Includes total counts of banks,
#' branches, and offices, plus aggregate assets, deposits, and net income.
#' Results are sorted by year descending.
#'
#' @param state Character. Two-letter USPS state abbreviation (e.g.,
#'   \code{"CA"}, \code{"NY"}). \code{NULL} (default) returns national totals.
#' @param cb_si Character. Institution type filter: \code{"CB"} for
#'   commercial banks, \code{"SI"} for savings institutions. \code{NULL}
#'   (default) returns both types (in separate rows).
#' @param max_results Integer. Maximum rows to return (default 200).
#' @return A tibble with columns:
#'   \describe{
#'     \item{YEAR}{Integer. Reporting year.}
#'     \item{STALP}{Character. State abbreviation.}
#'     \item{STNAME}{Character. Full state name.}
#'     \item{CB_SI}{Character. Institution type (\code{"CB"} or \code{"SI"}).}
#'     \item{BANKS}{Integer. Number of institutions.}
#'     \item{BRANCHES}{Integer. Number of branches.}
#'     \item{ASSET}{Numeric. Aggregate total assets (thousands of dollars).}
#'     \item{DEP}{Numeric. Aggregate total deposits (thousands).}
#'     \item{NETINC}{Numeric. Aggregate net income (thousands).}
#'     \item{OFFICES}{Integer. Number of offices.}
#'     \item{TOTAL}{Integer. Total count.}
#'   }
#' @export
#' @family FDIC functions
#' @examples
#' \dontrun{
#' # National summary by year
#' fdic_summary()
#'
#' # California commercial banks only
#' fdic_summary(state = "CA", cb_si = "CB")
#' }
fdic_summary <- function(state = NULL, cb_si = NULL, max_results = 200) {
  filt_parts <- character()
  if (!is.null(state)) filt_parts <- c(filt_parts, paste0("STALP:", state))
  if (!is.null(cb_si)) filt_parts <- c(filt_parts, paste0("CB_SI:", cb_si))
  filters <- if (length(filt_parts) > 0) paste(filt_parts, collapse = " AND ") else NULL

  .fdic_fetch_all("summary",
                   filters = filters,
                   fields = "YEAR,STALP,STNAME,CB_SI,BANKS,BRANCHES,ASSET,DEP,NETINC,OFFICES,TOTAL",
                   sort_by = "YEAR", sort_order = "DESC",
                   max_results = max_results) |>
    mutate(
      YEAR     = as.integer(YEAR),
      BANKS    = as.integer(BANKS),
      BRANCHES = as.integer(BRANCHES),
      ASSET    = as.numeric(ASSET),
      DEP      = as.numeric(DEP),
      NETINC   = suppressWarnings(as.numeric(NETINC)),
      OFFICES  = as.integer(OFFICES),
      TOTAL    = as.integer(TOTAL)
    )
}

# == Context ===================================================================

#' Get fdic.gov client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fdic.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fdic.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fdic.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fdic.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
