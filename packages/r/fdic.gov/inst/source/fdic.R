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
#' @param state Two-letter state code (e.g. "CA", "NY") or NULL for all
#' @param active Logical; TRUE for active only, FALSE for all (default TRUE)
#' @param sort_by Field to sort by (default "ASSET")
#' @param max_results Maximum rows (default 500)
#' @return tibble of institutions
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

#' Search FDIC institutions by state and asset filters
#'
#' Since the FDIC API does not support free-text name search, this function
#' fetches institutions and filters locally by name pattern.
#'
#' @param name Name pattern to match (case-insensitive, partial match)
#' @param state Two-letter state code or NULL
#' @param min_assets Minimum total assets (in thousands) or NULL
#' @param active Logical (default TRUE)
#' @param max_results Maximum rows (default 200)
#' @return tibble of matching institutions
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

#' Get detailed info for a single institution by CERT number
#'
#' @param cert FDIC certificate number (integer)
#' @return tibble (1 row) with full institution details
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

#' Fetch financial data for an institution
#'
#' @param cert FDIC certificate number
#' @param max_results Maximum quarterly reports (default 40 = ~10 years)
#' @return tibble of quarterly financial data
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

#' List bank failures
#'
#' @param state Two-letter state code or NULL for all
#' @param start_year Earliest failure year or NULL
#' @param end_year Latest failure year or NULL
#' @param max_results Maximum rows (default 500)
#' @return tibble of bank failures
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

#' Fetch bank event history
#'
#' @param cert FDIC certificate number
#' @param max_results Maximum rows (default 100)
#' @return tibble of history events
fdic_history <- function(cert, max_results = 100) {
  .fdic_fetch_all("history",
                   filters = paste0("CERT:", cert),
                   fields = "CERT,INSTNAME,CHANGECODE,CHANGECODE_DESC,EFFDATE,PROCDATE,CITY,STALP",
                   sort_by = "EFFDATE", sort_order = "DESC",
                   max_results = max_results) |>
    mutate(CERT = as.integer(CERT))
}

# == Summary / Aggregate =======================================================

#' Fetch aggregate banking summary statistics
#'
#' @param state Two-letter state code or NULL for national
#' @param cb_si "CB" for commercial banks, "SI" for savings institutions, or NULL for both
#' @param max_results Maximum rows (default 200)
#' @return tibble of aggregate annual data
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

#' Return full function source for LLM context
#'
#' @return Prints and invisibly returns all public function bodies
fdic_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/fdic.gov.R"
  if (!file.exists(src_file)) {
    cat("# fdic.gov context - source not found\n")
    return(invisible("# fdic.gov context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) -
        nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
