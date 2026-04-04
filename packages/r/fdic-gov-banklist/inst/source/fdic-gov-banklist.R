# fdic-gov-banklist.R
# Self-contained FDIC Failed Banks List client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Note: This is the failed bank list, separate from the active bank financials
#   in banks.data.fdic.gov

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fdic_bl_url <- "https://www.fdic.gov/bank-failures/download-data.csv"

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

# == Schemas ===================================================================

.schema_failed_banks <- tibble(
  bank_name = character(), city = character(), state = character(),
  cert = integer(), acquiring_institution = character(),
  closing_date = as.Date(character()), fund = integer()
)

# == Failed banks ==============================================================

#' Fetch the FDIC failed bank list
#'
#' Downloads the complete list of FDIC-insured banks that have failed since
#' October 1, 2000. The data is sourced from the official FDIC CSV file and
#' includes bank name, location, certificate number, acquiring institution,
#' closing date, and insurance fund number.
#'
#' @details The FDIC updates this file as new bank failures occur. The CSV
#'   may contain non-ASCII characters (BOM marks), which are handled
#'   internally. Date parsing supports multiple formats used across different
#'   versions of the file.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{bank_name}{Name of the failed bank}
#'     \item{city}{City where the bank was headquartered}
#'     \item{state}{Two-letter state abbreviation}
#'     \item{cert}{FDIC certificate number (unique bank identifier)}
#'     \item{acquiring_institution}{Name of the institution that acquired the failed bank's assets}
#'     \item{closing_date}{Date the bank was closed (Date)}
#'     \item{fund}{FDIC insurance fund number}
#'   }
#' @examples
#' fdic_failed_banks()
#' @seealso [fdic_bl_context()]
#' @source <https://www.fdic.gov/bank-failures/download-data.csv>
#' @export
fdic_failed_banks <- function() {
  f <- .fetch(.fdic_bl_url, ext = ".csv")
  # Read lines, strip non-ASCII (BOM, \xa0 chars in headers)
  lines <- readLines(f, warn = FALSE, encoding = "latin1")
  lines <- iconv(lines, from = "latin1", to = "ASCII", sub = "")
  tc <- textConnection(lines)
  df <- tryCatch(utils::read.csv(tc, stringsAsFactors = FALSE),
                 error = function(e) NULL)
  close(tc)
  if (is.null(df) || nrow(df) == 0) return(.schema_failed_banks)
  df <- as_tibble(df)
  # Normalize column names — strip BOM and special chars
  nms <- names(df)
  nms <- gsub("\uFEFF", "", nms)  # strip BOM
  nms <- tolower(gsub("[^a-zA-Z0-9]", "_", nms))
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  names(df) <- nms

  # Map to standardized names
  bank_col <- intersect(c("bank_name", "bank", "institution_name", "name"), names(df))
  city_col <- intersect(c("city", "city_name"), names(df))
  state_col <- intersect(c("state", "st"), names(df))
  cert_col <- intersect(c("cert", "cert_number", "certificate_number", "cert_"), names(df))
  acq_col <- intersect(c("acquiring_institution", "acquirer", "acquiring_inst"), names(df))
  close_col <- intersect(c("closing_date", "close_date", "date_closed", "closing"), names(df))
  fund_col <- intersect(c("fund", "fund_number"), names(df))

  tibble(
    bank_name = if (length(bank_col) > 0) as.character(df[[bank_col[1]]]) else NA_character_,
    city = if (length(city_col) > 0) as.character(df[[city_col[1]]]) else NA_character_,
    state = if (length(state_col) > 0) as.character(df[[state_col[1]]]) else NA_character_,
    cert = if (length(cert_col) > 0) suppressWarnings(as.integer(df[[cert_col[1]]])) else NA_integer_,
    acquiring_institution = if (length(acq_col) > 0) as.character(df[[acq_col[1]]]) else NA_character_,
    closing_date = if (length(close_col) > 0) {
      tryCatch(as.Date(df[[close_col[1]]], format = "%d-%b-%y"), error = function(e) {
        tryCatch(as.Date(df[[close_col[1]]], format = "%B %d, %Y"), error = function(e2) {
          tryCatch(as.Date(df[[close_col[1]]]), error = function(e3) {
            as.Date(rep(NA, nrow(df)))
          })
        })
      })
    } else as.Date(rep(NA, nrow(df))),
    fund = if (length(fund_col) > 0) suppressWarnings(as.integer(df[[fund_col[1]]])) else NA_integer_
  )
}

# == Context ===================================================================

#' Get fdic-gov-banklist client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the FDIC failed bank list client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' fdic_bl_context()
#' @export
fdic_bl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fdic_bl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fdic-gov-banklist.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fdic-gov-banklist")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fdic-gov-banklist context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fdic-gov-banklist", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
