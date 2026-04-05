# dol.R
# Self-contained Department of Labor foreign labor disclosure client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2, readxl
# Auth: none
# Data source: https://www.dol.gov/agencies/eta/foreign-labor/performance
# Updated quarterly. XLSX files for PERM, H-1B, H-2A, H-2B, CW-1, PW programs.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.dol_page <- "https://www.dol.gov/agencies/eta/foreign-labor/performance"

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

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_html <- function(url) xml2::read_html(.fetch(url, ".html"))

# -- Program classifier -------------------------------------------------------

.classify_program <- function(filename) {
  f <- toupper(filename)
  case_when(
    grepl("PERM", f)        ~ "PERM",
    grepl("H.?2A|_2A", f)   ~ "H-2A",
    grepl("H.?2B|_2B", f)   ~ "H-2B",
    grepl("LCA|H.?1B", f)   ~ "H-1B",
    grepl("PW", f)           ~ "PW",
    grepl("CW", f)           ~ "CW-1",
    TRUE                     ~ "OTHER"
  )
}

# == Schemas ===================================================================

.schema_files <- tibble(
  url = character(), filename = character(), program = character()
)


# == Discovery =================================================================

#' List available DOL foreign labor disclosure files
#'
#' Scrapes the U.S. Department of Labor ETA foreign labor certification
#' performance page and extracts links to all XLSX disclosure data files.
#' Supplementary files (addendums, appendices, worksite files) are filtered
#' out automatically. Each file is classified into a program type.
#'
#' @return A tibble with one row per disclosure file and columns:
#' \describe{
#'   \item{url}{Character. Full download URL for the XLSX file.}
#'   \item{filename}{Character. Basename of the file.}
#'   \item{program}{Character. Program classification: \code{"PERM"},
#'     \code{"H-1B"}, \code{"H-2A"}, \code{"H-2B"}, \code{"CW-1"},
#'     \code{"PW"}, or \code{"OTHER"}.}
#' }
#' @examples
#' \dontrun{
#' dol_files()
#' }
#' @export
dol_files <- function() {
  doc <- .fetch_html(.dol_page)
  links <- xml2::xml_find_all(doc, ".//table//a")
  hrefs <- xml2::xml_attr(links, "href")

  # Keep only XLSX links
  keep <- !is.na(hrefs) & grepl("\\.xlsx$", hrefs, ignore.case = TRUE)
  if (!any(keep)) return(.schema_files)
  hrefs <- hrefs[keep]

  # Build full URLs
  urls <- ifelse(grepl("^https?://", hrefs), hrefs,
                 paste0("https://www.dol.gov", hrefs))
  filenames <- basename(hrefs)

  df <- tibble(url = urls, filename = filenames) |>
    # Filter out supplementary files
    filter(!grepl("addendum|appendix|worksite", filename, ignore.case = TRUE)) |>
    mutate(program = .classify_program(filename))

  df
}


# == Data fetching =============================================================

#' Download and parse a DOL disclosure XLSX file
#'
#' Downloads a single XLSX disclosure file from the DOL website and reads
#' the specified sheet into a tibble. Column names are normalised to
#' \code{snake_case}. Requires the \pkg{readxl} package.
#'
#' @param url Character. Full URL to the XLSX file (obtain from
#'   \code{\link{dol_files}}).
#' @param sheet Integer or character. Sheet number or name to read
#'   (default 1).
#' @return A tibble of disclosure records with snake_case column names.
#'   Columns vary by program type and fiscal year.
#' @examples
#' \dontrun{
#' files <- dol_files()
#' dol_disclosure(files$url[1])
#' }
#' @export
dol_disclosure <- function(url, sheet = 1) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' required for dol_disclosure", call. = FALSE)

  path <- .fetch(url, ".xlsx")
  sheets <- readxl::excel_sheets(path)

  if (is.numeric(sheet) && sheet > length(sheets))
    stop(sprintf("Sheet %d not found. Available: %s",
                 sheet, paste(sheets, collapse = ", ")), call. = FALSE)

  as_tibble(readxl::read_excel(path, sheet = sheet, .name_repair = "unique")) |>
    rename_with(~ gsub("[. ]+", "_", tolower(.x)))
}

#' Download all sheets from a DOL disclosure XLSX file
#'
#' Downloads a single XLSX file and reads every sheet, row-binding the
#' results into one tibble. A \code{sheet_name} column identifies the
#' source sheet. Requires the \pkg{readxl} package.
#'
#' @param url Character. Full URL to the XLSX file.
#' @return A tibble with all sheets stacked. Includes a \code{sheet_name}
#'   column indicating which Excel sheet each row came from.
#' @examples
#' \dontrun{
#' files <- dol_files()
#' dol_disclosure_all(files$url[1])
#' }
#' @export
dol_disclosure_all <- function(url) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' required", call. = FALSE)

  path <- .fetch(url, ".xlsx")
  sheets <- readxl::excel_sheets(path)

  results <- lapply(sheets, function(s) {
    tryCatch({
      as_tibble(readxl::read_excel(path, sheet = s, .name_repair = "unique")) |>
        rename_with(~ gsub("[. ]+", "_", tolower(.x))) |>
        mutate(sheet_name = s)
    }, error = function(e) NULL)
  })
  bind_rows(results)
}


# == Convenience ===============================================================

#' Fetch the most recent DOL disclosure file for a program
#'
#' Convenience function that scrapes the file listing, filters to the
#' specified program type, and downloads the first (most recent) match.
#' Prints a message showing the file being downloaded.
#'
#' @param program Character. One of \code{"PERM"}, \code{"H-1B"},
#'   \code{"H-2A"}, \code{"H-2B"}, \code{"CW-1"}, or \code{"PW"}.
#'   Default \code{"H-1B"}.
#' @param sheet Integer or character. Sheet number or name (default 1).
#' @return A tibble of disclosure records (see \code{\link{dol_disclosure}}).
#' @examples
#' \dontrun{
#' dol_program("H-1B")
#' dol_program("PERM", sheet = 1)
#' }
#' @export
dol_program <- function(program = "H-1B", sheet = 1) {
  files <- dol_files()
  if (nrow(files) == 0) stop("No DOL files found", call. = FALSE)

  match <- files |> filter(.data$program == toupper(.env$program))
  if (nrow(match) == 0)
    stop(sprintf("No files found for program: %s. Available: %s",
                 program, paste(unique(files$program), collapse = ", ")),
         call. = FALSE)

  message(sprintf("Downloading %s: %s", match$filename[1], match$url[1]))
  dol_disclosure(match$url[1], sheet)
}

#' Fetch all available files for a program
#'
#' Downloads every XLSX file for the given program type from the DOL
#' performance page and stacks all records into a single tibble. A
#' \code{source_file} column identifies the origin of each row. Progress
#' messages are printed as each file is downloaded.
#'
#' @param program Character. One of \code{"PERM"}, \code{"H-1B"},
#'   \code{"H-2A"}, \code{"H-2B"}, \code{"CW-1"}, or \code{"PW"}.
#'   Default \code{"H-1B"}.
#' @param sheet Integer or character. Sheet number or name (default 1).
#' @return A tibble of all disclosure records with a \code{source_file}
#'   column indicating the XLSX filename each row was read from.
#' @examples
#' \dontrun{
#' dol_program_bulk("H-1B")
#' dol_program_bulk("PERM", sheet = 1)
#' }
#' @export
dol_program_bulk <- function(program = "H-1B", sheet = 1) {
  files <- dol_files()
  if (nrow(files) == 0) stop("No DOL files found", call. = FALSE)

  match <- files |> filter(.data$program == toupper(.env$program))
  if (nrow(match) == 0)
    stop(sprintf("No files found for program: %s", program), call. = FALSE)

  n <- nrow(match)
  results <- lapply(seq_len(n), function(i) {
    message(sprintf("[%d/%d] %s", i, n, match$filename[i]))
    tryCatch({
      dol_disclosure(match$url[i], sheet) |>
        mutate(source_file = match$filename[i])
    }, error = function(e) {
      message("  Failed: ", e$message)
      NULL
    })
  })
  bind_rows(results)
}


# == Context (LLM injection) ==================================================

#' Get dol client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
dol_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(dol_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/dol.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "dol")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# dol context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# dol", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
