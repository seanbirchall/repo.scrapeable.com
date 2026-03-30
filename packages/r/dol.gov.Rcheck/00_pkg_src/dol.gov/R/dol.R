# == Discovery =================================================================

#' List available DOL foreign labor disclosure files
#'
#' Scrapes the DOL performance page to find downloadable XLSX files.
#' Filters out addendums, appendices, and worksite files.
#'
#' @return tibble: url, filename, program (PERM, H-1B, H-2A, H-2B, CW-1, PW)
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
#' @param url Full URL to the XLSX file
#' @param sheet Sheet number or name (default 1)
#' @return tibble of disclosure records with clean column names
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
#' @param url Full URL to the XLSX file
#' @return tibble with all sheets stacked, with a sheet_name column
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
#' Scrapes the file listing, filters to the given program, and downloads
#' the first (most recent) match.
#'
#' @param program One of: "PERM", "H-1B", "H-2A", "H-2B", "CW-1", "PW"
#' @param sheet Sheet number or name (default 1)
#' @return tibble of disclosure records
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
#' Downloads every XLSX for the given program type and stacks them.
#'
#' @param program One of: "PERM", "H-1B", "H-2A", "H-2B", "CW-1", "PW"
#' @param sheet Sheet number or name (default 1)
#' @return tibble of all disclosure records with a source_file column
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

#' Generate LLM-friendly context for the dol.gov package
#'
#' @param compact If TRUE (default), concise output
#' @return Character string (invisibly), also printed
#' @export
dol_context <- function(compact = TRUE) {
  fns <- list(
    list("dol_files", "()",
         "List available XLSX files from the DOL performance page. Returns url, filename, program."),
    list("dol_disclosure", "(url, sheet)",
         "Download and parse a specific XLSX file. Returns tibble with clean column names."),
    list("dol_disclosure_all", "(url)",
         "Download all sheets from an XLSX file. Stacked tibble with sheet_name column."),
    list("dol_program", "(program, sheet)",
         "Fetch most recent file for a program. program = 'H-1B', 'PERM', 'H-2A', 'H-2B', 'CW-1', 'PW'."),
    list("dol_program_bulk", "(program, sheet)",
         "Fetch ALL files for a program, stacked with source_file column.")
  )

  lines <- c(
    "# dol.gov - Department of Labor Foreign Labor Disclosure Client for R",
    "# Dependencies: httr2, dplyr, tibble, xml2, readxl",
    "# Auth: none",
    "# Data: quarterly XLSX files from https://www.dol.gov/agencies/eta/foreign-labor/performance",
    "#",
    "# Programs:",
    "#   PERM  = Permanent labor certification (green card sponsorship)",
    "#   H-1B  = Specialty occupation workers (LCA filings)",
    "#   H-2A  = Temporary agricultural workers",
    "#   H-2B  = Temporary non-agricultural workers",
    "#   CW-1  = CNMI-only transitional workers",
    "#   PW    = Prevailing wage determinations",
    "#",
    "# == Functions ==",
    "#"
  )

  for (fn in fns) {
    lines <- c(lines,
      sprintf("# %s%s", fn[[1]], fn[[2]]),
      sprintf("#   %s", fn[[3]]),
      sprintf("#   Run `%s` to view source or `?%s` for help.", fn[[1]], fn[[1]]),
      "#"
    )
  }

  lines <- c(lines,
    "# == Quick examples ==",
    "#",
    "# dol_files()                        # see all available files",
    "# dol_program('H-1B')                # latest H-1B disclosure data",
    "# dol_program('PERM')                # latest PERM data",
    "# dol_program_bulk('H-1B')           # all H-1B files combined",
    "#"
  )

  out <- paste(lines, collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
