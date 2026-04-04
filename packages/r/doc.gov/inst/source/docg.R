# doc.gov.R
# Self-contained U.S. Department of Commerce (doc.gov) client.
# Data sources: NTIA spectrum inventory, NTIA Digital Nation datasets.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public CSV downloads)
# Docs: https://www.ntia.gov/category/data-central
#        https://www.ntia.gov/other-publication/2015/spectrum-compendium-datasets-and-archived-band-reports

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"

# -- Context generator ---------------------------------------------------------

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

# -- CSV fetcher ---------------------------------------------------------------

.docg_fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE) |>
    as_tibble()
}

.docg_clean_names <- function(df) {
  nms <- names(df)
  nms <- gsub("[/ ()-]+", "_", nms)
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  nms <- tolower(nms)
  names(df) <- nms
  df
}


# == Schemas ===================================================================

.schema_spectrum <- tibble(
  frequency = character(), agency = character(),
  service = character(), total = integer()
)

.schema_digital_nation <- tibble(
  dataset = character(), variable = character(),
  description = character(), universe = character(),
  us_prop = numeric(), us_count = numeric()
)


# == Discovery =================================================================

#' List available Department of Commerce datasets
#'
#' Returns a tibble describing each downloadable dataset from
#' NTIA (National Telecommunications and Information Administration)
#' and other doc.gov subdomains.
#'
#' @return tibble with dataset name, description, source, and download URL
docg_list <- function() {
  tibble(
    name = c(
      "spectrum_inventory_225_5000",
      "spectrum_inventory_225_5000_old",
      "spectrum_5_6ghz",
      "spectrum_summary_5925_7125",
      "digital_nation_analyze_table"
    ),
    description = c(
      "Federal frequency assignments 225-5000 MHz by agency, band, and service (Dec 2015)",
      "Federal frequency assignments 225-5000 MHz (May 2015 snapshot)",
      "Federal frequency assignments 5-6 GHz band by agency and service (Dec 2015)",
      "Summary of federal assignments in 5925-7125 MHz band by agency and application",
      "NTIA Digital Nation summary statistics on computer and Internet use in the US (1994-2023)"
    ),
    source = c(
      "NTIA Spectrum Compendium",
      "NTIA Spectrum Compendium",
      "NTIA Spectrum Compendium",
      "NTIA Spectrum Compendium",
      "NTIA Data Central / CPS Supplement"
    ),
    url = c(
      "https://www.ntia.gov/sites/default/files/publications/225-5000-composite-inventory_2015-12-16_0.csv",
      "https://www.ntia.gov/sites/default/files/publications/225-5000-composite-inventory_05012015_0.csv",
      "https://www.ntia.gov/sites/default/files/publications/5-6ghz_2015-12-16_0.csv",
      "https://www.ntia.gov/sites/default/files/publications/summary_5925-7125_mhz_0.csv",
      "https://www.ntia.gov/sites/default/files/data_central_downloads/datasets/ntia-analyze-table.csv"
    )
  )
}


#' Search Department of Commerce datasets by keyword
#'
#' Searches dataset names and descriptions for a keyword match.
#'
#' @param query Character string to search for (case-insensitive)
#' @return tibble of matching datasets
docg_search <- function(query) {
  ds <- docg_list()
  pattern <- tolower(query)
  matches <- grepl(pattern, tolower(ds$name)) |
    grepl(pattern, tolower(ds$description)) |
    grepl(pattern, tolower(ds$source))
  ds[matches, ]
}


# == Spectrum Inventory ========================================================

#' Federal spectrum inventory (225-5000 MHz)
#'
#' Count of federal frequency assignments by frequency band, agency, and
#' radio service. Data from NTIA's composite inventory (December 2015).
#'
#' @param agency Regex pattern for agency code (e.g. "AF" for Air Force,
#'   "AR" for Army, "N" for Navy, "DOC", "DOE", "DOI", "DOJ"). NULL = all.
#' @param service Regex pattern for radio service type. NULL = all.
#' @param band Regex pattern for frequency band (e.g. "225", "5000"). NULL = all.
#' @return tibble with columns: frequency, agency, service, total
docg_spectrum <- function(agency = NULL, service = NULL, band = NULL) {
  url <- "https://www.ntia.gov/sites/default/files/publications/225-5000-composite-inventory_2015-12-16_0.csv"
  df <- .docg_fetch_csv(url)
  df <- .docg_clean_names(df)
  df$total <- suppressWarnings(as.integer(df$total))
  df$agency <- trimws(df$agency)
  if (!is.null(agency)) df <- df[grepl(agency, df$agency, ignore.case = TRUE), ]
  if (!is.null(service)) df <- df[grepl(service, df$service, ignore.case = TRUE), ]
  if (!is.null(band)) df <- df[grepl(band, df$frequency, ignore.case = TRUE), ]
  as_tibble(df)
}


#' Spectrum assignments in the 5-6 GHz band
#'
#' Federal frequency assignments in the 5000-6000 MHz range by agency
#' and radio service type.
#'
#' @param agency Regex pattern for agency code. NULL = all.
#' @param service Regex pattern for radio service. NULL = all.
#' @return tibble with columns: frequency, agency, service, total
docg_spectrum_5ghz <- function(agency = NULL, service = NULL) {
  url <- "https://www.ntia.gov/sites/default/files/publications/5-6ghz_2015-12-16_0.csv"
  df <- .docg_fetch_csv(url)
  df <- .docg_clean_names(df)
  df$total <- suppressWarnings(as.integer(df$total))
  df$agency <- trimws(df$agency)
  if (!is.null(agency)) df <- df[grepl(agency, df$agency, ignore.case = TRUE), ]
  if (!is.null(service)) df <- df[grepl(service, df$service, ignore.case = TRUE), ]
  as_tibble(df)
}


#' Summary of spectrum assignments in 5925-7125 MHz band
#'
#' Summary count of federal frequency assignments by agency and application
#' in the 5925-7125 MHz band.
#'
#' @param agency Regex pattern for agency code. NULL = all.
#' @return tibble with columns: freq_mhz, agency, application, count
docg_spectrum_summary <- function(agency = NULL) {
  url <- "https://www.ntia.gov/sites/default/files/publications/summary_5925-7125_mhz_0.csv"
  df <- .docg_fetch_csv(url)
  df <- .docg_clean_names(df)
  # Rename columns for clarity
  if ("freq_mhz" %in% names(df)) {
    # good
  } else if (any(grepl("freq", names(df)))) {
    names(df)[grep("freq", names(df))[1]] <- "freq_mhz"
  }
  df$agency <- trimws(df$agency)
  df$count <- suppressWarnings(as.integer(df$count))
  if (!is.null(agency)) df <- df[grepl(agency, df$agency, ignore.case = TRUE), ]
  as_tibble(df)
}


#' Aggregate spectrum assignments by agency
#'
#' Summarizes total frequency assignments across all bands for each
#' federal agency. Useful for comparing spectrum usage between agencies.
#'
#' @return tibble with columns: agency, total_assignments, n_bands, n_services
docg_spectrum_by_agency <- function() {
  df <- docg_spectrum()
  df |>
    group_by(agency) |>
    summarise(
      total_assignments = sum(total, na.rm = TRUE),
      n_bands = n_distinct(frequency),
      n_services = n_distinct(service),
      .groups = "drop"
    ) |>
    arrange(desc(total_assignments))
}


# == Digital Nation ============================================================

#' NTIA Digital Nation analyze table
#'
#' Summary statistics on computer and Internet use in the US from NTIA's
#' Current Population Survey (CPS) supplements (1994-2023). Includes
#' national proportions and counts broken down by demographics including
#' age, income, education, sex, race, disability, and metro status.
#'
#' @param variable Regex pattern for variable name (e.g. "internetUser",
#'   "computerAtHome", "smartphoneUser"). NULL = all.
#' @param dataset Regex pattern for dataset/survey (e.g. "Nov 2023",
#'   "Nov 2019"). NULL = all.
#' @return tibble with dataset, variable, description, universe,
#'   and national-level proportion and count columns
docg_digital_nation <- function(variable = NULL, dataset = NULL) {
  url <- "https://www.ntia.gov/sites/default/files/data_central_downloads/datasets/ntia-analyze-table.csv"
  df <- .docg_fetch_csv(url)
  # The CSV has many columns — keep key ones and convert numerics
  df <- .docg_clean_names(df)
  # Convert proportion and count columns to numeric
  num_cols <- grep("prop$|propse$|count$|countse$", names(df), value = TRUE)
  for (col in num_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  if (!is.null(variable)) df <- df[grepl(variable, df$variable, ignore.case = TRUE), ]
  if (!is.null(dataset)) df <- df[grepl(dataset, df$dataset, ignore.case = TRUE), ]
  as_tibble(df)
}


#' Digital Nation summary: national-level Internet/computer use over time
#'
#' Extracts a simplified time series of national-level proportions for key
#' digital adoption metrics across all CPS survey waves (1994-2023).
#'
#' @param variable Regex pattern for variable (e.g. "internetUser",
#'   "computerAtHome"). NULL returns all variables.
#' @return tibble with columns: dataset, variable, description,
#'   us_proportion, us_count
docg_digital_nation_summary <- function(variable = NULL) {
  df <- docg_digital_nation(variable = variable)
  # Extract just the national-level summary columns
  cols_keep <- intersect(
    c("dataset", "variable", "description", "universe", "usprop", "usse", "uscount", "uscountse",
      "uspropse"),
    names(df)
  )
  if (length(cols_keep) < 4) {
    # Fallback: keep first 6 columns
    cols_keep <- names(df)[1:min(6, ncol(df))]
  }
  df <- df[, cols_keep, drop = FALSE]
  as_tibble(df)
}


#' List Digital Nation survey variables
#'
#' Returns the unique variable names and descriptions available in the
#' Digital Nation analyze table, useful for discovering what metrics
#' are tracked.
#'
#' @return tibble with variable, description, universe, and number of surveys
docg_digital_nation_variables <- function() {
  df <- docg_digital_nation()
  df |>
    group_by(variable, description, universe) |>
    summarise(
      n_surveys = n(),
      .groups = "drop"
    ) |>
    arrange(desc(n_surveys))
}


# == Context ===================================================================

#' Show Department of Commerce client context
#'
#' Returns function signatures and documentation for this client.
#' Useful for LLM tool integration and discoverability.
#'
#' @return Character string of function signatures (printed and returned invisibly)
docg_context <- function() {
  src <- this_src_file()
  .build_context(
    pkg_name = "doc.gov",
    src_file = src,
    header_lines = c(
      "# Department of Commerce (doc.gov) Data Client",
      "# Sources: NTIA Spectrum Inventory, NTIA Digital Nation",
      "# Datasets: Federal spectrum assignments (225-5000 MHz, 5-6 GHz),",
      "#           Digital Nation computer/Internet use statistics (1994-2023)",
      "# No authentication required"
    )
  )
}

this_src_file <- function() {
  pkg_src <- system.file("source", "docg.R", package = "doc.gov")
  if (pkg_src != "") return(pkg_src)
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)
    fn <- get0("ofile", envir = f, ifnotFound = NULL)
    if (!is.null(fn)) return(fn)
  }
  NULL
}
