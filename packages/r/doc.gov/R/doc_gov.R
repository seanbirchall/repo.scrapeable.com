# doc.gov.R
# Self-contained U.S. Department of Commerce (doc.gov) client.
# Data sources: NTIA spectrum inventory, NTIA Digital Nation datasets.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public CSV downloads)
# Docs: https://www.ntia.gov/category/data-central
#        https://www.ntia.gov/other-publication/2015/spectrum-compendium-datasets-and-archived-band-reports


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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' including spectrum inventory and Digital Nation survey data.
#'
#' @return A tibble with 4 columns:
#'   \describe{
#'     \item{name}{Character. Dataset identifier (e.g. "spectrum_inventory_225_5000").}
#'     \item{description}{Character. What the dataset contains.}
#'     \item{source}{Character. Data source (e.g. "NTIA Spectrum Compendium").}
#'     \item{url}{Character. Direct CSV download URL.}
#'   }
#' @examples
#' docg_list()
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
#' Searches dataset names, descriptions, and sources for a keyword match.
#'
#' @param query Character. Search term (case-insensitive).
#'   Example: \code{"spectrum"}, \code{"digital"}, \code{"internet"}
#' @return A tibble with same columns as \code{\link{docg_list}}: name,
#'   description, source, url. Only matching rows returned.
#' @examples
#' docg_search("spectrum")
#' docg_search("internet")
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
#' @param agency Character or NULL. Regex pattern for federal agency code.
#'   Common codes: \code{"AF"} (Air Force), \code{"AR"} (Army), \code{"N"} (Navy),
#'   \code{"FAA"}, \code{"DOC"}, \code{"DOE"}, \code{"DOI"}, \code{"DOJ"}.
#'   \code{NULL} returns all agencies.
#' @param service Character or NULL. Regex pattern for radio service type.
#'   Example: \code{"SATELLITE"}, \code{"RADAR"}, \code{"TELEMETRY"}.
#' @param band Character or NULL. Regex pattern for frequency band.
#'   Example: \code{"225"}, \code{"5000"}.
#' @return A tibble with 4 columns:
#'   \describe{
#'     \item{frequency}{Character. Frequency band range (e.g. "225.00-328.60").}
#'     \item{agency}{Character. Federal agency code (e.g. "AF", "AR").}
#'     \item{service}{Character. Radio service type (e.g. "RADAR").}
#'     \item{total}{Integer. Number of assignments.}
#'   }
#' @examples
#' docg_spectrum(agency = "AF")
#' docg_spectrum(service = "RADAR")
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
#' and radio service type. Data from NTIA (December 2015).
#'
#' @param agency Character or NULL. Regex pattern for agency code.
#'   Example: \code{"AF"}, \code{"FAA"}. \code{NULL} returns all.
#' @param service Character or NULL. Regex pattern for radio service.
#'   Example: \code{"SATELLITE"}, \code{"RADAR"}.
#' @return A tibble with 4 columns:
#'   \describe{
#'     \item{frequency}{Character. Frequency band range.}
#'     \item{agency}{Character. Federal agency code.}
#'     \item{service}{Character. Radio service type.}
#'     \item{total}{Integer. Number of assignments.}
#'   }
#' @examples
#' docg_spectrum_5ghz()
#' docg_spectrum_5ghz(agency = "AF")
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
#' in the 5925-7125 MHz band (relevant to Wi-Fi 6E/7 spectrum sharing).
#'
#' @param agency Character or NULL. Regex pattern for agency code.
#'   Example: \code{"AF"}, \code{"AR"}. \code{NULL} returns all.
#' @return A tibble with 4 columns:
#'   \describe{
#'     \item{freq_mhz}{Character. Frequency range in MHz.}
#'     \item{agency}{Character. Federal agency code.}
#'     \item{application}{Character. Application type (e.g. "Fixed point to point microwave").}
#'     \item{count}{Integer. Number of assignments.}
#'   }
#' @examples
#' docg_spectrum_summary()
#' docg_spectrum_summary(agency = "AF")
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
#' Summarizes total frequency assignments across all bands (225-5000 MHz)
#' for each federal agency. Useful for comparing spectrum usage.
#'
#' @return A tibble with 4 columns, sorted by total_assignments descending:
#'   \describe{
#'     \item{agency}{Character. Federal agency code (e.g. "AR", "AF", "N").}
#'     \item{total_assignments}{Integer. Total assignments across all bands.}
#'     \item{n_bands}{Integer. Number of distinct frequency bands used.}
#'     \item{n_services}{Integer. Number of distinct radio service types.}
#'   }
#' @examples
#' docg_spectrum_by_agency()
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
#' Current Population Survey (CPS) supplements (1994-2023). Contains
#' 400+ columns of demographic breakdowns (age, income, education, sex,
#' race, disability, metro status, and all 50 states + DC).
#'
#' @param variable Character or NULL. Regex pattern for variable name.
#'   Common variables: \code{"internetUser"}, \code{"computerAtHome"},
#'   \code{"smartphoneUser"}, \code{"homeInternetUser"}, \code{"broadband"}.
#'   \code{NULL} returns all variables.
#' @param dataset Character or NULL. Regex pattern for survey wave.
#'   Example: \code{"Nov 2023"}, \code{"Nov 2019"}, \code{"Dec 1998"}.
#' @return A tibble with 400+ columns. Key columns:
#'   \describe{
#'     \item{dataset}{Character. Survey wave (e.g. "Nov 2023").}
#'     \item{variable}{Character. Metric name (e.g. "internetUser").}
#'     \item{description}{Character. Human-readable description.}
#'     \item{universe}{Character. Universe definition.}
#'     \item{usprop}{Numeric. National proportion (0-1).}
#'     \item{uspropse}{Numeric. Standard error of proportion.}
#'     \item{uscount}{Numeric. National count.}
#'     \item{uscountse}{Numeric. Standard error of count.}
#'   }
#'   Plus demographic breakdowns (age, income, education, sex, race, etc.)
#'   and state-level estimates (e.g. caprop, nyprop).
#' @examples
#' docg_digital_nation(variable = "internetUser")
#' docg_digital_nation(dataset = "Nov 2023")
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
#' Returns only the national-level columns (no demographic breakdowns).
#'
#' @param variable Character or NULL. Regex pattern for variable.
#'   Example: \code{"internetUser"}, \code{"computerAtHome"}.
#'   \code{NULL} returns all variables.
#' @return A tibble with 8 columns:
#'   \describe{
#'     \item{dataset}{Character. Survey wave (e.g. "Nov 2023").}
#'     \item{variable}{Character. Metric name.}
#'     \item{description}{Character. Human-readable description.}
#'     \item{universe}{Character. Universe definition.}
#'     \item{usprop}{Numeric. National proportion (0-1).}
#'     \item{uscount}{Numeric. National count.}
#'     \item{uscountse}{Numeric. Standard error of count.}
#'     \item{uspropse}{Numeric. Standard error of proportion.}
#'   }
#' @examples
#' docg_digital_nation_summary(variable = "internetUser")
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
#' Digital Nation analyze table. Useful for discovering what metrics
#' are tracked before calling \code{\link{docg_digital_nation}}.
#'
#' @return A tibble with 4 columns, sorted by n_surveys descending:
#'   \describe{
#'     \item{variable}{Character. Variable name (e.g. "internetUser").}
#'     \item{description}{Character. What the variable measures.}
#'     \item{universe}{Character. Population universe.}
#'     \item{n_surveys}{Integer. Number of survey waves with this variable.}
#'   }
#' @examples
#' docg_digital_nation_variables()
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

#' Get doc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
docg_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(docg_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/doc.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "doc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# doc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# doc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
