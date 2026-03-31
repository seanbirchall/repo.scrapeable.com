# whitehouse-gov.R
# Self-contained OMB Historical Budget Data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, tidyr, readxl
# Auth: none required
# Data source: XLSX files from whitehouse.gov/omb

library(dplyr, warn.conflicts = FALSE)
library(tibble)
library(tidyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.omb_hist_url <- "https://www.whitehouse.gov/wp-content/uploads/2025/06/BUDGET-2026-HIST.xlsx"

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

.fetch <- function(url, ext = ".xlsx") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# -- Table catalog -------------------------------------------------------------

.omb_tables <- tibble(
  table_id = c("1.1", "1.2", "1.3", "1.4",
               "2.1", "2.2", "2.3", "2.4", "2.5",
               "3.1", "3.2",
               "4.1", "4.2",
               "5.1", "5.2", "5.3", "5.4", "5.5", "5.6",
               "6.1",
               "7.1", "7.2", "7.3",
               "10.1"),
  title = c("Summary of Receipts, Outlays, and Surpluses or Deficits",
            "Summary as Percentages of GDP",
            "Summary in Constant (FY 2017) Dollars",
            "Receipts, Outlays, and Surpluses or Deficits by Fund Group",
            "Receipts by Source",
            "Percentage Composition of Receipts by Source",
            "Receipts by Source as Percentages of GDP",
            "Composition of Social Insurance and Retirement Receipts",
            "Composition of Other Receipts",
            "Outlays by Superfunction and Function",
            "Outlays by Function and Subfunction",
            "Outlays by Agency",
            "Percentage Distribution of Outlays by Agency",
            "Budget Authority by Function and Subfunction",
            "Budget Authority by Agency",
            "Percentage Distribution of Budget Authority by Agency",
            "Discretionary Budget Authority by Agency",
            "Percentage Distribution of Discretionary Budget Authority by Agency",
            "Budget Authority for Discretionary Programs",
            "Composition of Outlays: Mandatory and Discretionary",
            "Federal Debt at the End of Year",
            "Percentage Composition of Federal Debt",
            "Statutory Debt Limit",
            "GDP, Deflators, Population, and Other"),
  section = c(rep("Budget Totals", 4),
              rep("Receipts", 5),
              rep("Outlays by Function", 2),
              rep("Outlays by Agency", 2),
              rep("Budget Authority", 6),
              "Composition",
              rep("Federal Debt", 3),
              "Economic Indicators")
)

# -- Sheet name resolver -------------------------------------------------------

.table_to_sheet <- function(table_id) {
  # Table 1.1 -> hist01z1, Table 3.2 -> hist03z2, Table 10.1 -> hist10z1
  parts <- strsplit(table_id, "\\.")[[1]]
  sprintf("hist%02dz%s", as.integer(parts[1]), parts[2])
}

# -- XLSX parser ---------------------------------------------------------------

.parse_hist_sheet <- function(xlsx_path, sheet_name, table_id) {
  df <- tryCatch(
    readxl::read_excel(xlsx_path, sheet = sheet_name, col_names = FALSE),
    error = function(e) {
      warning("Failed to read sheet ", sheet_name, ": ", e$message)
      return(NULL)
    }
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Convert all columns to character for uniform processing
  df <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)

  # Detect if this is a "tall" table (year in col A) or "wide" table (years as columns)
  # Tall tables: first column has 4-digit years
  col1 <- df[[1]]
  year_rows <- grepl("^\\d{4}$", trimws(col1))

  if (sum(year_rows) > 10) {
    # Tall format
    .parse_tall(df, table_id)
  } else {
    # Wide format
    .parse_wide(df, table_id)
  }
}

.parse_tall <- function(df, table_id) {
  col1 <- trimws(df[[1]])

  # Find the "Year" header row
  year_label_row <- which(grepl("^Year$", col1, ignore.case = TRUE))[1]

  # Find first single-year data row (4-digit year, not a range like 1789-1849)
  single_year_rows <- which(grepl("^\\d{4}$", col1))
  first_year_row <- single_year_rows[1]
  if (is.na(first_year_row)) return(NULL)

  # Build column names from the multi-row header
  # Typically row 3 has group headers ("Total", "On-Budget", "Off-Budget")
  # and row 4 has sub-headers ("Receipts", "Outlays", "Surplus or Deficit")
  if (!is.na(year_label_row)) {
    # Combine header rows: group_subheader
    n_header_rows <- first_year_row - year_label_row
    if (n_header_rows == 1) {
      header <- trimws(as.character(df[year_label_row, ]))
    } else {
      # Multi-row header: combine group + sub
      group_row <- trimws(as.character(df[year_label_row, ]))
      sub_row <- trimws(as.character(df[year_label_row + 1, ]))
      # Fill forward group names
      cur_group <- ""
      for (j in seq_along(group_row)) {
        if (!is.na(group_row[j]) && group_row[j] != "") cur_group <- group_row[j]
        else group_row[j] <- cur_group
      }
      header <- ifelse(
        !is.na(sub_row) & sub_row != "",
        paste(group_row, sub_row, sep = "_"),
        group_row
      )
    }
    header[1] <- "fiscal_year"
  } else {
    # Fallback: use row before first data
    header <- trimws(as.character(df[first_year_row - 1, ]))
    header[1] <- "fiscal_year"
  }

  # Clean header names
  header <- gsub("[^a-zA-Z0-9_]", "_", header)
  header <- gsub("_+", "_", header)
  header <- gsub("^_|_$", "", header)
  header <- make.names(header, unique = TRUE)

  # Remove empty trailing columns
  non_empty <- which(header != "" & !grepl("^NA", header) & !grepl("^X$", header))
  if (length(non_empty) > 0) {
    last_col <- max(non_empty)
    header <- header[1:last_col]
    df <- df[, 1:last_col, drop = FALSE]
  }

  # Extract only single-year data rows
  data_rows <- df[single_year_rows, 1:length(header), drop = FALSE]
  names(data_rows) <- header

  result <- as_tibble(data_rows)
  result$fiscal_year <- as.integer(result$fiscal_year)

  # Convert numeric columns (replace dots placeholder with NA)
  for (col in names(result)) {
    if (col == "fiscal_year") next
    vals <- result[[col]]
    vals[grepl("^\\.+$", vals)] <- NA
    result[[col]] <- suppressWarnings(as.numeric(gsub(",", "", vals)))
  }

  result$table_id <- table_id
  result
}

.parse_wide <- function(df, table_id) {
  # Find the row that contains fiscal years as column headers
  # Look for a row where most cells are 4-digit years
  year_header_row <- NA
  for (i in 1:min(10, nrow(df))) {
    row_vals <- trimws(as.character(df[i, ]))
    n_years <- sum(grepl("^\\d{4}$", row_vals))
    if (n_years >= 3) {
      year_header_row <- i
      break
    }
  }

  if (is.na(year_header_row)) return(NULL)

  years <- trimws(as.character(df[year_header_row, ]))
  label_col <- which(!grepl("^\\d{4}$", years) & !is.na(years))[1]
  if (is.na(label_col)) label_col <- 1
  year_cols <- which(grepl("^\\d{4}$", years))

  if (length(year_cols) == 0) return(NULL)

  # Data starts after header
  data_rows <- df[(year_header_row + 1):nrow(df), ]

  # Get category labels
  categories <- trimws(as.character(data_rows[[label_col]]))

  # Build long-form tibble
  results <- lapply(year_cols, function(yc) {
    vals <- as.character(data_rows[[yc]])
    vals[grepl("^\\.+$", vals)] <- NA
    tibble(
      category    = categories,
      fiscal_year = as.integer(years[yc]),
      value       = as.numeric(gsub(",", "", vals))
    )
  })

  result <- bind_rows(results) |>
    filter(!is.na(category), category != "", !grepl("Return to", category, ignore.case = TRUE))

  result$table_id <- table_id
  result
}

# == Schemas ===================================================================

.schema_tables <- tibble(
  table_id = character(), title = character(), section = character()
)

.schema_tall <- tibble(
  fiscal_year = integer(), table_id = character()
)

.schema_wide <- tibble(
  category = character(), fiscal_year = integer(),
  value = numeric(), table_id = character()
)

# == Public functions ==========================================================

#' List available OMB historical budget tables
#'
#' Returns a catalog of the 24 most commonly used historical budget tables.
#' These cover budget totals, receipts, outlays, budget authority, debt,
#' and economic indicators from 1789 to present.
#'
#' @return tibble: table_id (character), title (character), section (character)
omb_tables <- function() {
  .omb_tables
}


#' Fetch an OMB historical budget table
#'
#' Downloads and parses a specific table from the OMB Historical Tables XLSX.
#' Tables in "tall" format return fiscal_year + named value columns.
#' Tables in "wide" format return category + fiscal_year + value (long form).
#'
#' @param table_id Table identifier (e.g. "1.1", "3.2", "7.1").
#'   Use omb_tables() to see available tables.
#' @param url Optional URL to the HIST.xlsx file (default: FY2026 budget)
#' @return tibble with fiscal_year and table data
omb_table <- function(table_id, url = NULL) {
  if (is.null(url)) url <- .omb_hist_url

  sheet <- .table_to_sheet(table_id)
  xlsx_path <- .fetch(url)
  result <- .parse_hist_sheet(xlsx_path, sheet, table_id)

  if (is.null(result) || nrow(result) == 0) {
    warning("No data parsed from table ", table_id)
    return(.schema_wide)
  }
  result
}


#' Fetch budget summary (Table 1.1)
#'
#' Returns receipts, outlays, and surplus/deficit from 1789 to present.
#' Values in millions of dollars.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: fiscal_year, receipts, outlays, surplus_deficit, table_id
omb_summary <- function(url = NULL) {
  omb_table("1.1", url = url)
}


#' Fetch budget as percentage of GDP (Table 1.2)
#'
#' Returns receipts, outlays, and surplus/deficit as percentages of GDP.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: fiscal_year, receipts_pct_gdp, outlays_pct_gdp,
#'   surplus_deficit_pct_gdp, table_id
omb_gdp_pct <- function(url = NULL) {
  omb_table("1.2", url = url)
}


#' Fetch outlays by agency (Table 4.1)
#'
#' Returns federal outlays by department/agency in long format.
#' Values in millions of dollars.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: category (agency name), fiscal_year, value, table_id
omb_outlays_by_agency <- function(url = NULL) {
  omb_table("4.1", url = url)
}


#' Fetch federal debt (Table 7.1)
#'
#' Returns federal debt at end of year: gross, held by public,
#' held by government accounts. Values in millions of dollars.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: fiscal_year, debt columns, table_id
omb_debt <- function(url = NULL) {
  omb_table("7.1", url = url)
}


#' Fetch GDP and economic indicators (Table 10.1)
#'
#' Returns GDP, deflators, population, and other economic series.
#'
#' @param url Optional URL to the HIST.xlsx file
#' @return tibble: fiscal_year, economic indicator columns, table_id
omb_gdp <- function(url = NULL) {
  omb_table("10.1", url = url)
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the OMB package
#'
#' @return Character string (invisibly), also printed
omb_context <- function() {
  .build_context("whitehouse.gov", header_lines = c(
    "# whitehouse.gov - OMB Historical Budget Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble, tidyr, readxl",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Source: OMB Historical Tables XLSX (48 tables, 1789-present)",
    "# Values in millions of dollars unless noted as percentages.",
    "#",
    "# Key tables:",
    "#   1.1 = Budget summary (receipts, outlays, surplus/deficit)",
    "#   1.2 = Budget as % of GDP",
    "#   3.2 = Outlays by function/subfunction",
    "#   4.1 = Outlays by agency",
    "#   7.1 = Federal debt",
    "#   10.1 = GDP and economic indicators"
  ))
}
