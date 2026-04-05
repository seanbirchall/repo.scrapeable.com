# whitehouse.gov.R - Self-contained whitehouse.gov client



# whitehouse-gov.R
# Self-contained OMB Historical Budget Data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble, tidyr, readxl
# Auth: none required
# Data source: XLSX files from whitehouse.gov/omb


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.omb_hist_url <- "https://www.whitehouse.gov/wp-content/uploads/2025/06/BUDGET-2026-HIST.xlsx"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".xlsx") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# -- Table catalog -------------------------------------------------------------

.omb_tables <- tibble::tibble(
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
  col1 <- df[[1]]
  year_rows <- grepl("^\\d{4}$", trimws(col1))

  if (sum(year_rows) > 10) {
    .parse_tall(df, table_id)
  } else {
    .parse_wide(df, table_id)
  }
}

.parse_tall <- function(df, table_id) {
  col1 <- trimws(df[[1]])

  year_label_row <- which(grepl("^Year$", col1, ignore.case = TRUE))[1]

  single_year_rows <- which(grepl("^\\d{4}$", col1))
  first_year_row <- single_year_rows[1]
  if (is.na(first_year_row)) return(NULL)

  if (!is.na(year_label_row)) {
    n_header_rows <- first_year_row - year_label_row
    if (n_header_rows == 1) {
      header <- trimws(as.character(df[year_label_row, ]))
    } else {
      group_row <- trimws(as.character(df[year_label_row, ]))
      sub_row <- trimws(as.character(df[year_label_row + 1, ]))
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
    header <- trimws(as.character(df[first_year_row - 1, ]))
    header[1] <- "fiscal_year"
  }

  header <- gsub("[^a-zA-Z0-9_]", "_", header)
  header <- gsub("_+", "_", header)
  header <- gsub("^_|_$", "", header)
  header <- make.names(header, unique = TRUE)

  non_empty <- which(header != "" & !grepl("^NA", header) & !grepl("^X$", header))
  if (length(non_empty) > 0) {
    last_col <- max(non_empty)
    header <- header[1:last_col]
    df <- df[, 1:last_col, drop = FALSE]
  }

  data_rows <- df[single_year_rows, 1:length(header), drop = FALSE]
  names(data_rows) <- header

  result <- tibble::as_tibble(data_rows)
  result$fiscal_year <- as.integer(result$fiscal_year)

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

  data_rows <- df[(year_header_row + 1):nrow(df), ]

  categories <- trimws(as.character(data_rows[[label_col]]))

  results <- lapply(year_cols, function(yc) {
    vals <- as.character(data_rows[[yc]])
    vals[grepl("^\\.+$", vals)] <- NA
    tibble::tibble(
      category    = categories,
      fiscal_year = as.integer(years[yc]),
      value       = as.numeric(gsub(",", "", vals))
    )
  })

  result <- dplyr::bind_rows(results) |>
    dplyr::filter(!is.na(.data$category), .data$category != "",
                  !grepl("Return to", .data$category, ignore.case = TRUE))

  result$table_id <- table_id
  result
}

# == Schemas ===================================================================

.schema_tables <- tibble::tibble(
  table_id = character(), title = character(), section = character()
)

.schema_tall <- tibble::tibble(
  fiscal_year = integer(), table_id = character()
)

.schema_wide <- tibble::tibble(
  category = character(), fiscal_year = integer(),
  value = numeric(), table_id = character()
)

# == Public functions ==========================================================

#' List available OMB historical budget tables
#'
#' Returns a catalog of the 24 most commonly used historical budget tables
#' from the Office of Management and Budget. These cover budget totals,
#' receipts, outlays, budget authority, federal debt, and economic
#' indicators from 1789 to present.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{table_id}{Table identifier (e.g., "1.1", "7.1")}
#'     \item{title}{Human-readable table title}
#'     \item{section}{Budget section category}
#'   }
#' @export
#' @family OMB functions
#' @seealso \code{\link{omb_table}} to fetch any table by ID
#' @examples
#' \dontrun{
#' omb_tables()
#' }
omb_tables <- function() {
  .omb_tables
}


#' Fetch an OMB historical budget table
#'
#' Downloads and parses a specific table from the OMB Historical Tables
#' XLSX file. Automatically detects whether the table is in "tall" format
#' (fiscal_year + named value columns) or "wide" format (category +
#' fiscal_year + value in long form).
#'
#' @param table_id Character table identifier (e.g., \code{"1.1"},
#'   \code{"3.2"}, \code{"7.1"}). Use \code{\link{omb_tables}} to see
#'   available table IDs and their descriptions.
#' @param url Optional character URL to the HIST.xlsx file. Defaults
#'   to the FY2026 President's Budget.
#' @return A tibble with fiscal_year and table data. Tall tables have
#'   named numeric columns; wide tables have category, fiscal_year, and
#'   value columns.
#' @export
#' @family OMB functions
#' @seealso \code{\link{omb_tables}} for the table catalog
#' @examples
#' \dontrun{
#' omb_table("1.1")  # Budget summary
#' omb_table("7.1")  # Federal debt
#' }
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
#' Convenience wrapper for Table 1.1: Summary of Receipts, Outlays,
#' and Surpluses or Deficits from 1789 to present. Values in millions
#' of dollars.
#'
#' @param url Optional character URL to the HIST.xlsx file.
#' @return A tibble with fiscal_year, receipts, outlays, surplus_deficit,
#'   and table_id columns.
#' @export
#' @family OMB functions
#' @seealso \code{\link{omb_gdp_pct}} for the same data as percent of GDP
#' @examples
#' \dontrun{
#' omb_summary()
#' }
omb_summary <- function(url = NULL) {
  omb_table("1.1", url = url)
}


#' Fetch budget as percentage of GDP (Table 1.2)
#'
#' Convenience wrapper for Table 1.2: Summary as Percentages of GDP.
#'
#' @param url Optional character URL to the HIST.xlsx file.
#' @return A tibble with fiscal_year, receipts/outlays/surplus_deficit
#'   as percentages of GDP, and table_id.
#' @export
#' @family OMB functions
#' @seealso \code{\link{omb_summary}} for nominal dollar values
omb_gdp_pct <- function(url = NULL) {
  omb_table("1.2", url = url)
}


#' Fetch outlays by agency (Table 4.1)
#'
#' Convenience wrapper for Table 4.1: Outlays by Agency. Returns
#' federal outlays by department/agency in long format. Values in
#' millions of dollars.
#'
#' @param url Optional character URL to the HIST.xlsx file.
#' @return A tibble with category (agency name), fiscal_year, value,
#'   and table_id columns.
#' @export
#' @family OMB functions
omb_outlays_by_agency <- function(url = NULL) {
  omb_table("4.1", url = url)
}


#' Fetch federal debt (Table 7.1)
#'
#' Convenience wrapper for Table 7.1: Federal Debt at the End of Year.
#' Returns gross debt, debt held by the public, and debt held by
#' government accounts. Values in millions of dollars.
#'
#' @param url Optional character URL to the HIST.xlsx file.
#' @return A tibble with fiscal_year, debt columns, and table_id.
#' @export
#' @family OMB functions
#' @examples
#' \dontrun{
#' omb_debt()
#' }
omb_debt <- function(url = NULL) {
  omb_table("7.1", url = url)
}


#' Fetch GDP and economic indicators (Table 10.1)
#'
#' Convenience wrapper for Table 10.1: GDP, Deflators, Population,
#' and Other economic series.
#'
#' @param url Optional character URL to the HIST.xlsx file.
#' @return A tibble with fiscal_year, economic indicator columns, and table_id.
#' @export
#' @family OMB functions
#' @examples
#' \dontrun{
#' omb_gdp()
#' }
omb_gdp <- function(url = NULL) {
  omb_table("10.1", url = url)
}


# == Context ===================================================================

#' Get whitehouse.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
omb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(omb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/whitehouse.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "whitehouse.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# whitehouse.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# whitehouse.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
