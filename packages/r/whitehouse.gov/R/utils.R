#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @keywords internal
NULL

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
