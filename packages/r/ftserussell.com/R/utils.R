#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#' @keywords internal
NULL

# ftserussell-com.R
# Self-contained FTSE Russell index data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none required
# Data sources: research.ftserussell.com (CSV for Russell, XLSX for FTSE)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

.russell_inventory_url <- "https://www.lseg.com/content/lseg/en_us/ftse-russell/index-resources/russell-index-values/jcr:content/root/container/page_content_region/page-content-region/section/section-fw/data_table.russellindexvalues.json"

.ftse_inventory_url <- "https://www.lseg.com/content/lseg/en_us/ftse-russell/index-resources/historic-index-values/jcr:content/root/container/page_content_region/page-content-region/section/section-fw/data_table.historicindex.json"

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

.fetch_csv <- function(url) {
  tmp <- .fetch(url, ext = ".csv")
  utils::read.csv(tmp, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM",
                  check.names = FALSE)
}

.fetch_xlsx <- function(url) {
  tmp <- .fetch(url, ext = ".xlsx")
  tmp
}

# == Schemas ===================================================================

.schema_russell_inventory <- tibble(
  family = character(), index = character(),
  historical_url = character(), ytd_url = character()
)

.schema_russell_values <- tibble(
  date = as.Date(character()), index_name = character(),
  price_return = numeric(), total_return = numeric()
)

.schema_ftse_inventory <- tibble(
  group = character(), index_name = character(), index_id = character(),
  currency = character(), url = character()
)

.schema_ftse_values <- tibble(
  date = as.Date(character()), index_name = character(),
  index_id = character(), value = numeric()
)

# -- Russell CSV parser (private) ----------------------------------------------

.parse_russell_csv <- function(url) {
  df <- tryCatch(.fetch_csv(url), error = function(e) {
    warning("Failed to fetch Russell CSV: ", e$message)
    return(NULL)
  })
  if (is.null(df) || nrow(df) == 0) return(.schema_russell_values)

  # Column names may have underscores for spaces and currency suffix
  # Normalize: find columns by pattern
  cols <- names(df)
  date_col <- cols[grepl("Date", cols, ignore.case = TRUE)][1]
  name_col <- cols[grepl("Index.?Name|Index_Name", cols, ignore.case = TRUE)][1]
  price_col <- cols[grepl("Without.?Dividends|Without_Dividends", cols, ignore.case = TRUE)][1]
  total_col <- cols[grepl("With.?Dividends|With_Dividends", cols, ignore.case = TRUE)][1]

  if (is.na(date_col) || is.na(price_col)) {
    warning("Unexpected CSV column structure: ", paste(cols, collapse = ", "))
    return(.schema_russell_values)
  }

  result <- tibble(
    date         = as.Date(df[[date_col]], format = "%m/%d/%Y"),
    index_name   = if (!is.na(name_col)) as.character(df[[name_col]]) else NA_character_,
    price_return = as.numeric(df[[price_col]]),
    total_return = if (!is.na(total_col)) as.numeric(df[[total_col]]) else NA_real_
  ) |>
    filter(!is.na(date)) |>
    arrange(date)

  result
}
