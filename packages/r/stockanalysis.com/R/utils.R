#' @import dplyr
#' @import tidyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom xml2 read_html xml_find_all xml_text
#' @keywords internal
NULL

# stockanalysis-com.R
# Self-contained Stock Analysis financial data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, xml2, dplyr, tidyr, tibble
# Auth: none required (HTML scraping)
# Rate limits: undocumented, be polite (~1 req/sec)
# Coverage: income, balance sheet, cash flow, ratios for US public companies


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.sa_base <- "https://stockanalysis.com/stocks"

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

# -- Scraping engine -----------------------------------------------------------

.sa_fetch_table <- function(ticker, page) {
  url <- sprintf("%s/%s/%s/", .sa_base, tolower(ticker), page)

  tmp <- tempfile(fileext = ".html")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  doc <- xml2::read_html(tmp)
  tables <- xml2::xml_find_all(doc, ".//table")
  if (length(tables) == 0) return(tibble())

  tbl <- tables[[1]]
  rows <- xml2::xml_find_all(tbl, ".//tr")
  if (length(rows) < 3) return(tibble())

  # Parse all rows
  cells <- lapply(rows, function(r) {
    xml2::xml_text(xml2::xml_find_all(r, ".//td|.//th"), trim = TRUE)
  })

  # Row 1 = period labels (TTM, FY 2025, FY 2024, ...)
  # Row 2 = period ending dates
  # Row 3+ = metrics
  header <- cells[[1]]
  if (length(header) < 2) return(tibble())

  # Extract year columns (skip first which is the metric name)
  year_labels <- header[-1]
  n_cols <- length(year_labels)

  # Parse data rows (skip header rows 1-2)
  data_rows <- cells[-(1:2)]
  if (length(data_rows) == 0) return(tibble())

  # Build tibble: one row per metric-year combination (long format)
  results <- lapply(data_rows, function(r) {
    if (length(r) < 2) return(NULL)
    metric <- r[1]
    if (metric == "" || is.na(metric)) return(NULL)
    vals <- r[-1]
    # Pad if needed
    length(vals) <- n_cols

    tibble(
      metric = metric,
      period = year_labels[seq_along(vals)],
      value_raw = vals
    )
  })

  df <- bind_rows(results)
  if (nrow(df) == 0) return(tibble())

  # Clean values: remove $, %, commas, handle (negative), convert
  df |>
    mutate(
      ticker = toupper(ticker),
      # Extract fiscal year from period label
      fiscal_year = as.integer(gsub(".*?(\\d{4}).*", "\\1", period)),
      is_ttm = period == "TTM",
      # Clean numeric values
      value = gsub("[$,%]", "", value_raw),
      value = gsub("\\((.+)\\)", "-\\1", value),  # (123) -> -123
      value = gsub(",", "", value),
      value = ifelse(value %in% c("-", "n/a", "N/A", ""), NA_character_, value),
      value = as.numeric(value)
    ) |>
    select(ticker, metric, period, fiscal_year, is_ttm, value)
}


# == Schemas ===================================================================

.schema_financials <- tibble(
  ticker = character(), metric = character(), period = character(),
  fiscal_year = integer(), is_ttm = logical(), value = numeric()
)


