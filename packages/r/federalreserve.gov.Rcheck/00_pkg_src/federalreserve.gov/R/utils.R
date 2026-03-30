#' @import dplyr
#' @import tidyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @keywords internal
NULL

# federalreserve-gov.R
# Self-contained Federal Reserve Board data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, dplyr, tidyr, tibble
# Auth: none required (public CSV downloads from Fed data program)
# Data source: https://www.federalreserve.gov/datadownload/
#
# Note: The Fed Board publishes data directly via CSV. This client
# uses hardcoded series hashes for major releases. For broader discovery,
# use the fred.stlouisfed.org package which indexes all Fed data.


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
.fed_base <- "https://www.federalreserve.gov/datadownload"

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

# -- Known series hashes (from the Fed data download program) ------------------

.fed_series <- list(
  # H.15 Selected Interest Rates — Treasury yields
  h15_treasury = "bf17364827e38702b42a58cf8eaa3f78",
  # H.15 — Federal funds rate
  h15_fedfunds = "1345e87c69e0ca0d3ff12584d4b3d656",
  # H.15 — Commercial paper rates
  h15_cp       = "dedd2a0e7e88a0f86f1b7b52315e43ff",
  # H.15 — Bank prime rate
  h15_prime    = "22ae479f5d28fbc8b83eaca6b8b4e152"
)

# -- CSV fetch + parse engine --------------------------------------------------

.fed_fetch_csv <- function(release, series_hash, lastobs = NULL,
                           from = NULL, to = NULL) {
  url <- sprintf("%s/Output.aspx?rel=%s&series=%s&filetype=csv&label=include&layout=seriescolumn",
                 .fed_base, release, series_hash)
  if (!is.null(lastobs)) url <- paste0(url, "&lastobs=", lastobs)
  if (!is.null(from))    url <- paste0(url, "&from=", format(as.Date(from), "%m/%d/%Y"))
  if (!is.null(to))      url <- paste0(url, "&to=", format(as.Date(to), "%m/%d/%Y"))

  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  lines <- readLines(tmp, warn = FALSE)
  if (length(lines) < 7) return(tibble())

  # Fed CSV format:
  # Row 1: Series Description (long names)
  # Row 2: Unit
  # Row 3: Multiplier
  # Row 4: Currency
  # Row 5: Unique Identifier (series codes)
  # Row 6: Time Period header + series codes
  # Row 7+: Data

  # Parse descriptions (row 1)
  desc_line <- read.csv(text = lines[1], header = FALSE, stringsAsFactors = FALSE)
  descriptions <- as.character(desc_line[1, -1])

  # Parse series codes (row 6)
  header_line <- read.csv(text = lines[6], header = FALSE, stringsAsFactors = FALSE)
  series_codes <- as.character(header_line[1, -1])

  # Parse data (row 7+)
  data_lines <- lines[7:length(lines)]
  data_lines <- data_lines[nchar(data_lines) > 0]
  if (length(data_lines) == 0) return(tibble())

  data_text <- paste(c(lines[6], data_lines), collapse = "\n")
  df <- read.csv(text = data_text, stringsAsFactors = FALSE, check.names = FALSE)

  # Rename first column
  names(df)[1] <- "date"
  df$date <- as.Date(df$date)

  # Pivot to long format
  df <- as_tibble(df)
  value_cols <- setdiff(names(df), "date")

  result <- df |>
    pivot_longer(cols = all_of(value_cols), names_to = "series_code",
                 values_to = "value") |>
    mutate(value = suppressWarnings(as.numeric(value)))

  # Add descriptions
  desc_lookup <- tibble(series_code = series_codes, description = descriptions)
  result <- result |> left_join(desc_lookup, by = "series_code")

  result |> filter(!is.na(value)) |> arrange(date, series_code)
}


# == Schemas ===================================================================

.schema_rates <- tibble(
  date = as.Date(character()), series_code = character(),
  value = numeric(), description = character()
)


