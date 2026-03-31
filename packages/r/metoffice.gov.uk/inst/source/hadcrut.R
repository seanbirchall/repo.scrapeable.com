# metoffice-gov-uk.R
# Self-contained Met Office HadCRUT5 temperature data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Data: CSV download from Met Office Hadley Centre

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hadcrut_base <- "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/analysis/diagnostics"

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

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_annual <- tibble(
  year = integer(), anomaly = numeric(),
  lower_ci = numeric(), upper_ci = numeric()
)

.schema_monthly <- tibble(
  year = integer(), month = integer(), anomaly = numeric(),
  lower_ci = numeric(), upper_ci = numeric()
)

# == Annual global temperature =================================================

#' Fetch HadCRUT5 global annual temperature anomalies
#'
#' Downloads annual global mean temperature anomalies from the Met Office
#' HadCRUT5 dataset. Anomalies are relative to the 1961-1990 average.
#'
#' @return tibble: year (integer), anomaly (numeric), lower_ci (numeric),
#'   upper_ci (numeric)
hadcrut_global_annual <- function() {
  url <- paste0(.hadcrut_base,
                "/HadCRUT.5.0.2.0.analysis.summary_series.global.annual.csv")
  f <- .fetch(url, ext = ".csv")
  df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_annual)
  df <- as_tibble(df)
  names(df) <- tolower(names(df))
  # Columns typically: Time, Anomaly (deg C), Lower confidence limit, Upper confidence limit
  year_col <- intersect(c("time", "year"), names(df))
  if (length(year_col) == 0) year_col <- names(df)[1]
  tibble(
    year = as.integer(df[[year_col[1]]]),
    anomaly = if (ncol(df) >= 2) suppressWarnings(as.numeric(df[[2]])) else NA_real_,
    lower_ci = if (ncol(df) >= 3) suppressWarnings(as.numeric(df[[3]])) else NA_real_,
    upper_ci = if (ncol(df) >= 4) suppressWarnings(as.numeric(df[[4]])) else NA_real_
  ) |> filter(!is.na(year))
}

# == Monthly global temperature ================================================

#' Fetch HadCRUT5 global monthly temperature anomalies
#'
#' Downloads monthly global mean temperature anomalies from the Met Office
#' HadCRUT5 dataset. Anomalies are relative to the 1961-1990 average.
#'
#' @return tibble: year (integer), month (integer), anomaly (numeric),
#'   lower_ci (numeric), upper_ci (numeric)
hadcrut_global_monthly <- function() {
  url <- paste0(.hadcrut_base,
                "/HadCRUT.5.0.2.0.analysis.summary_series.global.monthly.csv")
  f <- .fetch(url, ext = ".csv")
  df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_monthly)
  df <- as_tibble(df)
  names(df) <- tolower(names(df))
  # Time column is YYYY-MM format
  time_col <- intersect(c("time", "year"), names(df))
  if (length(time_col) == 0) time_col <- names(df)[1]
  time_vals <- as.character(df[[time_col[1]]])
  tibble(
    year = as.integer(sub("-.*", "", time_vals)),
    month = as.integer(sub(".*-", "", time_vals)),
    anomaly = if (ncol(df) >= 2) suppressWarnings(as.numeric(df[[2]])) else NA_real_,
    lower_ci = if (ncol(df) >= 3) suppressWarnings(as.numeric(df[[3]])) else NA_real_,
    upper_ci = if (ncol(df) >= 4) suppressWarnings(as.numeric(df[[4]])) else NA_real_
  ) |> filter(!is.na(year))
}

# == Context ===================================================================

#' Show HadCRUT5 API context for LLMs
#'
#' Displays package overview and function signatures.
#' @return Invisibly returns the context string
hadcrut_context <- function() {
  .build_context(
    "metoffice.gov.uk",
    header_lines = c(
      "# metoffice.gov.uk",
      "# Met Office HadCRUT5 global temperature anomaly client",
      "# Auth: none required",
      "# Data: Global mean temperature anomalies vs 1961-1990 average",
      "#",
      "# Source: Met Office Hadley Centre / University of East Anglia CRU",
      "# Coverage: 1850 to present, annual and monthly resolution"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
