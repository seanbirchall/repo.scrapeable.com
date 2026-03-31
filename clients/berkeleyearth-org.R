# berkeleyearth-org.R
# Self-contained Berkeley Earth temperature data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Data: tab-separated text with % comment lines

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.berk_s3 <- "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global"

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

.fetch <- function(url, ext = ".txt") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.parse_berk_txt <- function(file) {
  lines <- readLines(file, warn = FALSE)
  # Remove comment lines starting with %
  data_lines <- lines[!grepl("^%", lines) & nchar(trimws(lines)) > 0]
  if (length(data_lines) == 0) return(NULL)
  tc <- textConnection(data_lines)
  on.exit(close(tc))
  tryCatch(
    utils::read.table(tc, header = FALSE, stringsAsFactors = FALSE,
                      fill = TRUE, comment.char = "%"),
    error = function(e) NULL
  )
}

# == Schemas ===================================================================

.schema_global_temp <- tibble(
  year = integer(), month = integer(), anomaly = numeric(),
  anomaly_unc = numeric(), annual_anomaly = numeric(),
  annual_anomaly_unc = numeric()
)

# == Global temperature ========================================================

#' Fetch Berkeley Earth global land+ocean temperature data
#'
#' Downloads the complete Land_and_Ocean data from Berkeley Earth.
#' Contains monthly temperature anomalies relative to the 1951-1980 average.
#'
#' @return tibble: year (integer), month (integer), anomaly (numeric),
#'   anomaly_unc (numeric), annual_anomaly (numeric),
#'   annual_anomaly_unc (numeric)
berk_global_temp <- function() {
  url <- paste0(.berk_s3, "/Land_and_Ocean_complete.txt")
  f <- .fetch(url, ext = ".txt")
  df <- .parse_berk_txt(f)
  if (is.null(df) || nrow(df) == 0) return(.schema_global_temp)
  # The file has monthly data with columns:
  # Year, Month, Monthly_Anomaly, Monthly_Unc, Annual_Anomaly, Annual_Unc, ...
  # Number of columns can vary; take first 6
  nc <- min(ncol(df), 6)
  result <- tibble(
    year = as.integer(df[[1]]),
    month = as.integer(df[[2]]),
    anomaly = suppressWarnings(as.numeric(df[[3]])),
    anomaly_unc = if (nc >= 4) suppressWarnings(as.numeric(df[[4]])) else NA_real_,
    annual_anomaly = if (nc >= 5) suppressWarnings(as.numeric(df[[5]])) else NA_real_,
    annual_anomaly_unc = if (nc >= 6) suppressWarnings(as.numeric(df[[6]])) else NA_real_
  )
  result |> filter(!is.na(year), !is.na(month))
}

# == Context ===================================================================

#' Show Berkeley Earth context for LLMs
#'
#' Displays package overview and function signatures.
#' @return Invisibly returns the context string
berk_context <- function() {
  .build_context(
    "berkeleyearth.org",
    header_lines = c(
      "# berkeleyearth.org",
      "# Berkeley Earth global temperature data client",
      "# Auth: none required",
      "# Data: Global land+ocean temperature anomalies (vs 1951-1980 avg)",
      "#",
      "# Source: Berkeley Earth Surface Temperature project",
      "# Resolution: monthly, from 1850 to present"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
