# eeoc.gov.R
# Self-contained EEOC workforce data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public data files)
# Docs: https://www.eeoc.gov/data
#
# Data sources:
#   EEO-4 (state & local government): CSV 2005-2021
#   EEO-1 (private industry): XLSX by year (2014-2021)


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.eeoc_base <- "https://www.eeoc.gov"

# EEO-4 CSV URL (state & local government, 2005-2021)
.eeo4_url <- "https://www.eeoc.gov/sites/default/files/2022-09/puf_eeo4_2005_2021.csv"

# EEO-1 XLSX URLs by year (private industry)
.eeo1_years <- c(
  "2021" = "https://www.eeoc.gov/sites/default/files/2023-10/EEO-1%20Component%201%20Public%20Use%20File.xlsx",
  "2020" = "https://www.eeoc.gov/sites/default/files/2023-10/EEO-1%20Component%201%20Public%20Use%20File.xlsx",
  "2019" = "https://www.eeoc.gov/sites/default/files/2023-10/EEO-1%20Component%201%20Public%20Use%20File.xlsx",
  "2018" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202018%20PUF.xlsx",
  "2017" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202017%20PUF.xlsx",
  "2016" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202016%20PUF.xlsx",
  "2015" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202015%20PUF.xlsx",
  "2014" = "https://www.eeoc.gov/sites/default/files/2021-12/EEO1%202014%20PUF.xlsx"
)

# -- Fetch helper (always via httr2) -------------------------------------------

.eeoc_download <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(120) |>
    httr2::req_perform(path = tmp)
  tmp
}

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

# == Public functions ==========================================================

#' List available EEOC datasets
#'
#' Returns a tibble describing all EEOC public use datasets available
#' through this client, including EEO-4 (state/local government) and
#' EEO-1 (private industry) workforce data by year.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{dataset}{character -- dataset identifier (\code{"eeo4"} or \code{"eeo1"})}
#'     \item{description}{character -- human-readable dataset name with year}
#'     \item{format}{character -- file format (\code{"CSV"} or \code{"XLSX"})}
#'     \item{years}{character -- year or year range covered}
#'     \item{url}{character -- direct download URL}
#'   }
#' @examples
#' eeoc_list()
#' @export
eeoc_list <- function() {
  tibble(
    dataset = c("eeo4", rep("eeo1", length(.eeo1_years))),
    description = c(
      "State & Local Government Workforce (EEO-4)",
      paste0("Private Industry Workforce (EEO-1) ", names(.eeo1_years))
    ),
    format = c("CSV", rep("XLSX", length(.eeo1_years))),
    years = c("2005-2021", names(.eeo1_years)),
    url = c(.eeo4_url, unname(.eeo1_years))
  )
}

#' Search EEOC EEO-4 state/local government workforce data
#'
#' Downloads and filters the EEO-4 public use CSV (state and local
#' government workforce data from 2005--2021). The full file is about
#' 13 MB and is cached as a temporary file per R session.
#'
#' @param state Character. Two-letter state abbreviation (e.g., \code{"CA"},
#'   \code{"NY"}). Default \code{NULL} returns all states.
#' @param year Integer. Year to filter (2005--2021). Default \code{NULL}
#'   returns all years.
#' @param sex Character. \code{"Male"} or \code{"Female"}. Default \code{NULL}.
#' @param data_type Character. Race/ethnicity category: \code{"White"},
#'   \code{"Black"}, \code{"Hispanic"}, \code{"Asian"}, \code{"AIAN"},
#'   \code{"NHOPI"}, \code{"Two_or_more"}. Default \code{NULL} returns all.
#' @param max_results Integer. Maximum rows to return (default 1000).
#' @return A tibble with columns:
#'   \describe{
#'     \item{state}{logical/character -- state abbreviation}
#'     \item{emp_type}{character -- employment type}
#'     \item{type}{character -- data record type}
#'     \item{govt_type}{character -- government type}
#'     \item{year}{integer -- reporting year (2005--2021)}
#'     \item{sex}{character -- \code{"Male"} or \code{"Female"}}
#'     \item{bracket}{character -- salary bracket}
#'     \item{job_cat}{logical/character -- job category}
#'     \item{functcd}{logical/character -- government function code}
#'     \item{data_type}{character -- race/ethnicity category}
#'     \item{value}{integer -- workforce count}
#'   }
#' @examples
#' eeoc_search(state = "CA", year = 2021, max_results = 50)
#' @export
eeoc_search <- function(state = NULL, year = NULL, sex = NULL,
                        data_type = NULL, max_results = 1000) {
  tmp <- .eeoc_download(.eeo4_url, ".csv")
  df <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  df <- as_tibble(df)

  if (!is.null(state))     df <- df |> filter(.data$state == !!state)
  if (!is.null(year))      df <- df |> filter(.data$year == !!as.integer(year))
  if (!is.null(sex))       df <- df |> filter(.data$sex == !!sex)
  if (!is.null(data_type)) df <- df |> filter(.data$data_type == !!data_type)

  df |> head(max_results)
}

#' Get EEOC EEO-4 summary by state and year
#'
#' Aggregates EEO-4 data to show total workforce counts by state and
#' demographic group for a given year.
#'
#' @param year Integer. Year (2005--2021). Default 2021.
#' @return A tibble with columns:
#'   \describe{
#'     \item{state}{character -- 2-letter state abbreviation}
#'     \item{data_type}{character -- race/ethnicity category}
#'     \item{total}{numeric -- sum of workforce counts}
#'   }
#' @examples
#' eeoc_state_summary(year = 2021)
#' @export
eeoc_state_summary <- function(year = 2021) {
  tmp <- .eeoc_download(.eeo4_url, ".csv")
  df <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  df <- as_tibble(df)

  df |>
    filter(.data$year == !!as.integer(year)) |>
    filter(.data$state != "") |>
    group_by(.data$state, .data$data_type) |>
    summarise(total = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    arrange(.data$state, .data$data_type)
}

#' Get EEOC EEO-4 data by government function
#'
#' Aggregates EEO-4 workforce counts by government function code
#' and demographic group.
#'
#' @param year Integer. Year (2005--2021). Default 2021.
#' @param state Character. Two-letter state abbreviation to filter by.
#'   Default \code{NULL} returns all states.
#' @return A tibble with columns:
#'   \describe{
#'     \item{functcd}{character -- government function code}
#'     \item{data_type}{character -- race/ethnicity category}
#'     \item{total}{numeric -- sum of workforce counts}
#'   }
#' @examples
#' eeoc_by_function(year = 2021, state = "CA")
#' @export
eeoc_by_function <- function(year = 2021, state = NULL) {
  tmp <- .eeoc_download(.eeo4_url, ".csv")
  df <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  df <- as_tibble(df)

  df <- df |> filter(.data$year == !!as.integer(year))
  if (!is.null(state)) df <- df |> filter(.data$state == !!state)

  df |>
    filter(.data$functcd != "") |>
    group_by(.data$functcd, .data$data_type) |>
    summarise(total = sum(.data$value, na.rm = TRUE), .groups = "drop") |>
    arrange(.data$functcd, .data$data_type)
}

#' Get available years in EEO-4 data
#'
#' Downloads the EEO-4 CSV and returns the distinct years present.
#' Typically 2005--2021.
#'
#' @return Integer vector of available years (sorted ascending).
#' @examples
#' eeoc_years()
#' @export
eeoc_years <- function() {
  tmp <- .eeoc_download(.eeo4_url, ".csv")
  df <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE,
                        nrows = 100000)
  sort(unique(df$year))
}

#' Get eeoc.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
eeoc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(eeoc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/eeoc.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "eeoc.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# eeoc.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# eeoc.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
