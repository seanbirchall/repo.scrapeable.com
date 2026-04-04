# nasa.gov.R
# Self-contained NASA client.
# Covers: GISS temperature data and NASA Technical Reports Server (NTRS).
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)


# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.ua <- "support@scrapeable.com"
.giss_base <- "https://data.giss.nasa.gov/gistemp/tabledata_v4"
.ntrs_base <- "https://ntrs.nasa.gov/api"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))


# == Schemas ===================================================================

.schema_global_temp <- tibble(
  year = integer(), jan = numeric(), feb = numeric(), mar = numeric(),
  apr = numeric(), may = numeric(), jun = numeric(), jul = numeric(),
  aug = numeric(), sep = numeric(), oct = numeric(), nov = numeric(),
  dec = numeric(), j_d = numeric(), d_n = numeric(), djf = numeric(),
  mam = numeric(), jja = numeric(), son = numeric()
)

.schema_citations <- tibble(
  id = character(), title = character(), sti_type = character(),
  distribution = character(), created = as.Date(character()),
  authors = character()
)




#' Download and parse GISTEMP global temperature anomalies
#'
#' Fetches CSV data from NASA Goddard Institute for Space Studies (GISS),
#' containing monthly global mean surface temperature anomalies in degrees C
#' relative to the 1951--1980 base period. Data spans from 1880 to present.
#'
#' @param dataset Character. One of:
#'   \describe{
#'     \item{\code{"global"}}{Land + ocean combined (default).}
#'     \item{\code{"land"}}{Land surface only.}
#'     \item{\code{"northern"}}{Northern hemisphere, land + ocean.}
#'     \item{\code{"southern"}}{Southern hemisphere, land + ocean.}
#'   }
#' @return A tibble with columns:
#'   \describe{
#'     \item{year}{Integer. Calendar year.}
#'     \item{jan--dec}{Numeric. Monthly temperature anomalies (deg C).}
#'     \item{j_d}{Numeric. January--December annual mean.}
#'     \item{d_n}{Numeric. December--November annual mean.}
#'     \item{djf, mam, jja, son}{Numeric. Seasonal means.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' giss_global_temp()
#' giss_global_temp("northern")
#' }
giss_global_temp <- function(dataset = "global") {
  file_map <- list(
    global   = "GLB.Ts+dSST.csv",
    land     = "GLB.Ts.csv",
    northern = "NH.Ts+dSST.csv",
    southern = "SH.Ts+dSST.csv"
  )
  fname <- file_map[[dataset]]
  if (is.null(fname)) stop("dataset must be one of: global, land, northern, southern")

  url <- paste0(.giss_base, "/", fname)
  tmp <- .fetch(url, ext = ".csv")
  raw_lines <- readLines(tmp, warn = FALSE)

  # Find the header row (starts with "Year")
  header_idx <- grep("^Year", raw_lines)[1]
  if (is.na(header_idx)) stop("Could not find header row in CSV")

  # Read from header row, replacing *** with NA
  data_lines <- raw_lines[header_idx:length(raw_lines)]
  data_lines <- gsub("\\*\\*\\*", "NA", data_lines)

  # Remove any trailing non-data lines (like "Year" appearing again)

  con <- textConnection(data_lines)
  df <- tryCatch(
    read.csv(con, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  close(con)

  if (is.null(df) || nrow(df) == 0) return(.schema_global_temp)

  # Clean column names
  names(df) <- tolower(names(df))
  names(df) <- gsub("-", "_", names(df))

  # Remove non-numeric year rows

  df <- df[grepl("^\\d{4}$", trimws(df$year)), , drop = FALSE]

  df <- as_tibble(df) |>
    mutate(
      year = as.integer(year),
      across(-year, ~ suppressWarnings(as.numeric(.x)))
    )

  df
}


#' Download GISTEMP zonal mean temperature anomalies
#'
#' Fetches zonal annual mean surface temperature anomalies by latitude band
#' from NASA GISS. Data spans from 1880 to present.
#'
#' @param dataset Character. One of \code{"combined"} (land + ocean, default)
#'   or \code{"land"} (land surface only).
#' @return A tibble with columns: year (integer), glob, nhem, shem (numeric),
#'   and latitude band columns (e.g. 24n_90n, 24s_24n, etc.).
#' @export
#' @examples
#' \dontrun{
#' giss_zonal_temp()
#' giss_zonal_temp("land")
#' }
giss_zonal_temp <- function(dataset = "combined") {
  file_map <- list(
    combined = "ZonAnn.Ts+dSST.csv",
    land     = "ZonAnn.Ts.csv"
  )
  fname <- file_map[[dataset]]
  if (is.null(fname)) stop("dataset must be one of: combined, land")

  url <- paste0(.giss_base, "/", fname)
  tmp <- .fetch(url, ext = ".csv")
  raw_lines <- readLines(tmp, warn = FALSE)

  header_idx <- grep("^Year", raw_lines)[1]
  if (is.na(header_idx)) stop("Could not find header row in CSV")

  data_lines <- raw_lines[header_idx:length(raw_lines)]
  data_lines <- gsub("\\*\\*\\*", "NA", data_lines)

  con <- textConnection(data_lines)
  df <- tryCatch(
    read.csv(con, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  close(con)

  if (is.null(df) || nrow(df) == 0) return(tibble(year = integer(), glob = numeric()))

  names(df) <- tolower(names(df))
  names(df) <- gsub("-", "_", names(df))
  df <- df[grepl("^\\d{4}$", trimws(df$year)), , drop = FALSE]

  as_tibble(df) |>
    mutate(
      year = as.integer(year),
      across(-year, ~ suppressWarnings(as.numeric(.x)))
    )
}


#' Get GISTEMP temperature anomaly for a specific year
#'
#' Convenience wrapper around \code{\link{giss_global_temp}} that filters
#' to a single year.
#'
#' @param year Integer. Calendar year (e.g. \code{2023}).
#' @param dataset Character. One of \code{"global"}, \code{"land"},
#'   \code{"northern"}, \code{"southern"}. Default \code{"global"}.
#' @return A tibble with one row of monthly and seasonal anomalies.
#' @export
#' @examples
#' \dontrun{
#' giss_year(2023)
#' giss_year(2020, dataset = "northern")
#' }
giss_year <- function(year, dataset = "global") {
  df <- giss_global_temp(dataset = dataset)
  df[df$year == as.integer(year), , drop = FALSE]
}


#' Get GISTEMP trend summary (first and last decade averages)
#'
#' Computes average annual temperature anomalies for the first and last
#' full decades in the GISTEMP dataset, providing a quick assessment of
#' long-term warming trends.
#'
#' @param dataset Character. One of \code{"global"}, \code{"land"},
#'   \code{"northern"}, \code{"southern"}. Default \code{"global"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{period}{Character. Decade range (e.g. "1880-1889").}
#'     \item{avg_annual_anomaly}{Numeric. Mean annual anomaly in deg C.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' giss_trend()
#' giss_trend("southern")
#' }
giss_trend <- function(dataset = "global") {
  df <- giss_global_temp(dataset = dataset)
  if (nrow(df) == 0) return(tibble(period = character(), avg_annual_anomaly = numeric()))

  # Use j_d (Jan-Dec annual mean) if available
  annual_col <- if ("j_d" %in% names(df)) "j_d" else if ("d_n" %in% names(df)) "d_n" else NULL
  if (is.null(annual_col)) return(tibble(period = character(), avg_annual_anomaly = numeric()))

  min_yr <- min(df$year, na.rm = TRUE)
  max_yr <- max(df$year, na.rm = TRUE)

  first_decade <- df[df$year >= min_yr & df$year < min_yr + 10, ]
  last_decade <- df[df$year > max_yr - 10 & df$year <= max_yr, ]

  tibble(
    period = c(
      paste0(min_yr, "-", min_yr + 9),
      paste0(max_yr - 9, "-", max_yr)
    ),
    avg_annual_anomaly = c(
      mean(first_decade[[annual_col]], na.rm = TRUE),
      mean(last_decade[[annual_col]], na.rm = TRUE)
    )
  )
}


#' Search NASA Technical Reports Server
#'
#' Searches the NASA Technical Reports Server (NTRS) for technical
#' publications including reports, conference papers, journal articles,
#' and other documents produced by NASA centers.
#'
#' @param query Character. Search query string.
#' @param page_size Integer. Results per page (default 25).
#' @param page Integer. Page number, 0-indexed (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. NTRS citation ID.}
#'     \item{title}{Character. Publication title.}
#'     \item{sti_type}{Character. STI type (e.g. "TECHNICAL_REPORT").}
#'     \item{distribution}{Character. Distribution restriction.}
#'     \item{created}{Date. Publication date.}
#'     \item{authors}{Character. Semicolon-separated author names.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' ntrs_search("Mars rover")
#' ntrs_search("exoplanet", page_size = 10)
#' }
ntrs_search <- function(query, page_size = 25, page = 0) {
  body <- list(
    query = query,
    page = list(size = page_size, from = page * page_size)
  )
  raw <- .ntrs_post("/citations/search", body)
  df <- raw$results
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(.schema_citations)

  auth_strs <- vapply(seq_len(nrow(df)), function(i) {
    auths <- df$authorAffiliations[[i]]
    if (is.null(auths) || !is.data.frame(auths) || nrow(auths) == 0) return(NA_character_)
    paste(vapply(seq_len(nrow(auths)), function(j) {
      tryCatch(auths$meta$author$name[j] %||% NA_character_,
               error = function(e) NA_character_)
    }, character(1)), collapse = "; ")
  }, character(1))

  tibble(
    id = as.character(df$id),
    title = as.character(df$title),
    sti_type = as.character(df$stiType),
    distribution = as.character(df$distribution),
    created = tryCatch(as.Date(substr(df$created, 1, 10)),
                       error = function(e) rep(as.Date(NA), nrow(df))),
    authors = auth_strs
  )
}


#' Fetch a single NTRS citation by ID
#'
#' Retrieves detailed metadata for a single NASA Technical Reports Server
#' citation.
#'
#' @param id Character or integer. NTRS citation ID.
#' @return A tibble with one row: id, title, sti_type, distribution,
#'   created, authors.
#' @export
#' @examples
#' \dontrun{
#' ntrs_citation(20230012345)
#' }
ntrs_citation <- function(id) {
  url <- sprintf("%s/citations/%s", .ntrs_base, as.character(id))
  raw <- .fetch_json(url)
  if (is.null(raw)) return(.schema_citations)

  auths <- raw$authorAffiliations
  auth_str <- if (!is.null(auths) && length(auths) > 0) {
    if (is.data.frame(auths)) {
      paste(vapply(seq_len(nrow(auths)), function(i) {
        tryCatch(auths$meta[[i]]$author$name %||% NA_character_,
                 error = function(e) NA_character_)
      }, character(1)), collapse = "; ")
    } else {
      paste(vapply(auths, function(a) a$meta$author$name %||% NA_character_, character(1)), collapse = "; ")
    }
  } else NA_character_

  tibble(
    id = as.character(raw$id %||% NA),
    title = as.character(raw$title %||% NA),
    sti_type = as.character(raw$stiType %||% NA),
    distribution = as.character(raw$distribution %||% NA),
    created = tryCatch(as.Date(substr(raw$created %||% "", 1, 10)),
                       error = function(e) as.Date(NA)),
    authors = auth_str
  )
}


#' Search NASA Technical Reports by center
#'
#' Returns technical reports from a specific NASA center.
#'
#' @param center Character. NASA center code (e.g. \code{"JPL"} for Jet
#'   Propulsion Laboratory, \code{"GSFC"} for Goddard, \code{"ARC"} for Ames,
#'   \code{"MSFC"} for Marshall).
#' @param page_size Integer. Results per page (default 25).
#' @return A tibble with columns: id, title, sti_type, distribution,
#'   created, authors.
#' @export
#' @examples
#' \dontrun{
#' ntrs_by_center("JPL")
#' ntrs_by_center("GSFC", page_size = 10)
#' }
ntrs_by_center <- function(center, page_size = 25) {
  body <- list(
    center = center,
    page = list(size = page_size, from = 0)
  )
  raw <- tryCatch(.ntrs_post("/citations/search", body), error = function(e) NULL)
  if (is.null(raw)) return(.schema_citations)
  df <- raw$results
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(.schema_citations)

  auth_strs <- vapply(seq_len(nrow(df)), function(i) {
    auths <- df$authorAffiliations[[i]]
    if (is.null(auths) || !is.data.frame(auths) || nrow(auths) == 0) return(NA_character_)
    paste(vapply(seq_len(nrow(auths)), function(j) {
      tryCatch(auths$meta$author$name[j] %||% NA_character_,
               error = function(e) NA_character_)
    }, character(1)), collapse = "; ")
  }, character(1))

  tibble(
    id = as.character(df$id),
    title = as.character(df$title),
    sti_type = as.character(df$stiType),
    distribution = as.character(df$distribution),
    created = tryCatch(as.Date(substr(df$created, 1, 10)),
                       error = function(e) rep(as.Date(NA), nrow(df))),
    authors = auth_strs
  )
}


#' Get recent NASA Technical Reports
#'
#' Returns the most recently published documents from the NASA Technical
#' Reports Server, optionally filtered by STI document type.
#'
#' @param page_size Integer. Number of results to return (default 25).
#' @param type Character or NULL. STI type filter: \code{"TECHNICAL_REPORT"},
#'   \code{"CONFERENCE_PAPER"}, \code{"REPRINT"}, \code{"JOURNAL_ARTICLE"}, etc.
#' @return A tibble with columns: id, title, sti_type, distribution,
#'   created, authors.
#' @export
#' @examples
#' \dontrun{
#' ntrs_recent()
#' ntrs_recent(type = "TECHNICAL_REPORT", page_size = 10)
#' }
ntrs_recent <- function(page_size = 25, type = NULL) {
  body <- list(
    page = list(size = page_size, from = 0)
  )
  if (!is.null(type)) body$stiType <- type
  raw <- tryCatch(.ntrs_post("/citations/search", body), error = function(e) NULL)
  if (is.null(raw)) return(.schema_citations)
  df <- raw$results
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(.schema_citations)

  auth_strs <- vapply(seq_len(nrow(df)), function(i) {
    auths <- df$authorAffiliations[[i]]
    if (is.null(auths) || !is.data.frame(auths) || nrow(auths) == 0) return(NA_character_)
    paste(vapply(seq_len(nrow(auths)), function(j) {
      tryCatch(auths$meta$author$name[j] %||% NA_character_,
               error = function(e) NA_character_)
    }, character(1)), collapse = "; ")
  }, character(1))

  tibble(
    id = as.character(df$id),
    title = as.character(df$title),
    sti_type = as.character(df$stiType),
    distribution = as.character(df$distribution),
    created = tryCatch(as.Date(substr(df$created, 1, 10)),
                       error = function(e) rep(as.Date(NA), nrow(df))),
    authors = auth_strs
  )
}


# == Context ===================================================================

#' Get nasa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nasa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nasa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nasa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nasa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nasa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nasa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
