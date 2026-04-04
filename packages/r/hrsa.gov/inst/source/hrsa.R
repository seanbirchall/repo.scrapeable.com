# hrsa.gov.R
# Self-contained HRSA (Health Resources & Services Administration) client.
# Uses downloadable CSVs from data.hrsa.gov for HPSA and Health Center data.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public data)
# Source: https://data.hrsa.gov/data/download

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)

`%||%` <- function(x, y) if (is.null(x)) y else x

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hrsa_hpsa_url <- "https://data.hrsa.gov/DataDownload/DD_Files/BCD_HPSA_FCT_DET_PC.csv"
.hrsa_hc_url <- "https://data.hrsa.gov/DataDownload/DD_Files/Health_Center_Service_Delivery_and_LookAlike_Sites.csv"

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.hrsa_cache <- new.env(parent = emptyenv())

.hrsa_load_hpsa <- function(force = FALSE) {
  if (!force && exists("hpsa", envir = .hrsa_cache)) {
    return(get("hpsa", envir = .hrsa_cache))
  }
  tmp <- .fetch_csv(.hrsa_hpsa_url)
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  # Remove empty trailing columns from trailing comma
  raw <- raw[, nchar(trimws(names(raw))) > 0, drop = FALSE]
  df <- tibble::as_tibble(raw, .name_repair = "unique")

  df <- df |>
    transmute(
      hpsa_name = as.character(`HPSA Name`),
      hpsa_id = as.character(`HPSA ID`),
      designation_type = as.character(`Designation Type`),
      discipline_class = as.character(`HPSA Discipline Class`),
      hpsa_score = suppressWarnings(as.integer(`HPSA Score`)),
      state = as.character(`Primary State Abbreviation`),
      status = as.character(`HPSA Status`),
      designation_date = suppressWarnings(as.Date(`HPSA Designation Date`, format = "%m/%d/%Y")),
      metro_indicator = as.character(`Metropolitan Indicator`),
      designation_pop = suppressWarnings(as.numeric(`HPSA Designation Population`)),
      pct_poverty = suppressWarnings(as.numeric(`% of Population Below 100% Poverty`)),
      formal_ratio = as.character(`HPSA Formal Ratio`),
      population_type = as.character(`HPSA Population Type`),
      rural_status = as.character(`Rural Status`),
      longitude = suppressWarnings(as.numeric(Longitude)),
      latitude = suppressWarnings(as.numeric(Latitude)),
      county = as.character(`Common County Name`),
      state_name = as.character(`Common State Name`),
      provider_type = as.character(`Provider Type`)
    )

  assign("hpsa", df, envir = .hrsa_cache)
  df
}

.hrsa_load_hc <- function(force = FALSE) {
  if (!force && exists("hc", envir = .hrsa_cache)) {
    return(get("hc", envir = .hrsa_cache))
  }
  tmp <- .fetch_csv(.hrsa_hc_url)
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  raw <- raw[, nchar(trimws(names(raw))) > 0, drop = FALSE]
  df <- tibble::as_tibble(raw, .name_repair = "unique")

  df <- df |>
    transmute(
      site_name = as.character(`Site Name`),
      health_center_name = as.character(`Health Center Name`),
      site_address = as.character(`Site Address`),
      site_city = as.character(`Site City`),
      site_state = as.character(`Site State Abbreviation`),
      site_zip = as.character(`Site Postal Code`),
      site_phone = as.character(`Site Telephone Number`),
      site_web = as.character(`Site Web Address`),
      hours_per_week = suppressWarnings(as.numeric(`Operating Hours per Week`)),
      health_center_type = as.character(`Health Center Type`),
      site_status = as.character(`Site Status Description`),
      location_type = as.character(`Health Center Location Type Description`),
      operator_type = as.character(`Health Center Operator Description`),
      longitude = suppressWarnings(as.numeric(`Geocoding Artifact Address Primary X Coordinate`)),
      latitude = suppressWarnings(as.numeric(`Geocoding Artifact Address Primary Y Coordinate`)),
      county = as.character(`Complete County Name`),
      state_name = as.character(`State Name`),
      hhs_region = as.character(`HHS Region Name`)
    )

  assign("hc", df, envir = .hrsa_cache)
  df
}

# == Public functions ==========================================================

#' List Health Professional Shortage Areas (HPSAs)
#'
#' @param state Two-letter state abbreviation (e.g. "CA").
#' @param status Filter by HPSA status (e.g. "Designated", "Withdrawn").
#' @param discipline Filter by discipline class (e.g. "Primary Care", "Dental Health", "Mental Health").
#' @param limit Maximum rows (default 500).
#' @return tibble of HPSAs
#' @export
hrsa_hpsa <- function(state = NULL, status = NULL, discipline = NULL, limit = 500) {
  df <- .hrsa_load_hpsa()
  if (!is.null(state)) {
    st <- state
    df <- df |> filter(.data$state == st)
  }
  if (!is.null(status)) {
    pat <- status
    df <- df |> filter(grepl(pat, status, ignore.case = TRUE))
  }
  if (!is.null(discipline)) {
    pat <- discipline
    df <- df |> filter(grepl(pat, discipline_class, ignore.case = TRUE))
  }
  head(df, limit)
}

#' Search HPSAs by name or county
#'
#' @param query Search term (case-insensitive, matched against name and county).
#' @param state Optional state filter.
#' @param limit Maximum rows (default 100).
#' @return tibble of matching HPSAs
#' @export
hrsa_search <- function(query, state = NULL, limit = 100) {
  df <- .hrsa_load_hpsa()
  if (!is.null(state)) df <- df |> filter(.data$state == !!state)
  pattern <- query
  df <- df |>
    filter(
      grepl(pattern, hpsa_name, ignore.case = TRUE) |
      grepl(pattern, county, ignore.case = TRUE)
    )
  head(df, limit)
}

#' List HRSA-funded Health Centers
#'
#' @param state Two-letter state abbreviation.
#' @param city Filter by city (case-insensitive partial match).
#' @param limit Maximum rows (default 500).
#' @return tibble of health center sites
#' @export
hrsa_health_centers <- function(state = NULL, city = NULL, limit = 500) {
  df <- .hrsa_load_hc()
  if (!is.null(state)) df <- df |> filter(.data$site_state == !!state)
  if (!is.null(city)) df <- df |> filter(grepl(city, site_city, ignore.case = TRUE))
  head(df, limit)
}

#' Search health centers by name
#'
#' @param query Search term (case-insensitive, matched against site and center names).
#' @param state Optional state filter.
#' @param limit Maximum rows (default 100).
#' @return tibble of matching health centers
#' @export
hrsa_hc_search <- function(query, state = NULL, limit = 100) {
  df <- .hrsa_load_hc()
  if (!is.null(state)) df <- df |> filter(.data$site_state == !!state)
  pattern <- query
  df <- df |>
    filter(
      grepl(pattern, site_name, ignore.case = TRUE) |
      grepl(pattern, health_center_name, ignore.case = TRUE)
    )
  head(df, limit)
}

#' Summarize HPSAs by state
#'
#' @param status Filter by status (default "Designated" for active only).
#' @return tibble: state, n_hpsa, avg_score, total_pop
#' @export
hrsa_hpsa_by_state <- function(status = "Designated") {
  df <- .hrsa_load_hpsa()
  if (!is.null(status)) {
    pat <- status
    df <- df |> filter(grepl(pat, .data$status, ignore.case = TRUE))
  }
  df |>
    group_by(state) |>
    summarise(
      n_hpsa = n(),
      avg_score = mean(hpsa_score, na.rm = TRUE),
      total_pop = sum(designation_pop, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(n_hpsa))
}

#' Summarize health centers by state
#'
#' @return tibble: state, n_sites, n_centers
#' @export
hrsa_hc_by_state <- function() {
  df <- .hrsa_load_hc()
  df |>
    group_by(state = site_state) |>
    summarise(
      n_sites = n(),
      n_centers = n_distinct(health_center_name),
      .groups = "drop"
    ) |>
    arrange(desc(n_sites))
}

# == Context ===================================================================

#' Show HRSA client context for LLM use
#'
#' @return Invisibly returns context string
#' @export
hrsa_context <- function() {
  .build_context("hrsa.gov")
}

.build_context <- function(pkg_name) {
  src_dir <- system.file("source", package = pkg_name)
  if (src_dir != "" && length(list.files(src_dir, pattern = "[.]R$")) > 0) {
    src_file <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)[1]
  } else {
    src_file <- NULL
    tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
    if (is.null(src_file)) src_file <- paste0("clients/", pkg_name, ".R")
  }
  if (is.null(src_file) || !file.exists(src_file)) {
    cat("# ", pkg_name, " context - source not found\n")
    return(invisible(paste0("# ", pkg_name, " context - source not found")))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
