# hrsa.gov.R
# Self-contained HRSA (Health Resources & Services Administration) client.
# Uses downloadable CSVs from data.hrsa.gov for HPSA and Health Center data.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public data)
# Source: https://data.hrsa.gov/data/download


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
#' Downloads and filters the HRSA HPSA dataset (Primary Care).
#' Data is cached in-session after first download.
#'
#' @param state Character or NULL. Two-letter state abbreviation
#'   (e.g. \code{"CA"}, \code{"TX"}, \code{"NY"}, \code{"VT"}).
#' @param status Character or NULL. Filter by HPSA status. Common values:
#'   \code{"Designated"}, \code{"Withdrawn"}, \code{"Proposed Withdrawal"}.
#' @param discipline Character or NULL. Filter by discipline class. Valid values:
#'   \code{"Primary Care"}, \code{"Dental Health"}, \code{"Mental Health"}.
#' @param limit Integer. Maximum rows returned (default 500).
#' @return A tibble with columns: \code{hpsa_name} (character),
#'   \code{hpsa_id} (character), \code{designation_type} (character),
#'   \code{discipline_class} (character), \code{hpsa_score} (integer, 0-25),
#'   \code{state} (character), \code{status} (character),
#'   \code{designation_date} (Date), \code{metro_indicator} (character),
#'   \code{designation_pop} (numeric), \code{pct_poverty} (numeric),
#'   \code{formal_ratio} (character), \code{population_type} (character),
#'   \code{rural_status} (character), \code{longitude} (numeric),
#'   \code{latitude} (numeric), \code{county} (character),
#'   \code{state_name} (character), \code{provider_type} (character).
#' @examples
#' \dontrun{
#' hrsa_hpsa(state = "CA", limit = 50)
#' hrsa_hpsa(discipline = "Mental Health", status = "Designated")
#' }
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
#' Searches the HPSA dataset by name or county using case-insensitive
#' partial matching.
#'
#' @param query Character. Search term matched against HPSA name and county
#'   (e.g. \code{"Los Angeles"}, \code{"rural"}, \code{"community health"}).
#' @param state Character or NULL. Optional two-letter state abbreviation filter.
#' @param limit Integer. Maximum rows returned (default 100).
#' @return A tibble with the same columns as \code{\link{hrsa_hpsa}}.
#' @examples
#' \dontrun{
#' hrsa_search("Los Angeles")
#' hrsa_search("rural", state = "TX")
#' }
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
#' Downloads and filters HRSA-funded health center site locations.
#' Data is cached in-session after first download.
#'
#' @param state Character or NULL. Two-letter state abbreviation
#'   (e.g. \code{"CA"}, \code{"VT"}, \code{"NY"}).
#' @param city Character or NULL. City name filter, case-insensitive partial
#'   match (e.g. \code{"Boston"}, \code{"Denver"}).
#' @param limit Integer. Maximum rows returned (default 500).
#' @return A tibble with columns: \code{site_name} (character),
#'   \code{health_center_name} (character), \code{site_address} (character),
#'   \code{site_city} (character), \code{site_state} (character),
#'   \code{site_zip} (character), \code{site_phone} (character),
#'   \code{site_web} (character, URL), \code{hours_per_week} (numeric),
#'   \code{health_center_type} (character), \code{site_status} (character),
#'   \code{location_type} (character), \code{operator_type} (character),
#'   \code{longitude} (numeric), \code{latitude} (numeric),
#'   \code{county} (character), \code{state_name} (character),
#'   \code{hhs_region} (character).
#' @examples
#' \dontrun{
#' hrsa_health_centers(state = "VT")
#' hrsa_health_centers(state = "CA", city = "Los Angeles")
#' }
#' @export
hrsa_health_centers <- function(state = NULL, city = NULL, limit = 500) {
  df <- .hrsa_load_hc()
  if (!is.null(state)) df <- df |> filter(.data$site_state == !!state)
  if (!is.null(city)) df <- df |> filter(grepl(city, site_city, ignore.case = TRUE))
  head(df, limit)
}

#' Search health centers by name
#'
#' Searches HRSA health center data by site name or center name using
#' case-insensitive partial matching.
#'
#' @param query Character. Search term matched against site name and health
#'   center name (e.g. \code{"community"}, \code{"dental"}, \code{"family"}).
#' @param state Character or NULL. Optional two-letter state abbreviation filter.
#' @param limit Integer. Maximum rows returned (default 100).
#' @return A tibble with the same columns as \code{\link{hrsa_health_centers}}.
#' @examples
#' \dontrun{
#' hrsa_hc_search("community")
#' hrsa_hc_search("dental", state = "NY")
#' }
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
#' Aggregates HPSA counts, average scores, and total designation population
#' by state. Sorted by number of HPSAs descending.
#'
#' @param status Character or NULL. Filter by HPSA status before aggregating
#'   (default \code{"Designated"} for active HPSAs only). Set to \code{NULL}
#'   to include all statuses.
#' @return A tibble with columns: \code{state} (character, 2-letter),
#'   \code{n_hpsa} (integer, count of HPSAs), \code{avg_score} (numeric,
#'   mean HPSA score 0-25), \code{total_pop} (numeric, sum of designation
#'   populations).
#' @examples
#' \dontrun{
#' hrsa_hpsa_by_state()
#' hrsa_hpsa_by_state(status = NULL)
#' }
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
#' Aggregates health center site counts and unique center counts by state.
#' Sorted by number of sites descending.
#'
#' @return A tibble with columns: \code{state} (character, 2-letter),
#'   \code{n_sites} (integer, total site locations), \code{n_centers}
#'   (integer, distinct health center organizations).
#' @examples
#' \dontrun{
#' hrsa_hc_by_state()
#' }
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

#' Get hrsa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hrsa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hrsa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/hrsa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "hrsa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# hrsa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# hrsa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
