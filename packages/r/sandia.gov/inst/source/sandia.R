# sandia.gov.R - Self-contained sandia.gov client
# DOE Global Energy Storage Database (GESDB) at gesdb.sandia.gov

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# sandia.gov.R
# Self-contained client for Sandia National Laboratories GESDB.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none documented
# Source: https://gesdb.sandia.gov/backend/projectdata


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sandia_base <- "https://gesdb.sandia.gov/backend"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# Cache for all projects (API returns full dataset each call)
.sandia_cache <- new.env(parent = emptyenv())

.get_all_projects <- function(refresh = FALSE) {
  if (!refresh && exists("projects", envir = .sandia_cache)) {
    return(get("projects", envir = .sandia_cache))
  }
  raw <- .fetch_json(sprintf("%s/projectdata", .sandia_base))
  parsed <- .parse_projects(raw)
  assign("projects", parsed, envir = .sandia_cache)
  parsed
}

# == Schemas ===================================================================

.schema_projects <- tibble(
  id = integer(),
  subsystem_id = numeric(),
  name = character(),
  status = character(),
  rated_power_kw = numeric(),
  duration_hrs = numeric(),
  capacity_kwh = numeric(),
  description = character(),
  url = character(),
  country = character(),
  city = character(),
  state = character(),
  latitude = numeric(),
  longitude = numeric(),
  commissioned_date = character(),
  technology_provider = character(),
  developer = character(),
  ownership_model = character(),
  capex_usd = numeric(),
  interconnection_level = character(),
  data_source = character(),
  validated = character()
)

.schema_summary <- tibble(
  country = character(),
  n_projects = integer(),
  total_power_kw = numeric(),
  total_capacity_kwh = numeric()
)

# == Private parsers ===========================================================

.safe_num <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (is.null(v) || length(v) == 0 || is.na(v)) NA_real_ else v
}

.safe_int <- function(x) {
  v <- suppressWarnings(as.integer(x))
  if (is.null(v) || length(v) == 0 || is.na(v)) NA_integer_ else v
}

.safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
}

.parse_projects <- function(items) {
  if (is.null(items) || length(items) == 0) return(.schema_projects)

  rows <- lapply(items, function(p) {
    tibble(
      id                    = .safe_int(p[["ID"]]),
      subsystem_id          = .safe_num(p[["Subsystem ID"]]),
      name                  = .safe_chr(p[["Project/Plant Name"]]),
      status                = .safe_chr(p[["Status"]]),
      rated_power_kw        = .safe_num(p[["Rated Power (kW)"]]),
      duration_hrs          = .safe_num(p[["Discharge Duration at Rated Power (hrs)"]]),
      capacity_kwh          = .safe_num(p[["Storage Capacity (kWh)"]]),
      description           = .safe_chr(p[["Description/Notes"]]),
      url                   = .safe_chr(p[["URL"]]),
      country               = .safe_chr(p[["Country"]]),
      city                  = .safe_chr(p[["City"]]),
      state                 = .safe_chr(p[["State/Province/Territory"]]),
      latitude              = .safe_num(p[["Latitude"]]),
      longitude             = .safe_num(p[["Longitude"]]),
      commissioned_date     = .safe_chr(p[["Commissioned Date"]]),
      technology_provider   = .safe_chr(p[["Energy Storage Technology Provider"]]),
      developer             = .safe_chr(p[["Developer"]]),
      ownership_model       = .safe_chr(p[["Ownership Model"]]),
      capex_usd             = .safe_num(p[["Capital Expenditure - CAPEX (USD)"]]),
      interconnection_level = .safe_chr(p[["Grid Interconnection Level"]]),
      data_source           = .safe_chr(p[["Data Source"]]),
      validated             = .safe_chr(p[["Project Data Validated?"]])
    )
  })

  bind_rows(rows)
}


# == Public functions ==========================================================

#' List all energy storage projects from DOE GESDB
#'
#' Returns all projects from Sandia's Global Energy Storage Database.
#' Results are cached in-session; use refresh = TRUE to re-download.
#'
#' @param refresh Logical; re-fetch from API? Default FALSE (uses cache).
#' @return tibble with columns: id, subsystem_id, name, status, rated_power_kw,
#'   duration_hrs, capacity_kwh, description, url, country, city, state,
#'   latitude, longitude, commissioned_date, technology_provider, developer,
#'   ownership_model, capex_usd, interconnection_level, data_source, validated
sandia_projects <- function(refresh = FALSE) {
  .get_all_projects(refresh = refresh)
}

#' Search energy storage projects by keyword
#'
#' Searches project names and descriptions for matching terms.
#'
#' @param query Character string to search for (case-insensitive)
#' @param field Which fields to search: "all" (default), "name", or "description"
#' @return tibble of matching projects (same columns as sandia_projects)
sandia_search <- function(query, field = "all") {
  all_proj <- .get_all_projects()
  q <- tolower(as.character(query))

  matches <- switch(field,
    "name" = grepl(q, tolower(all_proj$name), fixed = TRUE),
    "description" = grepl(q, tolower(all_proj$description), fixed = TRUE),
    grepl(q, tolower(all_proj$name), fixed = TRUE) |
      grepl(q, tolower(all_proj$description), fixed = TRUE)
  )

  all_proj[matches, ]
}

#' Get a single project by ID
#'
#' @param id Integer project ID
#' @return tibble with one row (same columns as sandia_projects)
sandia_project <- function(id) {
  all_proj <- .get_all_projects()
  result <- all_proj |> filter(id == as.integer(!!id))
  if (nrow(result) == 0) {
    message("No project found with ID ", id)
    return(.schema_projects)
  }
  result
}

#' List projects filtered by country
#'
#' @param country Country name (case-insensitive partial match)
#' @param status Optional status filter (e.g. "Operational", "Announced")
#' @return tibble of matching projects
sandia_by_country <- function(country_query, status = NULL) {
  all_proj <- .get_all_projects()
  q <- tolower(as.character(country_query))
  result <- all_proj |>
    filter(grepl(q, tolower(country), fixed = TRUE))

  if (!is.null(status)) {
    s <- tolower(as.character(status))
    result <- result |>
      filter(grepl(s, tolower(.data$status), fixed = TRUE))
  }
  result
}

#' Summarize projects by country
#'
#' Returns aggregate statistics per country: project count, total rated power,
#' and total storage capacity.
#'
#' @param top_n Integer; return only the top N countries by project count.
#'   Default NULL returns all.
#' @return tibble: country, n_projects, total_power_kw, total_capacity_kwh
sandia_summary <- function(top_n = NULL) {
  all_proj <- .get_all_projects()
  result <- all_proj |>
    group_by(country) |>
    summarise(
      n_projects       = n(),
      total_power_kw   = sum(rated_power_kw, na.rm = TRUE),
      total_capacity_kwh = sum(capacity_kwh, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(n_projects))

  if (!is.null(top_n)) {
    result <- result |> head(as.integer(top_n))
  }
  result
}

#' List unique statuses in the GESDB
#'
#' @return tibble: status, n_projects
sandia_statuses <- function() {
  all_proj <- .get_all_projects()
  all_proj |>
    filter(!is.na(status)) |>
    group_by(status) |>
    summarise(n_projects = n(), .groups = "drop") |>
    arrange(desc(n_projects))
}

#' List unique countries in the GESDB
#'
#' @return tibble: country, n_projects
sandia_countries <- function() {
  all_proj <- .get_all_projects()
  all_proj |>
    filter(!is.na(country)) |>
    group_by(country) |>
    summarise(n_projects = n(), .groups = "drop") |>
    arrange(desc(n_projects))
}


# == Context ===================================================================

#' Generate LLM-friendly context for sandia.gov
#'
#' @return Character string with full function signatures and bodies
sandia_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/sandia.gov.R"
  if (!file.exists(src_file)) {
    cat("# sandia.gov context - source not found\n")
    return(invisible("# sandia.gov context - source not found"))
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
