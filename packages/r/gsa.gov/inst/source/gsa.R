# gsa.gov.R
# Self-contained GSA (General Services Administration) API client.
# Covers Per Diem travel rates, City Pair airfares, and Site Scanning.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key required. DEMO_KEY works but limited to 10 req/hr.
#   Get a key at https://api.gsa.gov or https://api.data.gov/signup/
# Rate limits: DEMO_KEY = 10/hr. Real key = 1000/hr.
# Docs: https://open.gsa.gov/api/

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.gsa_base <- "https://api.gsa.gov"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
.safe_dbl <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else as.double(x)
.safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x)

# -- JSON fetch helper ---------------------------------------------------------

.gsa_fetch <- function(path, params = list(), api_key = "DEMO_KEY") {
  params$api_key <- api_key
  url <- paste0(.gsa_base, "/", path)
  tmp <- tempfile(fileext = ".json")
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)

  status <- httr2::resp_status(resp)
  if (status == 429) {
    stop("GSA API rate limit exceeded. Get a key at https://api.data.gov/signup/",
         call. = FALSE)
  }
  if (status >= 400) {
    body <- tryCatch(jsonlite::fromJSON(tmp), error = function(e) list())
    msg <- body$error$message %||% paste("HTTP", status)
    stop(sprintf("GSA API error: %s", msg), call. = FALSE)
  }
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}


# == Per Diem Rates ============================================================

#' Get federal per diem rates by city and state
#'
#' Retrieves federal per diem rates (lodging and meals & incidentals)
#' for a specific city and state for a given fiscal year.
#'
#' @param city City name (e.g. "Washington", "New York", "San Francisco")
#' @param state Two-letter state abbreviation (e.g. "DC", "NY", "CA")
#' @param year Fiscal year (default: current year)
#' @param api_key API key (default: DEMO_KEY)
#' @return tibble with monthly lodging rates and M&IE rates
#' @export
gsa_perdiem <- function(city, state, year = as.integer(format(Sys.Date(), "%Y")),
                        api_key = "DEMO_KEY") {
  path <- sprintf("travel/perdiem/v2/rates/city/%s/state/%s/year/%s",
                  utils::URLencode(city, reserved = TRUE), state, year)
  raw <- .gsa_fetch(path, api_key = api_key)

  rates <- raw$rates
  if (is.null(rates) || length(rates) == 0) return(tibble())

  # Each rate entry has a rate array with monthly lodging values
  all_rows <- list()
  for (i in seq_along(rates$rate)) {
    entry <- rates[i, ]
    rate_data <- if (is.data.frame(rates$rate)) rates$rate[i, ] else rates$rate[[i]]

    if (is.data.frame(rate_data)) {
      months_df <- rate_data
      months_df$city <- entry$city %||% city
      months_df$state <- entry$state %||% state
      months_df$county <- entry$county %||% NA_character_
      months_df$meals <- .safe_dbl(entry$meals)
      months_df$year <- year
      all_rows[[length(all_rows) + 1]] <- months_df
    }
  }

  if (length(all_rows) == 0) return(tibble())
  df <- as_tibble(bind_rows(all_rows))

  # Type numeric columns
  for (col in names(df)) {
    if (grepl("^months\\.", col) || col %in% c("meals")) {
      if (is.character(df[[col]])) df[[col]] <- as.numeric(df[[col]])
    }
  }
  df
}


#' Get per diem rates by zip code
#'
#' @param zip Five-digit zip code
#' @param year Fiscal year (default: current year)
#' @param api_key API key
#' @return tibble with per diem rate details
#' @export
gsa_perdiem_zip <- function(zip, year = as.integer(format(Sys.Date(), "%Y")),
                            api_key = "DEMO_KEY") {
  path <- sprintf("travel/perdiem/v2/rates/zip/%s/year/%s", zip, year)
  raw <- .gsa_fetch(path, api_key = api_key)

  rates <- raw$rates
  if (is.null(rates) || length(rates) == 0) return(tibble())

  all_rows <- list()
  for (i in seq_along(rates$rate)) {
    entry <- rates[i, ]
    rate_data <- if (is.data.frame(rates$rate)) rates$rate[i, ] else rates$rate[[i]]
    if (is.data.frame(rate_data)) {
      months_df <- rate_data
      months_df$city <- .safe_chr(entry$city)
      months_df$state <- .safe_chr(entry$state)
      months_df$county <- .safe_chr(entry$county)
      months_df$meals <- .safe_dbl(entry$meals)
      months_df$zip <- zip
      months_df$year <- year
      all_rows[[length(all_rows) + 1]] <- months_df
    }
  }

  if (length(all_rows) == 0) return(tibble())
  as_tibble(bind_rows(all_rows))
}


#' List all per diem locations (CONUS) for a state
#'
#' Returns per diem rate areas defined for a state in a given fiscal year.
#'
#' @param state Two-letter state abbreviation
#' @param year Fiscal year
#' @param api_key API key
#' @return tibble: city, county, meals rate, and monthly lodging rates
#' @export
gsa_perdiem_state <- function(state, year = as.integer(format(Sys.Date(), "%Y")),
                              api_key = "DEMO_KEY") {
  path <- sprintf("travel/perdiem/v2/rates/state/%s/year/%s", state, year)
  raw <- .gsa_fetch(path, api_key = api_key)

  rates <- raw$rates
  if (is.null(rates) || length(rates) == 0) return(tibble())

  all_rows <- list()
  for (i in seq_len(nrow(rates))) {
    entry <- rates[i, ]
    rate_data <- if (is.data.frame(rates$rate)) rates$rate[i, ] else rates$rate[[i]]
    if (is.data.frame(rate_data)) {
      months_df <- rate_data
      months_df$city <- .safe_chr(entry$city)
      months_df$state <- state
      months_df$county <- .safe_chr(entry$county)
      months_df$meals <- .safe_dbl(entry$meals)
      months_df$year <- year
      all_rows[[length(all_rows) + 1]] <- months_df
    }
  }

  if (length(all_rows) == 0) return(tibble())
  as_tibble(bind_rows(all_rows))
}


# == Site Scanning =============================================================

#' Search federal government websites via Site Scanning API
#'
#' The Site Scanning program scans federal .gov websites daily to gather
#' data on accessibility, analytics, security headers, and more.
#'
#' @param target_url Filter by specific domain or URL (partial match)
#' @param agency Filter by agency (e.g. "gsa", "nasa", "dhs")
#' @param limit Max results (default 100)
#' @param page Page number (1-based, default 1)
#' @param api_key API key
#' @return tibble: target_url, final_url, agency, bureau, status_code,
#'   scan results (DAP analytics, USWDS usage, etc.)
#' @export
gsa_sites <- function(target_url = NULL, agency = NULL, limit = 100,
                      page = 1, api_key = "DEMO_KEY") {
  params <- list(limit = limit, page = page)
  if (!is.null(target_url)) params$target_url <- target_url
  if (!is.null(agency))     params$agency <- agency

  raw <- .gsa_fetch("technology/site-scanning/v1/websites", params, api_key)

  if (is.null(raw) || length(raw) == 0) return(tibble())
  if (is.data.frame(raw)) {
    return(as_tibble(raw))
  }
  if (is.list(raw) && !is.null(raw$items)) {
    return(as_tibble(raw$items))
  }
  as_tibble(raw)
}


#' Get scan results for a specific website
#'
#' @param target_url Full domain to look up (e.g. "www.gsa.gov")
#' @param api_key API key
#' @return tibble: one row with all scan fields
#' @export
gsa_site <- function(target_url, api_key = "DEMO_KEY") {
  path <- sprintf("technology/site-scanning/v1/websites/%s",
                  utils::URLencode(target_url, reserved = TRUE))
  raw <- .gsa_fetch(path, api_key = api_key)
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(as.list(raw))
}


# == City Pair Airfares ========================================================

#' Search City Pair Program airfares
#'
#' The City Pair Program offers discounted air transportation for
#' federal government travelers. Returns contract airfares between cities.
#'
#' @param origin Origin airport code (e.g. "DCA", "ORD", "LAX")
#' @param destination Destination airport code
#' @param award_year Fiscal year for awards (default: current year)
#' @param api_key API key
#' @return tibble: origin, destination, airline, fare (YCA/XCA),
#'   effective dates, contract info
#' @export
gsa_citypair <- function(origin = NULL, destination = NULL,
                         award_year = as.integer(format(Sys.Date(), "%Y")),
                         api_key = "DEMO_KEY") {
  params <- list(award_year = award_year)
  if (!is.null(origin))      params$origin_airport_abbrev <- origin
  if (!is.null(destination)) params$destination_airport_abbrev <- destination

  raw <- .gsa_fetch("travel/citypair/v0/airfares", params, api_key)

  if (is.null(raw) || length(raw) == 0) return(tibble())
  if (is.data.frame(raw)) return(as_tibble(raw))
  if (is.list(raw) && !is.null(raw$result)) return(as_tibble(raw$result))
  tibble()
}


# == Data.json Catalog =========================================================

#' List GSA datasets from the data.json catalog
#'
#' Searches the GSA open data catalog (Project Open Data format).
#' Note: this fetches the full catalog and filters locally.
#'
#' @param query Optional text search across titles and descriptions
#' @param keyword Optional keyword/tag filter
#' @param limit Max results to return (default 50)
#' @return tibble: title, description, keyword, modified, publisher,
#'   distribution (access URLs), contactPoint
#' @export
gsa_datasets <- function(query = NULL, keyword = NULL, limit = 50) {
  url <- "https://open.gsa.gov/data.json"
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5) |>
    httr2::req_perform(path = tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  datasets <- raw$dataset
  if (is.null(datasets) || length(datasets) == 0) return(tibble())

  df <- tibble(
    title       = vapply(datasets, function(d) .safe_chr(d$title), character(1)),
    description = vapply(datasets, function(d) .safe_chr(d$description), character(1)),
    modified    = vapply(datasets, function(d) .safe_chr(d$modified), character(1)),
    identifier  = vapply(datasets, function(d) .safe_chr(d$identifier), character(1)),
    keyword     = vapply(datasets, function(d) {
      kw <- d$keyword
      if (is.null(kw)) NA_character_ else paste(kw, collapse = "; ")
    }, character(1)),
    publisher   = vapply(datasets, function(d) .safe_chr(d$publisher$name), character(1)),
    contact     = vapply(datasets, function(d) .safe_chr(d$contactPoint$fn), character(1)),
    access_url  = vapply(datasets, function(d) {
      dist <- d$distribution
      if (is.null(dist) || length(dist) == 0) return(NA_character_)
      urls <- vapply(dist, function(dd) .safe_chr(dd$accessURL %||% dd$downloadURL), character(1))
      paste(urls[!is.na(urls)], collapse = "; ")
    }, character(1))
  )

  if (!is.null(query)) {
    df <- df |> filter(grepl(query, title, ignore.case = TRUE) |
                       grepl(query, description, ignore.case = TRUE))
  }
  if (!is.null(keyword)) {
    kw_pattern <- keyword[1]
    df <- df |> filter(grepl(kw_pattern, .data[["keyword"]], ignore.case = TRUE))
  }

  head(df, limit)
}


# == Context ===================================================================

#' Generate LLM-friendly context for gsa.gov
#'
#' @return Character string with full function signatures
#' @export
gsa_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/gsa.gov.R"
  if (!file.exists(src_file)) {
    cat("# gsa.gov context - source not found\n")
    return(invisible("# gsa.gov context - source not found"))
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
