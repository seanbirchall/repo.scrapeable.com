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
#' for a specific city and state for a given fiscal year from the GSA
#' Per Diem API.
#'
#' @param city Character. City name (e.g. \code{"Washington"}, \code{"New York"},
#'   \code{"San Francisco"}, \code{"Chicago"}).
#' @param state Character. Two-letter state abbreviation (e.g. \code{"DC"},
#'   \code{"NY"}, \code{"CA"}, \code{"IL"}).
#' @param year Integer. Fiscal year (default: current year). Valid range
#'   is approximately 2010 to current year.
#' @param api_key Character. API key (default: \code{"DEMO_KEY"}, limited to
#'   10 req/hr). Get a key at \url{https://api.data.gov/signup/}.
#' @return A tibble with columns: \code{meals} (numeric, daily M&IE rate),
#'   \code{zip}, \code{county}, \code{city}, \code{standardRate},
#'   \code{months.month} (nested data frame with monthly lodging values),
#'   \code{state}, \code{year}.
#' @examples
#' \dontrun{
#' gsa_perdiem("Washington", "DC", 2025)
#' gsa_perdiem("San Francisco", "CA")
#' }
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
#' Retrieves federal per diem rates (lodging and meals & incidentals)
#' for a specific zip code and fiscal year.
#'
#' @param zip Character. Five-digit zip code (e.g. \code{"20001"}, \code{"10001"},
#'   \code{"90210"}).
#' @param year Integer. Fiscal year (default: current year). Valid range
#'   is approximately 2010 to current year.
#' @param api_key Character. API key (default: \code{"DEMO_KEY"}).
#' @return A tibble with columns: \code{meals} (numeric, daily M&IE rate),
#'   \code{zip}, \code{county}, \code{city}, \code{standardRate},
#'   \code{months.month} (nested data frame with monthly lodging values),
#'   \code{state}, \code{year}.
#' @examples
#' \dontrun{
#' gsa_perdiem_zip("20001", 2025)
#' gsa_perdiem_zip("90210")
#' }
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
#' Returns all per diem rate areas defined for a state in a given fiscal
#' year, including every city/county combination with lodging rates.
#'
#' @param state Character. Two-letter state abbreviation (e.g. \code{"CA"},
#'   \code{"TX"}, \code{"NY"}).
#' @param year Integer. Fiscal year (default: current year).
#' @param api_key Character. API key (default: \code{"DEMO_KEY"}).
#' @return A tibble with columns: monthly lodging rates (nested),
#'   \code{city}, \code{state}, \code{county}, \code{meals} (numeric),
#'   \code{year}.
#' @examples
#' \dontrun{
#' gsa_perdiem_state("CA", 2025)
#' gsa_perdiem_state("DC")
#' }
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
#' data on accessibility, analytics, security headers, USWDS adoption,
#' DAP analytics, and more. Returns 100+ scan fields per site.
#'
#' @param target_url Character or NULL. Filter by specific domain or URL
#'   (partial match, e.g. \code{"gsa.gov"}, \code{"nasa.gov"}).
#' @param agency Character or NULL. Filter by agency abbreviation
#'   (e.g. \code{"gsa"}, \code{"nasa"}, \code{"dhs"}, \code{"dod"}).
#' @param limit Integer. Max results per page (default 100).
#' @param page Integer. Page number, 1-based (default 1).
#' @param api_key Character. API key (default: \code{"DEMO_KEY"}).
#' @return A tibble with 100+ columns including: \code{scan_date},
#'   \code{url}, \code{base_domain}, \code{status_code}, \code{live},
#'   \code{redirect}, \code{uswds_count}, \code{dap} (logical),
#'   \code{agency}, \code{bureau}, \code{title}, \code{description},
#'   \code{https_enforced}, \code{hsts}, and many more scan metrics.
#' @examples
#' \dontrun{
#' gsa_sites(agency = "gsa", limit = 10)
#' gsa_sites(target_url = "nasa.gov", limit = 5)
#' }
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
#' Retrieves the latest scan results for a single federal .gov website,
#' returning one row with all scan metrics.
#'
#' @param target_url Character. Full domain to look up (e.g. \code{"www.gsa.gov"},
#'   \code{"www.nasa.gov"}, \code{"data.gov"}).
#' @param api_key Character. API key (default: \code{"DEMO_KEY"}).
#' @return A tibble with one row and 100+ scan columns (same schema as
#'   \code{\link{gsa_sites}}).
#' @examples
#' \dontrun{
#' gsa_site("www.gsa.gov")
#' gsa_site("data.gov")
#' }
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
#' Note: this endpoint may not be available in all years.
#'
#' @param origin Character or NULL. Origin airport code (e.g. \code{"DCA"},
#'   \code{"ORD"}, \code{"LAX"}, \code{"SFO"}).
#' @param destination Character or NULL. Destination airport code.
#' @param award_year Integer. Fiscal year for awards (default: current year).
#' @param api_key Character. API key (default: \code{"DEMO_KEY"}).
#' @return A tibble with contract airfare data including origin, destination,
#'   airline, fare amounts, effective dates, and contract details.
#' @examples
#' \dontrun{
#' gsa_citypair(origin = "DCA")
#' gsa_citypair(origin = "ORD", destination = "LAX", award_year = 2024)
#' }
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
#' Downloads the full catalog from \url{https://open.gsa.gov/data.json}
#' and filters locally by query and/or keyword.
#'
#' @param query Character or NULL. Text search across titles and descriptions
#'   (e.g. \code{"travel"}, \code{"spending"}, \code{"real estate"}).
#' @param keyword Character or NULL. Keyword/tag filter
#'   (e.g. \code{"CPP"}, \code{"per diem"}).
#' @param limit Integer. Max results to return (default 50).
#' @return A tibble with columns: \code{title}, \code{description},
#'   \code{modified}, \code{identifier}, \code{keyword} (semicolon-separated),
#'   \code{publisher}, \code{contact}, \code{access_url} (semicolon-separated
#'   download/access URLs).
#' @examples
#' \dontrun{
#' gsa_datasets()
#' gsa_datasets(query = "travel")
#' gsa_datasets(keyword = "CPP", limit = 10)
#' }
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

#' Get gsa.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gsa_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gsa_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gsa.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gsa.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gsa.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gsa.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
