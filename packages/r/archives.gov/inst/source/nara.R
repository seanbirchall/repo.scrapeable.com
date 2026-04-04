# archives.gov.R
# Self-contained National Archives (NARA) client for R.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: The Catalog API v2 requires an API key (x-api-key header).
#   Request one at Catalog_API@nara.gov
#   Set NARA_API_KEY env var or pass api_key= to catalog functions.
#   Open dataset functions (amendments, grants, tapes) need no key.
# Docs: https://github.com/usnationalarchives/Catalog-API
#       https://www.archives.gov/open

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.nara_catalog_base <- "https://catalog.archives.gov/api/v2"
.nara_open_base <- "https://www.archives.gov"

# -- Core fetch helpers --------------------------------------------------------

.nara_fetch_json <- function(url, api_key = NULL) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json")
  key <- api_key %||% Sys.getenv("NARA_API_KEY", "")
  if (nzchar(key)) req <- req |> httr2::req_headers(`x-api-key` = key)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

.nara_fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_options(followlocation = TRUE) |>
    httr2::req_perform(path = tmp)
  utils::read.csv(tmp, stringsAsFactors = FALSE) |> as_tibble()
}

# == Schemas ===================================================================

.schema_catalog <- tibble(
  naId = character(), title = character(), description = character(),
  type = character(), level = character(), date = character(),
  creators = character(), url = character()
)

.schema_amendments <- tibble(
  identifier = character(), source_code = character(),
  source_citation = character(), source_index_number = integer(),
  title_or_description_from_source = character(),
  date_approximation = character(), year = integer(),
  month = character(), day = character(),
  congress = character(), congressional_session = character(),
  joint_resolution_chamber = character(), joint_resolution_number = character(),
  sponsor_name = character(), sponsor_state_or_territory = character(),
  committee_of_referral = character(), last_modified = character()
)

.schema_grants <- tibble(
  grant_number = character(), grant_type = character(),
  first_name = character(), last_name = character(),
  institution = character(), city = character(), state = character(),
  year_awarded = integer(), project_title = character(),
  program = character(), division = character(),
  grant_amount_approved = character(), final_grant_award = character(),
  grant_start_date = character(), grant_end_date = character(),
  project_description = character()
)

.schema_tapes <- tibble(
  conversationTitle = character(), tapeNumber = integer(),
  conversationNumber = integer(), identifier = character(),
  startDateTime = character(), endDateTime = character(),
  participants = character(), description = character(),
  locationCode = character(), recordingDevice = character(),
  collection = character()
)

# == Catalog API (requires API key) ============================================

#' Search the National Archives Catalog
#'
#' Queries the NARA Catalog API v2. Requires an API key.
#' Set NARA_API_KEY env var or pass api_key=.
#'
#' @param query Search term (e.g. "civil war", "constitution")
#' @param limit Max results to return (default 20, max 100)
#' @param offset Starting offset for pagination (default 0)
#' @param api_key Optional API key (overrides NARA_API_KEY env var)
#' @return tibble: naId, title, description, type, level, date, creators, url
nara_search <- function(query, limit = 20, offset = 0, api_key = NULL) {
  limit <- min(limit, 100)
  url <- paste0(.nara_catalog_base, "/records/search?q=",
                utils::URLencode(query, reserved = TRUE),
                "&limit=", limit, "&offset=", offset)
  raw <- tryCatch(.nara_fetch_json(url, api_key = api_key),
                  error = function(e) {
                    warning("Catalog API error: ", conditionMessage(e),
                            "\nDo you have a valid NARA_API_KEY?")
                    return(NULL)
                  })
  if (is.null(raw)) return(.schema_catalog)

  results <- raw$body$hits$hits %||% raw$hits$hits %||% raw$results %||% list()
  if (length(results) == 0) return(.schema_catalog)

  bind_rows(lapply(results, function(r) {
    src <- r$`_source` %||% r
    desc <- src$description %||% list()
    tibble(
      naId        = as.character(src$naId %||% r$`_id` %||% NA),
      title       = as.character(desc$title %||% src$title %||% NA),
      description = as.character(desc$scopeAndContentNote %||% NA),
      type        = as.character(desc$type %||% src$type %||% NA),
      level       = as.character(desc$level %||% NA),
      date        = as.character(desc$date %||% desc$inclusiveDates %||% NA),
      creators    = paste(unlist(lapply(desc$creators %||% list(),
                                       function(c) c$name %||% c$heading %||% "")),
                          collapse = "; "),
      url         = paste0("https://catalog.archives.gov/id/", src$naId %||% r$`_id` %||% "")
    )
  }))
}

#' Get a single record from the National Archives Catalog
#'
#' @param naid National Archives Identifier (e.g. "597542")
#' @param api_key Optional API key
#' @return tibble with one row of record metadata
nara_record <- function(naid, api_key = NULL) {
  url <- paste0(.nara_catalog_base, "/records/", naid)
  raw <- tryCatch(.nara_fetch_json(url, api_key = api_key),
                  error = function(e) {
                    warning("Catalog API error: ", conditionMessage(e))
                    return(NULL)
                  })
  if (is.null(raw)) return(.schema_catalog[0, ])

  src <- raw$body %||% raw
  desc <- src$description %||% list()
  tibble(
    naId        = as.character(src$naId %||% naid),
    title       = as.character(desc$title %||% src$title %||% NA),
    description = as.character(desc$scopeAndContentNote %||% NA),
    type        = as.character(desc$type %||% src$type %||% NA),
    level       = as.character(desc$level %||% NA),
    date        = as.character(desc$date %||% desc$inclusiveDates %||% NA),
    creators    = paste(unlist(lapply(desc$creators %||% list(),
                                     function(c) c$name %||% c$heading %||% "")),
                        collapse = "; "),
    url         = paste0("https://catalog.archives.gov/id/", naid)
  )
}

# == Open Datasets (no API key required) =======================================

#' Get NARA Amending America dataset
#'
#' Returns all 11,000+ proposed Constitutional amendments from 1787-2014.
#' No API key required.
#'
#' @param year Optional: filter to a specific year
#' @param congress Optional: filter to a specific Congress number
#' @param sponsor Optional: filter by sponsor name (partial match, case-insensitive)
#' @return tibble of proposed amendments with id, source, title, year, sponsor, etc.
nara_amendments <- function(year = NULL, congress = NULL, sponsor = NULL) {
  url <- paste0(.nara_open_base,
                "/open/amending-america/us-nara-amending-america-dataset-raw-2016-02-25.csv")
  df <- .nara_fetch_csv(url)

  # Normalize column names
  names(df) <- gsub("\\.", "_", tolower(names(df)))

  # Type conversions
  if ("year" %in% names(df)) df$year <- suppressWarnings(as.integer(df$year))
  if ("source_index_number" %in% names(df))
    df$source_index_number <- suppressWarnings(as.integer(df$source_index_number))

  # Filters
  if (!is.null(year)) df <- df |> filter(.data$year == !!year)
  if (!is.null(congress)) df <- df |> filter(.data$congress == as.character(!!congress))
  if (!is.null(sponsor)) df <- df |> filter(grepl(!!sponsor, .data$sponsor_name, ignore.case = TRUE))

  df
}

#' Get NHPRC grants dataset
#'
#' Returns National Historical Publications and Records Commission grants
#' awarded 1965-present. No API key required.
#'
#' @param state Optional: filter by two-letter state code (e.g. "CA", "NY")
#' @param year Optional: filter by year awarded
#' @param program Optional: filter by program (partial match, case-insensitive)
#' @return tibble of NHPRC grants with grant_number, institution, project_title, etc.
nara_grants <- function(state = NULL, year = NULL, program = NULL) {
  url <- paste0(.nara_open_base, "/files/open/data/nhprc-4-2016.csv")
  df <- .nara_fetch_csv(url)

  # Normalize column names
  names(df) <- gsub("\\s+", "_", gsub("[^a-zA-Z0-9_ ]", "", tolower(names(df))))

  # Type conversions
  if ("year_awarded" %in% names(df))
    df$year_awarded <- suppressWarnings(as.integer(df$year_awarded))

  # Filters
  if (!is.null(state)) df <- df |> filter(.data$state == !!state)
  if (!is.null(year)) df <- df |> filter(.data$year_awarded == !!year)
  if (!is.null(program)) df <- df |> filter(grepl(!!program, .data$program, ignore.case = TRUE))

  df
}

#' Get White House Tapes (Nixon) conversation list
#'
#' Returns metadata for all Nixon White House Tapes conversations (1971-1973).
#' No API key required.
#'
#' @param participant Optional: filter by participant name (partial match, case-insensitive)
#' @param start_date Optional: filter conversations on or after this date (YYYY-MM-DD)
#' @param end_date Optional: filter conversations on or before this date (YYYY-MM-DD)
#' @return tibble of tape conversations with identifier, datetime, participants, description, etc.
nara_nixon_tapes <- function(participant = NULL, start_date = NULL, end_date = NULL) {
  url <- paste0(.nara_open_base,
                "/open/data/37-wht-dataset-conversationlist-2015-09-22.csv")
  df <- .nara_fetch_csv(url)

  # Normalize column names
  names(df) <- gsub("([a-z])([A-Z])", "\\1_\\2", names(df)) |> tolower()

  # Type conversions
  if ("tape_number" %in% names(df))
    df$tape_number <- suppressWarnings(as.integer(df$tape_number))
  if ("conversation_number" %in% names(df))
    df$conversation_number <- suppressWarnings(as.integer(df$conversation_number))

  # Filters
  if (!is.null(participant))
    df <- df |> filter(grepl(!!participant, .data$participants, ignore.case = TRUE))
  if (!is.null(start_date))
    df <- df |> filter(.data$start_date_time >= !!start_date)
  if (!is.null(end_date))
    df <- df |> filter(.data$start_date_time <= !!paste0(end_date, "T23:59:59"))

  df
}

#' List NARA open datasets
#'
#' Returns a tibble describing available open datasets from the
#' National Archives with their names, descriptions, and URLs.
#' No API key required.
#'
#' @return tibble: name, description, format, url
nara_datasets <- function() {
  tibble(
    name = c("amendments", "nhprc_grants", "nixon_tapes",
             "social_media", "catalog"),
    description = c(
      "Proposed Constitutional amendments 1787-2014 (11,000+ records)",
      "NHPRC grants awarded 1965-present",
      "Nixon White House Tapes conversation list 1971-1973",
      "NARA social media channels and statistics",
      "National Archives Catalog (requires API key)"
    ),
    format = c("CSV", "CSV", "CSV", "HTML", "JSON API"),
    url = c(
      paste0(.nara_open_base, "/open/amending-america/us-nara-amending-america-dataset-raw-2016-02-25.csv"),
      paste0(.nara_open_base, "/files/open/data/nhprc-4-2016.csv"),
      paste0(.nara_open_base, "/open/data/37-wht-dataset-conversationlist-2015-09-22.csv"),
      paste0(.nara_open_base, "/social-media/"),
      "https://catalog.archives.gov/api/v2/"
    ),
    accessor = c("nara_amendments()", "nara_grants()", "nara_nixon_tapes()",
                 "N/A", "nara_search(), nara_record()")
  )
}

# == Context ===================================================================

#' Generate LLM-friendly context for archives.gov
#'
#' @return Character string with full function signatures and bodies
nara_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/archives.gov.R"
  if (!file.exists(src_file)) {
    cat("# archives.gov context - source not found\n")
    return(invisible("# archives.gov context - source not found"))
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
      if (depth <= 0 && k > fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- paste(c(rox, body), collapse = "\n")
  }
  txt <- paste(c(
    "# archives.gov (National Archives) R client",
    "# Functions:", paste("#  -", sapply(blocks, function(b) sub(" <- function.*", "", strsplit(b, "\n")[[1]][grep("<- function", strsplit(b, "\n")[[1]])[1]])), collapse = "\n"),
    "", paste(blocks, collapse = "\n\n")
  ), collapse = "\n")
  cat(txt)
  invisible(txt)
}
