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

#' @title Search the National Archives Catalog
#'
#' Full-text search across the National Archives Catalog API v2. Searches
#' archival descriptions, series, file units, and items. Requires an API key
#' (set \code{NARA_API_KEY} environment variable or pass \code{api_key}).
#' Request a key at \email{Catalog_API@@nara.gov}.
#'
#' @param query Search term. Examples: \code{"civil war"}, \code{"constitution"},
#'   \code{"japanese internment"}, \code{"moon landing"}.
#' @param limit Maximum results to return. Default \code{20}. Maximum \code{100}.
#' @param offset Starting offset for pagination. Default \code{0}.
#' @param api_key Optional API key string. Overrides \code{NARA_API_KEY}
#'   environment variable. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{naId} (character): National Archives Identifier
#'     \item \code{title} (character): Record title
#'     \item \code{description} (character): Scope and content note
#'     \item \code{type} (character): Record type (e.g. item, file unit, series)
#'     \item \code{level} (character): Archival level
#'     \item \code{date} (character): Date or inclusive date range
#'     \item \code{creators} (character): Semicolon-separated creator names
#'     \item \code{url} (character): URL to the catalog record
#'   }
#' @examples
#' \dontrun{
#' # Search for Civil War records
#' nara_search("civil war", limit = 50)
#'
#' # Search for Constitution-related records
#' nara_search("constitution")
#'
#' # Paginate through results
#' page1 <- nara_search("world war II", limit = 100, offset = 0)
#' page2 <- nara_search("world war II", limit = 100, offset = 100)
#' }
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

#' @title Get a single record from the National Archives Catalog
#'
#' Fetches detailed metadata for a single archival record by its National
#' Archives Identifier (naId). Returns the same column structure as
#' \code{nara_search()} but always one row. Requires an API key.
#'
#' @param naid National Archives Identifier (character or numeric). Example:
#'   \code{"597542"}. Find naIds using \code{nara_search()}.
#' @param api_key Optional API key. Overrides \code{NARA_API_KEY} env var.
#'   Default \code{NULL}.
#' @return A tibble with one row and columns:
#'   \itemize{
#'     \item \code{naId} (character): National Archives Identifier
#'     \item \code{title} (character): Record title
#'     \item \code{description} (character): Scope and content note
#'     \item \code{type} (character): Record type
#'     \item \code{level} (character): Archival level
#'     \item \code{date} (character): Date or date range
#'     \item \code{creators} (character): Semicolon-separated creator names
#'     \item \code{url} (character): URL to the catalog record
#'   }
#' @examples
#' \dontrun{
#' # Fetch a specific record
#' nara_record("597542")
#'
#' # Use an ID found via search
#' results <- nara_search("emancipation proclamation")
#' nara_record(results$naId[1])
#' }
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

#' @title Get NARA Amending America dataset
#'
#' Returns all 11,000+ proposed Constitutional amendments from 1787 to 2014.
#' Data is downloaded from a static CSV on archives.gov (no API key required).
#' Filter by year, Congress, or sponsor to narrow results.
#'
#' @param year Integer year to filter by. Example: \code{1920}. \code{NULL}
#'   for all years. Default \code{NULL}.
#' @param congress Congress number to filter by (character or numeric).
#'   Example: \code{66}. \code{NULL} for all. Default \code{NULL}.
#' @param sponsor Sponsor name filter (partial match, case-insensitive).
#'   Example: \code{"Madison"}. \code{NULL} for all. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{identifier} (character): Unique record identifier
#'     \item \code{source_code} (character): Source document code
#'     \item \code{source_citation} (character): Full source citation
#'     \item \code{source_index_number} (integer): Index within source
#'     \item \code{title_or_description_from_source} (character): Amendment title/description
#'     \item \code{date_approximation} (character): Date precision indicator
#'     \item \code{year} (integer): Year proposed
#'     \item \code{month} (integer): Month proposed
#'     \item \code{day} (integer): Day proposed
#'     \item \code{congress} (integer): Congress number
#'     \item \code{congressional_session} (character): Session within Congress
#'     \item \code{joint_resolution_chamber} (character): Chamber (House/Senate)
#'     \item \code{joint_resolution_number} (integer): Resolution number
#'     \item \code{sponsor_name} (character): Name of the sponsor
#'     \item \code{sponsor_state_or_territory} (character): Sponsor's state
#'     \item \code{committee_of_referral} (character): Committee referred to
#'     \item \code{last_modified} (character): Last modification date
#'   }
#' @examples
#' \dontrun{
#' # All proposed amendments
#' nara_amendments()
#'
#' # Amendments proposed in 1920 (19th Amendment era)
#' nara_amendments(year = 1920)
#'
#' # Amendments sponsored by Madison
#' nara_amendments(sponsor = "Madison")
#' }
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

#' @title Get NHPRC grants dataset
#'
#' Returns National Historical Publications and Records Commission (NHPRC)
#' grants awarded from 1965 to present. Data is downloaded from a static CSV
#' on archives.gov (no API key required). Note: column names in the raw CSV
#' are lowercased without separators (e.g. \code{grantnumber}, \code{yearawarded}).
#'
#' @param state Two-letter state code filter. Examples: \code{"CA"}, \code{"NY"}.
#'   \code{NULL} for all states. Default \code{NULL}.
#' @param year Year awarded filter (integer). Note: the year filter uses the
#'   \code{yearawarded} column which may not be present in all CSV versions.
#'   \code{NULL} for all years. Default \code{NULL}.
#' @param program Program name filter (partial match, case-insensitive).
#'   Example: \code{"archives"}. \code{NULL} for all. Default \code{NULL}.
#' @return A tibble with columns (all character except yearawarded):
#'   \itemize{
#'     \item \code{grantnumber} (character): Grant identifier
#'     \item \code{granttype} (character): Type of grant
#'     \item \code{firstname} (character): PI first name
#'     \item \code{lastname} (character): PI last name
#'     \item \code{institution} (character): Institution name
#'     \item \code{city} (character): City
#'     \item \code{state} (character): Two-letter state code
#'     \item \code{yearawarded} (character): Year the grant was awarded
#'     \item \code{projecttitle} (character): Project title
#'     \item \code{program} (character): NHPRC program
#'     \item \code{division} (character): Division
#'     \item \code{grantamountapproved} (character): Approved grant amount
#'     \item \code{finalgrantaward} (character): Final award amount
#'     \item \code{grantstartdate} (character): Grant start date
#'     \item \code{grantenddate} (character): Grant end date
#'     \item \code{projectdescription} (character): Description of the project
#'   }
#' @examples
#' \dontrun{
#' # All NHPRC grants
#' nara_grants()
#'
#' # Grants in California
#' nara_grants(state = "CA")
#'
#' # Grants related to archives programs
#' nara_grants(program = "archives")
#' }
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

#' @title Get White House Tapes (Nixon) conversation list
#'
#' Returns metadata for all Nixon White House Tapes conversations (1971-1973).
#' Data is downloaded from a static CSV on archives.gov (no API key required).
#' Contains approximately 4,000 conversations with participants, timestamps,
#' locations, and release information.
#'
#' @param participant Participant name filter (partial match, case-insensitive).
#'   Example: \code{"Kissinger"}, \code{"Haldeman"}. \code{NULL} for all
#'   conversations. Default \code{NULL}.
#' @param start_date Filter conversations on or after this date (character,
#'   \code{"YYYY-MM-DD"} format). Example: \code{"1972-01-01"}. Default \code{NULL}.
#' @param end_date Filter conversations on or before this date (character,
#'   \code{"YYYY-MM-DD"} format). Example: \code{"1972-12-31"}. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{conversation_title} (character): Title of the conversation
#'     \item \code{tape_number} (integer): Tape reel number
#'     \item \code{conversation_number} (integer): Conversation number on tape
#'     \item \code{identifier} (character): Unique identifier
#'     \item \code{start_date_time} (character): Start date and time
#'     \item \code{end_date_time} (character): End date and time
#'     \item \code{start_date} (character): Start date
#'     \item \code{start_time} (character): Start time
#'     \item \code{end_date} (character): End date
#'     \item \code{end_time} (character): End time
#'     \item \code{participants} (character): Semicolon-separated participant names
#'     \item \code{description} (character): Conversation description
#'     \item \code{location_code} (character): Recording location code
#'     \item \code{recording_device} (character): Recording device used
#'     \item \code{collection} (character): Collection name
#'     \item \code{digital_access} (character): Digital access availability
#'     \item \code{physical_access} (character): Physical access information
#'   }
#'   Plus additional metadata columns (chron_code, release dates, coordinates, etc.).
#' @examples
#' \dontrun{
#' # All conversations
#' nara_nixon_tapes()
#'
#' # Conversations with Kissinger
#' nara_nixon_tapes(participant = "Kissinger")
#'
#' # Conversations in 1972
#' nara_nixon_tapes(start_date = "1972-01-01", end_date = "1972-12-31")
#' }
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

#' @title List NARA open datasets
#'
#' Returns a hardcoded catalog of available open datasets from the National
#' Archives, including CSV downloads and the Catalog API. Each row shows the
#' dataset name, description, format, URL, and which R function accesses it.
#' No API key required.
#'
#' @return A tibble with 5 rows and columns:
#'   \itemize{
#'     \item \code{name} (character): Short dataset name (e.g. \code{"amendments"}, \code{"nixon_tapes"})
#'     \item \code{description} (character): Human-readable description
#'     \item \code{format} (character): Data format (\code{"CSV"}, \code{"JSON API"}, \code{"HTML"})
#'     \item \code{url} (character): URL to the dataset or API
#'     \item \code{accessor} (character): R function to access this dataset
#'   }
#' @examples
#' \dontrun{
#' # See what datasets are available
#' nara_datasets()
#' }
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

#' Get archives.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nara_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nara_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/archives.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "archives.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# archives.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# archives.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
