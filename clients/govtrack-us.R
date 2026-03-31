# govtrack-us.R
# Self-contained GovTrack congressional data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://www.govtrack.us/api/v2

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "R/4.4 httr2/1.0"
.gt_base <- "https://www.govtrack.us/api/v2"

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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_user_agent(.ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -- URL builder ---------------------------------------------------------------

.gt_url <- function(endpoint, ...) {
  params <- list(...)
  params <- params[!vapply(params, is.null, logical(1))]
  if (length(params) == 0) return(paste0(.gt_base, "/", endpoint))
  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  paste0(.gt_base, "/", endpoint, "?", query)
}

# == Schemas ===================================================================

.schema_bills <- tibble(
  id = integer(), bill_type = character(), number = integer(),
  congress = integer(), title = character(),
  current_status = character(), current_status_date = as.Date(character()),
  introduced_date = as.Date(character()), sponsor_name = character(),
  link = character()
)

.schema_members <- tibble(
  person_id = integer(), name = character(), party = character(),
  state = character(), district = integer(), role_type = character(),
  start_date = as.Date(character()), end_date = as.Date(character()),
  website = character()
)

.schema_votes <- tibble(
  id = integer(), congress = integer(), session = character(),
  chamber = character(), number = integer(),
  question = character(), result = character(),
  created = as.POSIXct(character()),
  total_plus = integer(), total_minus = integer(),
  link = character()
)

.schema_committees <- tibble(
  code = character(), name = character(),
  committee_type = character(), url = character(), obsolete = logical()
)

# == Bills =====================================================================

#' Search congressional bills
#'
#' Search and filter bills from the 93rd Congress (1973) to present.
#' Total archive: ~428,000 bills.
#'
#' @param congress Congress number (e.g. 118 for 2023-2024). Default: current.
#' @param bill_type Bill type: "senate_bill", "house_bill",
#'   "senate_resolution", "house_resolution", "senate_joint_resolution",
#'   "house_joint_resolution". Default: all types.
#' @param status Current status filter (e.g. "enacted_signed", "introduced")
#' @param sponsor Person ID of bill sponsor
#' @param limit Results per page (default 50)
#' @param offset Pagination offset (default 0)
#' @param order Sort field (default "-current_status_date" for newest first)
#' @return tibble: id, bill_type, number, congress, title, current_status,
#'   current_status_date (Date), introduced_date (Date), sponsor_name, link
gt_bills <- function(congress = NULL, bill_type = NULL, status = NULL,
                     sponsor = NULL, limit = 50, offset = 0,
                     order = "-current_status_date") {
  url <- .gt_url("bill", congress = congress, bill_type = bill_type,
                 current_status = status, sponsor = sponsor,
                 limit = limit, offset = offset, order_by = order)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("GovTrack API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_bills)

  objects <- raw$objects
  if (length(objects) == 0) return(.schema_bills)

  tibble(
    id                  = vapply(objects, function(b) as.integer(b$id %||% NA_integer_), integer(1)),
    bill_type           = vapply(objects, function(b) b$bill_type %||% NA_character_, character(1)),
    number              = vapply(objects, function(b) as.integer(b$number %||% NA_integer_), integer(1)),
    congress            = vapply(objects, function(b) as.integer(b$congress %||% NA_integer_), integer(1)),
    title               = vapply(objects, function(b) {
      t <- b$title_without_number %||% b$title %||% NA_character_
      if (nchar(t) > 200) paste0(substr(t, 1, 200), "...") else t
    }, character(1)),
    current_status      = vapply(objects, function(b) b$current_status %||% NA_character_, character(1)),
    current_status_date = as.Date(vapply(objects, function(b) b$current_status_date %||% NA_character_, character(1))),
    introduced_date     = as.Date(vapply(objects, function(b) b$introduced_date %||% NA_character_, character(1))),
    sponsor_name        = vapply(objects, function(b) {
      s <- b$sponsor
      if (is.null(s)) NA_character_ else s$name %||% NA_character_
    }, character(1)),
    link                = vapply(objects, function(b) b$link %||% NA_character_, character(1))
  )
}


# == Members ===================================================================

#' List current members of Congress
#'
#' Returns current senators and representatives via the role endpoint.
#'
#' @param role_type "senator" or "representative" (default: both)
#' @param state Two-letter state code (e.g. "CA", "NY")
#' @param party Party name (e.g. "Democrat", "Republican")
#' @param limit Results per page (default 100)
#' @param offset Pagination offset (default 0)
#' @return tibble: person_id, name, party, state, district, role_type,
#'   start_date (Date), end_date (Date), website
gt_members <- function(role_type = NULL, state = NULL, party = NULL,
                       limit = 100, offset = 0) {
  url <- .gt_url("role", current = "true", role_type = role_type,
                 state = state, party = party,
                 limit = limit, offset = offset)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("GovTrack API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_members)

  objects <- raw$objects
  if (length(objects) == 0) return(.schema_members)

  tibble(
    person_id  = vapply(objects, function(r) as.integer(r$person$id %||% NA_integer_), integer(1)),
    name       = vapply(objects, function(r) r$person$name %||% NA_character_, character(1)),
    party      = vapply(objects, function(r) r$party %||% NA_character_, character(1)),
    state      = vapply(objects, function(r) r$state %||% NA_character_, character(1)),
    district   = vapply(objects, function(r) as.integer(r$district %||% NA_integer_), integer(1)),
    role_type  = vapply(objects, function(r) r$role_type %||% NA_character_, character(1)),
    start_date = as.Date(vapply(objects, function(r) r$startdate %||% NA_character_, character(1))),
    end_date   = as.Date(vapply(objects, function(r) r$enddate %||% NA_character_, character(1))),
    website    = vapply(objects, function(r) r$website %||% NA_character_, character(1))
  )
}


# == Votes =====================================================================

#' Search congressional votes
#'
#' Returns roll call votes from both chambers.
#' Total archive: ~113,000 votes.
#'
#' @param congress Congress number
#' @param chamber "house" or "senate"
#' @param limit Results per page (default 50)
#' @param offset Pagination offset (default 0)
#' @param order Sort field (default "-created" for newest first)
#' @return tibble: id, congress, session, chamber, number, question,
#'   result, created (POSIXct), total_plus, total_minus, link
gt_votes <- function(congress = NULL, chamber = NULL,
                     limit = 50, offset = 0, order = "-created") {
  url <- .gt_url("vote", congress = congress, chamber = chamber,
                 limit = limit, offset = offset, order_by = order)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("GovTrack API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_votes)

  objects <- raw$objects
  if (length(objects) == 0) return(.schema_votes)

  tibble(
    id          = vapply(objects, function(v) as.integer(v$id %||% NA_integer_), integer(1)),
    congress    = vapply(objects, function(v) as.integer(v$congress %||% NA_integer_), integer(1)),
    session     = vapply(objects, function(v) v$session %||% NA_character_, character(1)),
    chamber     = vapply(objects, function(v) v$chamber_label %||% v$chamber %||% NA_character_, character(1)),
    number      = vapply(objects, function(v) as.integer(v$number %||% NA_integer_), integer(1)),
    question    = vapply(objects, function(v) v$question %||% NA_character_, character(1)),
    result      = vapply(objects, function(v) v$result %||% NA_character_, character(1)),
    created     = as.POSIXct(vapply(objects, function(v) v$created %||% NA_character_, character(1))),
    total_plus  = vapply(objects, function(v) as.integer(v$total_plus %||% NA_integer_), integer(1)),
    total_minus = vapply(objects, function(v) as.integer(v$total_minus %||% NA_integer_), integer(1)),
    link        = vapply(objects, function(v) v$link %||% NA_character_, character(1))
  )
}


# == Committees ================================================================

#' List congressional committees
#'
#' Returns all current and historical committees.
#'
#' @param committee_type "senate", "house", or "joint" (default: all)
#' @param obsolete Include obsolete committees? (default FALSE)
#' @param limit Results per page (default 100)
#' @return tibble: code, name, committee_type, url, obsolete
gt_committees <- function(committee_type = NULL, obsolete = FALSE,
                          limit = 100) {
  url <- .gt_url("committee", committee_type = committee_type,
                 obsolete = if (obsolete) NULL else "false",
                 limit = limit)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("GovTrack API error: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_committees)

  objects <- raw$objects
  if (length(objects) == 0) return(.schema_committees)

  tibble(
    code           = vapply(objects, function(c) c$code %||% NA_character_, character(1)),
    name           = vapply(objects, function(c) c$name %||% NA_character_, character(1)),
    committee_type = vapply(objects, function(c) c$committee_type_label %||% c$committee_type %||% NA_character_, character(1)),
    url            = vapply(objects, function(c) c$url %||% NA_character_, character(1)),
    obsolete       = vapply(objects, function(c) as.logical(c$obsolete %||% FALSE), logical(1))
  )
}


# == Context ===================================================================

#' Generate LLM-friendly context for the GovTrack package
#'
#' @return Character string (invisibly), also printed
gt_context <- function() {
  .build_context("govtrack.us", header_lines = c(
    "# govtrack.us - Congressional Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# All functions return tibbles with typed columns.",
    "#",
    "# 428,000+ bills, 113,000+ votes, 540 current members",
    "# Coverage: 93rd Congress (1973) to present",
    "#",
    "# Pagination: limit/offset based",
    "# Sort: prefix with - for descending (e.g. '-created')"
  ))
}
