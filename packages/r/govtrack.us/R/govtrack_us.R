# govtrack.us.R - Self-contained govtrack.us client



# govtrack-us.R
# Self-contained GovTrack congressional data client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://www.govtrack.us/api/v2


# == Private utilities =========================================================

.ua <- "R/4.4 httr2/1.0"
.gt_base <- "https://www.govtrack.us/api/v2"

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
#' Search and filter bills from the 93rd Congress (1973) to present using
#' the GovTrack API. The archive contains approximately 428,000 bills.
#' Results can be filtered by congress, bill type, status, and sponsor.
#'
#' @param congress Integer or \code{NULL}. Congress number (e.g. 118 for
#'   2023-2024). \code{NULL} (default) returns bills from all congresses.
#' @param bill_type Character or \code{NULL}. Bill type filter:
#'   \code{"senate_bill"}, \code{"house_bill"}, \code{"senate_resolution"},
#'   \code{"house_resolution"}, \code{"senate_joint_resolution"},
#'   \code{"house_joint_resolution"}. \code{NULL} (default) returns all types.
#' @param status Character or \code{NULL}. Current status filter (e.g.
#'   \code{"enacted_signed"}, \code{"introduced"}, \code{"passed_bill"}).
#' @param sponsor Integer or \code{NULL}. GovTrack person ID of the bill sponsor.
#' @param limit Integer. Results per page (default 50).
#' @param offset Integer. Pagination offset (default 0).
#' @param order Character. Sort field. Prefix with \code{"-"} for descending.
#'   Default \code{"-current_status_date"} (newest first).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. GovTrack bill ID.}
#'     \item{bill_type}{Character. Type of bill (e.g. "house_bill").}
#'     \item{number}{Integer. Bill number within its type.}
#'     \item{congress}{Integer. Congress number.}
#'     \item{title}{Character. Bill title (truncated to 200 characters).}
#'     \item{current_status}{Character. Current status code.}
#'     \item{current_status_date}{Date. Date of current status.}
#'     \item{introduced_date}{Date. Date the bill was introduced.}
#'     \item{sponsor_name}{Character. Name of the bill's sponsor.}
#'     \item{link}{Character. GovTrack URL for the bill.}
#'   }
#' @export
#' @family govtrack functions
#' @seealso [gt_members()] for sponsor lookup, [gt_votes()] for roll call votes
#' @examples
#' \dontrun{
#' # Recently enacted bills from the 118th Congress
#' gt_bills(congress = 118, status = "enacted_signed", limit = 10)
#'
#' # All Senate bills
#' gt_bills(bill_type = "senate_bill", limit = 20)
#' }
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
#' Returns current senators and representatives via the GovTrack role
#' endpoint. Can be filtered by chamber, state, and party. Includes
#' term dates and official website URLs.
#'
#' @param role_type Character or \code{NULL}. \code{"senator"} or
#'   \code{"representative"}. \code{NULL} (default) returns both.
#' @param state Character or \code{NULL}. Two-letter state code
#'   (e.g. \code{"CA"}, \code{"NY"}).
#' @param party Character or \code{NULL}. Party name (e.g. \code{"Democrat"},
#'   \code{"Republican"}).
#' @param limit Integer. Results per page (default 100).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{person_id}{Integer. GovTrack person ID.}
#'     \item{name}{Character. Full name with title (e.g. "Sen. John Doe").}
#'     \item{party}{Character. Political party name.}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{district}{Integer. Congressional district (NA for senators).}
#'     \item{role_type}{Character. "senator" or "representative".}
#'     \item{start_date}{Date. Start of current term.}
#'     \item{end_date}{Date. End of current term.}
#'     \item{website}{Character. Official website URL.}
#'   }
#' @export
#' @family govtrack functions
#' @seealso [gt_bills()] to find bills by sponsor,
#'   [gt_committees()] for committee memberships
#' @examples
#' \dontrun{
#' # All current members from California
#' gt_members(state = "CA")
#'
#' # All current Republican senators
#' gt_members(role_type = "senator", party = "Republican")
#' }
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
#' Returns roll call votes from both chambers of Congress. The archive
#' contains approximately 113,000 votes. Results can be filtered by
#' congress number and chamber.
#'
#' @param congress Integer or \code{NULL}. Congress number. \code{NULL}
#'   (default) returns votes from all congresses.
#' @param chamber Character or \code{NULL}. \code{"house"} or \code{"senate"}.
#'   \code{NULL} (default) returns votes from both chambers.
#' @param limit Integer. Results per page (default 50).
#' @param offset Integer. Pagination offset (default 0).
#' @param order Character. Sort field. Prefix with \code{"-"} for descending.
#'   Default \code{"-created"} (newest first).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. GovTrack vote ID.}
#'     \item{congress}{Integer. Congress number.}
#'     \item{session}{Character. Session year.}
#'     \item{chamber}{Character. "House" or "Senate".}
#'     \item{number}{Integer. Roll call vote number.}
#'     \item{question}{Character. The question being voted on.}
#'     \item{result}{Character. Vote result (e.g. "Passed", "Failed").}
#'     \item{created}{POSIXct. Date/time the vote was taken.}
#'     \item{total_plus}{Integer. Number of Yea/Aye votes.}
#'     \item{total_minus}{Integer. Number of Nay/No votes.}
#'     \item{link}{Character. GovTrack URL for the vote.}
#'   }
#' @export
#' @family govtrack functions
#' @seealso [gt_bills()] for the bills being voted on
#' @examples
#' \dontrun{
#' # Recent House votes
#' gt_votes(chamber = "house", limit = 10)
#'
#' # Votes from the 118th Congress
#' gt_votes(congress = 118, limit = 20)
#' }
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
#' Returns all current (and optionally historical) congressional committees
#' from the GovTrack API. Can be filtered by chamber type.
#'
#' @param committee_type Character or \code{NULL}. \code{"senate"},
#'   \code{"house"}, or \code{"joint"}. \code{NULL} (default) returns
#'   committees from all chambers.
#' @param obsolete Logical. Include obsolete/historical committees?
#'   Default \code{FALSE}.
#' @param limit Integer. Results per page (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{Character. Committee code (e.g. "SSAP01").}
#'     \item{name}{Character. Committee name.}
#'     \item{committee_type}{Character. Chamber type label.}
#'     \item{url}{Character. GovTrack URL for the committee.}
#'     \item{obsolete}{Logical. Whether the committee is defunct.}
#'   }
#' @export
#' @family govtrack functions
#' @seealso [gt_members()] for committee membership information
#' @examples
#' \dontrun{
#' # All current Senate committees
#' gt_committees(committee_type = "senate")
#'
#' # Include historical committees
#' gt_committees(obsolete = TRUE)
#' }
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

#' Get govtrack.us client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gt_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gt_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/govtrack.us.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "govtrack.us")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# govtrack.us context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# govtrack.us", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
