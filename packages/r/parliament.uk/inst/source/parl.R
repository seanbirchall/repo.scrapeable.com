# parliament-uk.R
# Self-contained UK Parliament Members API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.parl_base <- "https://members-api.parliament.uk/api"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_members <- tibble(
  id = integer(), name = character(), party = character(),
  constituency = character(), house = character(), gender = character(),
  start_date = as.Date(character()), status = character()
)

.schema_member <- tibble(
  id = integer(), name = character(), full_title = character(),
  party = character(), constituency = character(), house = character(),
  gender = character(), start_date = as.Date(character()),
  status = character()
)

# == Public functions ==========================================================

#' Search UK Parliament members
#'
#' Returns members of the UK Parliament matching search criteria.
#'
#' @param name Optional name to search for
#' @param house House filter: 1 = Commons, 2 = Lords, NULL = both (default)
#' @param is_current Logical: TRUE for current members only (default TRUE)
#' @param take Number of results per page (default 20, max 100)
#' @param skip Number of results to skip for pagination (default 0)
#' @return tibble: id, name, party, constituency, house, gender, start_date, status
parl_members <- function(name = NULL, house = NULL, is_current = TRUE,
                         take = 20, skip = 0) {
  params <- list(
    Name = name, House = house, IsCurrentMember = tolower(as.character(is_current)),
    take = take, skip = skip
  )
  params <- params[!vapply(params, is.null, logical(1))]
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/Members/Search?%s", .parl_base, query)

  raw <- .fetch_json(url)
  items <- raw$items
  if (is.null(items) || length(items) == 0) return(.schema_members)

  rows <- lapply(items, function(item) {
    v <- item$value
    membership <- v$latestHouseMembership
    tibble(
      id = as.integer(v$id),
      name = as.character(v$nameDisplayAs %||% NA),
      party = as.character(v$latestParty$name %||% NA),
      constituency = as.character(membership$membershipFrom %||% NA),
      house = if (identical(membership$house, 1L)) "Commons" else if (identical(membership$house, 2L)) "Lords" else as.character(membership$house %||% NA),
      gender = as.character(v$gender %||% NA),
      start_date = tryCatch(as.Date(sub("T.*", "", membership$membershipStartDate)), error = function(e) NA_real_),
      status = as.character(membership$membershipStatus$statusDescription %||% NA)
    )
  })
  bind_rows(rows)
}

#' Get a specific UK Parliament member by ID
#'
#' @param id Member ID (integer)
#' @return tibble: id, name, full_title, party, constituency, house,
#'   gender, start_date, status
parl_member <- function(id) {
  url <- sprintf("%s/Members/%d", .parl_base, as.integer(id))
  raw <- .fetch_json(url)
  v <- raw$value
  if (is.null(v)) return(.schema_member)

  membership <- v$latestHouseMembership
  tibble(
    id = as.integer(v$id),
    name = as.character(v$nameDisplayAs %||% NA),
    full_title = as.character(v$nameFullTitle %||% NA),
    party = as.character(v$latestParty$name %||% NA),
    constituency = as.character(membership$membershipFrom %||% NA),
    house = if (identical(membership$house, 1L)) "Commons" else if (identical(membership$house, 2L)) "Lords" else as.character(membership$house %||% NA),
    gender = as.character(v$gender %||% NA),
    start_date = tryCatch(as.Date(sub("T.*", "", membership$membershipStartDate)), error = function(e) NA_real_),
    status = as.character(membership$membershipStatus$statusDescription %||% NA)
  )
}

#' UK Parliament API context for LLM use
#'
#' Prints package overview, auth info, and function signatures.
#' @return Invisible string with context info
parl_context <- function() {
  header <- c(
    "# parliament.uk - UK Parliament Members API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: not documented",
    "#",
    "# House codes: 1 = House of Commons, 2 = House of Lords",
    "# Typical flow: parl_members(house=1) to browse, parl_member(id) for detail"
  )
  .build_context("parliament.uk", header_lines = header)
}

# -- null coalesce operator ---
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
