# parliament.uk.R - Self-contained parliament.uk client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# parliament-uk.R
# Self-contained UK Parliament Members API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.parl_base <- "https://members-api.parliament.uk/api"
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
#' Search for current or historical members of the UK Parliament (MPs and
#' Lords). Supports filtering by name, house, and current-member status.
#' Results are paginated via \code{take} and \code{skip}.
#'
#' @param name Character. Name to search for (e.g., \code{"Smith"},
#'   \code{"Keir Starmer"}). Default NULL returns all members.
#' @param house Integer. House filter: \code{1} = House of Commons,
#'   \code{2} = House of Lords, \code{NULL} = both (default).
#' @param is_current Logical. \code{TRUE} for current members only (default),
#'   \code{FALSE} to include historical members.
#' @param take Integer. Number of results per page (default 20, max 100).
#' @param skip Integer. Number of results to skip for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{integer -- Parliament member ID}
#'     \item{name}{character -- display name}
#'     \item{party}{character -- political party name}
#'     \item{constituency}{character -- constituency or peerage type}
#'     \item{house}{character -- \code{"Commons"} or \code{"Lords"}}
#'     \item{gender}{character -- \code{"M"} or \code{"F"}}
#'     \item{start_date}{Date -- date membership began}
#'     \item{status}{character -- membership status description}
#'   }
#' @export
#' @examples
#' \dontrun{
#' parl_members(name = "Smith", take = 5)
#' parl_members(house = 1, take = 10)
#' }
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
#' Fetches detailed information for a single Parliament member, including
#' their full ceremonial title and current membership status.
#'
#' @param id Integer. Parliament member ID (e.g., \code{4514} for Keir Starmer,
#'   \code{172} for Boris Johnson). Discover IDs via \code{parl_members()}.
#' @return A tibble (one row) with columns:
#'   \describe{
#'     \item{id}{integer -- Parliament member ID}
#'     \item{name}{character -- display name}
#'     \item{full_title}{character -- full ceremonial title (e.g., \code{"Rt Hon Sir Keir Starmer KCB KC MP"})}
#'     \item{party}{character -- political party name}
#'     \item{constituency}{character -- constituency or peerage type}
#'     \item{house}{character -- \code{"Commons"} or \code{"Lords"}}
#'     \item{gender}{character -- \code{"M"} or \code{"F"}}
#'     \item{start_date}{Date -- date membership began}
#'     \item{status}{character -- membership status description}
#'   }
#' @export
#' @examples
#' \dontrun{
#' parl_member(4514)
#' }
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
# -- null coalesce operator ---
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

# == Context ===================================================================

#' Get parliament.uk client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
parl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(parl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/parliament.uk.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "parliament.uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# parliament.uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# parliament.uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
