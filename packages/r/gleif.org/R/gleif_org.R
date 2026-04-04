# gleif.org.R - Self-contained gleif.org client



# gleif-org.R
# Self-contained GLEIF LEI (Legal Entity Identifier) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.lei_base <- "https://api.gleif.org/api/v1"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# == Schemas ===================================================================

.schema_lei <- tibble(
  lei = character(), legal_name = character(), status = character(),
  jurisdiction = character(), country = character(), city = character(),
  address = character(), category = character(),
  registration_date = as.Date(character()), last_update = as.Date(character())
)

# == Helper: parse lei-record ==================================================

.parse_lei_record <- function(rec) {
  a <- rec$attributes
  ent <- a$entity %||% list()
  reg <- a$registration %||% list()
  addr <- ent$legalAddress %||% list()
  tibble(
    lei               = as.character(a$lei %||% NA),
    legal_name        = as.character((ent$legalName %||% list())$name %||% NA),
    status            = as.character(ent$status %||% NA),
    jurisdiction      = as.character(ent$jurisdiction %||% NA),
    country           = as.character(addr$country %||% NA),
    city              = as.character(addr$city %||% NA),
    address           = as.character(paste((addr$addressLines %||% list()), collapse = ", ")),
    category          = as.character(ent$category %||% NA),
    registration_date = tryCatch(as.Date(reg$initialRegistrationDate %||% NA), error = function(e) NA),
    last_update       = tryCatch(as.Date(reg$lastUpdateDate %||% NA), error = function(e) NA)
  )
}

# == Search ====================================================================


#' Search GLEIF LEI records by legal entity name
#'
#' Search the Global LEI Foundation (GLEIF) database for Legal Entity
#' Identifiers by entity name. The LEI is a 20-character alphanumeric
#' code used to uniquely identify legal entities participating in
#' financial transactions worldwide.
#'
#' @param name Character. Legal name to search (e.g. \code{"Google"},
#'   \code{"Apple Inc"}). Partial matches are supported.
#' @param size Integer. Results per page (default 20, maximum 200).
#' @param page Integer. Page number for pagination (default 1).
#' @return A tibble with columns:
#'   \describe{
#'     \item{lei}{Character. The 20-character LEI code.}
#'     \item{legal_name}{Character. Official registered legal name.}
#'     \item{status}{Character. Entity status (e.g. "ACTIVE", "RETIRED").}
#'     \item{jurisdiction}{Character. Registration jurisdiction code.}
#'     \item{country}{Character. ISO 3166-1 alpha-2 country code.}
#'     \item{city}{Character. City of legal address.}
#'     \item{address}{Character. Street address lines.}
#'     \item{category}{Character. Entity category (e.g. "GENERAL").}
#'     \item{registration_date}{Date. LEI initial registration date.}
#'     \item{last_update}{Date. Date of last LEI record update.}
#'   }
#' @export
#' @family lei functions
#' @seealso [lei_record()] for a single LEI, [lei_by_country()] to browse by country
#' @examples
#' \dontrun{
#' lei_search("Google")
#' lei_search("Apple Inc", size = 5)
#' }
lei_search <- function(name, size = 20, page = 1) {
  url <- sprintf(
    "%s/lei-records?filter%%5Bentity.legalName%%5D=%s&page%%5Bsize%%5D=%d&page%%5Bnumber%%5D=%d",
    .lei_base, utils::URLencode(name), as.integer(size), as.integer(page)
  )
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_lei)
  bind_rows(lapply(d, .parse_lei_record))
}

#' Fetch a single LEI record by LEI code
#'
#' Retrieve the full registration record for a known LEI. Returns entity
#' name, address, jurisdiction, status, and registration dates.
#'
#' @param lei Character. The 20-character LEI code
#'   (e.g. \code{"5493006MHB84DD0ZWV18"} for Alphabet Inc).
#' @return A single-row tibble with the same columns as \code{\link{lei_search}}:
#'   lei, legal_name, status, jurisdiction, country, city, address, category,
#'   registration_date, last_update.
#' @export
#' @family lei functions
#' @seealso [lei_search()] to find LEIs by name,
#'   [lei_relationships()] for parent/child hierarchy
#' @examples
#' \dontrun{
#' lei_record("5493006MHB84DD0ZWV18")
#' }
lei_record <- function(lei) {
  url <- paste0(.lei_base, "/lei-records/", lei)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d)) return(.schema_lei)
  .parse_lei_record(d)
}

#' Get parent/child relationships for a LEI entity
#'
#' Retrieve the corporate ownership hierarchy for a given LEI entity.
#' Queries both the direct parent and ultimate parent relationships
#' from the GLEIF relationship records.
#'
#' @param lei Character. The 20-character LEI code to look up.
#' @return A tibble with columns:
#'   \describe{
#'     \item{relationship_type}{Character. \code{"direct_parent"} or \code{"ultimate_parent"}.}
#'     \item{start_lei}{Character. LEI of the child entity.}
#'     \item{start_name}{Character. Name of the child entity (may be NA).}
#'     \item{end_lei}{Character. LEI of the parent entity.}
#'     \item{end_name}{Character. Name of the parent entity (may be NA).}
#'     \item{status}{Character. Relationship registration status.}
#'   }
#' @export
#' @family lei functions
#' @seealso [lei_record()] to look up the parent/child entities by LEI
#' @examples
#' \dontrun{
#' # Ownership hierarchy for Google LLC
#' lei_relationships("7ZW8QJWVPR4P1J1KQY45")
#' }
lei_relationships <- function(lei) {
  schema <- tibble(relationship_type = character(), start_lei = character(),
                   start_name = character(), end_lei = character(),
                   end_name = character(), status = character())

  url <- sprintf("%s/lei-records/%s/direct-parent-relationship", .lei_base, lei)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)

  rows <- list()
  if (!is.null(raw) && !is.null(raw$data)) {
    d <- raw$data
    a <- d$attributes %||% list()
    rel <- a$relationship %||% list()
    rows[[1]] <- tibble(
      relationship_type = "direct_parent",
      start_lei = as.character(rel$startNode$id %||% lei),
      start_name = NA_character_,
      end_lei = as.character(rel$endNode$id %||% NA_character_),
      end_name = NA_character_,
      status = as.character(a$registration$status %||% NA_character_)
    )
  }

  # Also try ultimate parent
  url2 <- sprintf("%s/lei-records/%s/ultimate-parent-relationship", .lei_base, lei)
  raw2 <- tryCatch(.fetch_json(url2), error = function(e) NULL)
  if (!is.null(raw2) && !is.null(raw2$data)) {
    d <- raw2$data
    a <- d$attributes %||% list()
    rel <- a$relationship %||% list()
    rows[[length(rows) + 1]] <- tibble(
      relationship_type = "ultimate_parent",
      start_lei = as.character(rel$startNode$id %||% lei),
      start_name = NA_character_,
      end_lei = as.character(rel$endNode$id %||% NA_character_),
      end_name = NA_character_,
      status = as.character(a$registration$status %||% NA_character_)
    )
  }

  if (length(rows) == 0) return(schema)
  bind_rows(rows)
}

#' Search LEI records by country
#'
#' Browse registered legal entities by country of legal address. Returns
#' the same fields as \code{\link{lei_search}} filtered to a specific
#' jurisdiction.
#'
#' @param country Character. ISO 3166-1 alpha-2 country code
#'   (e.g. \code{"US"}, \code{"GB"}, \code{"DE"}).
#' @param size Integer. Results per page (default 20, maximum 200).
#' @return A tibble with the same columns as \code{\link{lei_search}}: lei,
#'   legal_name, status, jurisdiction, country, city, address, category,
#'   registration_date, last_update.
#' @export
#' @family lei functions
#' @seealso [lei_search()] to search by name
#' @examples
#' \dontrun{
#' lei_by_country("DE", size = 10)
#' lei_by_country("GB")
#' }
lei_by_country <- function(country, size = 20) {
  url <- sprintf(
    "%s/lei-records?filter%%5Bentity.legalAddress.country%%5D=%s&page%%5Bsize%%5D=%d",
    .lei_base, toupper(country), as.integer(size)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_lei)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_lei)
  bind_rows(lapply(d, .parse_lei_record))
}

# == Context ===================================================================

#' Get gleif.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
lei_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(lei_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gleif.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gleif.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gleif.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gleif.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
