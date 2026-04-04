# sba.gov.R - Self-contained SBA (Small Business Administration) data client
#
# Data source: data.sba.gov CKAN v3 Action API
# Provides access to SBA datasets: loan programs (7a, 504, PPP, EIDL),
# disaster loans, size standards, HUBZone designations, and more.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sba_base <- "https://data.sba.gov/api/3/action"

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- CKAN URL builder ----------------------------------------------------------

.sba_url <- function(action, ...) {
  params <- list(...)
  params <- params[!vapply(params, is.null, logical(1))]
  if (length(params) == 0) return(paste0(.sba_base, "/", action))
  query <- paste(names(params), vapply(params, as.character, character(1)),
                 sep = "=", collapse = "&")
  paste0(.sba_base, "/", action, "?", query)
}

.sba_result <- function(url) {
  raw <- .fetch_json(url)
  if (!isTRUE(raw$success)) {
    warning("SBA CKAN API error: ", raw$error$message %||% "unknown")
    return(NULL)
  }
  raw$result
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), title = character(),
  organization = character(), num_resources = integer(),
  metadata_created = as.POSIXct(character()), notes = character()
)

.schema_resources <- tibble(
  id = character(), name = character(), format = character(),
  url = character(), description = character(),
  created = as.POSIXct(character()), size = numeric()
)

.schema_organizations <- tibble(
  id = character(), name = character(), title = character(),
  package_count = integer(), description = character()
)

.schema_tags <- tibble(
  id = character(), name = character()
)

# == Dataset search ============================================================

#' Search SBA open data catalog
#'
#' Searches the U.S. Small Business Administration open data catalog at
#' data.sba.gov (CKAN). Covers loan programs (7a, 504, PPP, EIDL), disaster
#' loans, size standards, HUBZone designations, and more.
#'
#' @param query Character or NULL. Free-text search term
#'   (e.g. \code{"loan"}, \code{"disaster"}, \code{"ppp"}, \code{"size standards"}).
#'   NULL returns all datasets.
#' @param organization Character or NULL. Filter by SBA office slug
#'   (e.g. \code{"office-of-capital-access"}, \code{"advocacy"},
#'   \code{"disaster"}, \code{"foia"}). Use \code{sba_organizations()} to
#'   list valid slugs.
#' @param tag Character or NULL. Filter by tag
#'   (e.g. \code{"sba"}, \code{"covid-19"}, \code{"banking"}). Use
#'   \code{sba_tags()} to list valid tags.
#' @param rows Integer. Number of results per page (default 50, max 1000).
#' @param start Integer. Offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. CKAN dataset UUID.}
#'     \item{name}{Character. Dataset slug (e.g. \code{"7-a-504-foia"}).}
#'     \item{title}{Character. Human-readable dataset title.}
#'     \item{organization}{Character. Publishing SBA office slug.}
#'     \item{num_resources}{Integer. Number of downloadable files.}
#'     \item{metadata_created}{POSIXct. Date the dataset was first published.}
#'     \item{notes}{Character. Description (truncated to 300 chars).}
#'   }
#' @examples
#' sba_search("loan")
#' sba_search(organization = "disaster")
#' @export
sba_search <- function(query = NULL, organization = NULL, tag = NULL,
                       rows = 50, start = 0) {
  fq_parts <- character()
  if (!is.null(organization)) fq_parts <- c(fq_parts, paste0("organization:", organization))
  if (!is.null(tag)) fq_parts <- c(fq_parts, paste0("tags:", tag))
  fq <- if (length(fq_parts) > 0) paste(fq_parts, collapse = " ") else NULL

  url <- .sba_url("package_search",
    q     = if (!is.null(query)) utils::URLencode(query, reserved = TRUE) else NULL,
    fq    = if (!is.null(fq)) utils::URLencode(fq, reserved = TRUE) else NULL,
    rows  = rows,
    start = start
  )

  result <- .sba_result(url)
  if (is.null(result)) return(.schema_datasets)

  datasets <- result$results
  if (length(datasets) == 0) return(.schema_datasets)

  tibble(
    id               = vapply(datasets, function(d) d$id %||% NA_character_, character(1)),
    name             = vapply(datasets, function(d) d$name %||% NA_character_, character(1)),
    title            = vapply(datasets, function(d) d$title %||% NA_character_, character(1)),
    organization     = vapply(datasets, function(d) {
      d$organization$name %||% NA_character_
    }, character(1)),
    num_resources    = vapply(datasets, function(d) as.integer(d$num_resources %||% 0L), integer(1)),
    metadata_created = as.POSIXct(vapply(datasets, function(d) {
      d$metadata_created %||% NA_character_
    }, character(1))),
    notes            = vapply(datasets, function(d) {
      n <- d$notes %||% ""
      if (nchar(n) > 300) paste0(substr(n, 1, 300), "...") else n
    }, character(1))
  )
}

# == List all datasets =========================================================

#' List all SBA dataset names
#'
#' Returns a character vector of every dataset slug on data.sba.gov.
#' Use a slug with \code{sba_dataset()} or \code{sba_resources()} to
#' get details or downloadable files.
#'
#' @return Character vector of dataset slugs (e.g. \code{"7-a-504-foia"},
#'   \code{"ppp-foia"}, \code{"disaster-loan-data"}).
#' @examples
#' sba_list()
#' @export
sba_list <- function() {
  result <- .sba_result(.sba_url("package_list"))
  if (is.null(result)) return(character())
  as.character(unlist(result))
}

# == Dataset details ===========================================================

#' Get full metadata for an SBA dataset
#'
#' Returns a single-row tibble of metadata for one dataset on data.sba.gov.
#'
#' @param id Character. Dataset slug or CKAN UUID
#'   (e.g. \code{"7-a-504-foia"}, \code{"ppp-foia"},
#'   \code{"disaster-loan-data"}). Obtain from \code{sba_list()} or
#'   \code{sba_search()}.
#' @return A tibble with one row and columns: id, name, title,
#'   organization, num_resources, metadata_created (POSIXct), notes.
#' @examples
#' sba_dataset("ppp-foia")
#' @export
sba_dataset <- function(id) {
  result <- .sba_result(.sba_url("package_show", id = id))
  if (is.null(result)) return(.schema_datasets)

  tibble(
    id               = result$id %||% NA_character_,
    name             = result$name %||% NA_character_,
    title            = result$title %||% NA_character_,
    organization     = result$organization$name %||% NA_character_,
    num_resources    = as.integer(result$num_resources %||% 0L),
    metadata_created = as.POSIXct(result$metadata_created %||% NA_character_),
    notes            = result$notes %||% NA_character_
  )
}

# == Dataset resources =========================================================

#' Get downloadable resources for an SBA dataset
#'
#' Lists all downloadable files (CSV, XLSX, JSON, etc.) within an SBA
#' dataset. Use the \code{url} column with \code{sba_download()} to
#' fetch CSV files directly into R.
#'
#' @param id Character. Dataset slug or CKAN UUID
#'   (e.g. \code{"7-a-504-foia"}, \code{"ppp-foia"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. CKAN resource UUID.}
#'     \item{name}{Character. Resource display name.}
#'     \item{format}{Character. File format in uppercase (e.g. \code{"CSV"}, \code{"XLSX"}, \code{"JSON"}).}
#'     \item{url}{Character. Direct download URL.}
#'     \item{description}{Character. Resource description (truncated to 200 chars).}
#'     \item{created}{POSIXct. Date the resource was created.}
#'     \item{size}{Numeric. File size in bytes (may be NA).}
#'   }
#' @examples
#' sba_resources("ppp-foia")
#' @export
sba_resources <- function(id) {
  result <- .sba_result(.sba_url("package_show", id = id))
  if (is.null(result)) return(.schema_resources)

  res <- result$resources
  if (length(res) == 0) return(.schema_resources)

  tibble(
    id          = vapply(res, function(r) r$id %||% NA_character_, character(1)),
    name        = vapply(res, function(r) r$name %||% NA_character_, character(1)),
    format      = vapply(res, function(r) toupper(r$format %||% ""), character(1)),
    url         = vapply(res, function(r) r$url %||% NA_character_, character(1)),
    description = vapply(res, function(r) {
      d <- r$description %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1)),
    created     = as.POSIXct(vapply(res, function(r) r$created %||% NA_character_, character(1))),
    size        = vapply(res, function(r) as.numeric(r$size %||% NA_real_), numeric(1))
  )
}

# == Organizations =============================================================

#' List SBA organizations (offices)
#'
#' Returns all SBA organizational units that publish data on data.sba.gov.
#' Useful for filtering \code{sba_search()} by \code{organization}.
#'
#' @param all_fields Logical. If \code{TRUE} (default), returns a tibble
#'   with full details. If \code{FALSE}, returns a character vector of
#'   organization slugs only.
#' @return If \code{all_fields = TRUE}, a tibble with columns:
#'   \describe{
#'     \item{id}{Character. CKAN organization UUID.}
#'     \item{name}{Character. Organization slug (e.g. \code{"advocacy"}, \code{"disaster"}).}
#'     \item{title}{Character. Full organization name.}
#'     \item{package_count}{Integer. Number of datasets published.}
#'     \item{description}{Character. Organization description (truncated to 200 chars).}
#'   }
#'   If \code{all_fields = FALSE}, a character vector of organization slugs.
#' @examples
#' sba_organizations()
#' sba_organizations(all_fields = FALSE)
#' @export
sba_organizations <- function(all_fields = TRUE) {
  if (!all_fields) {
    result <- .sba_result(.sba_url("organization_list"))
    if (is.null(result)) return(character())
    return(as.character(unlist(result)))
  }

  result <- .sba_result(.sba_url("organization_list", all_fields = "true"))
  if (is.null(result)) return(.schema_organizations)

  orgs <- result
  if (length(orgs) == 0) return(.schema_organizations)

  tibble(
    id            = vapply(orgs, function(o) o$id %||% NA_character_, character(1)),
    name          = vapply(orgs, function(o) o$name %||% NA_character_, character(1)),
    title         = vapply(orgs, function(o) o$title %||% NA_character_, character(1)),
    package_count = vapply(orgs, function(o) as.integer(o$package_count %||% 0L), integer(1)),
    description   = vapply(orgs, function(o) {
      d <- o$description %||% ""
      if (nchar(d) > 200) paste0(substr(d, 1, 200), "...") else d
    }, character(1))
  )
}

# == Tags ======================================================================

#' List SBA dataset tags
#'
#' Returns all tags used to categorize datasets on data.sba.gov.
#' Useful for filtering \code{sba_search()} by \code{tag}.
#'
#' @return Character vector of tag names (e.g. \code{"Advocacy"},
#'   \code{"banking"}, \code{"covid-19"}).
#' @examples
#' sba_tags()
#' @export
sba_tags <- function() {
  result <- .sba_result(.sba_url("tag_list"))
  if (is.null(result)) return(character())
  as.character(unlist(result))
}

# == Download resource as tibble ===============================================

#' Download and parse an SBA data resource
#'
#' Downloads a CSV resource from data.sba.gov and returns it as a tibble.
#' For XLSX or other formats, downloads the file and returns the local
#' file path.
#'
#' @param url Character. Full URL to the resource. Obtain from the
#'   \code{url} column of \code{sba_resources()}.
#' @param ... Additional arguments passed to \code{utils::read.csv()}
#'   (e.g. \code{nrows}, \code{colClasses}).
#' @return A tibble of parsed CSV data. For non-CSV formats, an invisible
#'   character string with the path to the downloaded temporary file.
#' @examples
#' res <- sba_resources("ppp-foia")
#' # sba_download(res$url[1])
#' @export
sba_download <- function(url, ...) {
  ext <- if (grepl("\\.xlsx?$", url, ignore.case = TRUE)) ".xlsx"
         else if (grepl("\\.csv$", url, ignore.case = TRUE)) ".csv"
         else ".dat"

  tmp <- .fetch(url, ext = ext)

  if (ext == ".csv") {
    df <- utils::read.csv(tmp, stringsAsFactors = FALSE, ...)
    return(tibble::as_tibble(df))
  }

  message("File downloaded to: ", tmp)
  invisible(tmp)
}

# == Convenience: Loan programs ================================================

#' List SBA loan program datasets
#'
#' Convenience wrapper around \code{sba_search("loan")} that returns
#' datasets related to SBA loan programs (7a, 504, PPP, EIDL, disaster
#' loans, microloans).
#'
#' @return A tibble with the same columns as \code{sba_search()}: id,
#'   name, title, organization, num_resources, metadata_created, notes.
#' @examples
#' sba_loan_programs()
#' @export
sba_loan_programs <- function() {
  sba_search(query = "loan", rows = 100)
}

# == Convenience: Size standards ===============================================

#' Get SBA small business size standards resources
#'
#' Returns downloadable resources for the current small business size
#' standards, which define revenue and employee thresholds for qualifying
#' as a "small business" by NAICS code.
#'
#' @return A tibble with the same columns as \code{sba_resources()}: id,
#'   name, format, url, description, created, size.
#' @examples
#' sba_size_standards()
#' @export
sba_size_standards <- function() {
  sba_resources("small-business-size-standards")
}

# == Convenience: PPP data =====================================================

#' Get SBA Paycheck Protection Program (PPP) FOIA resources
#'
#' Returns downloadable resources for PPP loan data released under FOIA.
#' Includes loans over $150k (with business names) and under $150k.
#'
#' @return A tibble with the same columns as \code{sba_resources()}: id,
#'   name, format, url, description, created, size.
#' @examples
#' sba_ppp()
#' @export
sba_ppp <- function() {
  sba_resources("ppp-foia")
}

# == Convenience: Disaster loans ===============================================

#' Get SBA disaster loan data resources
#'
#' Returns downloadable resources for historical SBA disaster loan data,
#' segmented by fiscal year (FY2000-FY2022).
#'
#' @return A tibble with the same columns as \code{sba_resources()}: id,
#'   name, format, url, description, created, size.
#' @examples
#' sba_disaster_loans()
#' @export
sba_disaster_loans <- function() {
  sba_resources("disaster-loan-data")
}

# == Context ===================================================================

#' Get sba.gov client context for LLM use
#'
#' Reads this source file and prints roxygen blocks and function signatures
#' for every public function, providing a compact reference suitable for
#' pasting into an LLM prompt.
#'
#' @return Character string of formatted documentation (printed to console
#'   and returned invisibly).
#' @examples
#' sba_context()
#' @export
sba_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(sba_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/sba.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "sba.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# sba.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# sba.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
