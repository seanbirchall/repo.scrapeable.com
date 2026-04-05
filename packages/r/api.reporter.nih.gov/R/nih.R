# api.reporter.nih.gov.R - Self-contained api.reporter.nih.gov client



# api-reporter-nih-gov.R
# Self-contained NIH RePORTER grants search client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: https://api.reporter.nih.gov/v2


.ua <- "support@scrapeable.com"
.nih_base <- "https://api.reporter.nih.gov/v2"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |> httr2::req_headers(`User-Agent` = .ua) |> httr2::req_perform(path = tmp)
  tmp
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.schema_projects <- tibble(
  project_num = character(), title = character(), pi_name = character(),
  org_name = character(), fiscal_year = integer(),
  award_amount = numeric(), agency = character()
)

.schema_pubs <- tibble(
  pmid = character(), title = character(), journal = character(),
  pub_year = integer(), project_num = character()
)


#' Search NIH-funded research projects
#'
#' Queries the NIH RePORTER v2 projects endpoint with full-text and structured
#' filters. Use this as the primary discovery function to find NIH grants by
#' topic, investigator, organization, or funding institute. Results can be
#' paginated with \code{limit} and \code{offset}. Combine with
#' \code{\link{nih_project_detail}} for abstracts and date ranges, or
#' \code{\link{nih_publications}} to find papers linked to a project number.
#'
#' @param text Character string for full-text search across project titles and
#'   terms. Example: \code{"cancer immunotherapy"}, \code{"CRISPR gene editing"}.
#' @param pi_names Character vector of principal investigator last names.
#'   Example: \code{c("Smith", "Jones")}.
#' @param org_names Character vector of organization/institution names.
#'   Example: \code{c("JOHNS HOPKINS UNIVERSITY")}.
#' @param fiscal_years Integer vector of fiscal years to filter.
#'   Example: \code{c(2022L, 2023L)}.
#' @param agencies Character vector of NIH institute/center abbreviation codes.
#'   Common values: \code{"NCI"} (cancer), \code{"NIAID"} (allergy/infectious
#'   disease), \code{"NHLBI"} (heart/lung/blood), \code{"NIGMS"} (general
#'   medical sciences), \code{"NINDS"} (neurological disorders).
#' @param limit Integer. Maximum number of results to return (default 50,
#'   API max 500).
#' @param offset Integer. Number of results to skip for pagination (default 0).
#'
#' @return A tibble with one row per project and the following columns:
#' \describe{
#'   \item{project_num}{Character. NIH project number (e.g. \code{"5P30CA015704-51"}).}
#'   \item{title}{Character. Project title.}
#'   \item{pi_name}{Character. Principal investigator name(s), semicolon-separated if multiple.}
#'   \item{org_name}{Character. Awardee organization name.}
#'   \item{fiscal_year}{Integer. Fiscal year of the award.}
#'   \item{award_amount}{Numeric. Dollar amount of the award.}
#'   \item{agency}{Character. Funding NIH institute abbreviation (e.g. \code{"NCI"}).}
#' }
#'
#' @examples
#' \dontrun{
#' # Search for cancer-related projects
#' nih_projects(text = "cancer immunotherapy", limit = 10)
#'
#' # Search by PI name and fiscal year
#' nih_projects(pi_names = "Collins", fiscal_years = 2023)
#'
#' # Filter by funding institute
#' nih_projects(text = "Alzheimer", agencies = c("NIA", "NINDS"), limit = 20)
#'
#' # Paginate through results
#' page1 <- nih_projects(text = "diabetes", limit = 50, offset = 0)
#' page2 <- nih_projects(text = "diabetes", limit = 50, offset = 50)
#' }
nih_projects <- function(text = NULL, pi_names = NULL, org_names = NULL,
                         fiscal_years = NULL, agencies = NULL,
                         limit = 50, offset = 0) {
  criteria <- list()
  if (!is.null(text))         criteria$advanced_text_search <- list(operator = "and", search_field = "projecttitle,terms", search_text = text)
  if (!is.null(pi_names))     criteria$pi_names <- lapply(pi_names, function(n) list(last_name = n, any_name = n))
  if (!is.null(org_names))    criteria$org_names <- as.list(org_names)
  if (!is.null(fiscal_years)) criteria$fiscal_years <- as.list(as.integer(fiscal_years))
  if (!is.null(agencies))     criteria$agencies <- as.list(agencies)

  body <- list(criteria = criteria, limit = limit, offset = offset)

  resp <- httr2::request(paste0(.nih_base, "/projects/search")) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_projects)

  tibble(
    project_num  = vapply(results, function(r) r$project_num %||% NA_character_, character(1)),
    title        = vapply(results, function(r) r$project_title %||% NA_character_, character(1)),
    pi_name      = vapply(results, function(r) {
      pis <- r$principal_investigators
      if (is.null(pis) || length(pis) == 0) NA_character_
      else paste(vapply(pis, function(p) p$full_name %||% "", character(1)), collapse = "; ")
    }, character(1)),
    org_name     = vapply(results, function(r) r$organization$org_name %||% NA_character_, character(1)),
    fiscal_year  = vapply(results, function(r) as.integer(r$fiscal_year %||% NA_integer_), integer(1)),
    award_amount = vapply(results, function(r) as.numeric(r$award_amount %||% NA_real_), numeric(1)),
    agency       = vapply(results, function(r) r$agency_ic_fundings[[1]]$abbreviation %||% r$agency_code %||% NA_character_, character(1))
  )
}

#' Search publications linked to NIH grants
#'
#' Searches the NIH RePORTER publications database for papers associated with
#' NIH-funded projects. Use this to find published research output from grants,
#' either by keyword or by filtering to specific project numbers obtained from
#' \code{\link{nih_projects}}.
#'
#' @param text Character string for full-text search across publication titles
#'   and abstracts. Example: \code{"CRISPR"}, \code{"mRNA vaccine"}.
#' @param project_nums Character vector of NIH project numbers to filter by.
#'   Example: \code{c("5R01CA012345-05")}. Use values from the
#'   \code{project_num} column returned by \code{\link{nih_projects}}.
#' @param limit Integer. Maximum number of results to return (default 50).
#' @param offset Integer. Number of results to skip for pagination (default 0).
#'
#' @return A tibble with one row per publication and the following columns:
#' \describe{
#'   \item{pmid}{Character. PubMed ID for the publication.}
#'   \item{title}{Character. Publication title (may be \code{NA} for some records).}
#'   \item{journal}{Character. Journal name (may be \code{NA}).}
#'   \item{pub_year}{Integer. Year of publication (may be \code{NA}).}
#'   \item{project_num}{Character. Core project number linked to this publication.}
#' }
#'
#' @examples
#' \dontrun{
#' # Search publications by topic
#' nih_publications(text = "CRISPR", limit = 10)
#'
#' # Find publications linked to a specific grant
#' nih_publications(project_nums = "5R01CA012345-05")
#'
#' # Paginate through results
#' page1 <- nih_publications(text = "mRNA", limit = 50, offset = 0)
#' page2 <- nih_publications(text = "mRNA", limit = 50, offset = 50)
#' }
nih_publications <- function(text = NULL, project_nums = NULL,
                             limit = 50, offset = 0) {
  criteria <- list()
  if (!is.null(text))         criteria$advanced_text_search <- list(operator = "and", search_field = "title,abstract", search_text = text)
  if (!is.null(project_nums)) criteria$project_nums <- as.list(project_nums)

  body <- list(criteria = criteria, limit = limit, offset = offset)

  resp <- httr2::request(paste0(.nih_base, "/publications/search")) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_pubs)

  tibble(
    pmid        = vapply(results, function(r) as.character(r$pmid %||% NA_character_), character(1)),
    title       = vapply(results, function(r) r$title %||% NA_character_, character(1)),
    journal     = vapply(results, function(r) r$journal %||% NA_character_, character(1)),
    pub_year    = vapply(results, function(r) as.integer(r$pub_year %||% NA_integer_), integer(1)),
    project_num = vapply(results, function(r) {
      cnums <- r$coreproject
      if (is.null(cnums)) NA_character_ else as.character(cnums)
    }, character(1))
  )
}

#' Get detailed information about a specific NIH project
#'
#' Fetches extended metadata for a single NIH project identified by its project
#' number. Returns the abstract, date range, and full award details not included
#' in the summary results from \code{\link{nih_projects}}. Use project numbers
#' obtained from \code{nih_projects()$project_num}.
#'
#' @param project_num Character. NIH project number to look up.
#'   Example: \code{"5R01CA123456-05"}, \code{"1R21AI154321-01"}.
#'
#' @return A tibble with one row and the following columns:
#' \describe{
#'   \item{project_num}{Character. NIH project number.}
#'   \item{title}{Character. Project title.}
#'   \item{abstract}{Character. Full project abstract text.}
#'   \item{pi_name}{Character. Principal investigator name(s), semicolon-separated.}
#'   \item{org_name}{Character. Awardee organization name.}
#'   \item{fiscal_year}{Integer. Fiscal year of the award.}
#'   \item{award_amount}{Numeric. Dollar amount of the award.}
#'   \item{start_date}{Character. Project start date (ISO 8601 format).}
#'   \item{end_date}{Character. Project end date (ISO 8601 format).}
#' }
#' Returns an empty tibble with the same schema if the project is not found.
#'
#' @examples
#' \dontrun{
#' # Look up a specific project
#' detail <- nih_project_detail("5R01CA012345-05")
#'
#' # Get detail for the first result of a search
#' projects <- nih_projects(text = "cancer", limit = 1)
#' detail <- nih_project_detail(projects$project_num[1])
#' }
nih_project_detail <- function(project_num) {
  schema <- tibble(project_num = character(), title = character(),
                   abstract = character(), pi_name = character(),
                   org_name = character(), fiscal_year = integer(),
                   award_amount = numeric(), start_date = character(),
                   end_date = character())

  body <- list(
    criteria = list(project_nums = list(project_num)),
    limit = 1, offset = 0
  )

  resp <- tryCatch({
    httr2::request(paste0(.nih_base, "/projects/search")) |>
      httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_perform()
  }, error = function(e) NULL)
  if (is.null(resp)) return(schema)

  tmp <- tempfile(fileext = ".json")
  writeLines(httr2::resp_body_string(resp), tmp)
  raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(schema)

  r <- results[[1]]
  tibble(
    project_num = as.character(r$project_num %||% NA_character_),
    title = as.character(r$project_title %||% NA_character_),
    abstract = as.character(r$abstract_text %||% NA_character_),
    pi_name = {
      pis <- r$principal_investigators
      if (is.null(pis) || length(pis) == 0) NA_character_
      else paste(vapply(pis, function(p) p$full_name %||% "", character(1)), collapse = "; ")
    },
    org_name = as.character(r$organization$org_name %||% NA_character_),
    fiscal_year = as.integer(r$fiscal_year %||% NA_integer_),
    award_amount = as.numeric(r$award_amount %||% NA_real_),
    start_date = as.character(r$project_start_date %||% NA_character_),
    end_date = as.character(r$project_end_date %||% NA_character_)
  )
}

#' Get NIH spending by institute/center
#'
#' Queries each major NIH institute/center to build a summary of project counts
#' for a given fiscal year. Covers 20 NIH institutes (NCI, NIAID, NHLBI, NIGMS,
#' NINDS, NIDDK, NIA, NIMH, NHGRI, NEI, NIBIB, NIDCR, NIEHS, NIDCD, NINR,
#' NICHD, NIAAA, NIDA, NCATS, NCCIH). Note: this makes one API call per
#' institute so may take several seconds to complete.
#'
#' @param fiscal_year Integer. Fiscal year to query (default: current year).
#'   Example: \code{2023L}.
#' @param limit Integer. Maximum number of agency rows to return (default 100).
#'   In practice, returns up to 20 rows (one per institute).
#'
#' @return A tibble sorted by descending project count with the following columns:
#' \describe{
#'   \item{agency}{Character. NIH institute abbreviation (e.g. \code{"NCI"}, \code{"NIAID"}).}
#'   \item{total_funding}{Numeric. Currently \code{NA} (funding totals not available from this endpoint).}
#'   \item{project_count}{Integer. Number of funded projects for this institute in the given fiscal year.}
#' }
#'
#' @examples
#' \dontrun{
#' # Get project counts for FY2023
#' spending <- nih_spending(fiscal_year = 2023)
#'
#' # Which institute funds the most projects this year?
#' spending <- nih_spending()
#' head(spending, 5)
#' }
nih_spending <- function(fiscal_year = as.integer(format(Sys.Date(), "%Y")), limit = 100) {
  schema <- tibble(agency = character(), total_funding = numeric(), project_count = integer())

  # Search for all projects in the fiscal year, grouped by agency
  agencies <- c("NCI", "NIAID", "NHLBI", "NIGMS", "NINDS", "NIDDK", "NIA",
                "NIMH", "NHGRI", "NEI", "NIBIB", "NIDCR", "NIEHS", "NIDCD",
                "NINR", "NICHD", "NIAAA", "NIDA", "NCATS", "NCCIH")

  rows <- lapply(agencies, function(ag) {
    tryCatch({
      body <- list(
        criteria = list(
          fiscal_years = list(as.integer(fiscal_year)),
          agencies = list(ag)
        ),
        limit = 1, offset = 0
      )
      resp <- httr2::request(paste0(.nih_base, "/projects/search")) |>
        httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json") |>
        httr2::req_body_json(body) |>
        httr2::req_perform()

      tmp <- tempfile(fileext = ".json")
      writeLines(httr2::resp_body_string(resp), tmp)
      raw <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)

      meta <- raw$meta
      total <- meta$total %||% 0
      funding <- if (total > 0 && length(raw$results) > 0) {
        as.numeric(meta$total %||% 0)
      } else 0

      tibble(agency = ag, total_funding = NA_real_, project_count = as.integer(total))
    }, error = function(e) NULL)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(schema)
  bind_rows(rows) |> arrange(desc(project_count))
}

# == Context ===================================================================

#' Get api.reporter.nih.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nih_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nih_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api.reporter.nih.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api.reporter.nih.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api.reporter.nih.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api.reporter.nih.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
