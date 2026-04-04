


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
#' @param text Full-text search across project title and abstract
#' @param pi_names PI last names (character vector)
#' @param org_names Organization names
#' @param fiscal_years Fiscal years (integer vector)
#' @param agencies NIH institute codes (e.g. "NCI", "NIAID")
#' @param limit Max results (default 50, max 500)
#' @param offset Pagination offset
#' @return tibble: project_num, title, pi_name, org_name, fiscal_year,
#'   award_amount, agency
#' @export
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
#' @param text Search text
#' @param project_nums Project numbers to filter by
#' @param limit Max results (default 50)
#' @param offset Pagination offset
#' @return tibble: pmid, title, journal, pub_year, project_num
#' @export
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

# == Context ===================================================================

#' Generate LLM-friendly context for api.reporter.nih.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
nih_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/api.reporter.nih.gov.R"
  if (!file.exists(src_file)) {
    cat("# api.reporter.nih.gov context - source not found\n")
    return(invisible("# api.reporter.nih.gov context - source not found"))
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
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

