# nsf.gov.R - National Science Foundation Awards API client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nsf_base <- "https://api.nsf.gov/services/v1"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.nsf_fields <- paste(
  "id", "title", "agency", "awardee", "awardeeName", "awardeeCity",
  "awardeeStateCode", "fundsObligatedAmt", "estimatedTotalAmt",
  "startDate", "expDate", "piFirstName", "piLastName", "piEmail",
  "program", "abstractText", "transType", "dirAbbr", "divAbbr",
  sep = ","
)

# == Schemas ===================================================================

.schema_awards <- tibble(
  id = character(), title = character(), pi_name = character(),
  awardee = character(), state = character(), city = character(),
  amount = numeric(), start_date = character(), end_date = character(),
  program = character(), directorate = character(), division = character(),
  type = character()
)

# == Public functions ==========================================================

#' Search NSF awards by keyword
#'
#' @param query Keyword to search (e.g. "climate change", "machine learning").
#' @param limit Number of results (default 25, max 25 per page).
#' @param offset Starting result (default 1).
#' @param date_start Filter by start date (format "mm/dd/yyyy").
#' @param date_end Filter by end date (format "mm/dd/yyyy").
#' @return tibble of matching awards
nsf_search <- function(query, limit = 25, offset = 1,
                       date_start = NULL, date_end = NULL) {
  url <- sprintf("%s/awards.json?keyword=%s&printFields=%s&rpp=%d&offset=%d",
                 .nsf_base, utils::URLencode(query, reserved = TRUE),
                 .nsf_fields, min(limit, 25), offset)
  if (!is.null(date_start)) url <- paste0(url, "&dateStart=", date_start)
  if (!is.null(date_end))   url <- paste0(url, "&dateEnd=", date_end)

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_awards)

  awards <- raw$response$award
  if (is.null(awards) || length(awards) == 0) return(.schema_awards)

  .parse_awards(awards)
}

#' Get details for a specific NSF award
#'
#' @param award_id NSF award ID (e.g. "2533679").
#' @return tibble with one row of award details
nsf_award <- function(award_id) {
  url <- sprintf("%s/awards/%s.json?printFields=%s,abstractText",
                 .nsf_base, award_id, .nsf_fields)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_awards)
  awards <- raw$response$award
  if (is.null(awards) || length(awards) == 0) return(.schema_awards)
  .parse_awards(awards)
}

#' Search NSF awards by PI last name
#'
#' @param last_name PI last name.
#' @param first_name PI first name (optional).
#' @param limit Number of results (default 25).
#' @return tibble of matching awards
nsf_pi <- function(last_name, first_name = NULL, limit = 25) {
  url <- sprintf("%s/awards.json?piLastName=%s&printFields=%s&rpp=%d",
                 .nsf_base, utils::URLencode(last_name, reserved = TRUE),
                 .nsf_fields, min(limit, 25))
  if (!is.null(first_name)) {
    url <- paste0(url, "&piFirstName=", utils::URLencode(first_name, reserved = TRUE))
  }
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_awards)
  awards <- raw$response$award
  if (is.null(awards) || length(awards) == 0) return(.schema_awards)
  .parse_awards(awards)
}

#' Search NSF awards by institution
#'
#' @param institution Institution name (e.g. "MIT", "Stanford").
#' @param limit Number of results (default 25).
#' @return tibble of matching awards
nsf_institution <- function(institution, limit = 25) {
  url <- sprintf("%s/awards.json?awardeeName=%s&printFields=%s&rpp=%d",
                 .nsf_base, utils::URLencode(institution, reserved = TRUE),
                 .nsf_fields, min(limit, 25))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_awards)
  awards <- raw$response$award
  if (is.null(awards) || length(awards) == 0) return(.schema_awards)
  .parse_awards(awards)
}

#' List NSF awards by program/directorate
#'
#' @param program Program name keyword (e.g. "STATISTICS", "ENGINEERING EDUCATION").
#' @param limit Number of results (default 25).
#' @return tibble of matching awards
nsf_program <- function(program, limit = 25) {
  url <- sprintf("%s/awards.json?fundProgramName=%s&printFields=%s&rpp=%d",
                 .nsf_base, utils::URLencode(program, reserved = TRUE),
                 .nsf_fields, min(limit, 25))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_awards)
  awards <- raw$response$award
  if (is.null(awards) || length(awards) == 0) return(.schema_awards)
  .parse_awards(awards)
}

#' Return function signatures and documentation for LLM context
#'
#' @return Printed function listing (invisibly returns string)
nsf_context <- function() {
  src_file <- tryCatch({
    f <- getSrcFilename(nsf_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else NULL
  }, error = function(e) NULL)

  if (is.null(src_file)) {
    src_dir <- system.file("source", package = "nsf.gov")
    if (src_dir != "") {
      src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
      if (length(src_files)) src_file <- src_files[1]
    }
  }
  if (is.null(src_file)) {
    msg <- paste(
      "# nsf.gov R client",
      "# Functions: nsf_search, nsf_award, nsf_pi, nsf_institution, nsf_program, nsf_context",
      "# NSF Awards API - search funded research grants",
      sep = "\n"
    )
    cat(msg, "\n"); return(invisible(msg))
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
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }
  out <- paste(c("# nsf.gov R client", "# National Science Foundation Awards API", "#",
                 "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

# == Private helpers ===========================================================

.parse_awards <- function(awards) {
  tibble(
    id          = awards$id %||% NA_character_,
    title       = awards$title %||% NA_character_,
    pi_name     = paste(awards$piFirstName %||% "", awards$piLastName %||% ""),
    awardee     = awards$awardeeName %||% NA_character_,
    state       = awards$awardeeStateCode %||% NA_character_,
    city        = awards$awardeeCity %||% NA_character_,
    amount      = as.numeric(awards$fundsObligatedAmt %||% NA),
    start_date  = awards$startDate %||% NA_character_,
    end_date    = awards$expDate %||% NA_character_,
    program     = awards$program %||% NA_character_,
    directorate = awards$dirAbbr %||% NA_character_,
    division    = awards$divAbbr %||% NA_character_,
    type        = awards$transType %||% NA_character_
  )
}
