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

# == Public functions ==========================================================

#' Search NSF awards by keyword
#'
#' Searches the NSF Awards API by keyword across award titles,
#' abstracts, and program names. Returns up to 25 results per page.
#'
#' @param query Character. Keyword to search (e.g. \code{"climate change"},
#'   \code{"machine learning"}, \code{"quantum computing"}).
#' @param limit Integer. Number of results (default 25, API max 25 per page).
#' @param offset Integer. Starting result index for pagination (default 1).
#' @param date_start Character or NULL. Filter by award start date in
#'   \code{"mm/dd/yyyy"} format (e.g. \code{"01/01/2024"}).
#' @param date_end Character or NULL. Filter by award end date in
#'   \code{"mm/dd/yyyy"} format.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- NSF award ID (e.g. \code{"2449122"})}
#'     \item{title}{character -- award title}
#'     \item{pi_name}{character -- principal investigator full name}
#'     \item{awardee}{character -- institution name}
#'     \item{state}{character -- two-letter state code}
#'     \item{city}{character -- institution city}
#'     \item{amount}{numeric -- funds obligated in USD}
#'     \item{start_date}{character -- award start date (mm/dd/yyyy)}
#'     \item{end_date}{character -- award end date (mm/dd/yyyy)}
#'     \item{program}{character -- funding program name}
#'     \item{directorate}{character -- NSF directorate abbreviation}
#'     \item{division}{character -- NSF division abbreviation}
#'     \item{type}{character -- transaction type (e.g. \code{"Grant"})}
#'   }
#' @examples
#' nsf_search("climate change")
#' nsf_search("quantum computing", date_start = "01/01/2024")
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
#' Retrieves full metadata including abstract for a single NSF award.
#'
#' @param award_id Character. NSF award ID (e.g. \code{"2533679"}).
#' @return A tibble with 1 row and 13 columns (same schema as
#'   \code{nsf_search()}): id, title, pi_name, awardee, state,
#'   city, amount, start_date, end_date, program, directorate,
#'   division, type.
#' @examples
#' nsf_award("2533679")
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
#' Finds awards by principal investigator name.
#'
#' @param last_name Character. PI last name (e.g. \code{"Smith"},
#'   \code{"Zhang"}).
#' @param first_name Character or NULL. PI first name to narrow results.
#' @param limit Integer. Number of results (default 25, max 25).
#' @return A tibble with 13 columns (same schema as \code{nsf_search()}).
#' @examples
#' nsf_pi("Smith")
#' nsf_pi("Smith", first_name = "John")
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
#' Finds awards by awardee institution name (partial match).
#'
#' @param institution Character. Institution name or abbreviation
#'   (e.g. \code{"MIT"}, \code{"Stanford"}, \code{"University of Michigan"}).
#' @param limit Integer. Number of results (default 25, max 25).
#' @return A tibble with 13 columns (same schema as \code{nsf_search()}).
#' @examples
#' nsf_institution("MIT")
#' nsf_institution("Stanford")
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
#' Finds awards by funding program name.
#'
#' @param program Character. Program name keyword (e.g.
#'   \code{"STATISTICS"}, \code{"ENGINEERING EDUCATION"},
#'   \code{"COMPUTER SCIENCE"}, \code{"Artificial Intelligence"}).
#' @param limit Integer. Number of results (default 25, max 25).
#' @return A tibble with 13 columns (same schema as \code{nsf_search()}).
#' @examples
#' nsf_program("STATISTICS")
#' nsf_program("Artificial Intelligence")
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

#' Get nsf.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nsf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nsf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nsf.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nsf.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nsf.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nsf.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
