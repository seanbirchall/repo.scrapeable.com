# nces.ed.gov.R - Self-contained nces.ed.gov client



# nces-ed-gov-ccd.R
# Self-contained NCES Common Core of Data (CCD) client.
# Uses the Urban Institute Education Data API.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ccd_base <- "https://educationdata.urban.org/api/v1"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_schools <- tibble(
  ncessch = character(), school_name = character(), leaid = character(),
  lea_name = character(), state_location = character(), city_location = character(),
  zip_location = character(), enrollment = integer(), year = integer()
)

.schema_districts <- tibble(
  leaid = character(), lea_name = character(), state_leaid = character(),
  state_location = character(), city_location = character(),
  zip_location = character(), enrollment = integer(), year = integer()
)

# == Public functions ==========================================================


#' Search the CCD school directory
#'
#' Query the NCES Common Core of Data (CCD) school directory via the
#' Urban Institute Education Data API. Returns school-level records
#' including name, location, district, and total enrollment.
#'
#' @details
#' The CCD is the Department of Education's primary database of public
#' elementary and secondary schools and school districts. Data are updated
#' annually. The \code{year} parameter refers to the fall of the school
#' year (e.g. 2021 = 2021--22). The API returns up to \code{limit}
#' records; set \code{limit = 10000} to page through larger result sets.
#'
#' FIPS codes for common states: \code{"06"} (CA), \code{"36"} (NY),
#' \code{"48"} (TX), \code{"17"} (IL). See
#' \url{https://www.census.gov/library/reference/code-lists/ansi.html}
#' for the full list.
#'
#' @param year Integer. School year (fall), e.g. 2021 for 2021--22.
#'   Default \code{2021}.
#' @param state Character or \code{NULL}. Two-digit FIPS code to filter by
#'   state (e.g. \code{"06"} for California). \code{NULL} returns all states.
#' @param limit Integer. Maximum records to return (default 100).
#' @return A tibble with columns:
#' \describe{
#'   \item{ncessch}{Character. Unique NCES school ID.}
#'   \item{school_name}{Character. School name.}
#'   \item{leaid}{Character. Local education agency (district) ID.}
#'   \item{lea_name}{Character. District name.}
#'   \item{state_location}{Character. Two-letter state abbreviation.}
#'   \item{city_location}{Character. City.}
#'   \item{zip_location}{Character. ZIP code.}
#'   \item{enrollment}{Integer. Total student enrollment.}
#'   \item{year}{Integer. School year.}
#' }
#' @export
#' @seealso \code{\link{ccd_districts}}, \code{\link{ccd_enrollment}}
#' @examples
#' ccd_schools(year = 2021, state = "06", limit = 10)
ccd_schools <- function(year = 2021, state = NULL, limit = 100) {
  url <- sprintf("%s/schools/ccd/directory/%d/?limit=%d",
                 .ccd_base, as.integer(year), as.integer(limit))
  if (!is.null(state)) url <- paste0(url, "&fips=", state)

  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || nrow(results) == 0) return(.schema_schools)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      ncessch = as.character(if ("ncessch" %in% nms) ncessch else NA_character_),
      school_name = as.character(if ("school_name" %in% nms) school_name else NA_character_),
      leaid = as.character(if ("leaid" %in% nms) leaid else NA_character_),
      lea_name = as.character(if ("lea_name" %in% nms) lea_name else NA_character_),
      state_location = as.character(if ("state_location" %in% nms) state_location else NA_character_),
      city_location = as.character(if ("city_location" %in% nms) city_location else NA_character_),
      zip_location = as.character(if ("zip_location" %in% nms) zip_location else NA_character_),
      enrollment = as.integer(if ("enrollment" %in% nms) enrollment else NA_integer_),
      year = as.integer(year)
    )
}

#' Search the CCD school district directory
#'
#' Query the NCES Common Core of Data (CCD) school district directory.
#' Returns district-level records including name, state-assigned ID,
#' location, and aggregate enrollment.
#'
#' @details
#' Each row represents one Local Education Agency (LEA), which is
#' typically a school district, charter-school authorizer, or
#' state-operated agency. The \code{leaid} is the nationally unique
#' NCES identifier; \code{state_leaid} is the state's own code.
#'
#' @param year Integer. School year (fall), e.g. 2021 for 2021--22.
#'   Default \code{2021}.
#' @param state Character or \code{NULL}. Two-digit FIPS code to filter by
#'   state. \code{NULL} returns all states.
#' @param limit Integer. Maximum records to return (default 100).
#' @return A tibble with columns:
#' \describe{
#'   \item{leaid}{Character. Unique NCES district ID.}
#'   \item{lea_name}{Character. District name.}
#'   \item{state_leaid}{Character. State-assigned district ID.}
#'   \item{state_location}{Character. Two-letter state abbreviation.}
#'   \item{city_location}{Character. City.}
#'   \item{zip_location}{Character. ZIP code.}
#'   \item{enrollment}{Integer. Total district enrollment.}
#'   \item{year}{Integer. School year.}
#' }
#' @export
#' @seealso \code{\link{ccd_schools}}, \code{\link{ccd_enrollment}}
#' @examples
#' ccd_districts(year = 2021, state = "36", limit = 10)
ccd_districts <- function(year = 2021, state = NULL, limit = 100) {
  url <- sprintf("%s/school-districts/ccd/directory/%d/?limit=%d",
                 .ccd_base, as.integer(year), as.integer(limit))
  if (!is.null(state)) url <- paste0(url, "&fips=", state)

  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || nrow(results) == 0) return(.schema_districts)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      leaid = as.character(if ("leaid" %in% nms) leaid else NA_character_),
      lea_name = as.character(if ("lea_name" %in% nms) lea_name else NA_character_),
      state_leaid = as.character(if ("state_leaid" %in% nms) state_leaid else NA_character_),
      state_location = as.character(if ("state_location" %in% nms) state_location else NA_character_),
      city_location = as.character(if ("city_location" %in% nms) city_location else NA_character_),
      zip_location = as.character(if ("zip_location" %in% nms) zip_location else NA_character_),
      enrollment = as.integer(if ("enrollment" %in% nms) enrollment else NA_integer_),
      year = as.integer(year)
    )
}

# == School enrollment =========================================================

#' Get school enrollment totals by race and sex
#'
#' Retrieve grade-99 (school total) enrollment from the CCD enrollment
#' endpoint, disaggregated by NCES race and sex codes.
#'
#' @details
#' Race codes follow the CCD standard: 1 = White, 2 = Black, 3 = Hispanic,
#' 4 = Asian, 5 = American Indian/Alaska Native, 6 = NH/PI, 7 = Two or more,
#' 9 = Unknown, 99 = Total. Sex codes: 1 = Male, 2 = Female, 9 = Unknown,
#' 99 = Total. Grade 99 represents the school-wide total across all grades.
#'
#' @param year Integer. School year (fall), e.g. 2020.
#' @param state Character or \code{NULL}. Two-digit FIPS code to filter by state.
#' @param limit Integer. Maximum records to return (default 100).
#' @return A tibble with columns:
#' \describe{
#'   \item{ncessch}{Character. NCES school ID.}
#'   \item{year}{Integer. School year.}
#'   \item{grade}{Integer. Grade level (99 = total).}
#'   \item{race}{Integer. NCES race code.}
#'   \item{sex}{Integer. NCES sex code.}
#'   \item{enrollment}{Integer. Enrollment count.}
#' }
#' @export
#' @seealso \code{\link{ccd_schools}}, \code{\link{ccd_districts}}
#' @examples
#' ccd_enrollment(year = 2020, state = "06", limit = 20)
ccd_enrollment <- function(year = 2020, state = NULL, limit = 100) {
  schema <- tibble(ncessch = character(), year = integer(), grade = integer(),
                   race = integer(), sex = integer(), enrollment = integer())
  url <- sprintf("%s/schools/ccd/enrollment/%d/grade-99/?limit=%d",
                 .ccd_base, as.integer(year), as.integer(limit))
  if (!is.null(state)) url <- paste0(url, "&fips=", state)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$results) || nrow(raw$results) == 0) return(schema)

  nms <- names(raw$results)
  as_tibble(raw$results) |>
    transmute(
      ncessch = as.character(if ("ncessch" %in% nms) ncessch else NA_character_),
      year = as.integer(if ("year" %in% nms) year else NA_integer_),
      grade = as.integer(if ("grade" %in% nms) grade else NA_integer_),
      race = as.integer(if ("race" %in% nms) race else NA_integer_),
      sex = as.integer(if ("sex" %in% nms) sex else NA_integer_),
      enrollment = as.integer(if ("enrollment" %in% nms) enrollment else NA_integer_)
    )
}

# == Context ===================================================================

#' Get nces.ed.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ccd_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ccd_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nces.ed.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nces.ed.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nces.ed.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nces.ed.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
