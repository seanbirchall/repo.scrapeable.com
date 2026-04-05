# nces-ed-gov-ipeds.R
# Self-contained NCES IPEDS (Integrated Postsecondary Education Data) client.
# Uses the Urban Institute Education Data API.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ipeds_base <- "https://educationdata.urban.org/api/v1"

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

.schema_institutions <- tibble(
  unitid = integer(), inst_name = character(), state_abbr = character(),
  city = character(), zip = character(), sector = integer(),
  inst_level = integer(), year = integer()
)

.schema_enrollment <- tibble(
  unitid = integer(), inst_name = character(), year = integer(),
  enrollment_fall = integer(), state_abbr = character()
)

# == Public functions ==========================================================

#' Search IPEDS institution directory
#'
#' Returns postsecondary institution data from the NCES IPEDS (Integrated
#' Postsecondary Education Data System) via the Urban Institute Education
#' Data API. Covers colleges, universities, and vocational schools.
#'
#' @param year Academic year as integer (e.g. 2021). Default 2021.
#'   Available years typically cover 2000-2022.
#' @param state Two-digit FIPS state code as string (e.g. "06" for California,
#'   "36" for New York). When NULL (default), returns institutions from all states.
#' @param limit Maximum number of records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{unitid}{IPEDS unique institution ID (6-digit integer)}
#'     \item{inst_name}{Institution name}
#'     \item{state_abbr}{Two-letter state abbreviation}
#'     \item{city}{City name}
#'     \item{zip}{ZIP code}
#'     \item{sector}{Sector code: 1=public 4yr, 2=private nonprofit 4yr, 3=private for-profit 4yr, 4=public 2yr, etc.}
#'     \item{inst_level}{Institution level code}
#'     \item{year}{Academic year}
#'   }
#' @examples
#' ipeds_institutions(year = 2021, state = "06", limit = 10)
#' @seealso [ipeds_enrollment()], [ipeds_completions()], [ipeds_context()]
#' @source <https://educationdata.urban.org/documentation/>
#' @export
ipeds_institutions <- function(year = 2021, state = NULL, limit = 100) {
  url <- sprintf("%s/college-university/ipeds/directory/%d/?limit=%d",
                 .ipeds_base, as.integer(year), as.integer(limit))
  if (!is.null(state)) url <- paste0(url, "&fips=", state)

  raw <- .fetch_json(url)
  results <- raw$results
  if (is.null(results) || nrow(results) == 0) return(.schema_institutions)

  nms <- names(results)
  as_tibble(results) |>
    transmute(
      unitid = as.integer(if ("unitid" %in% nms) unitid else NA_integer_),
      inst_name = as.character(if ("inst_name" %in% nms) inst_name else NA_character_),
      state_abbr = as.character(if ("state_abbr" %in% nms) state_abbr else NA_character_),
      city = as.character(if ("city" %in% nms) city else NA_character_),
      zip = as.character(if ("zip" %in% nms) zip else NA_character_),
      sector = as.integer(if ("sector" %in% nms) sector else NA_integer_),
      inst_level = as.integer(if ("inst_level" %in% nms) inst_level else NA_integer_),
      year = as.integer(year)
    )
}

#' Search IPEDS fall enrollment data
#'
#' Returns fall enrollment data from the NCES IPEDS database. Provides
#' headcount enrollment by institution. Falls back to the directory endpoint
#' if the detailed enrollment endpoint is unavailable.
#'
#' @param year Academic year as integer (e.g. 2021). Default 2021.
#' @param limit Maximum number of records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{unitid}{IPEDS institution ID (integer)}
#'     \item{inst_name}{Institution name}
#'     \item{year}{Academic year}
#'     \item{enrollment_fall}{Fall headcount enrollment (integer, may be NA)}
#'     \item{state_abbr}{Two-letter state abbreviation}
#'   }
#' @examples
#' ipeds_enrollment(year = 2021, limit = 10)
#' @seealso [ipeds_institutions()], [ipeds_completions()], [ipeds_context()]
#' @source <https://educationdata.urban.org/documentation/>
#' @export
ipeds_enrollment <- function(year = 2021, limit = 100) {
  url <- sprintf("%s/college-university/ipeds/fall-enrollment/race/2021/%d/?limit=%d&sex=99&level_of_study=1&class_level=99",
                 .ipeds_base, as.integer(year), as.integer(limit))

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) {
    # Fallback: use directory endpoint which has enrollment
    url2 <- sprintf("%s/college-university/ipeds/directory/%d/?limit=%d",
                    .ipeds_base, as.integer(year), as.integer(limit))
    raw <- .fetch_json(url2)
  }
  results <- raw$results
  if (is.null(results) || nrow(results) == 0) return(.schema_enrollment)

  nms <- names(results)
  enroll_col <- if ("enrollment_fall" %in% nms) "enrollment_fall" else
                if ("efytotlt" %in% nms) "efytotlt" else
                if ("enrollment" %in% nms) "enrollment" else NULL

  as_tibble(results) |>
    transmute(
      unitid = as.integer(if ("unitid" %in% nms) unitid else NA_integer_),
      inst_name = as.character(if ("inst_name" %in% nms) inst_name else NA_character_),
      year = as.integer(year),
      enrollment_fall = if (!is.null(enroll_col)) as.integer(.data[[enroll_col]]) else NA_integer_,
      state_abbr = as.character(if ("state_abbr" %in% nms) state_abbr else NA_character_)
    )
}

#' Get IPEDS completions (degrees awarded) data
#'
#' Returns degree and certificate completion counts from IPEDS, broken down
#' by CIP (Classification of Instructional Programs) code, award level,
#' race/ethnicity, and sex. Useful for analyzing graduation trends and
#' program output.
#'
#' @param year Academic year as integer (e.g. 2021). Default 2021.
#' @param limit Maximum number of records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{unitid}{IPEDS institution ID (integer)}
#'     \item{year}{Academic year}
#'     \item{cipcode}{CIP program code (e.g. "11.0101" for Computer Science)}
#'     \item{award_level}{Award level code (1=certificate, 5=bachelor's, 7=master's, etc.)}
#'     \item{race}{Race/ethnicity code (99=total)}
#'     \item{sex}{Sex code (99=total)}
#'     \item{awards}{Number of awards/degrees conferred (integer)}
#'   }
#' @examples
#' ipeds_completions(year = 2021, limit = 10)
#' @seealso [ipeds_institutions()], [ipeds_enrollment()], [ipeds_context()]
#' @source <https://educationdata.urban.org/documentation/>
ipeds_completions <- function(year = 2021, limit = 100) {
  schema <- tibble(unitid = integer(), year = integer(), cipcode = character(),
                   award_level = integer(), race = integer(),
                   sex = integer(), awards = integer())
  url <- sprintf("%s/college-university/ipeds/completions-cip-2/%d/?limit=%d",
                 .ipeds_base, as.integer(year), as.integer(limit))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$results) || nrow(raw$results) == 0) return(schema)

  nms <- names(raw$results)
  as_tibble(raw$results) |>
    transmute(
      unitid = as.integer(if ("unitid" %in% nms) unitid else NA_integer_),
      year = as.integer(if ("year" %in% nms) year else NA_integer_),
      cipcode = as.character(if ("cipcode" %in% nms) cipcode else NA_character_),
      award_level = as.integer(if ("award_level" %in% nms) award_level else NA_integer_),
      race = as.integer(if ("race" %in% nms) race else NA_integer_),
      sex = as.integer(if ("sex" %in% nms) sex else NA_integer_),
      awards = as.integer(if ("awards" %in% nms) awards else NA_integer_)
    )
}

#' Get nces-ed-gov-ipeds client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the NCES IPEDS (postsecondary education) client. Designed
#' for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' ipeds_context()
#' @export
ipeds_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ipeds_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nces-ed-gov-ipeds.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nces-ed-gov-ipeds")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nces-ed-gov-ipeds context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nces-ed-gov-ipeds", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
