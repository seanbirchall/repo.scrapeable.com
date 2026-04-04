# nces-ed-gov-ipeds.R
# Self-contained NCES IPEDS (Integrated Postsecondary Education Data) client.
# Uses the Urban Institute Education Data API.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known

library(dplyr, warn.conflicts = FALSE)
library(tibble)

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
#' Returns institution-level data from the NCES IPEDS via the Urban Institute
#' Education Data API.
#'
#' @param year Academic year (e.g., 2021). Default 2021.
#' @param state Two-digit FIPS code as string (e.g., "06" for CA), or NULL for all.
#' @param limit Maximum records to return. Default 100.
#' @return tibble: unitid, inst_name, state_abbr, city, zip, sector,
#'   inst_level, year
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
#' Returns enrollment data from IPEDS.
#'
#' @param year Academic year (e.g., 2021). Default 2021.
#' @param limit Maximum records to return. Default 100.
#' @return tibble: unitid, inst_name, year, enrollment_fall, state_abbr
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
#' Returns degree completion counts by CIP code, award level, race, and sex.
#'
#' @param year Academic year (e.g., 2021). Default 2021.
#' @param limit Maximum records to return. Default 100.
#' @return tibble: unitid, year, cipcode, award_level, race, sex, awards
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

#' Generate LLM-friendly context for IPEDS
#'
#' @return Character string with full function signatures and bodies
ipeds_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/nces-ed-gov-ipeds.R"
  if (!file.exists(src_file)) {
    cat("# nces-ed-gov-ipeds context - source not found\n")
    return(invisible("# nces-ed-gov-ipeds context - source not found"))
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
