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

#' Print IPEDS context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
ipeds_context <- function() {
  .build_context(
    pkg_name = "ipeds.nces.ed.gov",
    header_lines = c(
      "# Package: ipeds.nces.ed.gov",
      "# NCES IPEDS via Urban Institute Education Data API",
      "# Auth: none",
      "# Rate limits: none documented",
      "#",
      "# Data: Postsecondary institution directory and enrollment",
      "# Years available: approximately 1980-2022",
      "# Sector codes: 1=public 4yr, 2=private nonprofit 4yr,",
      "#   3=private for-profit 4yr, 4=public 2yr, etc."
    )
  )
}
