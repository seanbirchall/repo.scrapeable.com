# ed.gov.R
# Self-contained Department of Education client.
# Covers: NCES CCD (K-12 schools/districts), NCES IPEDS (postsecondary),
# and ERIC (education research database).
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# APIs: educationdata.urban.org (CCD/IPEDS), api.ies.ed.gov (ERIC)

library(httr2)
library(jsonlite)
library(dplyr, warn.conflicts = FALSE)
library(tibble)


# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a
.ua <- "support@scrapeable.com"
.ccd_base <- "https://educationdata.urban.org/api/v1"
.ipeds_base <- "https://educationdata.urban.org/api/v1"
.eric_base <- "https://api.ies.ed.gov/eric/"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))


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

.schema_institutions <- tibble(
  unitid = integer(), inst_name = character(), state_abbr = character(),
  city = character(), zip = character(), sector = integer(),
  inst_level = integer(), year = integer()
)

.schema_enrollment <- tibble(
  unitid = integer(), inst_name = character(), year = integer(),
  enrollment_fall = integer(), state_abbr = character()
)

.schema_search <- tibble(
  id = character(), title = character(), author = character(),
  source = character(), publicationdate = character(),
  description = character(), subject = character(),
  url = character()
)




#' Search CCD school directory
#'
#' Returns school-level data from the NCES Common Core of Data via the
#' Urban Institute Education Data API. Covers all U.S. public K-12 schools.
#'
#' @param year Integer. School year (e.g., 2021 for 2021-22). Default 2021.
#'   Available years: approximately 1986--2022.
#' @param state Character. Two-digit FIPS code (e.g., \code{"06"} for CA,
#'   \code{"36"} for NY, \code{"48"} for TX). Default \code{NULL} returns all states.
#'   Note: the API ignores \code{limit} when no state filter is set and
#'   may return up to 10,000 rows.
#' @param limit Integer. Maximum records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{ncessch}{character -- 12-digit NCES school ID}
#'     \item{school_name}{character -- school name}
#'     \item{leaid}{character -- 7-digit NCES district (LEA) ID}
#'     \item{lea_name}{character -- district name}
#'     \item{state_location}{character -- 2-letter state abbreviation}
#'     \item{city_location}{character -- city name}
#'     \item{zip_location}{character -- 5-digit ZIP code}
#'     \item{enrollment}{integer -- total enrollment count}
#'     \item{year}{integer -- school year}
#'   }
#' @examples
#' ccd_schools(year = 2021, state = "06", limit = 10)
#' @export
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


#' Search CCD school district directory
#'
#' Returns district-level (LEA) data from the NCES Common Core of Data.
#' Covers all U.S. public school districts.
#'
#' @param year Integer. School year (e.g., 2021 for 2021-22). Default 2021.
#' @param state Character. Two-digit FIPS code (e.g., \code{"06"} for CA).
#'   Default \code{NULL} returns all states.
#' @param limit Integer. Maximum records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{leaid}{character -- 7-digit NCES district (LEA) ID}
#'     \item{lea_name}{character -- district name}
#'     \item{state_leaid}{character -- state-assigned district ID (e.g. \code{"CA-1975309"})}
#'     \item{state_location}{character -- 2-letter state abbreviation}
#'     \item{city_location}{character -- city name}
#'     \item{zip_location}{character -- 5-digit ZIP code}
#'     \item{enrollment}{integer -- total enrollment count}
#'     \item{year}{integer -- school year}
#'   }
#' @examples
#' ccd_districts(year = 2021, state = "06", limit = 10)
#' @export
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


#' Get school enrollment totals by race and sex
#'
#' Returns grade-99 (total) enrollment from the CCD enrollment endpoint,
#' broken down by NCES race and sex codes. Race codes: 1=White, 2=Black,
#' 3=Hispanic, 4=Asian, 5=AIAN, 6=NHOPI, 7=Two or more, 99=Total.
#' Sex codes: 1=Male, 2=Female, 99=Total.
#'
#' @param year Integer. School year (e.g., 2020). Default 2020.
#' @param state Character. Two-digit FIPS code (e.g., \code{"06"} for CA).
#'   Default \code{NULL} returns all states.
#' @param limit Integer. Max records (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{ncessch}{character -- 12-digit NCES school ID}
#'     \item{year}{integer -- school year}
#'     \item{grade}{integer -- grade level (99 = total)}
#'     \item{race}{integer -- NCES race code (see above)}
#'     \item{sex}{integer -- NCES sex code (see above)}
#'     \item{enrollment}{integer -- enrollment count}
#'   }
#' @examples
#' ccd_enrollment(year = 2020, state = "06", limit = 10)
#' @export
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


#' Search IPEDS institution directory
#'
#' Returns institution-level data from the NCES IPEDS (Integrated
#' Postsecondary Education Data System) via the Urban Institute API.
#' Covers all U.S. postsecondary institutions.
#'
#' @param year Integer. Academic year (e.g., 2021). Default 2021.
#' @param state Character. Two-digit FIPS code (e.g., \code{"06"} for CA).
#'   Default \code{NULL} returns all states.
#' @param limit Integer. Maximum records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{unitid}{integer -- IPEDS institution ID}
#'     \item{inst_name}{character -- institution name}
#'     \item{state_abbr}{character -- 2-letter state abbreviation}
#'     \item{city}{character -- city name}
#'     \item{zip}{character -- ZIP code}
#'     \item{sector}{integer -- IPEDS sector code (1=public 4yr, 2=private nonprofit 4yr,
#'       3=private for-profit 4yr, 4=public 2yr, etc.)}
#'     \item{inst_level}{integer -- institution level code}
#'     \item{year}{integer -- academic year}
#'   }
#' @examples
#' ipeds_institutions(year = 2021, state = "06", limit = 10)
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
#' Returns fall enrollment data from IPEDS for postsecondary institutions.
#' Falls back to the directory endpoint when the enrollment endpoint
#' is unavailable.
#'
#' @param year Integer. Academic year (e.g., 2021). Default 2021.
#' @param limit Integer. Maximum records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{unitid}{integer -- IPEDS institution ID}
#'     \item{inst_name}{character -- institution name}
#'     \item{year}{integer -- academic year}
#'     \item{enrollment_fall}{integer -- total fall enrollment}
#'     \item{state_abbr}{character -- 2-letter state abbreviation}
#'   }
#' @examples
#' ipeds_enrollment(year = 2021, limit = 20)
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
#' Returns degree completion counts by CIP code, award level, race, and sex
#' from the IPEDS completions survey.
#'
#' @param year Integer. Academic year (e.g., 2021). Default 2021.
#' @param limit Integer. Maximum records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{unitid}{integer -- IPEDS institution ID}
#'     \item{year}{integer -- academic year}
#'     \item{cipcode}{character -- 6-digit CIP code for field of study}
#'     \item{award_level}{integer -- award level (1=certificate, 5=bachelor's,
#'       7=master's, 9=doctoral, etc.)}
#'     \item{race}{integer -- NCES race code}
#'     \item{sex}{integer -- NCES sex code}
#'     \item{awards}{integer -- number of awards conferred}
#'   }
#' @examples
#' ipeds_completions(year = 2021, limit = 20)
#' @export
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


#' Search ERIC education research database
#'
#' Searches the ERIC (Education Resources Information Center) database
#' of education research literature via the IES API.
#'
#' @param query Character. Search query (e.g. \code{"mathematics instruction"},
#'   \code{"STEM education"}, \code{"early childhood literacy"}).
#' @param rows Integer. Number of results (default 10, max 200).
#' @param start Integer. Starting record offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- ERIC document ID (e.g. \code{"EJ996400"})}
#'     \item{title}{character -- document title}
#'     \item{author}{character -- semicolon-separated author names}
#'     \item{source}{character -- publication source (may be \code{NA})}
#'     \item{publicationdate}{character -- publication date string}
#'     \item{description}{character -- abstract/description}
#'     \item{subject}{character -- semicolon-separated subject terms}
#'     \item{url}{character -- ERIC URL (e.g. \code{"https://eric.ed.gov/?id=EJ996400"})}
#'   }
#' @examples
#' eric_search("STEM education", rows = 5)
#' eric_search("early childhood", rows = 10, start = 10)
#' @export
eric_search <- function(query, rows = 10, start = 0) {
  url <- paste0(.eric_base, "?search=", utils::URLencode(query),
                "&rows=", rows, "&start=", start, "&format=json")
  raw <- .fetch_json(url)
  d <- raw$response$docs
  if (is.null(d) || length(d) == 0) return(.schema_search)

  # author and subject may be list columns
  .collapse_col <- function(x) {
    if (is.list(x)) vapply(x, function(v) paste(v, collapse = "; "), character(1))
    else as.character(x)
  }

  as_tibble(d) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      author = .collapse_col(if ("author" %in% names(d)) author else NA_character_),
      source = as.character(if ("source" %in% names(d)) source else NA_character_),
      publicationdate = as.character(if ("publicationdate" %in% names(d)) publicationdate else NA_character_),
      description = as.character(if ("description" %in% names(d)) description else NA_character_),
      subject = .collapse_col(if ("subject" %in% names(d)) subject else NA_character_),
      url = paste0("https://eric.ed.gov/?id=", id)
    )
}


#' Get a single ERIC document by ID
#'
#' Retrieves full metadata for one ERIC document including peer review
#' status and publication type.
#'
#' @param eric_id Character. ERIC document ID (e.g. \code{"EJ1234567"}
#'   for journal articles or \code{"ED654321"} for reports/documents).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{character -- ERIC document ID}
#'     \item{title}{character -- document title}
#'     \item{author}{character -- semicolon-separated author names}
#'     \item{source}{character -- publication source}
#'     \item{publicationdate}{character -- publication date string}
#'     \item{description}{character -- abstract/description}
#'     \item{subject}{character -- semicolon-separated subject terms}
#'     \item{url}{character -- ERIC URL}
#'     \item{peerreviewed}{character -- \code{"T"} if peer-reviewed}
#'     \item{publicationtype}{character -- semicolon-separated publication types}
#'   }
#' @examples
#' eric_document("EJ1234567")
#' @export
eric_document <- function(eric_id) {
  url <- paste0(.eric_base, "?search=id:", eric_id, "&rows=1&format=json")
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_search)

  d <- raw$response$docs
  if (is.null(d) || length(d) == 0) return(.schema_search)

  .collapse_col <- function(x) {
    if (is.list(x)) vapply(x, function(v) paste(v, collapse = "; "), character(1))
    else as.character(x)
  }

  nms <- names(d)
  as_tibble(d) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      author = .collapse_col(if ("author" %in% nms) author else NA_character_),
      source = as.character(if ("source" %in% nms) source else NA_character_),
      publicationdate = as.character(if ("publicationdate" %in% nms) publicationdate else NA_character_),
      description = as.character(if ("description" %in% nms) description else NA_character_),
      subject = .collapse_col(if ("subject" %in% nms) subject else NA_character_),
      url = paste0("https://eric.ed.gov/?id=", id),
      peerreviewed = as.character(if ("peerreviewed" %in% nms) peerreviewed else NA_character_),
      publicationtype = .collapse_col(if ("publicationtype" %in% nms) publicationtype else NA_character_)
    )
}


#' Search ERIC with faceted results
#'
#' Returns search results along with facet counts, useful for
#' understanding the distribution of results across categories.
#'
#' @param query Character. Search query.
#' @param rows Integer. Number of results (default 10).
#' @param facet_field Character. Field to facet on: \code{"subject"} (default),
#'   \code{"author"}, \code{"publicationtype"}, \code{"source"},
#'   \code{"educationlevel"}.
#' @return A list with two elements:
#'   \describe{
#'     \item{results}{tibble -- search results (same columns as \code{eric_search})}
#'     \item{facets}{tibble with columns \code{value} (character) and \code{count} (integer)
#'       showing the frequency of each facet value}
#'   }
#' @examples
#' eric_facets("STEM education", rows = 5, facet_field = "subject")
#' @export
eric_facets <- function(query, rows = 10, facet_field = "subject") {
  url <- paste0(.eric_base, "?search=", utils::URLencode(query),
                "&rows=", rows, "&format=json",
                "&facet=on&facet.field=", facet_field)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(list(results = .schema_search,
                                facets = tibble(value = character(), count = integer())))

  # Parse results
  results <- eric_search(query, rows = rows)

  # Parse facets
  fc <- raw$facet_counts$facet_fields
  if (is.null(fc)) {
    facets <- tibble(value = character(), count = integer())
  } else {
    fvals <- fc[[facet_field]]
    if (is.null(fvals) || length(fvals) == 0) {
      facets <- tibble(value = character(), count = integer())
    } else {
      # Facets come as alternating value, count pairs
      idx_vals <- seq(1, length(fvals), 2)
      idx_cnts <- seq(2, length(fvals), 2)
      facets <- tibble(
        value = as.character(fvals[idx_vals]),
        count = as.integer(fvals[idx_cnts])
      ) |> filter(count > 0)
    }
  }

  list(results = results, facets = facets)
}


# == Context ===================================================================

#' Get ed.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ed_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ed_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ed.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ed.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ed.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ed.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
