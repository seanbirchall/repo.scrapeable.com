# clinicaltrials.gov.R
# Self-contained clinicaltrials.gov client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble


# clinicaltrials-gov.R
# Self-contained ClinicalTrials.gov API v2 client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Rate limits: ~50 requests/minute
# Docs: https://clinicaltrials.gov/data-api/api


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.ct_base <- "https://clinicaltrials.gov/api/v2"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- Response flattening -------------------------------------------------------
# ClinicalTrials.gov returns deeply nested JSON. These helpers flatten to tibble.

.safe <- function(x, default = NA_character_) {
  if (is.null(x)) return(default)
  if (is.list(x) && length(x) == 0) return(default)
  if (length(x) > 1) return(paste(x, collapse = "; "))
  as.character(x)
}

# Safe deep access: .deep(list, "a", "b", "c") = list$a$b$c without $ errors
.deep <- function(x, ...) {
  keys <- list(...)
  for (k in keys) {
    if (is.null(x) || !is.list(x)) return(NULL)
    x <- x[[k]]
  }
  x
}

.safe_int <- function(x) {
  if (is.null(x)) return(NA_integer_)
  suppressWarnings(as.integer(x))
}

.safe_date <- function(x) {
  if (is.null(x)) return(as.Date(NA))
  # CT.gov dates can be "2024-03-25", "2024-03", or "2024"
  d <- tryCatch(as.Date(x), error = function(e) {
    tryCatch(as.Date(paste0(x, "-01")), error = function(e2) {
      tryCatch(as.Date(paste0(x, "-01-01")), error = function(e3) as.Date(NA))
    })
  })
  d
}

# -- Pagination helper ---------------------------------------------------------
# Transparently fetches all pages and combines results.

.ct_fetch_all <- function(url, max_pages = 100, page_size = 100) {
  all_studies <- list()
  page_url <- paste0(url, "&pageSize=", page_size, "&countTotal=true")
  total <- NULL

  for (i in seq_len(max_pages)) {
    raw <- .fetch_json(page_url)

    if (is.null(total) && !is.null(raw$totalCount)) {
      total <- raw$totalCount
      message(sprintf("Found %d studies, fetching...", total))
    }

    studies <- raw$studies
    if (is.null(studies) || length(studies) == 0) break
    all_studies <- c(all_studies, studies)

    token <- raw$nextPageToken
    if (is.null(token) || token == "") break

    # Build next page URL
    base <- sub("[&?]pageToken=[^&]*", "", page_url)
    page_url <- paste0(base, "&pageToken=", utils::URLencode(token))

    if (i %% 10 == 0) message(sprintf("  ...fetched %d studies", length(all_studies)))
  }

  all_studies
}

# -- Study flattener -----------------------------------------------------------
# Converts a single study (nested list) into a one-row tibble.

.flatten_study <- function(study) {
  tryCatch(.flatten_study_inner(study), error = function(e) {
    # Return a minimal row if parsing fails for this study
    tibble(
      nct_id = .safe(study$protocolSection$identificationModule$nctId),
      org_study_id = NA_character_, title = .safe(study$protocolSection$identificationModule$briefTitle),
      official_title = NA_character_, acronym = NA_character_,
      status = .safe(study$protocolSection$statusModule$overallStatus),
      start_date = as.Date(NA), completion_date = as.Date(NA),
      last_update = as.Date(NA), study_type = NA_character_,
      phase = NA_character_, enrollment = NA_integer_, enrollment_type = NA_character_,
      brief_summary = NA_character_, conditions = NA_character_, keywords = NA_character_,
      intervention_names = NA_character_, intervention_types = NA_character_,
      sponsor = NA_character_, sponsor_type = NA_character_, n_collaborators = 0L,
      sex = NA_character_, min_age = NA_character_, max_age = NA_character_,
      healthy_volunteers = NA_character_, n_sites = 0L,
      countries = NA_character_, has_results = FALSE
    )
  })
}

.flatten_study_inner <- function(study) {
  ps <- study$protocolSection
  id <- ps$identificationModule
  status <- ps$statusModule
  desc <- ps$descriptionModule
  design <- ps$designModule
  cond <- ps$conditionsModule
  interv <- ps$armsInterventionsModule
  elig <- ps$eligibilityModule
  contact <- ps$contactsLocationsModule
  sponsor <- ps$sponsorCollaboratorsModule
  oversight <- ps$oversightModule

  # Interventions
  interventions <- interv$interventions
  interv_names <- if (!is.null(interventions))
    paste(vapply(interventions, function(i) .safe(i$name), character(1)), collapse = "; ")
  else NA_character_
  interv_types <- if (!is.null(interventions))
    paste(unique(vapply(interventions, function(i) .safe(i$type), character(1))), collapse = "; ")
  else NA_character_

  # Locations
  locations <- contact$locations
  n_sites <- if (!is.null(locations)) length(locations) else 0L
  countries <- if (!is.null(locations))
    paste(unique(vapply(locations, function(l) .safe(l$country), character(1))), collapse = "; ")
  else NA_character_

  # Phases
  phases <- design$phases
  phase_str <- if (!is.null(phases)) paste(phases, collapse = "; ") else NA_character_

  tibble(
    nct_id            = .safe(.deep(id, "nctId")),
    org_study_id      = .safe(.deep(id, "orgStudyIdInfo", "id")),
    title             = .safe(.deep(id, "briefTitle")),
    official_title    = .safe(.deep(id, "officialTitle")),
    acronym           = .safe(.deep(id, "acronym")),
    status            = .safe(.deep(status, "overallStatus")),
    start_date        = .safe_date(.deep(status, "startDateStruct", "date")),
    completion_date   = .safe_date(.deep(status, "completionDateStruct", "date")),
    last_update       = .safe_date(.deep(status, "lastUpdatePostDateStruct", "date")),
    study_type        = .safe(.deep(design, "studyType")),
    phase             = phase_str,
    enrollment        = .safe_int(.deep(design, "enrollmentInfo", "count")),
    enrollment_type   = .safe(.deep(design, "enrollmentInfo", "type")),
    brief_summary     = .safe(.deep(desc, "briefSummary")),
    conditions        = .safe(.deep(cond, "conditions")),
    keywords          = .safe(.deep(cond, "keywords")),
    intervention_names = interv_names,
    intervention_types = interv_types,
    sponsor           = .safe(.deep(sponsor, "leadSponsor", "name")),
    sponsor_type      = .safe(.deep(sponsor, "leadSponsor", "class")),
    n_collaborators   = if (!is.null(.deep(sponsor, "collaborators"))) length(sponsor$collaborators) else 0L,
    sex               = .safe(.deep(elig, "sex")),
    min_age           = .safe(.deep(elig, "minimumAge")),
    max_age           = .safe(.deep(elig, "maximumAge")),
    healthy_volunteers = .safe(.deep(elig, "healthyVolunteers")),
    n_sites           = as.integer(n_sites),
    countries         = countries,
    has_results       = study$hasResults %||% FALSE
  )
}


# == Schemas ===================================================================

.schema_studies <- tibble(
  nct_id = character(), org_study_id = character(), title = character(),
  official_title = character(), acronym = character(), status = character(),
  start_date = as.Date(character()), completion_date = as.Date(character()),
  last_update = as.Date(character()), study_type = character(),
  phase = character(), enrollment = integer(), enrollment_type = character(),
  brief_summary = character(), conditions = character(), keywords = character(),
  intervention_names = character(), intervention_types = character(),
  sponsor = character(), sponsor_type = character(), n_collaborators = integer(),
  sex = character(), min_age = character(), max_age = character(),
  healthy_volunteers = character(), n_sites = integer(),
  countries = character(), has_results = logical()
)

.schema_outcomes <- tibble(
  nct_id = character(), group_id = character(), group_title = character(),
  measure_title = character(), measure_type = character(),
  time_frame = character(), value = character(), spread = character(),
  unit = character()
)

.schema_adverse <- tibble(
  nct_id = character(), group_id = character(), group_title = character(),
  event_term = character(), event_type = character(),
  organ_system = character(), affected_n = integer(), at_risk_n = integer()
)

.schema_stats <- tibble(
  metric = character(), value = integer()
)



# == Study search ==============================================================

#' Search ClinicalTrials.gov studies
#'
#' Full-text search of the ClinicalTrials.gov registry with rich filters.
#' Returns flattened study summaries as a tibble. Handles pagination
#' transparently via the v2 API.
#'
#' @param condition Character or NULL. Disease or condition keyword.
#'   Example: \code{"lung cancer"}, \code{"diabetes"}, \code{"alzheimer"}
#' @param intervention Character or NULL. Treatment or intervention keyword.
#'   Example: \code{"pembrolizumab"}, \code{"aspirin"}, \code{"surgery"}
#' @param term Character or NULL. General free-text search across all fields.
#' @param sponsor Character or NULL. Sponsor name filter.
#'   Example: \code{"Pfizer"}, \code{"National Cancer Institute"}
#' @param status Character or NULL. Overall study status. Comma-separate for
#'   multiple. Valid values: \code{"RECRUITING"}, \code{"COMPLETED"},
#'   \code{"ACTIVE_NOT_RECRUITING"}, \code{"NOT_YET_RECRUITING"},
#'   \code{"TERMINATED"}, \code{"WITHDRAWN"}, \code{"SUSPENDED"},
#'   \code{"ENROLLING_BY_INVITATION"}, \code{"UNKNOWN"}.
#' @param phase Character or NULL. Study phase. Comma-separate for multiple.
#'   Valid values: \code{"EARLY_PHASE1"}, \code{"PHASE1"}, \code{"PHASE2"},
#'   \code{"PHASE3"}, \code{"PHASE4"}, \code{"NA"} (not applicable).
#' @param study_type Character or NULL. Type of study.
#'   Valid values: \code{"INTERVENTIONAL"}, \code{"OBSERVATIONAL"},
#'   \code{"EXPANDED_ACCESS"}.
#' @param sort Character. Sort order (default \code{"LastUpdatePostDate:desc"}).
#'   Options: \code{"LastUpdatePostDate"}, \code{"EnrollmentCount"},
#'   \code{"StartDate"}, \code{"StudyFirstPostDate"}. Append \code{":asc"} or
#'   \code{":desc"}.
#' @param max_results Integer or NULL. Maximum studies to return (default 100).
#'   Set \code{NULL} to fetch all matching studies.
#' @return A tibble with one row per study and 28 columns:
#'   \describe{
#'     \item{nct_id}{Character. NCT identifier (e.g. "NCT04852770").}
#'     \item{org_study_id}{Character. Organization study ID.}
#'     \item{title}{Character. Brief study title.}
#'     \item{official_title}{Character. Full official title.}
#'     \item{acronym}{Character. Study acronym.}
#'     \item{status}{Character. Overall status (e.g. "RECRUITING").}
#'     \item{start_date}{Date. Study start date.}
#'     \item{completion_date}{Date. Estimated or actual completion date.}
#'     \item{last_update}{Date. Last update post date.}
#'     \item{study_type}{Character. "INTERVENTIONAL" or "OBSERVATIONAL".}
#'     \item{phase}{Character. Semicolon-delimited phases.}
#'     \item{enrollment}{Integer. Target or actual enrollment count.}
#'     \item{enrollment_type}{Character. "ESTIMATED" or "ACTUAL".}
#'     \item{brief_summary}{Character. Plain-text study summary.}
#'     \item{conditions}{Character. Semicolon-delimited conditions.}
#'     \item{keywords}{Character. Semicolon-delimited keywords.}
#'     \item{intervention_names}{Character. Semicolon-delimited intervention names.}
#'     \item{intervention_types}{Character. Semicolon-delimited intervention types.}
#'     \item{sponsor}{Character. Lead sponsor organization name.}
#'     \item{sponsor_type}{Character. "INDUSTRY", "OTHER", "FED", "NETWORK", etc.}
#'     \item{n_collaborators}{Integer. Number of collaborating organizations.}
#'     \item{sex}{Character. Eligible sex: "ALL", "FEMALE", "MALE".}
#'     \item{min_age}{Character. Minimum age (e.g. "18 Years").}
#'     \item{max_age}{Character. Maximum age (e.g. "65 Years").}
#'     \item{healthy_volunteers}{Character. "true" or "false".}
#'     \item{n_sites}{Integer. Number of study locations.}
#'     \item{countries}{Character. Semicolon-delimited country list.}
#'     \item{has_results}{Logical. Whether results have been posted.}
#'   }
#' @examples
#' ct_studies(condition = "diabetes", max_results = 5)
#' ct_studies(condition = "lung cancer", status = "RECRUITING", phase = "PHASE3")
#' ct_studies(sponsor = "Pfizer", max_results = 10)
ct_studies <- function(condition = NULL, intervention = NULL, term = NULL,
                       sponsor = NULL, status = NULL, phase = NULL,
                       study_type = NULL, sort = "LastUpdatePostDate:desc",
                       max_results = 100) {
  params <- list()
  if (!is.null(condition))    params[["query.cond"]] <- condition
  if (!is.null(intervention)) params[["query.intr"]] <- intervention
  if (!is.null(term))         params[["query.term"]] <- term
  if (!is.null(sponsor))      params[["query.spons"]] <- sponsor
  if (!is.null(status))       params[["filter.overallStatus"]] <- status
  if (!is.null(sort))         params[["sort"]] <- sort

  # Phase and study_type use AREA[] syntax via filter.advanced
  advanced_parts <- character()
  if (!is.null(phase))      advanced_parts <- c(advanced_parts, paste0("AREA[Phase]", phase))
  if (!is.null(study_type)) advanced_parts <- c(advanced_parts, paste0("AREA[StudyType]", study_type))
  if (length(advanced_parts) > 0)
    params[["filter.advanced"]] <- paste(advanced_parts, collapse = " AND ")

  query <- paste(names(params),
                 vapply(params, function(v) utils::URLencode(v, reserved = FALSE), character(1)),
                 sep = "=", collapse = "&")
  url <- paste0(.ct_base, "/studies?", query)

  page_size <- min(max_results %||% 1000, 1000)
  max_pages <- if (!is.null(max_results)) ceiling(max_results / page_size) else 100

  raw_studies <- .ct_fetch_all(url, max_pages = max_pages, page_size = page_size)
  if (length(raw_studies) == 0) return(.schema_studies)

  # Trim to max_results
  if (!is.null(max_results) && length(raw_studies) > max_results)
    raw_studies <- raw_studies[seq_len(max_results)]

  bind_rows(lapply(raw_studies, .flatten_study))
}


# == Individual study ==========================================================

#' Fetch a single study by NCT ID
#'
#' Returns the full flattened study record for one clinical trial.
#'
#' @param nct_id Character. NCT identifier string.
#'   Example: \code{"NCT04852770"}, \code{"NCT00000620"}
#' @return A tibble with one row and 28 columns (same schema as
#'   \code{\link{ct_studies}}). See \code{ct_studies()} for column details.
#' @examples
#' ct_study("NCT04852770")
ct_study <- function(nct_id) {
  url <- sprintf("%s/studies/%s", .ct_base, nct_id)
  raw <- .fetch_json(url)
  if (is.null(raw)) return(.schema_studies)
  .flatten_study(raw)
}


# == Study results =============================================================

#' Fetch outcome measures from a study's results
#'
#' Returns parsed outcome measure data from a completed study that has
#' posted results. Studies without results return an empty tibble.
#'
#' @param nct_id Character. NCT identifier.
#'   Example: \code{"NCT00000620"}
#' @return A tibble with one row per measurement and 9 columns:
#'   \describe{
#'     \item{nct_id}{Character. NCT identifier.}
#'     \item{group_id}{Character. Arm/group ID (e.g. "OG000").}
#'     \item{group_title}{Character. Arm/group label (e.g. "Placebo").}
#'     \item{measure_title}{Character. Outcome measure name.}
#'     \item{measure_type}{Character. "PRIMARY" or "SECONDARY".}
#'     \item{time_frame}{Character. Measurement time frame.}
#'     \item{value}{Character. Measured value.}
#'     \item{spread}{Character. Spread/dispersion value.}
#'     \item{unit}{Character. Unit of measure (e.g. "mg/dL").}
#'   }
#' @examples
#' ct_results("NCT00000620")
ct_results <- function(nct_id) {
  url <- sprintf("%s/studies/%s", .ct_base, nct_id)
  raw <- .fetch_json(url)
  rs <- raw$resultsSection
  if (is.null(rs)) return(.schema_outcomes)

  outcomes <- rs$outcomeMeasuresModule$outcomeMeasures
  if (is.null(outcomes) || length(outcomes) == 0) return(.schema_outcomes)

  bind_rows(lapply(outcomes, function(om) {
    groups <- om$groups
    classes <- om$classes

    if (is.null(classes) || length(classes) == 0) return(NULL)

    bind_rows(lapply(classes, function(cls) {
      cats <- cls$categories
      if (is.null(cats)) return(NULL)

      bind_rows(lapply(cats, function(cat) {
        measurements <- cat$measurements
        if (is.null(measurements)) return(NULL)

        bind_rows(lapply(measurements, function(m) {
          gid <- m$groupId %||% NA_character_
          # Find group title
          gtitle <- NA_character_
          if (!is.null(groups)) {
            for (g in groups) {
              if (!is.null(g$id) && g$id == gid) {
                gtitle <- g$title
                break
              }
            }
          }

          tibble(
            nct_id        = nct_id,
            group_id      = gid,
            group_title   = gtitle,
            measure_title = .safe(om$title),
            measure_type  = .safe(om$type),
            time_frame    = .safe(om$timeFrame),
            value         = .safe(m$value),
            spread        = .safe(m$spread),
            unit          = .safe(om$unitOfMeasure)
          )
        }))
      }))
    }))
  }))
}


#' Fetch adverse events from a study's results
#'
#' Returns serious and other adverse events reported in a completed study's
#' results section. Studies without results return an empty tibble.
#'
#' @param nct_id Character. NCT identifier.
#'   Example: \code{"NCT00000620"}
#' @return A tibble with one row per event-group combination and 8 columns:
#'   \describe{
#'     \item{nct_id}{Character. NCT identifier.}
#'     \item{group_id}{Character. Arm/group ID (e.g. "EG000").}
#'     \item{group_title}{Character. Arm/group label.}
#'     \item{event_term}{Character. Adverse event term (e.g. "Headache").}
#'     \item{event_type}{Character. \code{"serious"} or \code{"other"}.}
#'     \item{organ_system}{Character. MedDRA organ system class.}
#'     \item{affected_n}{Integer. Number of participants affected.}
#'     \item{at_risk_n}{Integer. Number of participants at risk.}
#'   }
#' @examples
#' ct_adverse_events("NCT00000620")
ct_adverse_events <- function(nct_id) {
  url <- sprintf("%s/studies/%s", .ct_base, nct_id)
  raw <- .fetch_json(url)
  rs <- raw$resultsSection
  if (is.null(rs)) return(.schema_adverse)

  ae <- rs$adverseEventsModule
  if (is.null(ae)) return(.schema_adverse)

  groups <- ae$eventGroups

  parse_events <- function(events, event_type) {
    if (is.null(events) || length(events) == 0) return(NULL)
    bind_rows(lapply(events, function(ev) {
      stats <- ev$stats
      if (is.null(stats)) return(NULL)
      bind_rows(lapply(stats, function(s) {
        gid <- s$groupId %||% NA_character_
        gtitle <- NA_character_
        if (!is.null(groups)) {
          for (g in groups) {
            if (!is.null(g$id) && g$id == gid) {
              gtitle <- g$title
              break
            }
          }
        }
        tibble(
          nct_id       = nct_id,
          group_id     = gid,
          group_title  = gtitle,
          event_term   = .safe(ev$term),
          event_type   = event_type,
          organ_system = .safe(ev$organSystem),
          affected_n   = .safe_int(s$numAffected),
          at_risk_n    = .safe_int(s$numAtRisk)
        )
      }))
    }))
  }

  bind_rows(
    parse_events(ae$seriousEvents, "serious"),
    parse_events(ae$otherEvents, "other")
  )
}


# == Database statistics =======================================================

#' Fetch ClinicalTrials.gov database-wide statistics
#'
#' Returns aggregate counts for the entire ClinicalTrials.gov database.
#'
#' @return A tibble with 2 rows and 2 columns:
#'   \describe{
#'     \item{metric}{Character. Metric name: \code{"totalStudies"} or
#'       \code{"averageSizeBytes"}.}
#'     \item{value}{Integer. Metric value (e.g. 579013 total studies).}
#'   }
#' @examples
#' ct_stats()
ct_stats <- function() {
  raw <- .fetch_json(paste0(.ct_base, "/stats/size"))
  if (is.null(raw)) return(.schema_stats)

  tibble(
    metric = c("totalStudies", "averageSizeBytes"),
    value = c(.safe_int(raw$totalStudies), .safe_int(raw$averageSizeBytes))
  )
}

#' Fetch value distributions for multiple fields at once
#'
#' Convenience wrapper around \code{\link{ct_field_values}} that queries
#' several fields and combines results into one tibble.
#'
#' @param fields Character vector of ClinicalTrials.gov field names.
#'   Default: \code{c("OverallStatus", "Phase", "StudyType")}.
#'   Other useful fields: \code{"InterventionType"}, \code{"LeadSponsorName"},
#'   \code{"LocationCountry"}.
#' @return A tibble with 3 columns:
#'   \describe{
#'     \item{field}{Character. Field name that was queried.}
#'     \item{value}{Character. Distinct value of the field (e.g. "RECRUITING").}
#'     \item{count}{Integer. Number of studies with that value.}
#'   }
#' @examples
#' ct_stats_fields()
#' ct_stats_fields(fields = c("Phase", "InterventionType"))
ct_stats_fields <- function(fields = c("OverallStatus", "Phase", "StudyType")) {
  bind_rows(lapply(fields, function(f) {
    df <- ct_field_values(f)
    if (nrow(df) > 0) df$field <- f
    df
  })) |> select(field, value, count)
}


# == Field value discovery =====================================================

#' Discover available values for a field
#'
#' Returns the distribution of values for a given study field, sorted by
#' count descending. Useful for discovering valid filter values.
#'
#' @param field Character. ClinicalTrials.gov field name.
#'   Common values: \code{"OverallStatus"}, \code{"Phase"},
#'   \code{"StudyType"}, \code{"InterventionType"},
#'   \code{"LeadSponsorName"}, \code{"LocationCountry"}.
#' @return A tibble with 2 columns:
#'   \describe{
#'     \item{value}{Character. Distinct field value (e.g. "COMPLETED").}
#'     \item{count}{Integer. Number of studies with that value.}
#'   }
#' @examples
#' ct_field_values("OverallStatus")
#' ct_field_values("Phase")
ct_field_values <- function(field) {
  url <- sprintf("%s/stats/field/values?fields=%s", .ct_base, utils::URLencode(field))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0)
    return(tibble(value = character(), count = integer()))

  entry <- raw[[1]]
  top <- entry$topValues
  if (is.null(top) || length(top) == 0)
    return(tibble(value = character(), count = integer()))

  bind_rows(lapply(top, function(v) {
    tibble(value = .safe(v$value), count = .safe_int(v$count))
  })) |> arrange(desc(count))
}


# == Context ===================================================================

#' Get clinicaltrials.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ct_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ct_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/clinicaltrials.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "clinicaltrials.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# clinicaltrials.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# clinicaltrials.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
