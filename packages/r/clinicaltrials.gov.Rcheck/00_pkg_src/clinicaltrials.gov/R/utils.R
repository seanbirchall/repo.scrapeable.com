#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

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


