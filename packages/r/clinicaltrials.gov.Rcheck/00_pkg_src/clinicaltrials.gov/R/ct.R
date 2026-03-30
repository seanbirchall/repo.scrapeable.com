# == Study search ==============================================================

#' Search ClinicalTrials.gov studies
#'
#' Full-text search with filters. Returns flattened study summaries.
#' Handles pagination transparently.
#'
#' @param condition Disease/condition (e.g. "lung cancer", "diabetes")
#' @param intervention Treatment (e.g. "pembrolizumab", "aspirin")
#' @param term General keyword search
#' @param sponsor Sponsor name
#' @param status Filter by status: "RECRUITING", "COMPLETED",
#'   "ACTIVE_NOT_RECRUITING", "NOT_YET_RECRUITING", etc.
#'   Comma-separated for multiple.
#' @param phase Filter by phase: "EARLY_PHASE1", "PHASE1", "PHASE2",
#'   "PHASE3", "PHASE4". Comma-separated for multiple.
#' @param study_type "INTERVENTIONAL", "OBSERVATIONAL", "EXPANDED_ACCESS"
#' @param sort Sort order. Default: "LastUpdatePostDate:desc".
#'   Options: "LastUpdatePostDate", "EnrollmentCount", "StartDate", "StudyFirstPostDate"
#' @param max_results Maximum studies to return (default 100). Set NULL for all.
#' @return tibble: one row per study with 27 columns
#' @export
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
#' Returns the full flattened study record.
#'
#' @param nct_id NCT identifier (e.g. "NCT04852770")
#' @return tibble: one row with 27 columns (same as ct_studies)
#' @export
ct_study <- function(nct_id) {
  url <- sprintf("%s/studies/%s", .ct_base, nct_id)
  raw <- .fetch_json(url)
  if (is.null(raw)) return(.schema_studies)
  .flatten_study(raw)
}


# == Study results =============================================================

#' Fetch outcome measures from a study's results
#'
#' @param nct_id NCT identifier
#' @return tibble: group_id, group_title, measure_title, measure_type,
#'   time_frame, value, spread, unit
#' @export
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
#' @param nct_id NCT identifier
#' @return tibble: group_id, group_title, event_term, event_type,
#'   organ_system, affected_n, at_risk_n
#' @export
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
#' @return tibble: metric, value
#' @export
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
#' Convenience wrapper around ct_field_values for common fields.
#'
#' @param fields Character vector of field names. Default: status, phase, study type.
#' @return tibble: field, value, count
#' @export
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
#' Returns the distribution of values for a given study field.
#'
#' @param field Field name (e.g. "OverallStatus", "Phase",
#'   "StudyType", "InterventionType", "LeadSponsorName")
#' @return tibble: value, count
#' @export
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


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the clinicaltrials.gov package
#'
#' @param compact If TRUE (default), concise output
#' @return Character string (invisibly), also printed
#' @export
ct_context <- function(compact = TRUE) {
  fns <- list(
    list("ct_studies", "(condition, intervention, term, sponsor, status, phase, study_type, sort, max_results)",
         "Search studies. Returns flattened tibble (27 cols). Handles pagination."),
    list("ct_study", "(nct_id)",
         "Fetch single study by NCT ID. Same 27-column schema as ct_studies."),
    list("ct_results", "(nct_id)",
         "Outcome measures from a study's results section."),
    list("ct_adverse_events", "(nct_id)",
         "Adverse events (serious + other) from a study's results."),
    list("ct_stats", "()",
         "Database-wide statistics (total studies, results, etc.)."),
    list("ct_stats_fields", "(fields)",
         "Value distributions for multiple fields (default: status, phase, type)."),
    list("ct_field_values", "(field)",
         "Value distribution for a field (e.g. 'OverallStatus', 'Phase').")
  )

  lines <- c(
    "# clinicaltrials.gov - ClinicalTrials.gov API v2 Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (public API)",
    "# Rate limit: ~50 requests/minute",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Status values: RECRUITING, COMPLETED, ACTIVE_NOT_RECRUITING,",
    "#   NOT_YET_RECRUITING, TERMINATED, WITHDRAWN, SUSPENDED",
    "# Phase values: EARLY_PHASE1, PHASE1, PHASE2, PHASE3, PHASE4",
    "# Study types: INTERVENTIONAL, OBSERVATIONAL, EXPANDED_ACCESS",
    "#",
    "# == Functions ==",
    "#"
  )

  for (fn in fns) {
    lines <- c(lines,
      sprintf("# %s%s", fn[[1]], fn[[2]]),
      sprintf("#   %s", fn[[3]]),
      sprintf("#   Run `%s` to view source or `?%s` for help.", fn[[1]], fn[[1]]),
      "#"
    )
  }

  lines <- c(lines,
    "# == Quick examples ==",
    "#",
    "# ct_studies(condition = 'lung cancer', status = 'RECRUITING', max_results = 20)",
    "# ct_studies(intervention = 'pembrolizumab', phase = 'PHASE3')",
    "# ct_study('NCT04852770')",
    "# ct_results('NCT04852770')  # outcome measures",
    "# ct_adverse_events('NCT04852770')",
    "# ct_stats()  # database totals",
    "# ct_field_values('Phase')  # see all phase values + counts",
    "#"
  )

  out <- paste(lines, collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
