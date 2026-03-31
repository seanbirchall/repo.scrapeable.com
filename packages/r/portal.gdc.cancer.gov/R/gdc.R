# == Public functions ==========================================================

#' Search GDC cases (cancer patient records)
#'
#' @param size Number of results (default 10, max 10000)
#' @param from Offset for pagination (default 0)
#' @param project_id Filter by project (e.g. "TCGA-BRCA", "TCGA-LUAD")
#' @param primary_site Filter by primary site (e.g. "Breast", "Lung")
#' @return tibble: id, submitter_id, project_id, primary_site, disease_type, state
#' @export
gdc_cases <- function(size = 10, from = 0, project_id = NULL, primary_site = NULL) {
  filters <- NULL
  content <- list()
  if (!is.null(project_id)) {
    content[[length(content) + 1]] <- list(
      op = "in", content = list(field = "project.project_id", value = list(project_id))
    )
  }
  if (!is.null(primary_site)) {
    content[[length(content) + 1]] <- list(
      op = "in", content = list(field = "primary_site", value = list(primary_site))
    )
  }
  if (length(content) > 0) {
    filters <- jsonlite::toJSON(list(op = "and", content = content), auto_unbox = TRUE)
  }

  params <- list(
    size = size, from = from,
    fields = "submitter_id,project.project_id,primary_site,disease_type,state"
  )
  if (!is.null(filters)) params$filters <- filters
  params <- params[!vapply(params, is.null, logical(1))]
  qstr <- paste(names(params), vapply(params, as.character, character(1)), sep = "=", collapse = "&")
  url <- paste0(.gdc_base, "/cases?", qstr)

  raw <- .fetch_json(url)
  hits <- raw$data$hits
  if (is.null(hits) || length(hits) == 0 ||
      (is.data.frame(hits) && nrow(hits) == 0)) return(.schema_cases)

  tibble(
    id = as.character(hits$id),
    submitter_id = as.character(hits$submitter_id %||% NA_character_),
    project_id = as.character(
      if (!is.null(hits$project) && !is.null(hits$project$project_id))
        hits$project$project_id else NA_character_
    ),
    primary_site = as.character(hits$primary_site %||% NA_character_),
    disease_type = as.character(hits$disease_type %||% NA_character_),
    state = as.character(hits$state %||% NA_character_)
  )
}

#' List GDC projects
#'
#' @param size Number of results (default 20, max 10000)
#' @param from Offset for pagination (default 0)
#' @return tibble: id, name, project_id, primary_site, disease_type,
#'   program_name, summary_case_count, summary_file_count
#' @export
gdc_projects <- function(size = 20, from = 0) {
  url <- sprintf(
    "%s/projects?size=%d&from=%d&fields=name,project_id,primary_site,disease_type,program.name,summary.case_count,summary.file_count",
    .gdc_base, as.integer(size), as.integer(from)
  )
  raw <- .fetch_json(url)
  hits <- raw$data$hits
  if (is.null(hits) || length(hits) == 0 ||
      (is.data.frame(hits) && nrow(hits) == 0)) return(.schema_projects)

  tibble(
    id = as.character(hits$id),
    name = as.character(hits$name %||% NA_character_),
    project_id = as.character(hits$project_id %||% NA_character_),
    primary_site = vapply(hits$primary_site, function(x) paste(x, collapse = "; "), character(1)),
    disease_type = vapply(hits$disease_type, function(x) paste(x, collapse = "; "), character(1)),
    program_name = as.character(
      if (!is.null(hits$program) && !is.null(hits$program$name)) hits$program$name
      else NA_character_
    ),
    summary_case_count = as.integer(
      if (!is.null(hits$summary) && !is.null(hits$summary$case_count)) hits$summary$case_count
      else NA_integer_
    ),
    summary_file_count = as.integer(
      if (!is.null(hits$summary) && !is.null(hits$summary$file_count)) hits$summary$file_count
      else NA_integer_
    )
  )
}

#' Show GDC client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
gdc_context <- function() {
  .build_context(
    pkg_name = "portal.gdc.cancer.gov",
    header_lines = c(
      "# portal.gdc.cancer.gov -- NCI Genomic Data Commons Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required for public data",
      "# Popular projects: TCGA-BRCA, TCGA-LUAD, TCGA-OV, TCGA-GBM",
      "# Primary sites: Breast, Lung, Ovary, Brain, Kidney, Liver"
    )
  )
}

