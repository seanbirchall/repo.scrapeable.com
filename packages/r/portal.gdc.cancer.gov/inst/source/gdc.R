# gdc.R
# Self-contained NCI Genomic Data Commons API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: generous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gdc_base <- "https://api.gdc.cancer.gov"

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
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
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_cases <- tibble(
  id = character(), submitter_id = character(), project_id = character(),
  primary_site = character(), disease_type = character(), state = character()
)

.schema_projects <- tibble(
  id = character(), name = character(), project_id = character(),
  primary_site = character(), disease_type = character(),
  program_name = character(), summary_case_count = integer(),
  summary_file_count = integer()
)

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

`%||%` <- function(x, y) if (is.null(x)) y else x
