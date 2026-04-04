# portal.gdc.cancer.gov.R - Self-contained portal.gdc.cancer.gov client



# gdc.R
# Self-contained NCI Genomic Data Commons API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: generous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gdc_base <- "https://api.gdc.cancer.gov"

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


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Search GDC cases (cancer patient records)
#'
#' Queries the NCI Genomic Data Commons API for case records. Each case
#' represents a patient enrolled in a cancer genomics project. Filter by
#' project or anatomical site to narrow results. Supports pagination for
#' large result sets.
#'
#' @param size Integer. Number of results per request (default 10, max 10000).
#' @param from Integer. Offset for pagination (default 0). Use with \code{size}
#'   to page through results.
#' @param project_id Character. Filter by GDC project identifier. Common values:
#'   \code{"TCGA-BRCA"} (breast), \code{"TCGA-LUAD"} (lung adenocarcinoma),
#'   \code{"TCGA-LGG"} (lower grade glioma), \code{"HCMI-CMDC"} (cancer models),
#'   \code{"CPTAC-3"} (proteogenomics).
#' @param primary_site Character. Filter by anatomical primary site. Examples:
#'   \code{"Breast"}, \code{"Lung"}, \code{"Brain"}, \code{"Colon"},
#'   \code{"Kidney"}, \code{"Ovary"}, \code{"Bladder"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- GDC case UUID}
#'     \item{submitter_id}{character -- Submitter-assigned case ID
#'       (e.g. "HCM-WCMC-0909-C43")}
#'     \item{project_id}{character -- Project identifier (e.g. "TCGA-BRCA")}
#'     \item{primary_site}{character -- Anatomical site (e.g. "Breast")}
#'     \item{disease_type}{character -- Disease classification (e.g.
#'       "Adenomas and Adenocarcinomas")}
#'     \item{state}{character -- Case state (e.g. "released")}
#'   }
#' @examples
#' gdc_cases(size = 5)
#' gdc_cases(project_id = "TCGA-BRCA", size = 10)
#' gdc_cases(primary_site = "Lung", size = 20)
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
#' Returns metadata for cancer genomics projects in the GDC. Each project
#' belongs to a program (TCGA, CPTAC, HCMI, etc.) and contains cases and
#' files. Use this to discover available projects before querying cases.
#'
#' @param size Integer. Number of projects to return (default 20, max 10000).
#' @param from Integer. Offset for pagination (default 0).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- Project UUID}
#'     \item{name}{character -- Full project name (e.g.
#'       "Brain Lower Grade Glioma")}
#'     \item{project_id}{character -- Short project ID (e.g. "TCGA-LGG")}
#'     \item{primary_site}{character -- Semicolon-separated anatomical sites
#'       (e.g. "Brain", "Ovary; Hematopoietic")}
#'     \item{disease_type}{character -- Semicolon-separated disease
#'       classifications}
#'     \item{program_name}{character -- Parent program (e.g. "TCGA", "CPTAC",
#'       "HCMI")}
#'     \item{summary_case_count}{integer -- Number of cases in project}
#'     \item{summary_file_count}{integer -- Number of files in project}
#'   }
#' @examples
#' gdc_projects(size = 10)
#' gdc_projects(size = 50, from = 0)
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

# == Context ===================================================================

#' Get cancer.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gdc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gdc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/cancer.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "cancer.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# cancer.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# cancer.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
