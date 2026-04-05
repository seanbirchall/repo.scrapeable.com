# githubusercontent.com.R - Self-contained GitHub raw content client
# Fetches open government datasets hosted on raw.githubusercontent.com
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: standard GitHub rate limits

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ghraw_base <- "https://raw.githubusercontent.com"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

.fetch_csv <- function(url) {
  tmp <- .fetch(url, ext = ".csv")
  utils::read.csv(tmp, stringsAsFactors = FALSE) |> tibble::as_tibble()
}

.safe_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}

# == Schemas ===================================================================

.schema_nasa_project <- tibble(
  name = character(), description = character(),
  organization = character(), repositoryURL = character(),
  homepageURL = character(), tags = character(),
  languages = character(), metadataLastUpdated = as.Date(character())
)

.schema_gsa_standard <- tibble(
  id = character(), name = character(), category = character(),
  status = character(), description = character(),
  deployment_type = character()
)

# == NASA Open Source Catalog ==================================================

#' List NASA open-source software projects from code.nasa.gov
#'
#' Fetches the full NASA Open Source Catalog (\code{code.json}) from GitHub
#' and returns it as a searchable tibble. Contains thousands of software
#' projects from across NASA centers.
#'
#' @param search Character or \code{NULL}. Optional case-insensitive search
#'   term to filter by project name or description (e.g. \code{"mars"},
#'   \code{"climate"}).
#' @param organization Character or \code{NULL}. Optional NASA center code
#'   to filter results (e.g. \code{"JPL"}, \code{"GSFC"}, \code{"ARC"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Project name.}
#'     \item{description}{Character. Project description.}
#'     \item{organization}{Character. NASA center code (e.g. \code{"JPL"}).}
#'     \item{repositoryURL}{Character. Source code repository URL.}
#'     \item{homepageURL}{Character. Project homepage URL.}
#'     \item{tags}{Character. Semicolon-separated keyword tags.}
#'     \item{languages}{Character. Semicolon-separated programming languages.}
#'     \item{metadataLastUpdated}{Date. When metadata was last refreshed.}
#'   }
#' @examples
#' ghraw_nasa_projects(search = "mars")
#' ghraw_nasa_projects(organization = "JPL")
#' @export
ghraw_nasa_projects <- function(search = NULL, organization = NULL) {
  url <- paste0(.ghraw_base, "/nasa/Open-Source-Catalog/master/code.json")
  res <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(res) || is.null(res$releases) || length(res$releases) == 0)
    return(.schema_nasa_project)

  d <- res$releases

  out <- tibble(
    name = as.character(.safe_col(d, "name")),
    description = as.character(.safe_col(d, "description")),
    organization = as.character(.safe_col(d, "organization")),
    repositoryURL = as.character(.safe_col(d, "repositoryURL")),
    homepageURL = as.character(.safe_col(d, "homepageURL")),
    tags = vapply(seq_len(nrow(d)), function(i) {
      t <- d$tags
      if (is.list(t)) paste(t[[i]], collapse = "; ") else NA_character_
    }, character(1)),
    languages = vapply(seq_len(nrow(d)), function(i) {
      l <- d$languages
      if (is.list(l)) paste(l[[i]], collapse = "; ") else NA_character_
    }, character(1)),
    metadataLastUpdated = as.Date(
      vapply(seq_len(nrow(d)), function(i) {
        dt <- d$date
        if (is.data.frame(dt)) dt$metadataLastUpdated[i] %||% NA_character_
        else NA_character_
      }, character(1))
    )
  )

  if (!is.null(search)) {
    pat <- tolower(search)
    out <- out |> filter(
      grepl(pat, tolower(name), fixed = TRUE) |
      grepl(pat, tolower(description), fixed = TRUE)
    )
  }
  if (!is.null(organization)) {
    out <- out |> filter(toupper(.data$organization) == toupper(organization))
  }
  out
}

# == GSA IT Standards ==========================================================

#' Get GSA IT Standards Profile list
#'
#' Fetches the U.S. General Services Administration IT Standards inventory
#' from their GitHub data repository. May return an empty tibble if the
#' upstream file format has changed.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Standard identifier.}
#'     \item{name}{Character. Standard name.}
#'     \item{category}{Character. Technology category.}
#'     \item{status}{Character. Approval status.}
#'     \item{description}{Character. Standard description.}
#'     \item{deployment_type}{Character. Deployment classification.}
#'   }
#' @examples
#' ghraw_gsa_standards()
#' @export
ghraw_gsa_standards <- function() {
  # Note: the catalog URL says .csv but it's actually JSON
  url <- paste0(.ghraw_base, "/GSA/data/gh-pages/enterprise-architecture/it-standards.csv")
  res <- tryCatch({
    tmp <- .fetch(url, ext = ".json")
    txt <- readLines(tmp, warn = FALSE)
    jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = TRUE)
  }, error = function(e) {
    # Try as CSV fallback
    tryCatch(.fetch_csv(url), error = function(e2) NULL)
  })
  if (is.null(res) || length(res) == 0) return(.schema_gsa_standard)

  if (is.data.frame(res)) {
    tibble(
      id = as.character(.safe_col(res, "ID", .safe_col(res, "id"))),
      name = as.character(.safe_col(res, "Name", .safe_col(res, "name"))),
      category = as.character(.safe_col(res, "Category", .safe_col(res, "category"))),
      status = as.character(.safe_col(res, "Status", .safe_col(res, "status"))),
      description = as.character(.safe_col(res, "Description", .safe_col(res, "description"))),
      deployment_type = as.character(.safe_col(res, "DeploymentType",
        .safe_col(res, "deployment_type")))
    )
  } else .schema_gsa_standard
}

# == Philadelphia Contracts ====================================================

#' Get Philadelphia professional services contract data
#'
#' Fetches City of Philadelphia professional services contract data from
#' their open-data GitHub repository. Returns one quarter's worth of
#' contract records.
#'
#' @param fiscal_year Integer. Fiscal year (e.g. \code{2020}).
#' @param quarter Integer. Quarter number, 1 through 4 (default 4).
#' @return A tibble with columns (typical set):
#'   \describe{
#'     \item{original_contract_id}{Character. Contract identifier.}
#'     \item{current_item_id}{Character. Current item ID.}
#'     \item{department_name}{Character. City department name.}
#'     \item{vendor}{Character. Vendor/contractor name.}
#'     \item{contract_structure_type}{Character. Contract type.}
#'     \item{short_desc}{Character. Contract description.}
#'     \item{start_dt}{Character. Contract start date.}
#'     \item{end_dt}{Character. Contract end date.}
#'     \item{days_remaining}{Integer. Days remaining on contract.}
#'     \item{amt}{Character. Contract amount.}
#'     \item{tot_payments}{Character. Total payments made.}
#'   }
#' @examples
#' ghraw_philly_contracts(fiscal_year = 2020, quarter = 4)
#' @export
ghraw_philly_contracts <- function(fiscal_year = 2020, quarter = 4) {
  url <- sprintf(
    "%s/CityOfPhiladelphia/contracts/gh-pages/professional-services/data/FY-%d-Q%d.csv",
    .ghraw_base, fiscal_year, quarter
  )
  tryCatch(.fetch_csv(url), error = function(e) tibble())
}

# == Context ===================================================================

#' Get githubusercontent.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ghraw_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ghraw_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/githubusercontent.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "githubusercontent.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# githubusercontent.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# githubusercontent.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
