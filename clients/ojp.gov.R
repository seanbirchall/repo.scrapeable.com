# ojp.gov.R
# Self-contained Office of Justice Programs (OJP) API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Docs: https://bjs.ojp.gov/national-crime-victimization-survey-ncvs-api
#       https://crimesolutions.ojp.gov/

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.ojp_api_base <- "https://api.ojp.gov"

# -- Core fetch engine (Socrata-style JSON API) --------------------------------

.ojp_fetch_json <- function(path, params = list(), limit = 1000, offset = 0) {
  params[["$limit"]] <- min(limit, 50000)
  if (offset > 0) params[["$offset"]] <- offset

  url <- paste0(.ojp_api_base, "/", path)
  tmp <- tempfile(fileext = ".json")
  resp <- httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning("OJP API returned HTTP ", status, " for ", path)
    return(list())
  }
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

# Paginated fetch returning tibble
.ojp_fetch_all <- function(path, params = list(), max_results = 1000) {
  all_rows <- list()
  offset <- 0
  page_size <- min(max_results, 50000)

  repeat {
    raw <- .ojp_fetch_json(path, params, limit = page_size, offset = offset)
    if (!is.data.frame(raw) && is.list(raw) && length(raw) == 0) break
    if (is.data.frame(raw)) {
      chunk <- as_tibble(raw)
    } else if (is.list(raw) && length(raw) > 0) {
      chunk <- tryCatch(bind_rows(lapply(raw, as_tibble)), error = function(e) tibble())
    } else {
      break
    }
    if (nrow(chunk) == 0) break
    all_rows[[length(all_rows) + 1]] <- chunk
    offset <- offset + nrow(chunk)
    if (offset >= max_results) break
    if (nrow(chunk) < page_size) break
  }

  if (length(all_rows) == 0) return(tibble())
  result <- bind_rows(all_rows)
  if (nrow(result) > max_results) result <- result[seq_len(max_results), ]
  result
}

# Fetch CSV endpoint
.ojp_fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning("OJP CSV returned HTTP ", status)
    return(tibble())
  }
  as_tibble(read.csv(tmp, stringsAsFactors = FALSE))
}

# == Dataset catalog ===========================================================

# Known datasets with metadata
.ojp_datasets <- tibble(
  id = c("ya4e-n9zp", "gcuy-rt5g", "98ww-3sks", "ak2j-ub9q", "esbe-p7rh"),
  name = c(
    "NCVS Household Population",
    "NCVS Personal Victimization",
    "PSRAC Risk Landscape",
    "Body Worn Camera Resources",
    "PSRAC Experts"
  ),
  type = c("bjsdataset", "bjsdataset", "ojpdataset", "ojpdataset", "ojpdataset"),
  description = c(
    "Demographic information of participating households in NCVS",
    "Personal crime victimizations from NCVS (rape, assault, robbery, theft)",
    "Risk assessment landscape by state for jail, parole, pretrial, prison, probation",
    "Body worn camera policy documents, research, and resources",
    "Public safety risk assessment experts directory"
  )
)

# == Discovery functions =======================================================

#' List available OJP API datasets
#'
#' Returns a catalog of known datasets available through the Office of Justice
#' Programs (OJP) and Bureau of Justice Statistics (BJS) Socrata-style APIs.
#' Each row describes a dataset by its four-by-four ID, name, source type,
#' and a brief description. Use the \code{id} column with \code{\link{ojp_data}}
#' for generic access.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{id}{Character. Socrata-style four-by-four dataset identifier.}
#'   \item{name}{Character. Human-readable dataset name.}
#'   \item{type}{Character. API endpoint type (\code{"bjsdataset"} or
#'     \code{"ojpdataset"}).}
#'   \item{description}{Character. Brief description of dataset contents.}
#' }
#'
#' @examples
#' \dontrun{
#' ojp_list()
#' }
#'
#' @export
ojp_list <- function() {
  .ojp_datasets
}

#' Search OJP datasets by keyword
#'
#' Performs a case-insensitive keyword search across dataset names and
#' descriptions in the OJP catalog. Returns all datasets if \code{query}
#' is missing or empty.
#'
#' @param query Character string. Keyword to match against dataset names and
#'   descriptions (case-insensitive). If \code{NULL} or empty, returns all
#'   datasets.
#'
#' @return A tibble of matching datasets with the same columns as
#'   \code{\link{ojp_list}}.
#'
#' @examples
#' \dontrun{
#' ojp_search("victimization")
#' ojp_search("risk")
#' }
#'
#' @seealso \code{\link{ojp_list}}
#' @export
ojp_search <- function(query) {
  if (missing(query) || is.null(query) || !nzchar(query)) return(.ojp_datasets)
  pattern <- tolower(query)
  .ojp_datasets |>
    filter(
      grepl(pattern, tolower(name)) | grepl(pattern, tolower(description))
    )
}

# == NCVS Household Population =================================================

#' Fetch NCVS household population data
#'
#' Returns demographic data from the National Crime Victimization Survey (NCVS)
#' household file. All sampled households are included regardless of whether a
#' victimization was reported. Records include head-of-household demographics,
#' income brackets, geographic region, and survey weights.
#'
#' @param year Character or numeric. Optional year filter (e.g. \code{"2020"}).
#'   When \code{NULL}, returns all available years.
#' @param max_results Integer. Maximum rows to return (default 1000). The API
#'   paginates internally in chunks of up to 50,000.
#'
#' @return A tibble with columns including:
#' \describe{
#'   \item{idhh}{Character. Household identifier.}
#'   \item{yearq}{Character. Year-quarter code.}
#'   \item{year}{Integer. Survey year.}
#'   \item{hhage}{Integer. Age of head of household.}
#'   \item{hhsex}{Integer. Sex of head of household (coded).}
#'   \item{wgthhcy}{Numeric. Household weight for population estimates.}
#' }
#' Additional columns vary by survey year (e.g. \code{hhhisp}, \code{hhrace},
#' \code{hincome1}, \code{region}, \code{msa}).
#'
#' @examples
#' \dontrun{
#' ojp_ncvs_household(year = "2020", max_results = 100)
#' }
#'
#' @seealso \code{\link{ojp_ncvs_victimization}} for personal victimization data.
#' @export
ojp_ncvs_household <- function(year = NULL, max_results = 1000) {
  params <- list()
  if (!is.null(year)) params[["$where"]] <- paste0("year='", year, "'")
  df <- .ojp_fetch_all("bjsdataset/v1/ya4e-n9zp.json", params, max_results)
  if (nrow(df) == 0) return(df)
  df |>
    mutate(
      year = as.integer(year),
      hhage = as.integer(hhage),
      hhsex = as.integer(hhsex),
      wgthhcy = as.numeric(wgthhcy)
    )
}

# == NCVS Personal Victimization ===============================================

#' Fetch NCVS personal victimization data
#'
#' Returns personal crime victimization records from the NCVS. Only persons
#' who reported a victimization event are included. Records cover violent
#' crimes (rape, assault, robbery) and personal theft, with coded offense
#' types, weapon presence, injury severity, and survey weights.
#'
#' @param year Character or numeric. Optional year filter (e.g. \code{"2020"}).
#'   When \code{NULL}, returns all available years.
#' @param max_results Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble with columns including:
#' \describe{
#'   \item{idper}{Character. Person identifier.}
#'   \item{yearq}{Character. Year-quarter code.}
#'   \item{year}{Integer. Survey year.}
#'   \item{ager}{Integer. Age of respondent (coded).}
#'   \item{sex}{Integer. Sex of respondent (coded).}
#'   \item{newcrime}{Character. Crime type code.}
#'   \item{seriousviolent}{Character. Serious violent crime indicator.}
#'   \item{weapon}{Character. Weapon presence code.}
#'   \item{injury}{Character. Injury indicator.}
#'   \item{wgtviccy}{Numeric. Victimization weight for population estimates.}
#' }
#' Additional columns vary by survey year.
#'
#' @examples
#' \dontrun{
#' ojp_ncvs_victimization(year = "2019", max_results = 100)
#' }
#'
#' @seealso \code{\link{ojp_ncvs_household}} for the household-level file.
#' @export
ojp_ncvs_victimization <- function(year = NULL, max_results = 1000) {
  params <- list()
  if (!is.null(year)) params[["$where"]] <- paste0("year='", year, "'")
  df <- .ojp_fetch_all("bjsdataset/v1/gcuy-rt5g.json", params, max_results)
  if (nrow(df) == 0) return(df)
  df |>
    mutate(
      year = as.integer(year),
      ager = as.integer(ager),
      sex = as.integer(sex),
      wgtviccy = as.numeric(wgtviccy)
    )
}

# == PSRAC Risk Landscape ======================================================

#' Fetch PSRAC risk assessment landscape by state
#'
#' Returns state-level data from the Public Safety Risk Assessment
#' Clearinghouse (PSRAC) describing which risk assessment tools are in use
#' across criminal justice decision points: jail, parole, pretrial, prison,
#' probation, and release. Covers all 50 states plus DC.
#'
#' @return A tibble with one row per state and the following columns:
#' \describe{
#'   \item{abbreviation}{Character. Two-letter state abbreviation.}
#'   \item{state}{Character. Full state name.}
#'   \item{region}{Character. Geographic region (e.g. "South", "West").}
#'   \item{jail}{Character. Risk tools used at the jail stage.}
#'   \item{parole}{Character. Risk tools used for parole decisions.}
#'   \item{pretrial}{Character. Risk tools used at pretrial.}
#'   \item{prison}{Character. Risk tools used in prison settings.}
#'   \item{probation}{Character. Risk tools used for probation.}
#'   \item{release}{Character. Risk tools used at release.}
#' }
#'
#' @examples
#' \dontrun{
#' ojp_risk_landscape()
#' ojp_risk_landscape() |> dplyr::filter(region == "West")
#' }
#'
#' @export
ojp_risk_landscape <- function() {
  .ojp_fetch_all("ojpdataset/v1/98ww-3sks.json", max_results = 100)
}

# == Body Worn Camera Resources ================================================

#' Fetch body worn camera resources
#'
#' Returns body-worn camera (BWC) policy documents, research papers, and
#' resources curated by the Bureau of Justice Assistance (BJA). Includes
#' document titles, authors, publication dates, keywords, and download URLs.
#'
#' @param state Character. Optional two-letter state abbreviation to filter
#'   results by location (e.g. \code{"CA"}).
#' @param max_results Integer. Maximum rows to return (default 500).
#'
#' @return A tibble with columns including:
#' \describe{
#'   \item{author}{Character. Document author(s).}
#'   \item{docdate}{Date. Publication date.}
#'   \item{documenttitle}{Character. Title of the resource.}
#'   \item{keywords}{Character. Associated keywords.}
#'   \item{state}{Character. State where the resource applies.}
#'   \item{resourcetype}{Character. Type of resource (policy, research, etc.).}
#'   \item{url}{Character. Download or access URL.}
#' }
#'
#' @examples
#' \dontrun{
#' ojp_bwc_resources()
#' ojp_bwc_resources(state = "TX")
#' }
#'
#' @export
ojp_bwc_resources <- function(state = NULL, max_results = 500) {
  params <- list()
  if (!is.null(state)) params[["$where"]] <- paste0("state='", state, "'")
  df <- .ojp_fetch_all("ojpdataset/v1/ak2j-ub9q.json", params, max_results)
  if (nrow(df) == 0) return(df)
  df |>
    mutate(
      docdate = as.Date(sub("T.*", "", docdate)),
      url = if ("url.url" %in% names(df)) url.url else NA_character_
    ) |>
    select(-any_of("url.url"))
}

# == PSRAC Experts =============================================================

#' Fetch PSRAC risk assessment experts
#'
#' Returns a directory of experts registered with the Public Safety Risk
#' Assessment Clearinghouse (PSRAC). Includes contact information,
#' institutional affiliations, areas of focus, and service populations.
#'
#' @param state Character. Optional state name to filter by expert location
#'   (e.g. \code{"California"}).
#'
#' @return A tibble with columns including:
#' \describe{
#'   \item{fullname}{Character. Expert's full name.}
#'   \item{firstname}{Character. First name.}
#'   \item{lastname}{Character. Last name.}
#'   \item{institution}{Character. Institutional affiliation.}
#'   \item{areaoffocus}{Character. Area of expertise (e.g. "General",
#'     "Adolescent/Juvenile").}
#'   \item{city}{Character. City.}
#'   \item{state}{Character. State.}
#'   \item{region}{Character. Geographic region.}
#'   \item{servicepopulation}{Character. Target population served.}
#'   \item{email}{Character. Contact email.}
#' }
#'
#' @examples
#' \dontrun{
#' ojp_experts()
#' ojp_experts(state = "California")
#' }
#'
#' @export
ojp_experts <- function(state = NULL) {
  params <- list()
  if (!is.null(state)) params[["$where"]] <- paste0("state='", state, "'")
  .ojp_fetch_all("ojpdataset/v1/esbe-p7rh.json", params, max_results = 500)
}

# == CrimeSolutions Practices ==================================================

#' Fetch CrimeSolutions rated practices
#'
#' Returns the CrimeSolutions.ojp.gov catalog of evidence-based criminal
#' justice practices. Each practice has been reviewed and rated for
#' effectiveness. Data is fetched from a CSV endpoint.
#'
#' @return A tibble with columns including:
#' \describe{
#'   \item{Title}{Character. Name of the practice.}
#'   \item{Topics}{Character. Associated topic areas.}
#'   \item{Summary}{Character. Brief summary of the practice.}
#'   \item{RCT}{Character. Whether randomized controlled trial evidence exists.}
#' }
#'
#' @examples
#' \dontrun{
#' ojp_practices()
#' }
#'
#' @seealso \code{\link{ojp_programs}} for rated programs.
#' @export
ojp_practices <- function() {
  .ojp_fetch_csv("https://crimesolutions.ojp.gov/topics/practices/content?all")
}

# == CrimeSolutions Programs ===================================================

#' Fetch CrimeSolutions rated programs
#'
#' Returns the CrimeSolutions.ojp.gov catalog of evidence-based criminal
#' justice programs. Each program has been reviewed and assigned an evidence
#' rating (Effective, Promising, or No Effects). Data is fetched from a CSV
#' endpoint.
#'
#' @return A tibble with columns including:
#' \describe{
#'   \item{Title}{Character. Name of the program.}
#'   \item{Evidence.Rating}{Character. Effectiveness rating
#'     (Effective / Promising / No Effects).}
#'   \item{Topics}{Character. Associated topic areas.}
#'   \item{Summary}{Character. Brief summary of the program.}
#'   \item{RCT}{Character. Whether RCT evidence exists.}
#' }
#'
#' @examples
#' \dontrun{
#' ojp_programs()
#' ojp_programs() |> dplyr::filter(Evidence.Rating == "Effective")
#' }
#'
#' @seealso \code{\link{ojp_practices}} for rated practices.
#' @export
ojp_programs <- function() {
  .ojp_fetch_csv("https://crimesolutions.ojp.gov/topics/programs/content?all")
}

# == Generic dataset fetch =====================================================

#' Fetch any OJP dataset by ID
#'
#' Generic accessor for any OJP or BJS Socrata-style dataset. Provide the
#' four-by-four dataset identifier (visible in \code{\link{ojp_list}}) and
#' optionally a SoQL WHERE clause for server-side filtering. Results are
#' paginated automatically up to \code{max_results}.
#'
#' @param dataset_id Character. Socrata-style four-by-four dataset identifier
#'   (e.g. \code{"ya4e-n9zp"} for NCVS Household Population).
#' @param type Character. API endpoint type: \code{"bjsdataset"} (Bureau of
#'   Justice Statistics) or \code{"ojpdataset"} (Office of Justice Programs).
#'   Default is \code{"ojpdataset"}.
#' @param where Character. Optional SoQL WHERE clause for server-side filtering
#'   (e.g. \code{"year='2020'"}).
#' @param max_results Integer. Maximum rows to return (default 1000).
#'
#' @return A tibble of dataset records. Column names and types depend on the
#'   specific dataset.
#'
#' @examples
#' \dontrun{
#' # Fetch NCVS household data
#' ojp_data("ya4e-n9zp", type = "bjsdataset", max_results = 50)
#'
#' # Fetch risk landscape
#' ojp_data("98ww-3sks", type = "ojpdataset")
#' }
#'
#' @seealso \code{\link{ojp_list}} for available dataset IDs.
#' @export
ojp_data <- function(dataset_id, type = "ojpdataset", where = NULL,
                     max_results = 1000) {
  path <- paste0(type, "/v1/", dataset_id, ".json")
  params <- list()
  if (!is.null(where)) params[["$where"]] <- where
  .ojp_fetch_all(path, params, max_results)
}

# == Context ===================================================================

#' Get ojp.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ojp_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ojp_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ojp.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ojp.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ojp.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ojp.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
