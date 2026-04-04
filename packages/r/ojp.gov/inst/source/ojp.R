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
#' Returns a tibble describing the known OJP/BJS API datasets.
#'
#' @return tibble: id, name, type, description
#' @export
ojp_list <- function() {
  .ojp_datasets
}

#' Search OJP datasets by keyword
#'
#' @param query Character string to search in dataset names and descriptions
#' @return tibble: matching datasets
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
#' Returns demographic data from the National Crime Victimization Survey
#' household file. All respondents are included regardless of victimization.
#'
#' @param year Optional year filter (e.g. "2020")
#' @param max_results Maximum rows to return (default 1000)
#' @return tibble with columns: idhh, yearq, year, hhage, hhsex, hhhisp,
#'   hhrace, hhrace_ethnicity, hincome1, hincome2, hnumber, popsize,
#'   region, msa, locality, wgthhcy
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
#' Returns personal crime victimization records (rape, assault, robbery,
#' personal theft). Only persons reporting victimizations are included.
#'
#' @param year Optional year filter (e.g. "2020")
#' @param max_results Maximum rows to return (default 1000)
#' @return tibble with columns: idper, yearq, year, ager, sex, hispanic,
#'   race, race_ethnicity, hincome1, marital, region, msa, educatn1,
#'   newcrime, newoff, seriousviolent, weapon, injury, etc.
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
#' Returns state-level data on risk assessment tools used across
#' jail, parole, pretrial, prison, probation, and release.
#'
#' @return tibble: abbreviation, state, region, jail, parole, pretrial,
#'   prison, probation, release
#' @export
ojp_risk_landscape <- function() {
  .ojp_fetch_all("ojpdataset/v1/98ww-3sks.json", max_results = 100)
}

# == Body Worn Camera Resources ================================================

#' Fetch body worn camera resources
#'
#' Returns BWC policy documents, research papers, and resources from BJA.
#'
#' @param state Optional state filter
#' @param max_results Maximum rows (default 500)
#' @return tibble: author, docdate, documenttitle, keywords, location,
#'   state, country, resourcetype, source, url, etc.
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
#' Returns directory of public safety risk assessment clearinghouse experts.
#'
#' @param state Optional state filter
#' @return tibble: fullname, firstname, lastname, institution, areaoffocus,
#'   city, state, region, servicepopulation, email, etc.
#' @export
ojp_experts <- function(state = NULL) {
  params <- list()
  if (!is.null(state)) params[["$where"]] <- paste0("state='", state, "'")
  .ojp_fetch_all("ojpdataset/v1/esbe-p7rh.json", params, max_results = 500)
}

# == CrimeSolutions Practices ==================================================

#' Fetch CrimeSolutions rated practices
#'
#' Returns CSV of evidence-based criminal justice practices with ratings.
#'
#' @return tibble: Title, Topics, Summary, RCT
#' @export
ojp_practices <- function() {
  .ojp_fetch_csv("https://crimesolutions.ojp.gov/topics/practices/content?all")
}

# == CrimeSolutions Programs ===================================================

#' Fetch CrimeSolutions rated programs
#'
#' Returns CSV of evidence-based criminal justice programs with ratings.
#'
#' @return tibble: Title, Evidence.Rating, Topics, Summary, RCT
#' @export
ojp_programs <- function() {
  .ojp_fetch_csv("https://crimesolutions.ojp.gov/topics/programs/content?all")
}

# == Generic dataset fetch =====================================================

#' Fetch any OJP dataset by ID
#'
#' @param dataset_id Socrata-style dataset ID (e.g. "ya4e-n9zp")
#' @param type Either "bjsdataset" or "ojpdataset"
#' @param where Optional SoQL WHERE clause
#' @param max_results Maximum rows (default 1000)
#' @return tibble
#' @export
ojp_data <- function(dataset_id, type = "ojpdataset", where = NULL,
                     max_results = 1000) {
  path <- paste0(type, "/v1/", dataset_id, ".json")
  params <- list()
  if (!is.null(where)) params[["$where"]] <- where
  .ojp_fetch_all(path, params, max_results)
}

# == Context ===================================================================

#' Return full source of all public functions
#'
#' @return character (invisibly); also printed to stdout
#' @export
ojp_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/ojp.gov.R"
  if (!file.exists(src_file)) {
    cat("# ojp.gov context - source not found\n")
    return(invisible("# ojp.gov context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- paste(c(rox, body), collapse = "\n")
  }
  txt <- paste(blocks, collapse = "\n\n")
  cat(txt, "\n")
  invisible(txt)
}
