# fema.gov.R - Self-contained fema.gov client



# fema-gov.R
# Self-contained FEMA OpenFEMA API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fema_base <- "https://www.fema.gov/api/open/v2"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))


# == Schemas ===================================================================

.schema_disasters <- tibble(
  disaster_number = integer(), state = character(), declaration_type = character(),
  declaration_date = as.Date(character()), incident_type = character(),
  title = character(), incident_begin = as.Date(character()),
  incident_end = as.Date(character()), designated_area = character()
)

.schema_housing <- tibble(
  disaster_number = integer(), state = character(), county = character(),
  city = character(), zip_code = character(), valid_registrations = integer(),
  total_inspected = integer(), total_damage = numeric(),
  approved_for_fema_assistance = integer()
)

# == Public functions ==========================================================

#' Search FEMA disaster declarations
#'
#' Query the OpenFEMA Disaster Declarations Summaries endpoint. Returns
#' presidentially declared disasters including major disasters, emergencies,
#' and fire management assistance. Supports filtering by state and incident type.
#'
#' @param top Integer. Maximum number of records to return (default 100).
#' @param orderby Character. OData orderby clause (default
#'   \code{"declarationDate desc"} for most recent first).
#' @param state Character. Two-letter state abbreviation filter (e.g. \code{"FL"}).
#' @param incident_type Character. Incident type filter. Common values:
#'   \code{"Fire"}, \code{"Hurricane"}, \code{"Flood"}, \code{"Tornado"},
#'   \code{"Severe Storm(s)"}, \code{"Earthquake"}, \code{"Snow"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{disaster_number}{Integer. FEMA disaster number (e.g. 4673).}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{declaration_type}{Character. Type: "DR" (major disaster),
#'       "EM" (emergency), "FM" (fire management).}
#'     \item{declaration_date}{Date. Date disaster was declared.}
#'     \item{incident_type}{Character. Type of incident.}
#'     \item{title}{Character. Disaster title (e.g. "HURRICANE IAN").}
#'     \item{incident_begin}{Date. Incident start date.}
#'     \item{incident_end}{Date. Incident end date (NA if ongoing).}
#'     \item{designated_area}{Character. Area designation.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fema_disasters(top = 10)
#' fema_disasters(state = "FL", incident_type = "Hurricane")
#' }
fema_disasters <- function(top = 100, orderby = "declarationDate desc",
                           state = NULL, incident_type = NULL) {
  url <- sprintf("%s/DisasterDeclarationsSummaries?$top=%d&$orderby=%s",
                 .fema_base, top, utils::URLencode(orderby))
  filters <- character()
  if (!is.null(state)) filters <- c(filters, sprintf("state eq '%s'", state))
  if (!is.null(incident_type)) filters <- c(filters, sprintf("incidentType eq '%s'", incident_type))
  if (length(filters) > 0) url <- paste0(url, "&$filter=", utils::URLencode(paste(filters, collapse = " and ")))

  raw <- .fetch_json(url)
  df <- raw$DisasterDeclarationsSummaries
  if (is.null(df) || nrow(df) == 0) return(.schema_disasters)

  as_tibble(df) |>
    transmute(
      disaster_number = as.integer(disasterNumber),
      state = as.character(state),
      declaration_type = as.character(declarationType),
      declaration_date = as.Date(substr(declarationDate, 1, 10)),
      incident_type = as.character(incidentType),
      title = as.character(declarationTitle),
      incident_begin = as.Date(substr(incidentBeginDate, 1, 10)),
      incident_end = tryCatch(as.Date(substr(incidentEndDate, 1, 10)), error = function(e) as.Date(NA)),
      designated_area = as.character(designatedArea)
    )
}

#' Fetch FEMA housing assistance data for owners
#'
#' Retrieves Housing Assistance for Owners data from OpenFEMA. Shows
#' registration and damage statistics aggregated by geographic area
#' for a specific disaster. Useful for assessing disaster impact.
#'
#' @param disaster_number Integer. FEMA disaster number to filter by
#'   (e.g. 4673 for Hurricane Ian).
#' @param state Character. Two-letter state abbreviation filter.
#' @param top Integer. Maximum records to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{disaster_number}{Integer. FEMA disaster number.}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{county}{Character. County name (e.g. "Brevard (County)").}
#'     \item{city}{Character. City name.}
#'     \item{zip_code}{Character. ZIP code.}
#'     \item{valid_registrations}{Integer. Number of valid assistance registrations.}
#'     \item{total_inspected}{Integer. Number of properties inspected.}
#'     \item{total_damage}{Numeric. Total estimated damage in dollars.}
#'     \item{approved_for_fema_assistance}{Integer. Number approved for assistance.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' fema_housing(disaster_number = 4673)
#' fema_housing(state = "FL", top = 50)
#' }
fema_housing <- function(disaster_number = NULL, state = NULL, top = 100) {
  url <- sprintf("%s/HousingAssistanceOwners?$top=%d", .fema_base, top)
  filters <- character()
  if (!is.null(disaster_number)) filters <- c(filters, sprintf("disasterNumber eq %d", disaster_number))
  if (!is.null(state)) filters <- c(filters, sprintf("state eq '%s'", state))
  if (length(filters) > 0) url <- paste0(url, "&$filter=", utils::URLencode(paste(filters, collapse = " and ")))

  raw <- .fetch_json(url)
  df <- raw$HousingAssistanceOwners
  if (is.null(df) || nrow(df) == 0) return(.schema_housing)

  as_tibble(df) |>
    transmute(
      disaster_number = as.integer(disasterNumber),
      state = as.character(state),
      county = as.character(county),
      city = as.character(city),
      zip_code = as.character(zipCode),
      valid_registrations = as.integer(validRegistrations),
      total_inspected = as.integer(totalInspected),
      total_damage = as.numeric(totalDamage),
      approved_for_fema_assistance = as.integer(approvedForFemaAssistance)
    )
}

#' Get FEMA disaster summary by disaster number
#'
#' Fetch a single disaster declaration by its FEMA disaster number.
#' Returns the same column structure as \code{\link{fema_disasters}}.
#'
#' @param disaster_number Integer. FEMA disaster number (e.g. 4673).
#' @return A tibble with one row containing columns: disaster_number, state,
#'   declaration_type, declaration_date, incident_type, title,
#'   incident_begin, incident_end, designated_area.
#' @export
#' @examples
#' \dontrun{
#' fema_disaster(4673)
#' }
fema_disaster <- function(disaster_number) {
  url <- sprintf("%s/DisasterDeclarationsSummaries?$filter=disasterNumber eq %d&$top=1",
                 .fema_base, disaster_number)
  raw <- .fetch_json(url)
  df <- raw$DisasterDeclarationsSummaries
  if (is.null(df) || nrow(df) == 0) return(.schema_disasters)

  as_tibble(df) |>
    transmute(
      disaster_number = as.integer(disasterNumber),
      state = as.character(state),
      declaration_type = as.character(declarationType),
      declaration_date = as.Date(substr(declarationDate, 1, 10)),
      incident_type = as.character(incidentType),
      title = as.character(declarationTitle),
      incident_begin = as.Date(substr(incidentBeginDate, 1, 10)),
      incident_end = tryCatch(as.Date(substr(incidentEndDate, 1, 10)), error = function(e) as.Date(NA)),
      designated_area = as.character(designatedArea)
    )
}

# == Context ===================================================================

#' Get fema.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fema_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fema_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fema.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fema.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fema.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fema.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
