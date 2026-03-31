# fema-gov.R
# Self-contained FEMA OpenFEMA API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: none documented

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fema_base <- "https://www.fema.gov/api/open/v2"

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
  cat(out, "\n")
  invisible(out)
}

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
#' @param top Maximum number of records to return (default 100)
#' @param orderby OData orderby clause (default "declarationDate desc")
#' @param state Optional two-letter state abbreviation filter
#' @param incident_type Optional incident type filter (e.g. "Fire", "Hurricane")
#' @return tibble: disaster_number, state, declaration_type, declaration_date,
#'   incident_type, title, incident_begin, incident_end, designated_area
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
#' @param disaster_number Integer disaster number to filter by
#' @param state Optional two-letter state abbreviation
#' @param top Maximum records (default 100)
#' @return tibble: disaster_number, state, county, city, zip_code,
#'   valid_registrations, total_inspected, total_damage, approved_for_fema_assistance
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
#' @param disaster_number Integer disaster number
#' @return tibble: single row with disaster details
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

#' FEMA package context for LLM integration
#'
#' Prints all public function signatures and documentation.
#' Designed for LLM context injection.
#'
#' @return Invisibly returns the context string
fema_context <- function() {
  .build_context("fema.gov", header_lines = c(
    "# fema.gov - FEMA OpenFEMA API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "# Common incident types: Fire, Hurricane, Flood, Severe Storm(s), Tornado",
    "# Common declaration types: DR (Major Disaster), EM (Emergency), FM (Fire Management)"
  ))
}
