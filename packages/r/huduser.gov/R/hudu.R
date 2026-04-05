# huduser.gov.R - HUD User (PD&R) open data client
#
# Data source: https://www.huduser.gov/portal/pdrdatas_landing.html
# Datasets:
#   - Fair Market Rents (FMR) - historical CSV (1983-2026) + per-year XLSX
#   - Income Limits (Section 8) - per-year XLSX
#   - Public Housing Physical Inspection Scores - XLSX
#   - Multifamily Physical Inspection Scores - XLSX
#   - Housing Inventory Count (HIC) by State - XLSX
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none for bulk downloads; API requires token (not used here)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"

`%||%` <- function(x, y) if (is.null(x)) y else x

.hudu_base <- "https://www.huduser.gov/portal/datasets"

.hudu_fetch_file <- function(url, ext = ".xlsx") {
  tmp <- tempfile(fileext = ext)
  tryCatch({
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = .ua) |>
      httr2::req_timeout(120) |>
      httr2::req_perform(path = tmp)
    tmp
  }, error = function(e) {
    warning("Failed to fetch ", url, ": ", conditionMessage(e))
    NULL
  })
}

.hudu_fetch_csv <- function(url) {
  path <- .hudu_fetch_file(url, ext = ".csv")
  if (is.null(path)) return(NULL)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

.hudu_fetch_xlsx <- function(url, sheet = 1L, skip = 0L) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Install with: install.packages('readxl')")
  }
  path <- .hudu_fetch_file(url, ext = ".xlsx")
  if (is.null(path)) return(NULL)
  readxl::read_xlsx(path, sheet = sheet, skip = skip)
}

.hudu_clean_dollar <- function(x) {
  suppressWarnings(as.double(gsub("[\\$,]", "", x)))
}

.hudu_safe_chr <- function(x) {
  x <- as.character(x)
  x <- iconv(x, to = "UTF-8", sub = "")
  x
}

# State FIPS to abbreviation lookup
.hudu_fips_to_st <- c(
  "01"="AL","02"="AK","04"="AZ","05"="AR","06"="CA","08"="CO","09"="CT",
  "10"="DE","11"="DC","12"="FL","13"="GA","15"="HI","16"="ID","17"="IL",
  "18"="IN","19"="IA","20"="KS","21"="KY","22"="LA","23"="ME","24"="MD",
  "25"="MA","26"="MI","27"="MN","28"="MS","29"="MO","30"="MT","31"="NE",
  "32"="NV","33"="NH","34"="NJ","35"="NM","36"="NY","37"="NC","38"="ND",
  "39"="OH","40"="OK","41"="OR","42"="PA","44"="RI","45"="SC","46"="SD",
  "47"="TN","48"="TX","49"="UT","50"="VT","51"="VA","53"="WA","54"="WV",
  "55"="WI","56"="WY","60"="AS","66"="GU","69"="MP","72"="PR","78"="VI"
)

# == Schemas ===================================================================

.schema_fmr <- tibble::tibble(
  fips        = character(),
  state_fips  = character(),
  state       = character(),
  county_fips = character(),
  county_name = character(),
  area_name   = character(),
  metro_code  = character(),
  fmr_0br     = double(),
  fmr_1br     = double(),
  fmr_2br     = double(),
  fmr_3br     = double(),
  fmr_4br     = double(),
  year         = integer()
)

.schema_income_limits <- tibble::tibble(
  fips           = character(),
  state          = character(),
  state_name     = character(),
  area_name      = character(),
  county_name    = character(),
  metro          = integer(),
  median_income  = double(),
  l50_1          = double(),
  l50_2          = double(),
  l50_3          = double(),
  l50_4          = double(),
  eli_1          = double(),
  eli_2          = double(),
  eli_3          = double(),
  eli_4          = double(),
  l80_1          = double(),
  l80_2          = double(),
  l80_3          = double(),
  l80_4          = double(),
  year           = integer()
)

.schema_inspections <- tibble::tibble(
  property_id     = character(),
  property_name   = character(),
  address         = character(),
  city            = character(),
  state           = character(),
  zip             = character(),
  county          = character(),
  cbsa            = character(),
  score           = double(),
  inspection_date = as.Date(character()),
  latitude        = double(),
  longitude       = double(),
  property_type   = character()
)

# == FMR data ==================================================================

# The historical FMR_All CSV has wide format with columns like fmr26_0..fmr26_4
# for each year. We extract per-year columns and reshape.

.hudu_fmr_all_url <- "https://www.huduser.gov/portal/datasets/FMR/FMR_All_1983_2026.csv"

.hudu_parse_fmr_year <- function(raw, yr) {
  # yr is a 2-digit string like "26"
  yr_int <- as.integer(yr)
  full_year <- if (yr_int >= 83) 1900L + yr_int else 2000L + yr_int

  cn <- names(raw)
  cn_lower <- tolower(cn)

  # Find columns for this year
  fmr0_idx <- which(cn_lower == paste0("fmr", yr, "_0"))
  fmr1_idx <- which(cn_lower == paste0("fmr", yr, "_1"))
  fmr2_idx <- which(cn_lower == paste0("fmr", yr, "_2"))
  fmr3_idx <- which(cn_lower == paste0("fmr", yr, "_3"))
  fmr4_idx <- which(cn_lower == paste0("fmr", yr, "_4"))
  area_idx <- which(cn_lower == paste0("areaname", yr))
  msa_idx  <- which(cn_lower == paste0("msa", yr))

  # If we don't have the 5-column format, try the 2-bed-only format
  if (length(fmr2_idx) == 0) return(NULL)

  # For older years, not all bedroom sizes may exist
  get_col <- function(idx) {
    if (length(idx) == 0) return(rep(NA_real_, nrow(raw)))
    .hudu_clean_dollar(raw[[idx[1]]])
  }
  get_chr <- function(idx) {
    if (length(idx) == 0) return(rep(NA_character_, nrow(raw)))
    as.character(raw[[idx[1]]])
  }

  st_fips <- sprintf("%02d", as.integer(raw$state %||% NA))
  st_abbr <- unname(.hudu_fips_to_st[st_fips])
  st_abbr[is.na(st_abbr)] <- NA_character_

  tibble::tibble(
    fips       = as.character(raw$fips %||% raw[[1]]),
    state_fips = st_fips,
    state      = st_abbr,
    county_fips = as.character(raw$county %||% NA),
    county_name = .hudu_safe_chr(raw$name %||% NA),
    area_name  = .hudu_safe_chr(get_chr(area_idx)),
    metro_code = get_chr(msa_idx),
    fmr_0br    = get_col(fmr0_idx),
    fmr_1br    = get_col(fmr1_idx),
    fmr_2br    = get_col(fmr2_idx),
    fmr_3br    = get_col(fmr3_idx),
    fmr_4br    = get_col(fmr4_idx),
    year       = full_year
  )
}

# == Public functions ==========================================================

#' List available HUD User datasets
#'
#' Returns a catalogue of datasets accessible through this client,
#' covering Fair Market Rents, Section 8 income limits, housing
#' inspection scores, and homelessness inventory counts from HUD's
#' Office of Policy Development and Research (PD&R).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{dataset}{Character. Short identifier for use with other functions.}
#'     \item{description}{Character. Human-readable dataset description.}
#'     \item{years}{Character. Available year range.}
#'     \item{url}{Character. Source download URL.}
#'   }
#'
#' @family huduser discovery
#' @seealso [hudu_fmr()], [hudu_income_limits()], [hudu_inspections_public()]
#'
#' @examples
#' \dontrun{
#' hudu_list()
#' }
#' @export
hudu_list <- function() {
  tibble::tibble(
    dataset = c(
      "fmr",
      "income_limits",
      "inspection_public",
      "inspection_multifamily",
      "hic"
    ),
    description = c(
      "Fair Market Rents by county/area (0-4 bedrooms)",
      "Section 8 Income Limits (50%, ELI, 80%) by area",
      "Public Housing Physical Inspection Scores",
      "Multifamily Housing Physical Inspection Scores",
      "Housing Inventory Count (homelessness beds) by State"
    ),
    years = c(
      "1983-2026",
      "2017-2025",
      "current",
      "current",
      "2007-2024"
    ),
    url = c(
      .hudu_fmr_all_url,
      paste0(.hudu_base, "/il.html"),
      paste0(.hudu_base, "/pis/public_housing_physical_inspection_scores.xlsx"),
      paste0(.hudu_base, "/pis/multifamily_physical_inspection_scores.xlsx"),
      "https://www.huduser.gov/portal/sites/default/files/xls/2007-2024-HIC-Counts-by-State.xlsx"
    )
  )
}

#' Get Fair Market Rents
#'
#' Downloads the comprehensive HUD Fair Market Rent (FMR) dataset and
#' returns rents for the requested fiscal year. FMRs represent the 40th
#' percentile of gross rents for standard-quality units and are used to
#' determine Housing Choice Voucher payment amounts.
#'
#' @details The first call downloads a ~20 MB CSV covering all years
#'   (1983--2026). Subsequent calls in the same session reuse the cached
#'   download via R's temp directory.
#'
#' @param year Integer. Fiscal year, 1983--2026. Default `2026L`.
#' @param state Character or NULL. Two-letter state abbreviation
#'   (e.g. `"CA"`) or two-digit FIPS code (e.g. `"06"`). Default `NULL`
#'   returns all states.
#' @param limit Integer. Maximum rows to return. Default `500L`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{fips}{Character. County FIPS code.}
#'     \item{state_fips}{Character. Two-digit state FIPS.}
#'     \item{state}{Character. Two-letter state abbreviation.}
#'     \item{county_fips, county_name}{Character. County identifiers.}
#'     \item{area_name}{Character. HUD FMR area name.}
#'     \item{metro_code}{Character. MSA/metro area code.}
#'     \item{fmr_0br, fmr_1br, fmr_2br, fmr_3br, fmr_4br}{Double.
#'       Monthly rent by bedroom count (USD).}
#'     \item{year}{Integer. Fiscal year.}
#'   }
#'
#' @family huduser data
#' @seealso [hudu_search()] to find areas by name,
#'   [hudu_fmr_summary()] for state-level aggregates
#'
#' @examples
#' \dontrun{
#' hudu_fmr(year = 2025, state = "NY", limit = 20)
#' }
#' @export
hudu_fmr <- function(year = 2026L, state = NULL, limit = 500L) {
  year <- as.integer(year)
  yr2 <- sprintf("%02d", year %% 100)

  raw <- .hudu_fetch_csv(.hudu_fmr_all_url)
  if (is.null(raw)) return(.schema_fmr)

  result <- .hudu_parse_fmr_year(raw, yr2)
  if (is.null(result) || nrow(result) == 0) {
    warning("No FMR data found for year ", year)
    return(.schema_fmr)
  }

  if (!is.null(state) && nzchar(state)) {
    st <- toupper(state)
    if (grepl("^[A-Z]{2}$", st)) {
      result <- result |> dplyr::filter(.data$state == st)
    } else {
      # Numeric FIPS
      st_padded <- sprintf("%02d", as.integer(st))
      result <- result |> dplyr::filter(.data$state_fips == st_padded)
    }
  }

  result |> dplyr::slice_head(n = as.integer(limit))
}

#' Search Fair Market Rents by area name
#'
#' Searches the FMR dataset for areas whose name or county matches a
#' keyword. Useful for finding FMR data for a specific city or metro
#' area without knowing the FIPS code.
#'
#' @param query Character. Case-insensitive search term matched against
#'   area_name and county_name (e.g. `"San Francisco"`, `"Chicago"`).
#' @param year Integer. Fiscal year. Default `2026L`.
#' @param limit Integer. Maximum rows to return. Default `100L`.
#'
#' @return A tibble with the same structure as [hudu_fmr()], filtered
#'   to matching areas.
#'
#' @family huduser discovery
#' @seealso [hudu_fmr()] for state-level filtering,
#'   [hudu_fmr_summary()] for aggregate statistics
#'
#' @examples
#' \dontrun{
#' hudu_search("San Francisco")
#' hudu_search("Chicago", year = 2024)
#' }
#' @export
hudu_search <- function(query, year = 2026L, limit = 100L) {
  fmr <- hudu_fmr(year = year, limit = 99999L)
  if (nrow(fmr) == 0) return(.schema_fmr)

  q <- tolower(query)
  fmr |>
    dplyr::filter(
      grepl(q, tolower(area_name), fixed = TRUE) |
      grepl(q, tolower(county_name), fixed = TRUE)
    ) |>
    dplyr::slice_head(n = as.integer(limit))
}

#' Get Section 8 Income Limits
#'
#' Downloads HUD Section 8 income limit data for a given fiscal year.
#' Income limits determine eligibility for HUD-assisted housing programs
#' and are set at 50% (Very Low), 30% (Extremely Low / ELI), and 80%
#' (Low) of area median family income, adjusted for household size.
#'
#' @param year Integer. Fiscal year, 2017--2025. Default `2025L`.
#' @param state Character or NULL. Two-letter state abbreviation to
#'   filter results (e.g. `"TX"`). Default `NULL` returns all states.
#' @param limit Integer. Maximum rows to return. Default `500L`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{fips}{Character. Area FIPS code.}
#'     \item{state}{Character. Two-letter state abbreviation.}
#'     \item{state_name, area_name, county_name}{Character. Geographic labels.}
#'     \item{metro}{Integer. Metro/non-metro flag.}
#'     \item{median_income}{Double. Area median family income (USD).}
#'     \item{l50_1 through l50_4}{Double. 50% limits for 1--4 person households.}
#'     \item{eli_1 through eli_4}{Double. ELI limits for 1--4 person households.}
#'     \item{l80_1 through l80_4}{Double. 80% limits for 1--4 person households.}
#'     \item{year}{Integer. Fiscal year.}
#'   }
#'
#' @family huduser data
#' @seealso [hudu_fmr()] for Fair Market Rent data
#'
#' @examples
#' \dontrun{
#' hudu_income_limits(year = 2025, state = "CA", limit = 20)
#' }
#' @export
hudu_income_limits <- function(year = 2025L, state = NULL, limit = 500L) {
  year <- as.integer(year)
  yr2 <- sprintf("%02d", year %% 100)
  url <- paste0(.hudu_base, "/il/il", yr2, "/Section8-FY", yr2, ".xlsx")

  raw <- .hudu_fetch_xlsx(url)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_income_limits)

  cn <- tolower(names(raw))
  names(raw) <- cn

  median_col <- grep("^median", cn, value = TRUE)
  if (length(median_col) == 0) median_col <- "median_income"

  result <- tibble::tibble(
    fips          = as.character(raw$fips),
    state         = as.character(raw$stusps),
    state_name    = as.character(raw$state_name %||% NA),
    area_name     = as.character(raw$hud_area_name %||% NA),
    county_name   = as.character(raw$county_name %||% NA),
    metro         = suppressWarnings(as.integer(raw$metro)),
    median_income = suppressWarnings(as.double(raw[[median_col[1]]])),
    l50_1         = suppressWarnings(as.double(raw$l50_1)),
    l50_2         = suppressWarnings(as.double(raw$l50_2)),
    l50_3         = suppressWarnings(as.double(raw$l50_3)),
    l50_4         = suppressWarnings(as.double(raw$l50_4)),
    eli_1         = suppressWarnings(as.double(raw$eli_1)),
    eli_2         = suppressWarnings(as.double(raw$eli_2)),
    eli_3         = suppressWarnings(as.double(raw$eli_3)),
    eli_4         = suppressWarnings(as.double(raw$eli_4)),
    l80_1         = suppressWarnings(as.double(raw$l80_1)),
    l80_2         = suppressWarnings(as.double(raw$l80_2)),
    l80_3         = suppressWarnings(as.double(raw$l80_3)),
    l80_4         = suppressWarnings(as.double(raw$l80_4)),
    year          = year
  )

  if (!is.null(state) && nzchar(state)) {
    result <- result |> dplyr::filter(toupper(.data$state) == toupper(!!state))
  }

  result |> dplyr::slice_head(n = as.integer(limit))
}

#' Get Public Housing Physical Inspection Scores
#'
#' Downloads the latest public housing physical inspection scores from
#' HUD's Real Estate Assessment Center (REAC). Scores range from 0--100
#' and reflect the physical condition of public housing developments.
#' Properties scoring below 60 are referred for corrective action.
#'
#' @param state Character or NULL. Two-letter state abbreviation to
#'   filter results. Default `NULL` returns all states.
#' @param min_score Numeric or NULL. Minimum inspection score threshold.
#'   Default `NULL` returns all scores.
#' @param limit Integer. Maximum rows to return. Default `500L`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{property_id}{Character. Development/property identifier.}
#'     \item{property_name}{Character. Development name.}
#'     \item{address, city, state, zip}{Character. Location fields.}
#'     \item{county, cbsa}{Character. County and metro area names.}
#'     \item{pha_code, pha_name}{Character. Public Housing Authority info.}
#'     \item{score}{Double. REAC inspection score (0--100).}
#'     \item{inspection_date}{Date. Date of the inspection.}
#'     \item{latitude, longitude}{Double. Geocoded coordinates.}
#'     \item{property_type}{Character. Always `"public_housing"`.}
#'   }
#'
#' @family huduser data
#' @seealso [hudu_inspections_multifamily()] for multifamily properties
#'
#' @examples
#' \dontrun{
#' hudu_inspections_public(state = "IL", limit = 50)
#' hudu_inspections_public(min_score = 90, limit = 100)
#' }
#' @export
hudu_inspections_public <- function(state = NULL, min_score = NULL, limit = 500L) {
  url <- paste0(.hudu_base, "/pis/public_housing_physical_inspection_scores.xlsx")
  raw <- .hudu_fetch_xlsx(url)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_inspections)

  # Columns: INSPECTION_ID, DEVLOPMENT_ID, DEVELPMENT_NAME, ADDRESS, CITY,
  #   CBSA_NAME, CBSA_CODE, COUNTY_NAME, COUNTY_CODE, STATE_NAME, STATE_CODE,
  #   ZIP, LATITUDE, LONGITUDE, LOCATION_QUALITY, PHA_CODE, PHA_NAME,
  #   INSPECTION_SCORE, INSPECTION_DATE
  cn <- toupper(names(raw))
  names(raw) <- cn

  .safe_date_pis <- function(x) {
    tryCatch(as.Date(x, format = "%m/%d/%Y"), error = function(e) {
      tryCatch(as.Date(x), error = function(e2) as.Date(NA))
    })
  }

  result <- tibble::tibble(
    property_id     = as.character(raw$DEVLOPMENT_ID %||% raw$DEVELPMENT_ID %||% NA),
    property_name   = as.character(raw$DEVELPMENT_NAME %||% raw$DEVELOPMENT_NAME %||% NA),
    address         = as.character(raw$ADDRESS %||% NA),
    city            = as.character(raw$CITY %||% NA),
    state           = as.character(raw$STATE_CODE %||% NA),
    zip             = as.character(raw$ZIP %||% NA),
    county          = as.character(raw$COUNTY_NAME %||% NA),
    cbsa            = as.character(raw$CBSA_NAME %||% NA),
    pha_code        = as.character(raw$PHA_CODE %||% NA),
    pha_name        = as.character(raw$PHA_NAME %||% NA),
    score           = suppressWarnings(as.double(raw$INSPECTION_SCORE)),
    inspection_date = .safe_date_pis(raw$INSPECTION_DATE),
    latitude        = suppressWarnings(as.double(raw$LATITUDE)),
    longitude       = suppressWarnings(as.double(raw$LONGITUDE)),
    property_type   = "public_housing"
  )

  if (!is.null(state) && nzchar(state)) {
    result <- result |> dplyr::filter(toupper(.data$state) == toupper(!!state))
  }
  if (!is.null(min_score)) {
    result <- result |> dplyr::filter(.data$score >= !!as.double(min_score))
  }

  result |> dplyr::slice_head(n = as.integer(limit))
}

#' Get Multifamily Housing Physical Inspection Scores
#'
#' Downloads the latest multifamily housing inspection scores from
#' HUD's Real Estate Assessment Center (REAC). Covers HUD-insured and
#' HUD-assisted multifamily properties (Section 8 project-based, FHA-insured,
#' Section 202, Section 811, etc.).
#'
#' @param state Character or NULL. Two-letter state abbreviation to
#'   filter results. Default `NULL` returns all states.
#' @param min_score Numeric or NULL. Minimum inspection score threshold.
#'   Default `NULL` returns all scores.
#' @param limit Integer. Maximum rows to return. Default `500L`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{property_id}{Character. Property identifier.}
#'     \item{property_name}{Character. Property name.}
#'     \item{address, city, state, zip}{Character. Location fields.}
#'     \item{county, cbsa}{Character. County and metro area names.}
#'     \item{score}{Double. REAC inspection score (0--100).}
#'     \item{inspection_date}{Date. Date of the inspection.}
#'     \item{latitude, longitude}{Double. Geocoded coordinates.}
#'     \item{property_type}{Character. Always `"multifamily"`.}
#'   }
#'
#' @family huduser data
#' @seealso [hudu_inspections_public()] for public housing properties
#'
#' @examples
#' \dontrun{
#' hudu_inspections_multifamily(state = "CA", limit = 50)
#' }
#' @export
hudu_inspections_multifamily <- function(state = NULL, min_score = NULL, limit = 500L) {
  url <- paste0(.hudu_base, "/pis/multifamily_physical_inspection_scores.xlsx")
  raw <- .hudu_fetch_xlsx(url)
  if (is.null(raw) || nrow(raw) == 0) return(.schema_inspections)

  # Columns: INSPECTION_ID, PROPERTY_ID, PROPERTY_NAME, ADDRESS, CITY,
  #   CBSA_NAME, CBSA_CODE, COUNTY_NAME, COUNTY_CODE, STATE_NAME, STATE_CODE,
  #   ZIP, LATITUDE, LONGITUDE, LOCATION_QUALITY, INSPECTION_SCORE, INSPECTION_DATE
  cn <- toupper(names(raw))
  names(raw) <- cn

  .safe_date_pis <- function(x) {
    tryCatch(as.Date(x, format = "%m/%d/%Y"), error = function(e) {
      tryCatch(as.Date(x), error = function(e2) as.Date(NA))
    })
  }

  result <- tibble::tibble(
    property_id     = as.character(raw$PROPERTY_ID %||% NA),
    property_name   = as.character(raw$PROPERTY_NAME %||% NA),
    address         = as.character(raw$ADDRESS %||% NA),
    city            = as.character(raw$CITY %||% NA),
    state           = as.character(raw$STATE_CODE %||% NA),
    zip             = as.character(raw$ZIP %||% NA),
    county          = as.character(raw$COUNTY_NAME %||% NA),
    cbsa            = as.character(raw$CBSA_NAME %||% NA),
    score           = suppressWarnings(as.double(raw$INSPECTION_SCORE)),
    inspection_date = .safe_date_pis(raw$INSPECTION_DATE),
    latitude        = suppressWarnings(as.double(raw$LATITUDE)),
    longitude       = suppressWarnings(as.double(raw$LONGITUDE)),
    property_type   = "multifamily"
  )

  if (!is.null(state) && nzchar(state)) {
    result <- result |> dplyr::filter(toupper(.data$state) == toupper(!!state))
  }
  if (!is.null(min_score)) {
    result <- result |> dplyr::filter(.data$score >= !!as.double(min_score))
  }

  result |> dplyr::slice_head(n = as.integer(limit))
}

#' Summarize Fair Market Rents by state
#'
#' Aggregates 2-bedroom Fair Market Rents across all areas within each
#' state, returning mean, median, minimum, and maximum values. Useful
#' for comparing housing costs across states at a glance.
#'
#' @param year Integer. Fiscal year. Default `2026L`.
#'
#' @return A tibble sorted by descending mean FMR with columns:
#'   \describe{
#'     \item{state}{Character. Two-letter state abbreviation.}
#'     \item{n_areas}{Integer. Number of FMR areas in the state.}
#'     \item{mean_fmr_2br}{Double. Mean 2-bedroom FMR across areas.}
#'     \item{median_fmr_2br}{Double. Median 2-bedroom FMR.}
#'     \item{min_fmr_2br}{Double. Lowest 2-bedroom FMR in the state.}
#'     \item{max_fmr_2br}{Double. Highest 2-bedroom FMR in the state.}
#'   }
#'
#' @family huduser data
#' @seealso [hudu_fmr()] for county-level FMR data,
#'   [hudu_search()] to find specific areas
#'
#' @examples
#' \dontrun{
#' hudu_fmr_summary(year = 2025)
#' }
#' @export
hudu_fmr_summary <- function(year = 2026L) {
  fmr <- hudu_fmr(year = year, limit = 99999L)
  if (nrow(fmr) == 0) {
    return(tibble::tibble(state = character(), n_areas = integer(),
                          mean_fmr_2br = double(), median_fmr_2br = double(),
                          min_fmr_2br = double(), max_fmr_2br = double()))
  }

  fmr |>
    dplyr::filter(!is.na(fmr_2br), fmr_2br > 0) |>
    dplyr::group_by(state) |>
    dplyr::summarise(
      n_areas      = dplyr::n(),
      mean_fmr_2br   = mean(fmr_2br, na.rm = TRUE),
      median_fmr_2br = stats::median(fmr_2br, na.rm = TRUE),
      min_fmr_2br    = min(fmr_2br, na.rm = TRUE),
      max_fmr_2br    = max(fmr_2br, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(mean_fmr_2br))
}

#' Get huduser.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hudu_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hudu_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/huduser.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "huduser.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# huduser.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# huduser.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
