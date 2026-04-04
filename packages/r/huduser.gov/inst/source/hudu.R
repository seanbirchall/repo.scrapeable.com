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

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

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
#' Returns a tibble describing each dataset accessible through this client.
#'
#' @return tibble with columns: dataset, description, years, url
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
#' Downloads the comprehensive FMR dataset and returns rents for the requested
#' year. Data covers all US counties and metro areas.
#'
#' @param year Integer. Fiscal year (1983-2026). Default 2026.
#' @param state Character. Two-digit FIPS state code (e.g. "06" for California)
#'   or two-letter abbreviation (matched against state column). Default NULL.
#' @param limit Integer. Max rows to return. Default 500.
#' @return tibble with fips, state, county, area_name, metro_code, fmr_0br
#'   through fmr_4br, and year
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
#' Searches the FMR dataset for areas matching a keyword.
#'
#' @param query Character. Case-insensitive search on area name or county.
#' @param year Integer. Fiscal year. Default 2026.
#' @param limit Integer. Max rows to return. Default 100.
#' @return tibble of matching FMR records
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
#' Downloads income limit data for a given fiscal year. Returns 50% median,
#' Extremely Low Income (ELI), and 80% median limits by household size (1-4).
#'
#' @param year Integer. Fiscal year (2017-2025). Default 2025.
#' @param state Character. Two-letter state abbreviation. Default NULL.
#' @param limit Integer. Max rows. Default 500.
#' @return tibble of income limits
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
#' Downloads the latest public housing inspection scores from HUD REAC.
#'
#' @param state Character. Two-letter state abbreviation. Default NULL.
#' @param min_score Numeric. Minimum inspection score filter. Default NULL.
#' @param limit Integer. Max rows. Default 500.
#' @return tibble of inspection records
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
#' Downloads the latest multifamily housing inspection scores from HUD REAC.
#'
#' @param state Character. Two-letter state abbreviation. Default NULL.
#' @param min_score Numeric. Minimum inspection score filter. Default NULL.
#' @param limit Integer. Max rows. Default 500.
#' @return tibble of inspection records
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
#' Returns mean and median 2-bedroom FMR by state for a given year.
#'
#' @param year Integer. Fiscal year. Default 2026.
#' @return tibble with state, n_areas, mean_fmr_2br, median_fmr_2br, min_fmr_2br, max_fmr_2br
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

#' Return the full source of this client file
#'
#' Reads the huduser.gov.R source file and returns it as a single string.
#'
#' @return character string containing the full source code
#' @export
hudu_context <- function() {
  src <- readLines(sys.frame(environment(hudu_context))$ofile %||% {
    f <- getSrcFilename(hudu_context, full.names = TRUE)
    if (length(f) && nzchar(f)) f else "clients/huduser.gov.R"
  })
  paste(src, collapse = "\n")
}
