# hud.gov.R - Self-contained HUD (Department of Housing and Urban Development) client
#
# Discovery: catalog search/browse of 220+ HUD datasets
# Access: Fair Market Rents, Income Limits, Inspection Scores,
#          Section 8 Contracts, Homeless Counts (all parsed XLSX)
#
# Dependencies: httr2, jsonlite, dplyr, tibble, readxl
# Auth: none (all public data)

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)
library(readxl)


# == Private utilities =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

.hud_ua <- "hud.gov-R-client/0.1 (support@scrapeable.com)"

.hud_fetch <- function(url, ext = ".tmp") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .hud_ua) |>
    httr2::req_timeout(120) |>
    httr2::req_perform(path = tmp)
  tmp
}

.hud_fetch_json <- function(url) {
  jsonlite::fromJSON(.hud_fetch(url, ext = ".json"), simplifyVector = TRUE)
}

.hud_fetch_xlsx <- function(url, sheet = 1, skip = 0) {
  tmp <- .hud_fetch(url, ext = ".xlsx")
  readxl::read_excel(tmp, sheet = sheet, skip = skip) |>
    tibble::as_tibble()
}

.safe_int <- function(x) suppressWarnings(as.integer(x))
.safe_num <- function(x) suppressWarnings(as.numeric(x))
.safe_chr <- function(x) as.character(x %||% NA_character_)

# Cache for the catalog (avoid repeated 650KB downloads)
.hud_catalog_cache <- new.env(parent = emptyenv())


# == Schemas ===================================================================

.schema_catalog <- tibble(
  title = character(), description = character(), keyword = character(),
  publisher = character(), modified = character(), identifier = character(),
  distribution_url = character(), media_type = character(),
  theme = character(), contact_name = character()
)

.schema_fmr <- tibble(
  state_abbr = character(), state_code = character(),
  hud_area_code = character(), county_name = character(),
  area_name = character(), metro = integer(), fips = character(),
  population = integer(),
  fmr_0br = integer(), fmr_1br = integer(), fmr_2br = integer(),
  fmr_3br = integer(), fmr_4br = integer()
)

.schema_income_limits <- tibble(
  fips = character(), state_abbr = character(), state_code = character(),
  state_name = character(), hud_area_code = character(),
  area_name = character(), county = character(), county_name = character(),
  metro = integer(), median_income = integer(),
  l50_1 = integer(), l50_2 = integer(), l50_3 = integer(), l50_4 = integer(),
  l80_1 = integer(), l80_2 = integer(), l80_3 = integer(), l80_4 = integer(),
  eli_1 = integer(), eli_2 = integer(), eli_3 = integer(), eli_4 = integer()
)

.schema_inspections <- tibble(
  inspection_id = character(), property_id = character(),
  property_name = character(), address = character(),
  city = character(), state_abbr = character(), state_fips = character(),
  zip = character(), county_name = character(),
  cbsa_name = character(), cbsa_code = character(),
  latitude = numeric(), longitude = numeric(),
  inspection_score = numeric(), inspection_date = character()
)

.schema_contracts <- tibble(
  property_id = integer(), contract_number = character(),
  bedroom_count = integer(), unit_count = integer(),
  contract_rent = numeric(), fair_market_rent = numeric(),
  utility_allowance = numeric()
)

.schema_homeless <- tibble(
  state = character(),
  total_yr_beds = integer(), total_yr_beds_es = integer(),
  total_yr_beds_th = integer(), total_yr_beds_sh = integer(),
  units_hh_children = integer(), beds_hh_children = integer()
)


# == Discovery: Catalog ========================================================

#' List all HUD datasets from the data catalog
#'
#' Downloads and caches the HUD data.json catalog containing 220+
#' datasets from the Department of Housing and Urban Development.
#'
#' @param refresh Logical. If \code{TRUE}, re-download catalog even if
#'   cached in current session (default \code{FALSE}).
#' @return A tibble with columns: \code{title} (character),
#'   \code{description} (character), \code{keyword} (character,
#'   semicolon-separated), \code{publisher} (character),
#'   \code{modified} (character, date), \code{identifier} (character),
#'   \code{distribution_url} (character, semicolon-separated download URLs),
#'   \code{media_type} (character), \code{theme} (character),
#'   \code{contact_name} (character).
#' @examples
#' \dontrun{
#' hud_catalog()
#' hud_catalog(refresh = TRUE)
#' }
#' @export
hud_catalog <- function(refresh = FALSE) {
  if (!refresh && exists("data", envir = .hud_catalog_cache)) {
    return(get("data", envir = .hud_catalog_cache))
  }

  raw <- .hud_fetch_json("https://data.hud.gov/data.json")
  datasets <- raw$dataset
  if (is.null(datasets) || length(datasets) == 0) return(.schema_catalog)

  n_ds <- length(datasets$title)

  # publisher is a data.frame, not a list
  pub_names <- if (is.data.frame(datasets$publisher)) {
    as.character(datasets$publisher$name)
  } else {
    rep(NA_character_, n_ds)
  }

  # contactPoint is a data.frame
  contact_names <- if (is.data.frame(datasets$contactPoint)) {
    as.character(datasets$contactPoint$fn)
  } else {
    rep(NA_character_, n_ds)
  }

  # keywords is a list of character vectors
  kw_list <- datasets$keyword %||% as.list(rep(NA_character_, n_ds))
  keywords <- vapply(kw_list, function(x) {
    if (is.null(x) || length(x) == 0) NA_character_ else paste(x, collapse = "; ")
  }, character(1))

  # distribution is a list of data.frames
  dist_list <- datasets$distribution %||% as.list(rep(list(NULL), n_ds))
  dist_urls <- vapply(dist_list, function(d) {
    if (is.null(d) || !is.data.frame(d)) return(NA_character_)
    url <- d$downloadURL %||% d$accessURL
    if (is.null(url)) NA_character_ else paste(url, collapse = "; ")
  }, character(1))
  dist_types <- vapply(dist_list, function(d) {
    if (is.null(d) || !is.data.frame(d)) return(NA_character_)
    mt <- d$mediaType
    if (is.null(mt)) NA_character_ else paste(mt, collapse = "; ")
  }, character(1))

  # theme may be NULL or a list
  theme_list <- datasets$theme
  themes <- if (is.null(theme_list)) {
    rep(NA_character_, n_ds)
  } else {
    vapply(theme_list, function(x) {
      if (is.null(x) || length(x) == 0) NA_character_ else paste(x, collapse = "; ")
    }, character(1))
  }

  result <- tibble(
    title            = as.character(datasets$title),
    description      = as.character(datasets$description),
    keyword          = keywords,
    publisher        = pub_names,
    modified         = as.character(datasets$modified),
    identifier       = as.character(datasets$identifier),
    distribution_url = dist_urls,
    media_type       = dist_types,
    theme            = themes,
    contact_name     = contact_names
  )

  assign("data", result, envir = .hud_catalog_cache)
  result
}


#' Search HUD data catalog by keyword
#'
#' Searches across titles, descriptions, and keywords of all HUD datasets
#' using case-insensitive matching.
#'
#' @param query Character. Search string (e.g. \code{"fair market rent"},
#'   \code{"homeless"}, \code{"income limits"}, \code{"section 8"},
#'   \code{"inspection"}).
#' @param max_results Integer. Maximum number of results to return (default 20).
#' @return A tibble with the same columns as \code{\link{hud_catalog}}.
#' @examples
#' \dontrun{
#' hud_search("fair market rent")
#' hud_search("homeless", max_results = 10)
#' }
#' @export
hud_search <- function(query, max_results = 20) {
  stopifnot(is.character(query), nchar(query) > 0)
  cat <- hud_catalog()
  q <- tolower(query)

  matches <- cat |>
    filter(
      grepl(q, tolower(title), fixed = TRUE) |
      grepl(q, tolower(description), fixed = TRUE) |
      grepl(q, tolower(keyword), fixed = TRUE)
    ) |>
    head(max_results)

  matches
}


# == Fair Market Rents =========================================================

#' Get HUD Fair Market Rents data
#'
#' Downloads the official Fair Market Rent (FMR) spreadsheet from HUD
#' for a given fiscal year. FMRs are used to determine payment standards
#' for the Housing Choice Voucher (Section 8) program.
#'
#' @param year Integer. Fiscal year (e.g. 2026, 2025, 2024). Default: 2026.
#' @param state Character or NULL. Two-letter state abbreviation to filter
#'   results (e.g. \code{"CA"}, \code{"NY"}).
#' @return A tibble with columns: \code{state_abbr} (character),
#'   \code{state_code} (character), \code{hud_area_code} (character),
#'   \code{county_name} (character), \code{area_name} (character),
#'   \code{metro} (integer), \code{fips} (character),
#'   \code{population} (integer), \code{fmr_0br} through \code{fmr_4br}
#'   (integer, monthly rent in dollars for 0-4 bedroom units).
#' @examples
#' \dontrun{
#' hud_fmr(year = 2026)
#' hud_fmr(year = 2025, state = "CA")
#' }
#' @export
hud_fmr <- function(year = 2026, state = NULL) {
  yr_short <- substr(as.character(year), 3, 4)
  url <- sprintf(
    "https://www.huduser.gov/portal/datasets/fmr/fmr%d/FY%s_FMRs.xlsx",
    year, yr_short
  )

  raw <- tryCatch(.hud_fetch_xlsx(url), error = function(e) {
    # Try alternate naming pattern
    url2 <- sprintf(
      "https://www.huduser.gov/portal/datasets/fmr/fmr%d/FY%d_4050_FMRs.xlsx",
      year, year
    )
    .hud_fetch_xlsx(url2)
  })

  if (nrow(raw) == 0) return(.schema_fmr)

  nms <- tolower(names(raw))
  names(raw) <- nms

  result <- tibble(
    state_abbr    = .safe_chr(raw$stusps %||% raw$state_alpha),
    state_code    = .safe_chr(raw$state),
    hud_area_code = .safe_chr(raw$hud_area_code %||% raw$metro_code),
    county_name   = .safe_chr(raw$countyname %||% raw$county),
    area_name     = .safe_chr(raw$hud_area_name %||% raw$areaname),
    metro         = .safe_int(raw$metro),
    fips          = .safe_chr(raw$fips %||% raw$fips2010),
    population    = .safe_int(raw$pop2023 %||% raw$pop2020 %||% raw$pop2010),
    fmr_0br       = .safe_int(raw$fmr_0),
    fmr_1br       = .safe_int(raw$fmr_1),
    fmr_2br       = .safe_int(raw$fmr_2),
    fmr_3br       = .safe_int(raw$fmr_3),
    fmr_4br       = .safe_int(raw$fmr_4)
  )

  if (!is.null(state)) {
    result <- result |> filter(toupper(state_abbr) == toupper(state))
  }

  result
}


# == Income Limits =============================================================

#' Get HUD Section 8 Income Limits
#'
#' Downloads official income limit data for a given fiscal year.
#' Includes median family income, Very Low Income (50%), Low Income (80%),
#' and Extremely Low Income (ELI) limits by household size.
#'
#' @param year Integer. Fiscal year (e.g. 2025, 2024). Default: 2025.
#' @param state Character or NULL. Two-letter state abbreviation to filter
#'   results (e.g. \code{"CA"}, \code{"TX"}).
#' @return A tibble with columns: \code{fips} (character),
#'   \code{state_abbr}, \code{state_code}, \code{state_name},
#'   \code{hud_area_code}, \code{area_name}, \code{county},
#'   \code{county_name}, \code{metro} (integer),
#'   \code{median_income} (integer), \code{l50_1} through \code{l50_4}
#'   (integer, 50% limits for 1-4 person households), \code{l80_1} through
#'   \code{l80_4} (integer, 80% limits), \code{eli_1} through \code{eli_4}
#'   (integer, extremely low income limits).
#' @examples
#' \dontrun{
#' hud_income_limits(year = 2025)
#' hud_income_limits(year = 2024, state = "NY")
#' }
#' @export
hud_income_limits <- function(year = 2025, state = NULL) {
  yr_short <- substr(as.character(year), 3, 4)
  url <- sprintf(
    "https://www.huduser.gov/portal/datasets/il/il%s/Section8-FY%s.xlsx",
    yr_short, yr_short
  )

  raw <- .hud_fetch_xlsx(url)
  if (nrow(raw) == 0) return(.schema_income_limits)

  nms <- tolower(names(raw))
  names(raw) <- nms

  med_col <- grep("^median", nms, value = TRUE)[1]

  result <- tibble(
    fips          = .safe_chr(raw$fips),
    state_abbr    = .safe_chr(raw$stusps),
    state_code    = .safe_chr(raw$state),
    state_name    = .safe_chr(raw$state_name),
    hud_area_code = .safe_chr(raw$hud_area_code),
    area_name     = .safe_chr(raw$hud_area_name),
    county        = .safe_chr(raw$county),
    county_name   = .safe_chr(raw$county_name),
    metro         = .safe_int(raw$metro),
    median_income = .safe_int(raw[[med_col]]),
    l50_1         = .safe_int(raw$l50_1),
    l50_2         = .safe_int(raw$l50_2),
    l50_3         = .safe_int(raw$l50_3),
    l50_4         = .safe_int(raw$l50_4),
    l80_1         = .safe_int(raw$l80_1),
    l80_2         = .safe_int(raw$l80_2),
    l80_3         = .safe_int(raw$l80_3),
    l80_4         = .safe_int(raw$l80_4),
    eli_1         = .safe_int(raw$eli_1),
    eli_2         = .safe_int(raw$eli_2),
    eli_3         = .safe_int(raw$eli_3),
    eli_4         = .safe_int(raw$eli_4)
  )

  if (!is.null(state)) {
    result <- result |> filter(toupper(state_abbr) == toupper(state))
  }

  result
}


# == Multifamily Inspection Scores =============================================

#' Get HUD Multifamily Physical Inspection Scores
#'
#' Downloads inspection scores for HUD-assisted multifamily properties.
#' Includes property details, location, and REAC inspection results.
#' Scores range from 0 to 100 (higher is better).
#'
#' @param state Character or NULL. Two-letter state code to filter results
#'   (e.g. \code{"CA"}, \code{"NY"}).
#' @param min_score Numeric or NULL. Minimum inspection score to filter
#'   (e.g. \code{60} to find properties passing inspection).
#' @return A tibble with columns: \code{inspection_id} (character),
#'   \code{property_id} (character), \code{property_name} (character),
#'   \code{address} (character), \code{city} (character),
#'   \code{state_abbr} (character), \code{state_fips} (character),
#'   \code{zip} (character), \code{county_name} (character),
#'   \code{cbsa_name} (character), \code{cbsa_code} (character),
#'   \code{latitude} (numeric), \code{longitude} (numeric),
#'   \code{inspection_score} (numeric, 0-100),
#'   \code{inspection_date} (character).
#' @examples
#' \dontrun{
#' hud_inspections(state = "NY")
#' hud_inspections(min_score = 90)
#' }
#' @export
hud_inspections <- function(state = NULL, min_score = NULL) {
  url <- "https://www.huduser.gov/portal/datasets/pis/multifamily_physical_inspection_scores.xlsx"
  raw <- .hud_fetch_xlsx(url)
  if (nrow(raw) == 0) return(.schema_inspections)

  nms <- toupper(names(raw))
  names(raw) <- nms

  result <- tibble(
    inspection_id    = .safe_chr(raw$INSPECTION_ID),
    property_id      = .safe_chr(raw$PROPERTY_ID),
    property_name    = .safe_chr(raw$PROPERTY_NAME),
    address          = .safe_chr(raw$ADDRESS),
    city             = .safe_chr(raw$CITY),
    state_abbr       = .safe_chr(raw$STATE_NAME),
    state_fips       = .safe_chr(raw$STATE_CODE),
    zip              = .safe_chr(raw$ZIP),
    county_name      = .safe_chr(raw$COUNTY_NAME),
    cbsa_name        = .safe_chr(raw$CBSA_NAME),
    cbsa_code        = .safe_chr(raw$CBSA_CODE),
    latitude         = .safe_num(raw$LATITUDE),
    longitude        = .safe_num(raw$LONGITUDE),
    inspection_score = .safe_num(raw$INSPECTION_SCORE),
    inspection_date  = .safe_chr(raw$INSPECTION_DATE)
  )

  if (!is.null(state)) {
    st <- toupper(state)
    result <- result |> filter(toupper(state_abbr) == st | state_fips == st)
  }
  if (!is.null(min_score)) {
    result <- result |> filter(inspection_score >= min_score)
  }

  result
}


# == Section 8 Contracts =======================================================

#' Get HUD Section 8 Contracts with Rent and Utility Allowance
#'
#' Downloads multifamily Section 8 contract data including rent amounts
#' and utility allowances by property and bedroom count. This is a large
#' dataset.
#'
#' @return A tibble with columns: \code{property_id} (integer),
#'   \code{contract_number} (character), \code{bedroom_count} (integer),
#'   \code{unit_count} (integer), \code{contract_rent} (numeric, dollars),
#'   \code{fair_market_rent} (numeric, dollars),
#'   \code{utility_allowance} (numeric, dollars).
#' @examples
#' \dontrun{
#' hud_section8_contracts()
#' }
#' @export
hud_section8_contracts <- function() {
  url <- "https://www.hud.gov/sites/dfiles/Housing/documents/contractsrentutilityamt.xlsx"
  raw <- .hud_fetch_xlsx(url)
  if (nrow(raw) == 0) return(.schema_contracts)

  nms <- tolower(names(raw))
  names(raw) <- nms

  tibble(
    property_id      = .safe_int(raw$property_id),
    contract_number  = .safe_chr(raw$contract_number),
    bedroom_count    = .safe_int(raw$assistance_bedroom_count),
    unit_count       = .safe_int(raw$assistance_unit_count),
    contract_rent    = .safe_num(raw$contract_rent_amount),
    fair_market_rent = .safe_num(raw$fair_market_rent_amount),
    utility_allowance = .safe_num(raw$utility_allowance_amount)
  )
}


# == Homeless Counts ===========================================================

#' Get HUD Annual Homeless Assessment Report (AHAR) counts by state
#'
#' Downloads the Housing Inventory Count (HIC) data showing shelter beds
#' and housing units for homeless populations by state. Data covers
#' Emergency Shelter (ES), Transitional Housing (TH), and Safe Haven (SH).
#'
#' @param year Integer. Year of the count, from 2007 to 2024
#'   (default: 2024).
#' @return A tibble with columns: \code{state} (character, 2-letter),
#'   \code{total_yr_beds} (integer, total year-round beds ES+TH+SH),
#'   \code{total_yr_beds_es} (integer, emergency shelter beds),
#'   \code{total_yr_beds_th} (integer, transitional housing beds),
#'   \code{total_yr_beds_sh} (integer, safe haven beds),
#'   \code{units_hh_children} (integer, units for households with children),
#'   \code{beds_hh_children} (integer, beds for households with children).
#'   Available columns may vary by year.
#' @examples
#' \dontrun{
#' hud_homeless_counts(year = 2024)
#' hud_homeless_counts(year = 2020)
#' }
#' @export
hud_homeless_counts <- function(year = 2024) {
  url <- "https://www.huduser.gov/portal/sites/default/files/xls/2007-2024-HIC-Counts-by-State.xlsx"
  valid_years <- 2007:2024

  if (!year %in% valid_years) {
    stop("year must be between 2007 and 2024")
  }

  raw <- tryCatch(
    .hud_fetch_xlsx(url, sheet = as.character(year), skip = 1),
    error = function(e) {
      # Sheets might be numbered by index
      idx <- which(valid_years == year)
      .hud_fetch_xlsx(url, sheet = length(valid_years) - idx + 1, skip = 1)
    }
  )

  if (nrow(raw) == 0) return(.schema_homeless)

  nms <- tolower(names(raw))
  names(raw) <- nms

  # Column names vary by year; find key columns by pattern
  state_col <- grep("^state", nms, value = TRUE)[1] %||% nms[1]
  yr_bed_cols <- grep("year.round.bed", nms, value = TRUE)
  es_col <- grep("\\(es\\)", nms, value = TRUE)
  th_col <- grep("\\(th\\)", nms, value = TRUE)
  sh_col <- grep("\\(sh\\)", nms, value = TRUE)
  units_children <- grep("units.*children", nms, value = TRUE)
  beds_children <- grep("beds.*children", nms, value = TRUE)

  result <- tibble(
    state = .safe_chr(raw[[state_col]])
  )

  # Total year-round beds (ES, TH, SH combined)
  if (length(yr_bed_cols) > 0) {
    total_col <- grep("total.*year.*round.*\\(es.*th.*sh\\)", yr_bed_cols, value = TRUE)
    non_dv <- grep("non.dv", total_col, value = TRUE)
    main_col <- setdiff(total_col, non_dv)
    if (length(main_col) > 0) result$total_yr_beds <- .safe_int(raw[[main_col[1]]])
  }

  # ES beds
  if (length(es_col) > 0) {
    es_yr <- grep("year.*round.*\\(es\\)", es_col, value = TRUE)
    if (length(es_yr) > 0) result$total_yr_beds_es <- .safe_int(raw[[es_yr[1]]])
  }

  # TH beds
  if (length(th_col) > 0) {
    th_yr <- grep("year.*round.*\\(th\\)", th_col, value = TRUE)
    if (length(th_yr) > 0) result$total_yr_beds_th <- .safe_int(raw[[th_yr[1]]])
  }

  # SH beds
  if (length(sh_col) > 0) {
    sh_yr <- grep("year.*round.*\\(sh\\)", sh_col, value = TRUE)
    if (length(sh_yr) > 0) result$total_yr_beds_sh <- .safe_int(raw[[sh_yr[1]]])
  }

  # Children units/beds
  if (length(units_children) > 0) {
    result$units_hh_children <- .safe_int(raw[[units_children[1]]])
  }
  if (length(beds_children) > 0) {
    result$beds_hh_children <- .safe_int(raw[[beds_children[1]]])
  }

  # Remove summary rows (e.g. "Total")

result <- result |> filter(!is.na(state), nchar(state) == 2)

  result
}


# == Context ===================================================================

#' Get hud.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hud_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hud_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/hud.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "hud.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# hud.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# hud.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
