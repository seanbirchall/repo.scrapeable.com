# mo.gov.R
# Self-contained Missouri Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA at data.mo.gov


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.mog_base <- "https://data.mo.gov"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

._soda_query <- function(view_id, where = NULL, select = NULL,
                         group = NULL, order = NULL, q = NULL,
                         limit = 1000, offset = 0) {
  params <- list(`$limit` = limit, `$offset` = offset)
  if (!is.null(where))  params[["$where"]]  <- where
  if (!is.null(select)) params[["$select"]] <- select
  if (!is.null(group))  params[["$group"]]  <- group
  if (!is.null(order))  params[["$order"]]  <- order
  if (!is.null(q))      params[["$q"]]      <- q

  query_str <- paste(
    names(params),
    vapply(params, as.character, character(1)),
    sep = "=", collapse = "&"
  )
  url <- sprintf("%s/resource/%s.json?%s", .mog_base, view_id, query_str)
  url <- utils::URLencode(url, reserved = FALSE)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Missouri SODA query error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(tibble())
  as_tibble(raw)
}

._soda_fetch_all <- function(view_id, where = NULL, select = NULL,
                             order = NULL, max_rows = 50000,
                             page_size = 1000) {
  results <- list()
  offset <- 0
  while (offset < max_rows) {
    batch <- ._soda_query(view_id, where = where, select = select,
                          order = order, limit = page_size, offset = offset)
    if (nrow(batch) == 0) break
    results[[length(results) + 1]] <- batch
    offset <- offset + nrow(batch)
    if (nrow(batch) < page_size) break
  }
  if (length(results) == 0) return(tibble())
  bind_rows(results)
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), category = character(),
  description = character(), updated_at = as.POSIXct(character())
)

# == Discovery =================================================================

#' List Missouri Open Data datasets
#'
#' Returns catalog metadata from the data.mo.gov Socrata portal.
#' Missouri publishes hundreds of datasets covering employment, agriculture,
#' public safety, health, and more.
#'
#' @param limit Integer. Maximum datasets to return (default 100).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Socrata 4x4 dataset identifier.}
#'     \item{name}{Character. Dataset name.}
#'     \item{category}{Character. Dataset category (e.g. "Public Safety").}
#'     \item{description}{Character. Truncated description (max 200 chars).}
#'     \item{updated_at}{POSIXct. Timestamp of last data update.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' mog_list(limit = 20)
#' }
mog_list <- function(limit = 100) {
  url <- sprintf("%s/api/views?limit=%d", .mog_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Missouri catalog error: ", e$message)
    return(NULL)
  })
  if (is.null(raw) || length(raw) == 0) return(.schema_datasets)

  tibble(
    id          = as.character(raw$id),
    name        = as.character(raw$name),
    category    = as.character(raw$category %||% NA_character_),
    description = vapply(raw$description %||% rep(NA_character_, length(raw$id)),
                         function(x) {
                           x <- x %||% ""
                           if (is.na(x)) x <- ""
                           if (nchar(x) > 200) paste0(substr(x, 1, 200), "...") else x
                         }, character(1)),
    updated_at  = as.POSIXct(as.numeric(raw$rowsUpdatedAt %||% NA_real_),
                             origin = "1970-01-01")
  )
}

#' Search Missouri Open Data datasets
#'
#' Searches catalog by keyword, filtering the full catalog list by
#' case-insensitive substring match against dataset name and description.
#'
#' @param query Character. Search term (case-insensitive, matches name and description).
#' @param limit Integer. Max datasets to scan from catalog (default 500).
#' @return A tibble with columns: id, name, category, description, updated_at.
#'   Same structure as \code{\link{mog_list}}.
#' @export
#' @examples
#' \dontrun{
#' mog_search("unemployment")
#' mog_search("alcohol")
#' }
mog_search <- function(query, limit = 500) {
  all_ds <- mog_list(limit = limit)
  if (nrow(all_ds) == 0) return(.schema_datasets)
  all_ds |> filter(
    grepl(query, name, ignore.case = TRUE) |
    grepl(query, description, ignore.case = TRUE)
  )
}

# == Generic SODA query ========================================================

#' Query any Missouri dataset via SODA
#'
#' Generic SODA query interface for data.mo.gov. Pass a Socrata view ID
#' and optional SoQL clauses to filter, aggregate, or order results.
#' Use \code{\link{mog_list}} to discover available dataset IDs.
#'
#' @param view_id Character. Socrata 4x4 view identifier (e.g. \code{"yyhn-562y"}).
#' @param where Character or NULL. SoQL WHERE clause for filtering rows
#'   (e.g. \code{"upper(city) = 'COLUMBIA'"}).
#' @param select Character or NULL. SoQL SELECT clause for choosing columns.
#' @param group Character or NULL. SoQL GROUP BY clause for aggregation.
#' @param order Character or NULL. SoQL ORDER BY clause for sorting
#'   (e.g. \code{"date DESC"}).
#' @param q Character or NULL. Full-text search query across all text fields.
#' @param limit Integer. Max rows to return (default 1000).
#' @param offset Integer. Pagination offset (default 0).
#' @return A tibble of query results. Columns depend on the dataset.
#' @export
#' @examples
#' \dontrun{
#' mog_view("yyhn-562y", limit = 10)
#' mog_view("yyhn-562y", where = "upper(city) = 'COLUMBIA'")
#' }
mog_view <- function(view_id, where = NULL, select = NULL,
                     group = NULL, order = NULL, q = NULL,
                     limit = 1000, offset = 0) {
  ._soda_query(view_id, where = where, select = select, group = group,
               order = order, q = q, limit = limit, offset = offset)
}

# == Alcohol Licenses ==========================================================

#' Missouri active alcohol licenses
#'
#' Current license information for businesses involved in manufacture, shipping,
#' and sale of alcohol in Missouri. Data sourced from the Division of Alcohol
#' and Tobacco Control.
#'
#' @param city Character or NULL. Filter by city name (case-insensitive exact match).
#' @param county Character or NULL. Filter by county name (case-insensitive substring).
#' @param license_type Character or NULL. Filter by primary_type
#'   (e.g. \code{"Retail by Drink"}, \code{"Manufacturer"}).
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of alcohol license records with columns including
#'   business name, address, city, county, primary_type, and license status.
#' @export
#' @examples
#' \dontrun{
#' mog_alcohol_licenses(city = "Springfield", limit = 50)
#' mog_alcohol_licenses(license_type = "Retail by Drink")
#' }
mog_alcohol_licenses <- function(city = NULL, county = NULL,
                                 license_type = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(city))
    clauses <- c(clauses, sprintf("upper(city) = '%s'", toupper(city)))
  if (!is.null(county))
    clauses <- c(clauses, sprintf("upper(county) LIKE '%%%s%%'", toupper(county)))
  if (!is.null(license_type))
    clauses <- c(clauses, sprintf("primary_type = '%s'", license_type))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("yyhn-562y", where = where, limit = limit)
}

# == Unemployment ==============================================================

#' Missouri weekly initial unemployment claims
#'
#' Number of people filing initial unemployment benefits by county and week.
#' Data published by the Missouri Department of Labor.
#'
#' @param county Character or NULL. Filter by county name (case-insensitive exact match).
#' @param start_date Character or NULL. Start date in \code{"YYYY-MM-DD"} format.
#' @param end_date Character or NULL. End date in \code{"YYYY-MM-DD"} format.
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble with columns:
#'   \describe{
#'     \item{weekending}{Date. End of the reporting week.}
#'     \item{county}{Character. Missouri county name.}
#'     \item{claims}{Integer. Number of initial claims filed.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' mog_unemployment_claims(county = "Jackson", limit = 52)
#' mog_unemployment_claims(start_date = "2024-01-01", end_date = "2024-12-31")
#' }
mog_unemployment_claims <- function(county = NULL, start_date = NULL,
                                    end_date = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(county))
    clauses <- c(clauses, sprintf("upper(county) = '%s'", toupper(county)))
  if (!is.null(start_date))
    clauses <- c(clauses, sprintf("weekending >= '%s'", start_date))
  if (!is.null(end_date))
    clauses <- c(clauses, sprintf("weekending <= '%s'", end_date))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("qet9-8yam", where = where,
                      order = "weekending DESC", limit = limit)
  if (nrow(df) > 0 && "claims" %in% names(df)) {
    df$claims <- as.integer(df$claims)
    if ("weekending" %in% names(df))
      df$weekending <- as.Date(substr(df$weekending, 1, 10))
  }
  df
}

#' Missouri monthly unemployment claims by demographics
#'
#' Returns unemployment claims broken down by demographic dimension.
#' Each dimension maps to a separate Socrata dataset maintained by
#' the Missouri Department of Labor.
#'
#' @param dimension Character. One of \code{"age"}, \code{"sex"}, \code{"race"},
#'   \code{"ethnicity"}, or \code{"industry"}.
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of unemployment claims grouped by the chosen dimension.
#'   Columns vary by dimension.
#' @export
#' @examples
#' \dontrun{
#' mog_unemployment_demographics("age")
#' mog_unemployment_demographics("industry", limit = 500)
#' }
mog_unemployment_demographics <- function(dimension = c("age", "sex", "race",
                                                        "ethnicity", "industry"),
                                          limit = 1000) {
  dimension <- match.arg(dimension)
  view_map <- c(
    age       = "5tqh-2x4m",
    sex       = "4v5t-4kqk",
    race      = "cq57-7qrb",
    ethnicity = "xm42-6a8n",
    industry  = "cj66-t7xq"
  )
  ._soda_query(view_map[[dimension]], limit = limit)
}

#' Missouri unemployment rates by region
#'
#' Unemployment rates for various Missouri workforce development regions
#' and metropolitan statistical areas.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of unemployment rate records by region.
#' @export
mog_unemployment_rates <- function(limit = 1000) {
  ._soda_query("uaxb-77vv", limit = limit)
}

#' Missourians currently receiving unemployment benefits
#'
#' Count of Missourians currently receiving regular or extended
#' unemployment insurance benefits, published by the Department of Labor.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of benefit recipient counts.
#' @export
mog_unemployment_benefits <- function(limit = 1000) {
  ._soda_query("uite-mset", limit = limit)
}

# == State Employee Pay ========================================================

#' Missouri state employee pay
#'
#' Pay information for Missouri state employees by agency, position, and name.
#' Data published by the Missouri Accountability Portal. Each fiscal year
#' maps to a separate Socrata dataset.
#'
#' @param year Integer. Calendar year (default 2025). Available: 2007--2025.
#' @param agency Character or NULL. Filter by agency name
#'   (case-insensitive substring match, e.g. \code{"HEALTH"}).
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble with columns including calendar_year, agency_name,
#'   position_title, employee_name, and ytd_gross_pay (numeric).
#' @export
#' @examples
#' \dontrun{
#' mog_employee_pay(year = 2024, limit = 50)
#' mog_employee_pay(year = 2023, agency = "CONSERVATION")
#' }
mog_employee_pay <- function(year = 2025, agency = NULL, limit = 1000) {
  year_map <- c(
    "2025" = "przm-8aj2", "2024" = "nkgr-ek2r", "2023" = "d5qn-3gs9",
    "2022" = "7m33-xh7t", "2021" = "7j8x-y8ki", "2020" = "53ys-svak",
    "2019" = "wnyf-5ce9", "2018" = "g8g2-6fyn", "2017" = "mesr-3mki",
    "2016" = "qy64-qzrf", "2015" = "8yf2-bu4d", "2014" = "i6tz-nkqw",
    "2013" = "a6nm-4wbm", "2012" = "32yd-5xay", "2011" = "hj93-xter",
    "2010" = "8ama-dpbv", "2009" = "j82q-875v", "2008" = "uccy-42en",
    "2007" = "gapb-jve5"
  )
  yr <- as.character(year)
  if (!(yr %in% names(year_map))) {
    warning("Year ", yr, " not available. Available: 2007-2025.")
    return(tibble())
  }
  clauses <- character()
  if (!is.null(agency))
    clauses <- c(clauses, sprintf("upper(agency_name) LIKE '%%%s%%'", toupper(agency)))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query(year_map[[yr]], where = where, limit = limit)
  if (nrow(df) > 0 && "ytd_gross_pay" %in% names(df))
    df$ytd_gross_pay <- as.numeric(df$ytd_gross_pay)
  df
}

# == State Expenditures ========================================================

#' Missouri state expenditures
#'
#' Financial data on purchases of goods, services, and program disbursements
#' from the Missouri Accountability Portal. Each fiscal year maps to a
#' separate Socrata dataset.
#'
#' @param year Integer. Fiscal year (default 2025). Available: 2000--2025.
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of state expenditure records with columns including
#'   vendor, amount, agency, and category.
#' @export
#' @examples
#' \dontrun{
#' mog_expenditures(year = 2024, limit = 100)
#' }
mog_expenditures <- function(year = 2025, limit = 1000) {
  year_map <- c(
    "2025" = "fzgp-ixr3", "2024" = "5inf-tn5a", "2023" = "k3n5-mge9",
    "2022" = "c9pi-83hm", "2021" = "p27p-ab3w", "2020" = "38q6-ispi",
    "2019" = "vdwt-j4az", "2018" = "vz8q-uras", "2017" = "9y8n-7j7s",
    "2016" = "qmp6-kir9", "2015" = "bumc-k7z5", "2014" = "22ge-mvu2",
    "2013" = "fzch-e4hj", "2012" = "rhxd-xjwr", "2011" = "i6et-maqi",
    "2010" = "wc3r-4zif", "2009" = "mtds-3zxq", "2008" = "82dy-es2p",
    "2007" = "semf-h84d", "2006" = "wasw-92p8", "2005" = "ypzi-kwbm",
    "2004" = "kgsy-y9bk", "2003" = "rpcp-f3vi", "2002" = "hesw-6i7u",
    "2001" = "fg7p-bsmx", "2000" = "27js-tn42"
  )
  yr <- as.character(year)
  if (!(yr %in% names(year_map))) {
    warning("Year ", yr, " not available. Available: 2000-2025.")
    return(tibble())
  }
  ._soda_query(year_map[[yr]], limit = limit)
}

# == Agriculture ===============================================================

#' Missouri farmers' markets
#'
#' Directory of farmers' markets across Missouri, maintained by the
#' Missouri Department of Agriculture. Includes location, hours, and
#' contact information.
#'
#' @param county Character or NULL. Filter by county name (case-insensitive exact match).
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of farmers' market records.
#' @export
#' @examples
#' \dontrun{
#' mog_farmers_markets()
#' mog_farmers_markets(county = "Boone")
#' }
mog_farmers_markets <- function(county = NULL, limit = 1000) {
  where <- if (!is.null(county))
    sprintf("upper(county) = '%s'", toupper(county)) else NULL
  ._soda_query("2zg8-cta8", where = where, limit = limit)
}

#' Missouri feed sample testing results
#'
#' Feed sample testing data from the Missouri Department of Agriculture's
#' Bureau of Feed, Seed, and Treated Timber.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of feed sample testing data.
#' @export
mog_feed_samples <- function(limit = 1000) {
  ._soda_query("y9w9-qkg2", limit = limit)
}

#' Missouri weekly feeder cattle weighted average report
#'
#' Weekly weighted average prices for feeder cattle in Missouri,
#' from USDA Market News reporting.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of cattle pricing data.
#' @export
mog_cattle_prices <- function(limit = 1000) {
  ._soda_query("4mcy-zagg", limit = limit)
}

# == Public Safety =============================================================

#' Missouri fire departments
#'
#' List of fire departments registered with the Missouri Division of
#' Fire Safety. Includes department name, county, and contact information.
#'
#' @param county Character or NULL. Filter by county name (case-insensitive exact match).
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of fire department records.
#' @export
#' @examples
#' \dontrun{
#' mog_fire_departments()
#' mog_fire_departments(county = "Boone")
#' }
mog_fire_departments <- function(county = NULL, limit = 1000) {
  where <- if (!is.null(county))
    sprintf("upper(countyname) = '%s'", toupper(county)) else NULL
  ._soda_query("gbr4-c765", where = where, limit = limit)
}

#' Missouri law enforcement agencies
#'
#' Active law enforcement agencies registered with the Missouri Department
#' of Public Safety, including Sheriff, Municipal, University, and Court agencies.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of law enforcement agency records.
#' @export
mog_law_enforcement <- function(limit = 1000) {
  ._soda_query("cgbu-k38b", limit = limit)
}

# == Health & Senior Services ==================================================

#' Missouri long-term care facilities directory
#'
#' LTC facilities licensed by the Missouri Department of Health and Senior
#' Services (DHSS), including residential care, assisted living, intermediate
#' care, and skilled nursing facilities.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of LTC facility records including name, type, capacity,
#'   and location.
#' @export
mog_ltc_facilities <- function(limit = 1000) {
  ._soda_query("fenu-sipv", limit = limit)
}

#' Missouri major medical equipment inventory
#'
#' Inventory of major medical equipment (MRI, CT, PET scanners, etc.)
#' reported to the Missouri Certificate of Need program.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of medical equipment records.
#' @export
mog_medical_equipment <- function(limit = 1000) {
  ._soda_query("bkz7-5dby", limit = limit)
}

#' Missouri WIC program data
#'
#' Women, Infants, and Children (WIC) households enrolled during state
#' fiscal year 2024 with aggregated benefit redemption values by county
#' and local agency.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of WIC enrollment and benefit data.
#' @export
mog_wic_data <- function(limit = 1000) {
  ._soda_query("diyi-fr2a", limit = limit)
}

# == Education & FAFSA =========================================================

#' Missouri FAFSA completion data
#'
#' Number of Free Applications for Federal Student Aid (FAFSA) completed
#' and reported to the Missouri Department of Higher Education (MDHE),
#' broken down by high school.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of FAFSA completion records by school.
#' @export
mog_fafsa <- function(limit = 1000) {
  ._soda_query("t9f4-ncza", limit = limit)
}

#' Missouri high school senior counts
#'
#' Number of high school seniors by school year and school, reported to
#' the Missouri Department of Higher Education.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of senior counts by school and year.
#' @export
mog_hs_seniors <- function(limit = 1000) {
  ._soda_query("8yaf-xv66", limit = limit)
}

# == Employment & Careers ======================================================

#' Missouri job centers
#'
#' Directory of Missouri Career Centers offering job search assistance,
#' training referrals, and hiring services. Maintained by the Division
#' of Workforce Development.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of job center records with location and contact info.
#' @export
mog_job_centers <- function(limit = 1000) {
  ._soda_query("p2ie-br32", limit = limit)
}

#' Missouri state government job openings
#'
#' Current openings in Missouri state government posted on MoCareers.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of current state government job openings.
#' @export
mog_state_jobs <- function(limit = 1000) {
  ._soda_query("83mm-j7ms", limit = limit)
}

#' Missouri top jobs projections
#'
#' Top career-graded jobs from 2018--2028 occupational projections
#' published by the Missouri Economic Research and Information Center.
#' Includes wages, education requirements, and growth rates.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of top job projections with wages and growth data.
#' @export
mog_top_jobs <- function(limit = 1000) {
  ._soda_query("fs9r-muy5", limit = limit)
}

#' Missouri first reports of injury by county
#'
#' Workers' compensation first reports of injury aggregated by county,
#' published by the Missouri Division of Workers' Compensation.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of injury report counts by county.
#' @export
mog_injury_reports <- function(limit = 1000) {
  ._soda_query("p7xr-4mcb", limit = limit)
}

# == Natural Resources =========================================================

#' Missouri springs
#'
#' Known and probable spring locations from USGS and field verification,
#' maintained by the Missouri Department of Natural Resources. Missouri has
#' over 3,100 identified springs.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of spring location records with name, county, and coordinates.
#' @export
mog_springs <- function(limit = 1000) {
  ._soda_query("ifez-23qm", limit = limit)
}

#' Missouri impaired water bodies (lakes)
#'
#' Missouri lakes assessed as impaired per Clean Water Act Section 305(b),
#' maintained by the Missouri Department of Natural Resources.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of impaired lake records with water body name, pollutant,
#'   and impairment status.
#' @export
mog_impaired_lakes <- function(limit = 1000) {
  ._soda_query("qsaa-z2sf", limit = limit)
}

#' Missouri impaired water bodies (rivers and streams)
#'
#' Missouri rivers and streams assessed as impaired per Clean Water Act
#' Section 305(b), maintained by the Department of Natural Resources.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of impaired river/stream records.
#' @export
mog_impaired_rivers <- function(limit = 1000) {
  ._soda_query("m57b-5i2q", limit = limit)
}

# == Tobacco & Licensing =======================================================

#' Missouri cigarette and tobacco product licensees
#'
#' Active licensees registered with the Missouri Division of Alcohol
#' and Tobacco Control for cigarette and tobacco product sales.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of tobacco licensee records.
#' @export
mog_tobacco_licensees <- function(limit = 1000) {
  ._soda_query("inpf-ekav", limit = limit)
}

#' Missouri motor fuel licensees
#'
#' Active motor fuel distributor and transporter licensees registered
#' with the Missouri Department of Revenue.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of motor fuel licensee records.
#' @export
mog_motor_fuel_licensees <- function(limit = 1000) {
  ._soda_query("kf27-upwz", limit = limit)
}

# == Geography & Reference =====================================================

#' Missouri driver and motor vehicle license offices
#'
#' Directory of Missouri Department of Revenue license offices
#' providing driver licensing and motor vehicle registration services.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of DMV office records with location and hours.
#' @export
mog_dmv_offices <- function(limit = 1000) {
  ._soda_query("835g-7keg", limit = limit)
}

#' Missouri counties reference
#'
#' Reference table of all 114 Missouri counties plus St. Louis City,
#' with county name, FIPS code, county seat, and geographic coordinates.
#'
#' @param limit Integer. Max rows to return (default 200).
#' @return A tibble of Missouri county records including name, FIPS, and
#'   centroid coordinates.
#' @export
mog_counties <- function(limit = 200) {
  ._soda_query("byps-gsbw", limit = limit)
}

#' Missouri zip codes by county and city
#'
#' Mapping of ZIP codes to counties and cities in Missouri.
#'
#' @param limit Integer. Max rows to return (default 2000).
#' @return A tibble of zip code/county/city mappings.
#' @export
mog_zip_codes <- function(limit = 2000) {
  ._soda_query("im7g-fucq", limit = limit)
}

# == Miscellaneous =============================================================

#' Missouri open meetings schedule
#'
#' Scheduled open meetings of Missouri state boards, commissions,
#' and agencies as required by the Sunshine Law.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of scheduled open meetings.
#' @export
mog_open_meetings <- function(limit = 1000) {
  ._soda_query("au6r-w9n3", limit = limit)
}

#' Missouri State Fair results
#'
#' Competition results from the Missouri State Fair in Sedalia.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of state fair competition result records.
#' @export
mog_state_fair <- function(limit = 1000) {
  ._soda_query("bh6t-9679", limit = limit)
}

#' Missouri hazardous waste facilities
#'
#' Facilities permitted for treatment, storage, or disposal of hazardous
#' waste, regulated by the Missouri Department of Natural Resources.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of hazardous waste facility records.
#' @export
mog_hazardous_waste <- function(limit = 1000) {
  ._soda_query("m7dn-rv29", limit = limit)
}

#' Missouri DNR chronic violators
#'
#' Facilities identified as chronic violators of environmental regulations
#' by the Missouri Department of Natural Resources.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of chronic environmental violator records.
#' @export
mog_dnr_violators <- function(limit = 1000) {
  ._soda_query("szw8-jbqy", limit = limit)
}

#' Missouri consumer confidence reports (drinking water)
#'
#' Annual consumer confidence reports (CCR) from public water systems
#' in Missouri, as required by the Safe Drinking Water Act.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of consumer confidence report records.
#' @export
mog_water_quality <- function(limit = 1000) {
  ._soda_query("3mwf-kse4", limit = limit)
}

#' Missouri finance entities
#'
#' Registered financial entities in Missouri, maintained by the
#' Division of Finance.
#'
#' @param limit Integer. Max rows to return (default 1000).
#' @return A tibble of Missouri finance entities.
#' @export
mog_finance_entities <- function(limit = 1000) {
  ._soda_query("vfrr-8z5c", limit = limit)
}

# == Context ===================================================================

#' Get mo.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
mog_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(mog_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/mo.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "mo.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# mo.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# mo.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
