# mo.gov.R
# Self-contained Missouri Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA at data.mo.gov

library(dplyr, warn.conflicts = FALSE)
library(tibble)

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
#'
#' @param limit Maximum datasets to return (default 100)
#' @return tibble: id, name, category, description, updated_at
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
#' Searches catalog by keyword. Filters the full catalog list.
#'
#' @param query Search term (case-insensitive, matches name and description)
#' @param limit Max datasets to scan (default 500)
#' @return tibble: id, name, category, description, updated_at
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
#' Generic SODA query interface. Pass a Socrata view ID and optional
#' SoQL clauses.
#'
#' @param view_id Socrata 4x4 view identifier (e.g. "yyhn-562y")
#' @param where SoQL WHERE clause
#' @param select SoQL SELECT clause
#' @param group SoQL GROUP BY clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search query
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
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
#' and sale of alcohol in Missouri.
#'
#' @param city Filter by city (case-insensitive)
#' @param county Filter by county (case-insensitive)
#' @param license_type Filter by primary_type (e.g. "Retail by Drink")
#' @param limit Max rows (default 1000)
#' @return tibble of alcohol license records
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
#'
#' @param county Filter by county name
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param limit Max rows (default 1000)
#' @return tibble: weekending, county, claims
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
#' Returns claims broken down by age, sex, race, ethnicity, or industry.
#'
#' @param dimension One of "age", "sex", "race", "ethnicity", "industry"
#' @param limit Max rows (default 1000)
#' @return tibble of unemployment claims by chosen dimension
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
#' @param limit Max rows (default 1000)
#' @return tibble of unemployment rates for various Missouri regions
mog_unemployment_rates <- function(limit = 1000) {
  ._soda_query("uaxb-77vv", limit = limit)
}

#' Missourians currently receiving unemployment benefits
#'
#' @param limit Max rows (default 1000)
#' @return tibble of benefit recipient counts
mog_unemployment_benefits <- function(limit = 1000) {
  ._soda_query("uite-mset", limit = limit)
}

# == State Employee Pay ========================================================

#' Missouri state employee pay
#'
#' Pay information for state employees by agency, position, and name.
#'
#' @param year Calendar year (default 2025). Available: 2007-2025.
#' @param agency Filter by agency name (case-insensitive partial match)
#' @param limit Max rows (default 1000)
#' @return tibble: calendar_year, agency_name, position_title, employee_name, ytd_gross_pay
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
#' Financial data on purchases of goods, services, and program disbursements.
#'
#' @param year Fiscal year (default 2025). Available: 2000-2025.
#' @param limit Max rows (default 1000)
#' @return tibble of state expenditure records
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
#' Directory of farmers' markets across Missouri.
#'
#' @param county Filter by county name
#' @param limit Max rows (default 1000)
#' @return tibble of farmers' market records
mog_farmers_markets <- function(county = NULL, limit = 1000) {
  where <- if (!is.null(county))
    sprintf("upper(county) = '%s'", toupper(county)) else NULL
  ._soda_query("2zg8-cta8", where = where, limit = limit)
}

#' Missouri feed sample testing results
#'
#' @param limit Max rows (default 1000)
#' @return tibble of feed sample testing data
mog_feed_samples <- function(limit = 1000) {
  ._soda_query("y9w9-qkg2", limit = limit)
}

#' Missouri weekly feeder cattle weighted average report
#'
#' @param limit Max rows (default 1000)
#' @return tibble of cattle pricing data
mog_cattle_prices <- function(limit = 1000) {
  ._soda_query("4mcy-zagg", limit = limit)
}

# == Public Safety =============================================================

#' Missouri fire departments
#'
#' List of fire departments registered with the Division of Fire Safety.
#'
#' @param county Filter by county
#' @param limit Max rows (default 1000)
#' @return tibble of fire department records
mog_fire_departments <- function(county = NULL, limit = 1000) {
  where <- if (!is.null(county))
    sprintf("upper(countyname) = '%s'", toupper(county)) else NULL
  ._soda_query("gbr4-c765", where = where, limit = limit)
}

#' Missouri law enforcement agencies
#'
#' Active law enforcement agencies (Sheriff, Municipal, University, Court, etc).
#'
#' @param limit Max rows (default 1000)
#' @return tibble of law enforcement agency records
mog_law_enforcement <- function(limit = 1000) {
  ._soda_query("cgbu-k38b", limit = limit)
}

# == Health & Senior Services ==================================================

#' Missouri long-term care facilities directory
#'
#' LTC facilities licensed by the Missouri DHSS including residential care,
#' assisted living, intermediate care, and skilled nursing facilities.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of LTC facility records
mog_ltc_facilities <- function(limit = 1000) {
  ._soda_query("fenu-sipv", limit = limit)
}

#' Missouri major medical equipment inventory
#'
#' @param limit Max rows (default 1000)
#' @return tibble of medical equipment records
mog_medical_equipment <- function(limit = 1000) {
  ._soda_query("bkz7-5dby", limit = limit)
}

#' Missouri WIC program data
#'
#' WIC households enrolled during state fiscal year 2024 with aggregated
#' benefit redemption values.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of WIC enrollment data
mog_wic_data <- function(limit = 1000) {
  ._soda_query("diyi-fr2a", limit = limit)
}

# == Education & FAFSA =========================================================

#' Missouri FAFSA completion data
#'
#' Number of FAFSAs completed, reported to MDHE, by school.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of FAFSA completion records
mog_fafsa <- function(limit = 1000) {
  ._soda_query("t9f4-ncza", limit = limit)
}

#' Missouri high school senior counts
#'
#' Number of seniors by school year and school.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of senior counts
mog_hs_seniors <- function(limit = 1000) {
  ._soda_query("8yaf-xv66", limit = limit)
}

# == Employment & Careers ======================================================

#' Missouri job centers
#'
#' Missouri Career Centers offering job search and hiring assistance.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of job center records
mog_job_centers <- function(limit = 1000) {
  ._soda_query("p2ie-br32", limit = limit)
}

#' Missouri state government job openings
#'
#' @param limit Max rows (default 1000)
#' @return tibble of current state government openings
mog_state_jobs <- function(limit = 1000) {
  ._soda_query("83mm-j7ms", limit = limit)
}

#' Missouri top jobs projections
#'
#' Top career-graded jobs from 2018-2028 occupational projections.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of top job projections with wages
mog_top_jobs <- function(limit = 1000) {
  ._soda_query("fs9r-muy5", limit = limit)
}

#' Missouri first reports of injury by county
#'
#' Workers' compensation first reports of injury by county.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of injury reports
mog_injury_reports <- function(limit = 1000) {
  ._soda_query("p7xr-4mcb", limit = limit)
}

# == Natural Resources =========================================================

#' Missouri springs
#'
#' Known and probable spring locations from USGS and field verification.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of spring location records
mog_springs <- function(limit = 1000) {
  ._soda_query("ifez-23qm", limit = limit)
}

#' Missouri impaired water bodies (lakes)
#'
#' Missouri waters assessed as impaired in 2016 per Clean Water Act Section 305(b).
#'
#' @param limit Max rows (default 1000)
#' @return tibble of impaired lake records
mog_impaired_lakes <- function(limit = 1000) {
  ._soda_query("qsaa-z2sf", limit = limit)
}

#' Missouri impaired water bodies (rivers and streams)
#'
#' @param limit Max rows (default 1000)
#' @return tibble of impaired river/stream records
mog_impaired_rivers <- function(limit = 1000) {
  ._soda_query("m57b-5i2q", limit = limit)
}

# == Tobacco & Licensing =======================================================

#' Missouri cigarette and tobacco product licensees
#'
#' @param limit Max rows (default 1000)
#' @return tibble of tobacco licensee records
mog_tobacco_licensees <- function(limit = 1000) {
  ._soda_query("inpf-ekav", limit = limit)
}

#' Missouri motor fuel licensees
#'
#' @param limit Max rows (default 1000)
#' @return tibble of motor fuel licensee records
mog_motor_fuel_licensees <- function(limit = 1000) {
  ._soda_query("kf27-upwz", limit = limit)
}

# == Geography & Reference =====================================================

#' Missouri driver and motor vehicle license offices
#'
#' @param limit Max rows (default 1000)
#' @return tibble of DMV office records
mog_dmv_offices <- function(limit = 1000) {
  ._soda_query("835g-7keg", limit = limit)
}

#' Missouri counties reference
#'
#' County name, FIPS, county seat, and geographic coordinates.
#'
#' @param limit Max rows (default 200)
#' @return tibble of Missouri county records
mog_counties <- function(limit = 200) {
  ._soda_query("byps-gsbw", limit = limit)
}

#' Missouri zip codes by county and city
#'
#' @param limit Max rows (default 2000)
#' @return tibble of zip code/county/city mappings
mog_zip_codes <- function(limit = 2000) {
  ._soda_query("im7g-fucq", limit = limit)
}

# == Miscellaneous =============================================================

#' Missouri open meetings schedule
#'
#' @param limit Max rows (default 1000)
#' @return tibble of scheduled open meetings
mog_open_meetings <- function(limit = 1000) {
  ._soda_query("au6r-w9n3", limit = limit)
}

#' Missouri State Fair results
#'
#' @param limit Max rows (default 1000)
#' @return tibble of state fair result records
mog_state_fair <- function(limit = 1000) {
  ._soda_query("bh6t-9679", limit = limit)
}

#' Missouri hazardous waste facilities
#'
#' @param limit Max rows (default 1000)
#' @return tibble of hazardous waste treatment/storage/disposal facilities
mog_hazardous_waste <- function(limit = 1000) {
  ._soda_query("m7dn-rv29", limit = limit)
}

#' Missouri DNR chronic violators
#'
#' @param limit Max rows (default 1000)
#' @return tibble of chronic environmental violator records
mog_dnr_violators <- function(limit = 1000) {
  ._soda_query("szw8-jbqy", limit = limit)
}

#' Missouri consumer confidence reports (drinking water)
#'
#' @param limit Max rows (default 1000)
#' @return tibble of consumer confidence report records
mog_water_quality <- function(limit = 1000) {
  ._soda_query("3mwf-kse4", limit = limit)
}

#' Missouri finance entities
#'
#' @param limit Max rows (default 1000)
#' @return tibble of Missouri finance entities
mog_finance_entities <- function(limit = 1000) {
  ._soda_query("vfrr-8z5c", limit = limit)
}

# == Context ===================================================================

#' Generate LLM-friendly context for the Missouri Open Data client
#'
#' Reads own source and prints a summary of all public functions.
#'
#' @return Character string (invisibly), also printed
mog_context <- function() {
  src <- tryCatch(
    {
      fn_env <- sys.frame(0)
      src_file <- getSrcFilename(fn_env$.self %||% mog_context, full.names = TRUE)
      if (is.null(src_file) || src_file == "") stop("no source")
      readLines(src_file, warn = FALSE)
    },
    error = function(e) {
      sf <- system.file("source", "mo.R", package = "mo.gov")
      if (sf != "") return(readLines(sf, warn = FALSE))
      NULL
    }
  )

  header <- c(
    "# mo.gov - Missouri Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Portal: https://data.mo.gov (168 datasets, 153 Socrata views)",
    "#",
    "# All functions return tibbles. Use mog_view(id) for any dataset.",
    "# SoQL supported: where, select, group, order, q (full-text)"
  )

  if (is.null(src)) {
    out <- paste(c(header, "# Source not available."), collapse = "\n")
    cat(out, "\n")
    return(invisible(out))
  }

  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", src)
  blocks <- list()
  n <- length(src)
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", src[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", src[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) src[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- src[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) {
      k <- k + 1; sig <- paste(sig, trimws(src[k]))
    }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }

  out <- paste(c(header, "#", "# == Functions ==", "#", unlist(blocks)),
               collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
