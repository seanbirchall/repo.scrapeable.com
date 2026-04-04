# providenceri.gov.R
# Self-contained City of Providence Open Data (Socrata SODA) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# API: Socrata SODA at data.providenceri.gov

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.pvd_base <- "https://data.providenceri.gov"

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
  url <- sprintf("%s/resource/%s.json?%s", .pvd_base, view_id, query_str)
  url <- utils::URLencode(url, reserved = FALSE)

  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Providence SODA query error: ", e$message)
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

#' List Providence Open Data datasets
#'
#' Returns catalog metadata from the data.providenceri.gov Socrata portal.
#'
#' @param limit Maximum datasets to return (default 100)
#' @return tibble: id, name, category, description, updated_at
pvd_list <- function(limit = 100) {
  url <- sprintf("%s/api/views?limit=%d", .pvd_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Providence catalog error: ", e$message)
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

#' Search Providence Open Data datasets
#'
#' Searches catalog by keyword. Filters the full catalog list.
#'
#' @param query Search term (case-insensitive, matches name and description)
#' @param limit Max datasets to scan (default 300)
#' @return tibble: id, name, category, description, updated_at
pvd_search <- function(query, limit = 300) {
  all_ds <- pvd_list(limit = limit)
  if (nrow(all_ds) == 0) return(.schema_datasets)
  all_ds |> filter(
    grepl(query, name, ignore.case = TRUE) |
    grepl(query, description, ignore.case = TRUE)
  )
}

# == Generic SODA query ========================================================

#' Query any Providence dataset via SODA
#'
#' Generic SODA query interface. Pass a Socrata view ID and optional
#' SoQL clauses.
#'
#' @param view_id Socrata 4x4 view identifier (e.g. "vank-fyx9")
#' @param where SoQL WHERE clause
#' @param select SoQL SELECT clause
#' @param group SoQL GROUP BY clause
#' @param order SoQL ORDER BY clause
#' @param q Full-text search query
#' @param limit Max rows (default 1000)
#' @param offset Pagination offset (default 0)
#' @return tibble of query results
pvd_view <- function(view_id, where = NULL, select = NULL,
                     group = NULL, order = NULL, q = NULL,
                     limit = 1000, offset = 0) {
  ._soda_query(view_id, where = where, select = select, group = group,
               order = order, q = q, limit = limit, offset = offset)
}

# == Police & Public Safety ====================================================

#' Providence police arrests and citations (past 180 days)
#'
#' Adults arrested or issued citations by the Providence Police Department.
#'
#' @param statute_type Filter by statute type (e.g. "RI Statute Violation")
#' @param gender Filter by gender
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param limit Max rows (default 1000)
#' @return tibble of arrest/citation records
pvd_arrests <- function(statute_type = NULL, gender = NULL,
                        start_date = NULL, end_date = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(statute_type))
    clauses <- c(clauses, sprintf("statute_type = '%s'", statute_type))
  if (!is.null(gender))
    clauses <- c(clauses, sprintf("gender = '%s'", gender))
  if (!is.null(start_date))
    clauses <- c(clauses, sprintf("arrest_date >= '%s'", start_date))
  if (!is.null(end_date))
    clauses <- c(clauses, sprintf("arrest_date <= '%s'", end_date))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("vank-fyx9", where = where,
                      order = "arrest_date DESC", limit = limit)
  if (nrow(df) > 0) {
    if ("arrest_date" %in% names(df))
      df$arrest_date <- as.POSIXct(df$arrest_date, format = "%Y-%m-%dT%H:%M:%S")
    if ("counts" %in% names(df))
      df$counts <- as.integer(df$counts)
    if ("age" %in% names(df))
      df$age <- as.integer(df$age)
  }
  df
}

#' Providence police case log (past 180 days)
#'
#' Recorded state and municipal offenses from AEGIS records management.
#'
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @param limit Max rows (default 1000)
#' @return tibble of case log records
pvd_case_log <- function(start_date = NULL, end_date = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(start_date))
    clauses <- c(clauses, sprintf("reported_date >= '%s'", start_date))
  if (!is.null(end_date))
    clauses <- c(clauses, sprintf("reported_date <= '%s'", end_date))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  ._soda_query("rz3y-pz8v", where = where,
               order = "reported_date DESC", limit = limit)
}

# == Finance ===================================================================

#' City of Providence expenditures
#'
#' Vendor payments made since FY2017.
#'
#' @param fiscal_year Filter by fiscal year
#' @param vendor_name Filter by vendor name (partial, case-insensitive)
#' @param department Filter by department (partial, case-insensitive)
#' @param limit Max rows (default 1000)
#' @return tibble of expenditure records
pvd_expenditures <- function(fiscal_year = NULL, vendor_name = NULL,
                             department = NULL, limit = 1000) {
  clauses <- character()
  if (!is.null(fiscal_year))
    clauses <- c(clauses, sprintf("fiscal_year = '%s'", fiscal_year))
  if (!is.null(vendor_name))
    clauses <- c(clauses, sprintf("upper(vendor_name) LIKE '%%%s%%'",
                                  toupper(vendor_name)))
  if (!is.null(department))
    clauses <- c(clauses, sprintf("upper(department) LIKE '%%%s%%'",
                                  toupper(department)))
  where <- if (length(clauses)) paste(clauses, collapse = " AND ") else NULL
  df <- ._soda_query("u4ks-kiwa", where = where, limit = limit)
  if (nrow(df) > 0 && "amount" %in% names(df))
    df$amount <- as.numeric(df$amount)
  df
}

#' City of Providence purchase orders
#'
#' City and School Department Purchase Orders from FY2020 onwards.
#'
#' @param fiscal_year Filter by fiscal year
#' @param limit Max rows (default 1000)
#' @return tibble of purchase order records
pvd_purchase_orders <- function(fiscal_year = NULL, limit = 1000) {
  where <- if (!is.null(fiscal_year))
    sprintf("fiscal_year = '%s'", fiscal_year) else NULL
  ._soda_query("425y-pm5m", where = where, limit = limit)
}

#' City of Providence revenue budget
#'
#' Annual revenue budgets per department from FY2012 onward.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of revenue budget records
pvd_revenue_budget <- function(limit = 1000) {
  ._soda_query("9jte-7uk8", limit = limit)
}

#' City of Providence pension payroll
#'
#' Monthly pension payroll grouped by benefit type.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of pension payroll records
pvd_pension_payroll <- function(limit = 1000) {
  ._soda_query("rehs-z98t", limit = limit)
}

#' Unclaimed property (uncashed city checks)
#'
#' Checks issued by Providence not cashed after one year.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of unclaimed property records
pvd_unclaimed_property <- function(limit = 1000) {
  ._soda_query("4hhd-fzq6", limit = limit)
}

# == Property Tax ==============================================================

#' Providence property tax roll
#'
#' Property tax information for a given calendar year. Includes residential,
#' commercial, and publicly held parcels.
#'
#' @param year Calendar year for tax data (default 2025). Available: 2002-2025.
#' @param limit Max rows (default 1000)
#' @return tibble of property tax records
pvd_property_tax <- function(year = 2025, limit = 1000) {
  year_map <- c(
    "2025" = "6ub4-iebe", "2024" = "xvti-7dtw", "2023" = "fd8d-n74v",
    "2022" = "c3q4-f95q", "2021" = "fawc-z8zq", "2020" = "y9h5-fefu",
    "2019" = "twey-459r", "2018" = "yghh-hsch", "2017" = "ku9m-5rhr",
    "2016" = "czje-unnn", "2014" = "t2vi-f8qs", "2013" = "a6uy-vymr",
    "2012" = "fgwg-7viq", "2010" = "p6bs-8exs", "2009" = "9p3h-3q5i",
    "2008" = "swgn-agtw", "2007" = "iafn-rvmp", "2006" = "aymi-rpqq",
    "2005" = "yqdi-fbw5", "2004" = "fvsz-rky3", "2003" = "fek9-hu38",
    "2002" = "e3qk-fibu"
  )
  yr <- as.character(year)
  if (!(yr %in% names(year_map))) {
    warning("Year ", yr, " not available. Available: ",
            paste(sort(names(year_map)), collapse = ", "))
    return(tibble())
  }
  df <- ._soda_query(year_map[[yr]], limit = limit)
  if (nrow(df) > 0) {
    for (col in c("total_assmt", "total_exempt", "total_taxes")) {
      if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
    }
  }
  df
}

#' Providence motor vehicle tax roll (2017)
#'
#' @param limit Max rows (default 1000)
#' @return tibble of motor vehicle tax records
pvd_motor_vehicle_tax <- function(limit = 1000) {
  ._soda_query("npyn-gmfa", limit = limit)
}

# == Budgets ===================================================================

#' Providence proposed expense budget
#'
#' City proposed expense budgets by fiscal year.
#'
#' @param fiscal_year One of "2023", "2022", "2021", "2020", "2019", "2018",
#'   "2017", "2015", "2014", "2013" (default "2023")
#' @param limit Max rows (default 1000)
#' @return tibble of budget line items
pvd_expense_budget <- function(fiscal_year = "2023", limit = 1000) {
  fy_map <- c(
    "2023" = "ka8x-eb9x", "2022" = "ejse-evtt", "2021" = "khch-nbyg",
    "2020" = "xxiz-eb4k", "2019" = "hfdy-4mye", "2018" = "jcrz-ukry",
    "2017" = "63nj-zuyh", "2015" = "xkd5-s9ff", "2014" = "sa6n-hp9d",
    "2013" = "7wa4-fft7"
  )
  fy <- as.character(fiscal_year)
  if (!(fy %in% names(fy_map))) {
    warning("FY ", fy, " not available. Available: ",
            paste(sort(names(fy_map)), collapse = ", "))
    return(tibble())
  }
  ._soda_query(fy_map[[fy]], limit = limit)
}

# == Neighborhoods & Infrastructure ============================================

#' Providence building permits (2009-2018)
#'
#' Building, Electrical, Mechanical, and Plumbing permits from the
#' Department of Inspections and Standards.
#'
#' @param permit_type Filter by type (e.g. "Building", "Electrical")
#' @param limit Max rows (default 1000)
#' @return tibble of permit records
pvd_permits <- function(permit_type = NULL, limit = 1000) {
  where <- if (!is.null(permit_type))
    sprintf("upper(permit_type) LIKE '%%%s%%'", toupper(permit_type)) else NULL
  ._soda_query("ufmm-rbej", where = where, limit = limit)
}

#' Providence fire hydrant locations
#'
#' 2013 survey of fire hydrants across Providence.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of hydrant location records
pvd_fire_hydrants <- function(limit = 1000) {
  ._soda_query("iuy2-4fux", limit = limit)
}

#' Providence tree inventory
#'
#' Complete inventory of street trees from 2006 census (updated 2016).
#'
#' @param limit Max rows (default 1000)
#' @return tibble of tree inventory records
pvd_trees <- function(limit = 1000) {
  ._soda_query("uv9w-h8i4", limit = limit)
}

#' Community gardens in Providence
#'
#' Public and privately owned community gardens (2014 data).
#'
#' @param limit Max rows (default 1000)
#' @return tibble of community garden records
pvd_community_gardens <- function(limit = 1000) {
  ._soda_query("uj2v-k4s7", limit = limit)
}

#' Goodwill bin locations in Providence
#'
#' @param limit Max rows (default 1000)
#' @return tibble of Goodwill collection bin locations
pvd_goodwill_bins <- function(limit = 1000) {
  ._soda_query("2azu-r5bu", limit = limit)
}

#' Providence trash pickup schedule
#'
#' @param limit Max rows (default 1000)
#' @return tibble of trash pickup schedule areas
pvd_trash_schedule <- function(limit = 1000) {
  ._soda_query("7es9-emf3", limit = limit)
}

# == Licensing =================================================================

#' Active business licenses in Providence
#'
#' NOTE: This dataset is no longer being updated.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of business license records
pvd_business_licenses <- function(limit = 1000) {
  ._soda_query("ui7z-kv69", limit = limit)
}

#' Mobile food establishment licenses
#'
#' Active food truck/mobile food licenses.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of mobile food establishment records
pvd_food_trucks <- function(limit = 1000) {
  ._soda_query("u7ik-g787", limit = limit)
}

#' Monthly entertainment licenses
#'
#' @param limit Max rows (default 1000)
#' @return tibble of entertainment license application records
pvd_entertainment_licenses <- function(limit = 1000) {
  ._soda_query("2f79-9nkc", limit = limit)
}

# == Schools & Energy ==========================================================

#' Providence public schools
#'
#' Location and contact information for public schools.
#'
#' @param limit Max rows (default 200)
#' @return tibble of public school records
pvd_schools <- function(limit = 200) {
  ._soda_query("pu8z-v46s", limit = limit)
}

#' Municipal building energy use
#'
#' Energy data on Providence municipal buildings by year.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of energy use records
pvd_building_energy <- function(limit = 1000) {
  ._soda_query("dmye-wwhm", limit = limit)
}

#' Total municipal electricity use (FY2010-FY2016)
#'
#' @param limit Max rows (default 1000)
#' @return tibble of electricity use records
pvd_electricity_use <- function(limit = 1000) {
  ._soda_query("5726-aqx9", limit = limit)
}

# == GIS / Zoning ==============================================================

#' Providence property ID catalog
#'
#' Property IDs with address and Plat/Lot/Unit data for homestead lookup.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of property ID records
pvd_property_ids <- function(limit = 1000) {
  ._soda_query("k6gu-363f", limit = limit)
}

#' Providence zip code boundaries
#'
#' @param limit Max rows (default 100)
#' @return tibble of zip code boundary records
pvd_zip_codes <- function(limit = 100) {
  ._soda_query("sm57-du4m", limit = limit)
}

#' Providence enterprise zones
#'
#' Areas designated for business tax incentives.
#'
#' @param limit Max rows (default 100)
#' @return tibble of enterprise zone records
pvd_enterprise_zones <- function(limit = 100) {
  ._soda_query("vynu-sgur", limit = limit)
}

# == Holidays ==================================================================

#' Providence city holiday schedule
#'
#' @param year Calendar year (default 2025). Available: 2013, 2014, 2025.
#' @param limit Max rows (default 50)
#' @return tibble of city holiday dates
pvd_holidays <- function(year = 2025, limit = 50) {
  year_map <- c(
    "2025" = "9zbu-vjd2",
    "2014" = "c6wc-vavf",
    "2013" = "ubra-37q7"
  )
  yr <- as.character(year)
  if (!(yr %in% names(year_map))) {
    warning("Year ", yr, " not available. Available: ",
            paste(names(year_map), collapse = ", "))
    return(tibble())
  }
  ._soda_query(year_map[[yr]], limit = limit)
}

# == 1940 Assessment Records ===================================================

#' 1940 City field assessment records
#'
#' Digitized residential assessment records (field cards) from 1940.
#'
#' @param limit Max rows (default 1000)
#' @return tibble of assessment record metadata
pvd_1940_assessments <- function(limit = 1000) {
  ._soda_query("2pca-62ru", limit = limit)
}

# == Context ===================================================================

#' Generate LLM-friendly context for the Providence Open Data client
#'
#' Reads own source and prints a summary of all public functions.
#'
#' @return Character string (invisibly), also printed
pvd_context <- function() {
  src <- tryCatch(
    {
      fn_env <- sys.frame(0)
      src_file <- getSrcFilename(fn_env$.self %||% pvd_context, full.names = TRUE)
      if (is.null(src_file) || src_file == "") stop("no source")
      readLines(src_file, warn = FALSE)
    },
    error = function(e) {
      sf <- system.file("source", "providenceri.R", package = "providenceri.gov")
      if (sf != "") return(readLines(sf, warn = FALSE))
      NULL
    }
  )

  header <- c(
    "# providenceri.gov - Providence RI Open Data (Socrata SODA) Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Portal: https://data.providenceri.gov (207 datasets, 99 Socrata views)",
    "#",
    "# All functions return tibbles. Use pvd_view(id) for any dataset.",
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
