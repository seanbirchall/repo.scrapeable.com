# americorps.gov.R
# Self-contained AmeriCorps Open Data (Socrata SODA) client.
# All public functions return tibbles.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public Socrata portal)
# Rate limits: standard Socrata throttling; pass app_token to increase
# Docs: https://dev.socrata.com/docs/queries/


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.acorps_base <- "https://data.americorps.gov"

# -- SODA query engine ---------------------------------------------------------

._soda_query <- function(view_id, where = NULL, select = NULL, group = NULL,
                         order = NULL, limit = 1000, offset = 0,
                         token = NULL, max_results = NULL) {
  all_data <- list()
  current_offset <- offset
  page_size <- min(limit, 50000)

  repeat {
    params <- list(`$limit` = page_size, `$offset` = current_offset)
    if (!is.null(where))  params[["$where"]]  <- where
    if (!is.null(select)) params[["$select"]] <- select
    if (!is.null(group))  params[["$group"]]  <- group
    if (!is.null(order))  params[["$order"]]  <- order

    query <- paste(
      names(params),
      vapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE),
             character(1)),
      sep = "=", collapse = "&"
    )
    url <- paste0(.acorps_base, "/resource/", view_id, ".json?", query)
    if (!is.null(token)) url <- paste0(url, "&$$app_token=", token)

    tmp <- tempfile(fileext = ".json")
    tryCatch({
      httr2::request(url) |>
        httr2::req_headers(`User-Agent` = .ua) |>
        httr2::req_perform(path = tmp)
    }, error = function(e) {
      warning("SODA request failed for ", view_id, ": ", conditionMessage(e))
      return(NULL)
    })

    if (!file.exists(tmp)) break
    raw <- tryCatch(jsonlite::fromJSON(tmp), error = function(e) NULL)
    if (is.null(raw) || length(raw) == 0 || (is.data.frame(raw) && nrow(raw) == 0)) break
    all_data[[length(all_data) + 1]] <- as_tibble(raw)

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (!is.null(max_results) && n_so_far >= max_results) break
    if (is.data.frame(raw) && nrow(raw) < page_size) break
    current_offset <- current_offset + page_size
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (!is.null(max_results)) result <- head(result, max_results)
  result
}

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# == Dataset catalog ===========================================================

.acorps_catalog <- tibble::tribble(
  ~view_id,     ~name,                                                              ~category,
  "yie5-ur4v",  "AmeriCorps NCCC Deployments (legacy)",                             "deployments",
  "45t9-x5kj",  "AmeriCorps NCCC Traditional Deployments",                          "deployments",
  "5d3h-y6uy",  "AmeriCorps NCCC Forest Deployments",                               "deployments",
  "c8jn-su9n",  "AmeriCorps NCCC FEMA Deployments",                                 "deployments",
  "kusw-dg5a",  "AmeriCorps NCCC Southern Region Traditional Deployments",           "deployments",
  "eqsg-p2xa",  "AmeriCorps NCCC Pacific Region Traditional Deployments",            "deployments",
  "m4cu-73zi",  "AmeriCorps NCCC Southwest Region Traditional Deployments",          "deployments",
  "xuki-piwn",  "Segal Education Award: Detailed Payments by Institution 1994-2023", "education_award",
  "dz6i-y5ak",  "Segal Education Award: Payments by State",                          "education_award",
  "26pv-trba",  "Segal Education Award: Payments and Alumni by Institution",          "education_award",
  "thgj-sd6i",  "Segal Education Award: Detailed Payments by Institution 2019",       "education_award",
  "dxxz-mepu",  "Segal Education Award: Detailed Payments by Institution 2020",       "education_award",
  "a97a-g8k4",  "Segal Education Award: Detailed Payments by Institution (legacy)",   "education_award",
  "8t8t-a6ry",  "Segal Education Award: Payments and Alumni by Institution 2020",     "education_award",
  "7kf8-4txu",  "Segal Education Award: Payments and Alumni by Institution 2019",     "education_award",
  "gujh-zj6b",  "2024 AmeriCorps Member Exit Survey",                                "member_survey",
  "frpm-n3u5",  "2023 AmeriCorps Member Exit Survey",                                "member_survey",
  "59ia-vsnv",  "2022 AmeriCorps Member Exit Survey",                                "member_survey",
  "58uq-h8je",  "2021 AmeriCorps Member Exit Survey",                                "member_survey",
  "tnxs-meph",  "2020 AmeriCorps Member Exit Survey",                                "member_survey",
  "3s9n-2btu",  "2019 AmeriCorps Member Exit Survey",                                "member_survey",
  "sypt-sh4u",  "2018 AmeriCorps Member Exit Survey",                                "member_survey",
  "tpbh-nswq",  "2017 AmeriCorps Member Exit Survey",                                "member_survey",
  "wqhv-fm5d",  "2016 AmeriCorps Member Exit Survey",                                "member_survey",
  "a63n-jsfz",  "AmeriCorps Member Exit Survey 01/01/2018-12/31/2018",               "member_survey",
  "rhng-qtzw",  "CEV Findings: National Rates of All Measures (2017-2023)",           "civic_engagement",
  "bhmf-84dy",  "CEV Findings: National Rates by Demographics (2017-2023)",           "civic_engagement",
  "4r6x-re58",  "CEV Findings: State-Level Rates (2017-2023)",                        "civic_engagement",
  "9zzx-8cwf",  "2023 CEV Findings: Volunteering Toplines",                           "civic_engagement",
  "twy4-5nxu",  "2017 CEV Data",                                                      "civic_engagement",
  "a6ak-yd7k",  "2019 CEV Data",                                                      "civic_engagement",
  "n3gv-uwex",  "2021 CEV Data",                                                      "civic_engagement",
  "be5g-4c5r",  "2023 CEV Data",                                                      "civic_engagement",
  "i9xs-fvag",  "AmeriCorps Participant Demographics Data",                            "demographics",
  "as6e-6nns",  "AmeriCorps Member Race and Ethnicity National Figures",               "demographics",
  "hznm-uizi",  "AmeriCorps Research Grantee Dataset",                                 "research",
  "c88b-pc22",  "Employers of National Service",                                       "employers",
  "f3y6-ew2i",  "Open Data Platform Asset Views and Downloads",                        "platform",
  "izv6-as2d",  "NSCHC Rule Change Data",                                              "compliance",
  "et85-j49w",  "Criminal History Check - Recurring Access",                           "compliance",
  "yhps-cx97",  "State Profile Geospatial Data",                                       "geospatial"
)


# == Discovery =================================================================

#' @title List all AmeriCorps open datasets
#'
#' Returns the full catalog of Socrata datasets available on
#' data.americorps.gov. Each row contains a view ID that can be passed to
#' \code{acorps_view()} or \code{acorps_columns()} for further exploration.
#' Use the \code{category} filter to narrow by topic area.
#'
#' @param category Optional category filter. Valid values: \code{"deployments"},
#'   \code{"education_award"}, \code{"member_survey"}, \code{"civic_engagement"},
#'   \code{"demographics"}, \code{"research"}, \code{"employers"},
#'   \code{"platform"}, \code{"compliance"}, \code{"geospatial"}.
#'   \code{NULL} returns all datasets. Default \code{NULL}.
#' @return A tibble with 41 rows (unfiltered) and columns:
#'   \itemize{
#'     \item \code{view_id} (character): Socrata 4x4 dataset identifier, e.g. \code{"45t9-x5kj"}
#'     \item \code{name} (character): Human-readable dataset title
#'     \item \code{category} (character): Topic grouping
#'   }
#' @examples
#' \dontrun{
#' # Browse all datasets
#' acorps_list()
#'
#' # Show only deployment datasets
#' acorps_list(category = "deployments")
#'
#' # Find education award datasets
#' acorps_list(category = "education_award")
#' }
acorps_list <- function(category = NULL) {
  out <- .acorps_catalog
  if (!is.null(category)) {
    out <- out |> filter(.data$category == .env$category)
  }
  out
}

#' @title Search AmeriCorps datasets by keyword
#'
#' Searches dataset names in the local catalog using case-insensitive
#' substring matching. Use this to find datasets when you know a topic
#' but not the exact view ID. For column-level metadata on a matched
#' dataset, pass its \code{view_id} to \code{acorps_columns()}.
#'
#' @param query Character string to match against dataset names.
#'   Case-insensitive. Examples: \code{"FEMA"}, \code{"survey"}, \code{"CEV"}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{view_id} (character): Socrata 4x4 dataset identifier
#'     \item \code{name} (character): Dataset title (matched against \code{query})
#'     \item \code{category} (character): Topic grouping
#'   }
#' @examples
#' \dontrun{
#' # Find deployment-related datasets
#' acorps_search("deployment")
#'
#' # Find education award datasets
#' acorps_search("Segal")
#'
#' # Find civic engagement datasets
#' acorps_search("CEV")
#' }
acorps_search <- function(query) {
  .acorps_catalog |>
    filter(grepl(query, name, ignore.case = TRUE))
}

#' @title Get column metadata for an AmeriCorps dataset
#'
#' Fetches the Socrata view metadata for a dataset and returns a tibble
#' describing each column's API field name, display name, data type, and
#' description. Use this to understand what fields are available before
#' querying with \code{acorps_view()}.
#'
#' @param view_id Socrata 4x4 view identifier. Find valid IDs using
#'   \code{acorps_list()} or \code{acorps_search()}. Example: \code{"45t9-x5kj"}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{field_name} (character): API field name used in SoQL queries
#'     \item \code{name} (character): Human-readable column label
#'     \item \code{datatype} (character): Socrata data type (e.g. \code{"text"}, \code{"number"}, \code{"calendar_date"})
#'     \item \code{description} (character): Column description (may be \code{NA})
#'   }
#' @examples
#' \dontrun{
#' # Inspect columns of NCCC Traditional Deployments
#' acorps_columns("45t9-x5kj")
#'
#' # Inspect columns of education award payments
#' acorps_columns("xuki-piwn")
#' }
acorps_columns <- function(view_id) {
  url <- sprintf("%s/api/views/%s.json", .acorps_base, view_id)
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    warning("Could not fetch metadata for ", view_id, ": ", conditionMessage(e))
    return(list())
  })
  cols <- raw$columns
  if (is.null(cols) || length(cols) == 0)
    return(tibble(field_name = character(), name = character(),
                  datatype = character(), description = character()))
  bind_rows(lapply(cols, function(c) {
    tibble(
      field_name  = c$fieldName %||% NA_character_,
      name        = c$name %||% NA_character_,
      datatype    = c$dataTypeName %||% NA_character_,
      description = c$description %||% NA_character_
    )
  }))
}


# == Generic SODA access =======================================================

#' @title Query any AmeriCorps Socrata dataset
#'
#' Generic SODA query interface for data.americorps.gov. Use this when the
#' convenience wrappers (e.g. \code{acorps_deployments()}) do not cover your
#' dataset, or when you need advanced SoQL features like aggregation. Supports
#' automatic pagination for large result sets.
#'
#' @param view_id Socrata 4x4 view identifier. Find valid IDs with
#'   \code{acorps_list()}. Example: \code{"45t9-x5kj"}.
#' @param limit Maximum rows to return. Default \code{1000}. Set higher
#'   for full dataset downloads (Socrata pages at 50,000 rows internally).
#' @param where SoQL WHERE clause for filtering. Examples:
#'   \code{"year='2024'"}, \code{"state='CA' AND year='2023'"}.
#'   \code{NULL} for no filter. Default \code{NULL}.
#' @param select SoQL SELECT clause for choosing/computing columns. Example:
#'   \code{"year, count(*) AS n"}. \code{NULL} returns all columns. Default \code{NULL}.
#' @param group SoQL GROUP BY clause for aggregation. Example: \code{"year"}.
#'   \code{NULL} for no grouping. Default \code{NULL}.
#' @param order SoQL ORDER BY clause. Example: \code{"year DESC"}.
#'   \code{NULL} for default order. Default \code{NULL}.
#' @param offset Row offset for manual pagination. Default \code{0}.
#' @param token Optional Socrata app token to increase rate limits.
#'   \code{NULL} uses anonymous access. Default \code{NULL}.
#' @return A tibble whose columns depend on the dataset queried. Use
#'   \code{acorps_columns(view_id)} to inspect available fields.
#' @examples
#' \dontrun{
#' # Fetch 50 rows from NCCC Traditional Deployments
#' acorps_view("45t9-x5kj", limit = 50)
#'
#' # Count deployments by year
#' acorps_view("45t9-x5kj", select = "year, count(*) AS n",
#'             group = "year", order = "year DESC")
#'
#' # Filter education awards to California
#' acorps_view("xuki-piwn", where = "state='CA'", limit = 500)
#' }
acorps_view <- function(view_id, limit = 1000, where = NULL, select = NULL,
                        group = NULL, order = NULL, offset = 0, token = NULL) {
  ._soda_query(view_id, where = where, select = select, group = group,
               order = order, limit = limit, offset = offset,
               token = token, max_results = limit)
}


# == NCCC Deployments ==========================================================

#' @title Fetch AmeriCorps NCCC traditional deployments
#'
#' Returns a tibble of AmeriCorps NCCC Traditional team deployments including
#' project descriptions, sponsors, locations, and service dates. Use this to
#' find where NCCC teams are serving. Filter by state, campus, or program year.
#'
#' @param year Program year filter (character or numeric). Example: \code{"2024"}.
#'   \code{NULL} for all years. Default \code{NULL}.
#' @param state Two-letter state abbreviation filter. Examples: \code{"CA"},
#'   \code{"TX"}, \code{"NY"}. \code{NULL} for all states. Default \code{NULL}.
#' @param campus Campus name filter (partial match via SoQL LIKE). Examples:
#'   \code{"Pacific"}, \code{"Southern"}, \code{"Atlantic"}, \code{"Southwest"}.
#'   \code{NULL} for all campuses. Default \code{NULL}.
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{year} (character): Program year
#'     \item \code{campus} (character): NCCC regional campus name
#'     \item \code{corps} (character): Corps designation
#'     \item \code{team} (character): Team identifier
#'     \item \code{nbr_of_corps_members} (character): Number of corps members on team
#'     \item \code{start_date} (character): Deployment start date (ISO format)
#'     \item \code{end_date} (character): Deployment end date (ISO format)
#'     \item \code{report_date} (character): Date record was reported
#'     \item \code{sponsor} (character): Organization sponsoring the project
#'     \item \code{project_description} (character): Description of the service project
#'     \item \code{zip} (character): ZIP code of deployment location
#'     \item \code{geocoded_column} (data.frame): Nested lat/lon coordinates
#'   }
#' @examples
#' \dontrun{
#' # Get all current deployments
#' acorps_deployments()
#'
#' # Find deployments in California
#' acorps_deployments(state = "CA")
#'
#' # Get 2024 Pacific Region deployments
#' acorps_deployments(year = "2024", campus = "Pacific")
#' }
acorps_deployments <- function(year = NULL, state = NULL, campus = NULL,
                               limit = 1000, token = NULL) {
  parts <- character()
  if (!is.null(year))   parts <- c(parts, sprintf("year='%s'", year))
  if (!is.null(state))  parts <- c(parts, sprintf("stabbr='%s'", state))
  if (!is.null(campus)) parts <- c(parts, sprintf("campus LIKE '%%%s%%'", campus))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL
  ._soda_query("45t9-x5kj", where = where, limit = limit,
               token = token, max_results = limit)
}

#' @title Fetch AmeriCorps NCCC forest deployments
#'
#' Returns NCCC Forest Corps deployment records. These are specialized
#' deployments focused on wildfire response, forest restoration, and
#' related conservation projects. Same column structure as
#' \code{acorps_deployments()} but sourced from view \code{"5d3h-y6uy"}.
#'
#' @param year Program year filter (character or numeric). Example: \code{"2024"}.
#'   \code{NULL} for all years. Default \code{NULL}.
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{year} (character): Program year
#'     \item \code{campus} (character): NCCC regional campus name
#'     \item \code{corps} (character): Corps designation
#'     \item \code{team} (character): Team identifier
#'     \item \code{nbr_of_corps_members} (character): Number of corps members
#'     \item \code{start_date} (character): Deployment start date
#'     \item \code{end_date} (character): Deployment end date
#'     \item \code{report_date} (character): Date record was reported
#'     \item \code{sponsor} (character): Sponsoring organization
#'     \item \code{project_description} (character): Description of the project
#'     \item \code{zip} (character): ZIP code
#'     \item \code{geocoded_column} (data.frame): Nested lat/lon coordinates
#'   }
#' @examples
#' \dontrun{
#' # All forest deployments
#' acorps_forest_deployments()
#'
#' # Forest deployments in 2023
#' acorps_forest_deployments(year = "2023")
#' }
acorps_forest_deployments <- function(year = NULL, limit = 1000, token = NULL) {
  where <- if (!is.null(year)) sprintf("year='%s'", year) else NULL
  ._soda_query("5d3h-y6uy", where = where, limit = limit,
               token = token, max_results = limit)
}

#' @title Fetch AmeriCorps NCCC FEMA deployments
#'
#' Returns NCCC FEMA Corps deployment records. These are disaster response
#' deployments where NCCC teams support FEMA operations. May return an empty
#' tibble if no FEMA deployments are currently published (view \code{"c8jn-su9n"}).
#'
#' @param year Program year filter (character or numeric). Example: \code{"2024"}.
#'   \code{NULL} for all years. Default \code{NULL}.
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble. When data is available, columns match the deployment
#'   schema (year, campus, corps, team, etc.). Returns an empty tibble
#'   if no records are found.
#' @examples
#' \dontrun{
#' # All FEMA deployments
#' acorps_fema_deployments()
#'
#' # FEMA deployments in 2022
#' acorps_fema_deployments(year = "2022")
#' }
acorps_fema_deployments <- function(year = NULL, limit = 1000, token = NULL) {
  where <- if (!is.null(year)) sprintf("year='%s'", year) else NULL
  ._soda_query("c8jn-su9n", where = where, limit = limit,
               token = token, max_results = limit)
}


# == Education Awards ==========================================================

#' @title Fetch Segal AmeriCorps Education Award payments by institution
#'
#' Returns detailed Segal AmeriCorps Education Award payment records by
#' institution from 1994 to 2023. Each row represents a specific payment
#' type and award type combination at an institution in a given year. Use
#' \code{acorps_education_awards_by_state()} for aggregate state-level totals.
#'
#' @param state Two-letter state abbreviation filter. Examples: \code{"CA"},
#'   \code{"NY"}. \code{NULL} for all states. Default \code{NULL}.
#' @param year Fiscal year filter (character or numeric). Examples: \code{"2020"},
#'   \code{2023}. \code{NULL} for all years. Default \code{NULL}.
#' @param institution Institution name filter (case-insensitive partial match).
#'   Example: \code{"HARVARD"}, \code{"community college"}. \code{NULL} for all.
#'   Default \code{NULL}.
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{institution} (character): Name of the educational institution
#'     \item \code{system} (character): University system (if applicable)
#'     \item \code{campus} (character): Campus name
#'     \item \code{state} (character): Two-letter state abbreviation
#'     \item \code{country_category} (character): Country classification
#'     \item \code{year} (character): Fiscal year
#'     \item \code{earned_award_type} (character): Type of award earned
#'     \item \code{payment_type} (character): Type of payment (education, loan, etc.)
#'     \item \code{total_payments} (character): Total payment amount
#'   }
#' @examples
#' \dontrun{
#' # Recent education award payments
#' acorps_education_awards(year = "2023", limit = 100)
#'
#' # Awards to California institutions
#' acorps_education_awards(state = "CA")
#'
#' # Search for a specific university
#' acorps_education_awards(institution = "HARVARD")
#' }
acorps_education_awards <- function(state = NULL, year = NULL,
                                    institution = NULL, limit = 1000,
                                    token = NULL) {
  parts <- character()
  if (!is.null(state)) parts <- c(parts, sprintf("state='%s'", state))
  if (!is.null(year))  parts <- c(parts, sprintf("year='%s'", year))
  if (!is.null(institution))
    parts <- c(parts, sprintf("institution LIKE '%%%s%%'", toupper(institution)))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL
  ._soda_query("xuki-piwn", where = where, limit = limit,
               token = token, max_results = limit)
}

#' @title Fetch Segal Education Award payments aggregated by state
#'
#' Returns aggregate Segal AmeriCorps Education Award payment totals and
#' alumni counts by state or territory. For institution-level detail, use
#' \code{acorps_education_awards()} instead.
#'
#' @param state Full state name filter (e.g. \code{"California"}, not \code{"CA"}).
#'   \code{NULL} for all states and territories. Default \code{NULL}.
#' @param limit Maximum rows to return. Default \code{100}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{location} (character): State or territory name
#'     \item \code{location_type} (character): Location classification
#'     \item \code{all_payments} (character): Total payments across all types
#'     \item \code{education_payments} (character): Total education payments
#'     \item \code{loan_payments} (character): Total loan payments
#'     \item \code{all_individuals} (character): Total number of individuals
#'     \item \code{individuals_education} (character): Individuals using education payments
#'     \item \code{individuals_loan} (character): Individuals using loan payments
#'     \item \code{lat_long} (data.frame): Nested latitude/longitude coordinates
#'   }
#' @examples
#' \dontrun{
#' # All states
#' acorps_education_awards_by_state()
#'
#' # Single state
#' acorps_education_awards_by_state(state = "California")
#' }
acorps_education_awards_by_state <- function(state = NULL, limit = 100,
                                             token = NULL) {
  where <- if (!is.null(state)) sprintf("location='%s'", state) else NULL
  ._soda_query("dz6i-y5ak", where = where, limit = limit,
               token = token, max_results = limit)
}


# == Member Exit Surveys =======================================================

#' @title Fetch AmeriCorps Member Exit Survey data
#'
#' Returns anonymized member exit survey responses for a given year. Surveys
#' capture members' experiences, satisfaction, and outcomes at the end of
#' their AmeriCorps service. Available years: 2016 through 2024. Column
#' names vary by year as survey questions change.
#'
#' @param year Survey year (character or numeric). Valid values: 2016, 2017,
#'   2018, 2019, 2020, 2021, 2022, 2023, 2024. Default \code{2024}.
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param where Optional SoQL WHERE clause for additional filtering.
#'   Example: \code{"program_type='State/National'"}. Default \code{NULL}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble of survey responses. Columns vary by survey year but
#'   typically include program type, service duration, satisfaction measures,
#'   and demographic indicators.
#' @examples
#' \dontrun{
#' # Latest survey data
#' acorps_member_survey(year = 2024, limit = 100)
#'
#' # Compare 2020 and 2023 surveys
#' s2020 <- acorps_member_survey(year = 2020)
#' s2023 <- acorps_member_survey(year = 2023)
#' }
acorps_member_survey <- function(year = 2024, limit = 1000, where = NULL,
                                 token = NULL) {
  ids <- c(
    "2024" = "gujh-zj6b", "2023" = "frpm-n3u5", "2022" = "59ia-vsnv",
    "2021" = "58uq-h8je", "2020" = "tnxs-meph", "2019" = "3s9n-2btu",
    "2018" = "sypt-sh4u", "2017" = "tpbh-nswq", "2016" = "wqhv-fm5d"
  )
  yr <- as.character(year)
  if (!yr %in% names(ids)) {
    stop("No member exit survey for year ", yr,
         ". Available: ", paste(names(ids), collapse = ", "))
  }
  ._soda_query(ids[[yr]], where = where, limit = limit,
               token = token, max_results = limit)
}


# == Civic Engagement & Volunteering ===========================================

#' @title Fetch CEV national rates of civic engagement measures (2017-2023)
#'
#' Returns national-level rates of civic engagement and volunteering measures
#' from the Current Population Survey Civic Engagement and Volunteering
#' supplement. Each row is one survey year with rates for 17 different
#' engagement measures.
#'
#' @param limit Maximum rows to return. Default \code{100}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with columns (all character except year):
#'   \itemize{
#'     \item \code{year} (character): Survey year
#'     \item \code{national_formal_volunteering} (character): Formal volunteering rate
#'     \item \code{national_organizational} (character): Organizational membership rate
#'     \item \code{national_charitable_giving} (character): Charitable giving rate
#'     \item \code{national_informal_helping} (character): Informal helping rate
#'     \item \code{national_talking_with_friends} (character): Talking with friends rate
#'     \item \code{national_talking_with} (character): Talking with neighbors rate
#'     \item \code{national_learning_about_issues} (character): Learning about issues rate
#'     \item \code{national_discussing_issues} (character): Discussing issues with friends rate
#'     \item \code{national_discussing_issues_1} (character): Discussing issues with neighbors rate
#'     \item \code{national_posting_views_online} (character): Posting views online rate
#'     \item \code{national_rate_of_voting_in} (character): Voting in local elections rate
#'     \item \code{national_contacting_public} (character): Contacting public officials rate
#'     \item \code{national_donating_to_a} (character): Donating to political causes rate
#'     \item \code{national_attending_public} (character): Attending public meetings rate
#'     \item \code{national_taking_action_with} (character): Taking action with neighbors rate
#'     \item \code{national_buycotting_or} (character): Buycotting/boycotting rate
#'     \item \code{national_employer_promotes} (character): Employer promotes volunteering rate
#'   }
#' @examples
#' \dontrun{
#' # Get all years of national civic engagement data
#' acorps_cev_national()
#' }
acorps_cev_national <- function(limit = 100, token = NULL) {
  ._soda_query("rhng-qtzw", limit = limit, token = token, max_results = limit)
}

#' @title Fetch CEV national rates by demographics (2017-2023)
#'
#' Returns civic engagement rates broken down by demographic subgroup
#' (age, gender, race/ethnicity, education, income, etc.) across survey
#' years 2017, 2019, 2021, and 2023. Each row is one demographic subgroup
#' with rates for each measure and year in wide format (67 columns).
#'
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with 67 columns. Key columns:
#'   \itemize{
#'     \item \code{demographic_subgroup} (character): Demographic group label (e.g. "18-24", "Female", "White")
#'     \item \code{national_YYYY_formal*} (character): Formal volunteering rate for year YYYY
#'     \item \code{national_YYYY_organizational*} (character): Organizational membership rate
#'     \item \code{national_YYYY_charitable*} (character): Charitable giving rate
#'     \item \code{national_YYYY_informal_helping*} (character): Informal helping rate
#'   }
#'   Plus similar columns for talking with friends/neighbors, learning about
#'   issues, discussing issues, posting views online, contacting officials,
#'   donating, attending meetings, taking action, buycotting, employer
#'   volunteering, and voting -- each by year.
#' @examples
#' \dontrun{
#' # All demographic breakdowns
#' acorps_cev_demographics()
#'
#' # Get data and inspect subgroups
#' d <- acorps_cev_demographics()
#' unique(d$demographic_subgroup)
#' }
acorps_cev_demographics <- function(limit = 1000, token = NULL) {
  ._soda_query("bhmf-84dy", limit = limit, token = token, max_results = limit)
}

#' @title Fetch CEV state-level civic engagement rates (2017-2023)
#'
#' Returns state-level rates of civic engagement and volunteering measures
#' for all U.S. states. Each row is one state with columns for each measure.
#' Filter by state abbreviation to get a single state's data.
#'
#' @param state Two-letter state abbreviation filter. Examples: \code{"CA"},
#'   \code{"AL"}, \code{"NY"}. \code{NULL} for all states. Default \code{NULL}.
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with state-level civic engagement rates. Columns include
#'   a \code{state} identifier and rates for formal volunteering, informal
#'   helping, charitable giving, and other civic engagement measures.
#' @examples
#' \dontrun{
#' # All states
#' acorps_cev_states()
#'
#' # California only
#' acorps_cev_states(state = "CA")
#'
#' # Compare two states
#' acorps_cev_states(state = "NY")
#' }
acorps_cev_states <- function(state = NULL, limit = 1000, token = NULL) {
  where <- if (!is.null(state)) sprintf("state='%s'", state) else NULL
  ._soda_query("4r6x-re58", where = where, limit = limit,
               token = token, max_results = limit)
}


# == Demographics ==============================================================

#' @title Fetch AmeriCorps participant demographics data
#'
#' Returns odds-ratio analysis comparing AmeriCorps member and volunteer
#' demographics to U.S. Census population estimates. Each row is one
#' demographic group within a program, with an odds ratio indicating whether
#' that group is over- or under-represented relative to the general population.
#'
#' @param program Program name filter (exact match). Examples:
#'   \code{"AmeriCorps State and National"}, \code{"AmeriCorps NCCC"},
#'   \code{"AmeriCorps VISTA"}. \code{NULL} for all programs. Default \code{NULL}.
#' @param state State name filter (exact match). Example: \code{"California"}.
#'   \code{NULL} for all states. Default \code{NULL}.
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{geographic_level} (character): Geographic scope (e.g. "National", "State")
#'     \item \code{demographic_category} (character): Category (e.g. "Age", "Race", "Gender")
#'     \item \code{demographic_group} (character): Specific group within category
#'     \item \code{time_period_fy} (character): Fiscal year time period
#'     \item \code{program} (character): AmeriCorps program name
#'     \item \code{service_region} (character): Service region
#'     \item \code{state} (character): State name
#'     \item \code{total_population_census} (character): Census population total
#'     \item \code{total_msys_vols_assessed} (character): Total members/volunteers assessed
#'     \item \code{msys_vols_this_group} (character): Members/volunteers in this group
#'     \item \code{population_in_group_census} (character): Census population in group
#'     \item \code{odds_ratio_estimate} (character): Odds ratio (>1 = overrepresented)
#'     \item \code{lower_95_ci_limit} (character): Lower 95\% confidence interval
#'     \item \code{upper_95_ci_limit} (character): Upper 95\% confidence interval
#'     \item \code{representation} (character): Representation classification
#'     \item \code{magnitude_range_size} (numeric): Size of the magnitude range
#'     \item \code{significant} (logical): Statistical significance flag
#'   }
#' @examples
#' \dontrun{
#' # All demographic data
#' acorps_demographics()
#'
#' # Demographics for NCCC only
#' acorps_demographics(program = "AmeriCorps NCCC")
#'
#' # California demographics
#' acorps_demographics(state = "California")
#' }
acorps_demographics <- function(program = NULL, state = NULL, limit = 1000,
                                token = NULL) {
  parts <- character()
  if (!is.null(program)) parts <- c(parts, sprintf("program='%s'", program))
  if (!is.null(state))   parts <- c(parts, sprintf("state='%s'", state))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL
  ._soda_query("i9xs-fvag", where = where, limit = limit,
               token = token, max_results = limit)
}


# == Research Grants ===========================================================

#' @title Fetch AmeriCorps research grantee dataset
#'
#' Returns grants awarded to researchers at institutions of higher education
#' for studies on civic engagement, volunteering, and national service.
#' Each row is one research grant with topic area flags (boolean columns)
#' indicating the grant's focus areas.
#'
#' @param state State name filter (exact match). Example: \code{"California"}.
#'   \code{NULL} for all states. Default \code{NULL}.
#' @param year Grant year filter (character or numeric). Example: \code{"2020"}.
#'   \code{NULL} for all years. Default \code{NULL}.
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{year} (character): Grant year
#'     \item \code{grant_status} (character): Status of the grant
#'     \item \code{university} (character): University or institution name
#'     \item \code{title} (character): Research project title
#'     \item \code{background} (character): Project background description
#'     \item \code{research_approach} (character): Research methodology description
#'     \item \code{location_type} (character): Location classification
#'     \item \code{city} (character): City of the institution
#'     \item \code{state} (character): State of the institution
#'     \item \code{country} (character): Country
#'     \item \code{city_state} (character): Combined city and state string
#'     \item \code{arts_and_culture} (logical): Topic flag
#'     \item \code{community_development} (logical): Topic flag
#'     \item \code{education_across_the_life} (logical): Topic flag
#'     \item \code{youth_development} (logical): Topic flag
#'     \item \code{environmental_stewardship} (logical): Topic flag
#'     \item \code{health_social_wellbeing} (logical): Topic flag
#'     \item \code{new_americans} (logical): Topic flag
#'     \item \code{economic_opportunity_and} (logical): Topic flag
#'     \item \code{senior_development} (logical): Topic flag
#'     \item \code{volunteering_nonprofit_studies} (logical): Topic flag
#'     \item \code{social_capital} (logical): Topic flag
#'     \item \code{georeferenced_column_} (data.frame): Nested lat/lon coordinates
#'   }
#' @examples
#' \dontrun{
#' # All research grants
#' acorps_research_grants()
#'
#' # Grants in California
#' acorps_research_grants(state = "California")
#'
#' # Grants from 2020
#' acorps_research_grants(year = "2020")
#' }
acorps_research_grants <- function(state = NULL, year = NULL, limit = 1000,
                                   token = NULL) {
  parts <- character()
  if (!is.null(state)) parts <- c(parts, sprintf("state='%s'", state))
  if (!is.null(year))  parts <- c(parts, sprintf("year='%s'", year))
  where <- if (length(parts) > 0) paste(parts, collapse = " AND ") else NULL
  ._soda_query("hznm-uizi", where = where, limit = limit,
               token = token, max_results = limit)
}


# == Employers of National Service =============================================

#' @title Fetch employers of national service
#'
#' Returns organizations that participate in the Employers of National Service
#' initiative, connecting AmeriCorps and Peace Corps alumni with employment
#' opportunities. Filter by employer category to narrow results.
#'
#' @param category Employer category filter (exact match). Valid values include:
#'   \code{"Private Sector"}, \code{"Institutions of Higher Education"},
#'   \code{"Nonprofit"}, \code{"Government"}. \code{NULL} for all categories.
#'   Default \code{NULL}.
#' @param limit Maximum rows to return. Default \code{1000}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{name_of_employer} (character): Name of the employer organization
#'     \item \code{employer_category} (character): Sector classification
#'     \item \code{geocoded_column} (data.frame): Nested lat/lon coordinates
#'   }
#' @examples
#' \dontrun{
#' # All employers
#' acorps_employers()
#'
#' # Private sector employers only
#' acorps_employers(category = "Private Sector")
#'
#' # Higher education employers
#' acorps_employers(category = "Institutions of Higher Education")
#' }
acorps_employers <- function(category = NULL, limit = 1000, token = NULL) {
  where <- if (!is.null(category)) {
    sprintf("employer_category='%s'", category)
  } else NULL
  ._soda_query("c88b-pc22", where = where, limit = limit,
               token = token, max_results = limit)
}


# == Compliance ================================================================

#' @title Fetch criminal history check recurring access data
#'
#' Returns criminal history check requirements, prices, and processing
#' times by state/territory. Covers NSOPW (National Sex Offender Public
#' Website), state-level, and FBI background check details.
#'
#' @param limit Maximum rows to return. Default \code{100}.
#' @param token Optional Socrata app token. Default \code{NULL}.
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{geocoded_column} (data.frame): Nested geographic data
#'     \item \code{state_type} (character): State/territory name
#'     \item \code{nsopw_price} (character): NSOPW check price
#'     \item \code{nsopw_average_time} (character): NSOPW average processing time
#'     \item \code{state_price} (character): State check price
#'     \item \code{state_average_time} (character): State check average processing time
#'     \item \code{fbi_price} (character): FBI check price
#'     \item \code{fbi_average_time} (character): FBI check average processing time
#'     \item \code{state_type_markers} (character): Marker classification
#'   }
#' @examples
#' \dontrun{
#' # All states
#' acorps_criminal_history_checks()
#' }
acorps_criminal_history_checks <- function(limit = 100, token = NULL) {
  ._soda_query("et85-j49w", limit = limit, token = token, max_results = limit)
}


# == Context ===================================================================

#' Get americorps.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
acorps_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(acorps_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/americorps.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "americorps.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# americorps.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# americorps.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
