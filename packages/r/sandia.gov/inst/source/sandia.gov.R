# sandia.gov.R - Self-contained sandia.gov client
# DOE Global Energy Storage Database (GESDB) at gesdb.sandia.gov

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# sandia.gov.R
# Self-contained client for Sandia National Laboratories GESDB.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none documented
# Source: https://gesdb.sandia.gov/backend/projectdata


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.sandia_base <- "https://gesdb.sandia.gov/backend"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# Cache for all projects (API returns full dataset each call)
.sandia_cache <- new.env(parent = emptyenv())

.get_all_projects <- function(refresh = FALSE) {
  if (!refresh && exists("projects", envir = .sandia_cache)) {
    return(get("projects", envir = .sandia_cache))
  }
  raw <- .fetch_json(sprintf("%s/projectdata", .sandia_base))
  parsed <- .parse_projects(raw)
  assign("projects", parsed, envir = .sandia_cache)
  parsed
}

# == Schemas ===================================================================

.schema_projects <- tibble(
  id = integer(),
  subsystem_id = numeric(),
  name = character(),
  status = character(),
  rated_power_kw = numeric(),
  duration_hrs = numeric(),
  capacity_kwh = numeric(),
  description = character(),
  url = character(),
  country = character(),
  city = character(),
  state = character(),
  latitude = numeric(),
  longitude = numeric(),
  commissioned_date = character(),
  technology_provider = character(),
  developer = character(),
  ownership_model = character(),
  capex_usd = numeric(),
  interconnection_level = character(),
  data_source = character(),
  validated = character()
)

.schema_summary <- tibble(
  country = character(),
  n_projects = integer(),
  total_power_kw = numeric(),
  total_capacity_kwh = numeric()
)

# == Private parsers ===========================================================

.safe_num <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (is.null(v) || length(v) == 0 || is.na(v)) NA_real_ else v
}

.safe_int <- function(x) {
  v <- suppressWarnings(as.integer(x))
  if (is.null(v) || length(v) == 0 || is.na(v)) NA_integer_ else v
}

.safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)
}

.parse_projects <- function(items) {
  if (is.null(items) || length(items) == 0) return(.schema_projects)

  rows <- lapply(items, function(p) {
    tibble(
      id                    = .safe_int(p[["ID"]]),
      subsystem_id          = .safe_num(p[["Subsystem ID"]]),
      name                  = .safe_chr(p[["Project/Plant Name"]]),
      status                = .safe_chr(p[["Status"]]),
      rated_power_kw        = .safe_num(p[["Rated Power (kW)"]]),
      duration_hrs          = .safe_num(p[["Discharge Duration at Rated Power (hrs)"]]),
      capacity_kwh          = .safe_num(p[["Storage Capacity (kWh)"]]),
      description           = .safe_chr(p[["Description/Notes"]]),
      url                   = .safe_chr(p[["URL"]]),
      country               = .safe_chr(p[["Country"]]),
      city                  = .safe_chr(p[["City"]]),
      state                 = .safe_chr(p[["State/Province/Territory"]]),
      latitude              = .safe_num(p[["Latitude"]]),
      longitude             = .safe_num(p[["Longitude"]]),
      commissioned_date     = .safe_chr(p[["Commissioned Date"]]),
      technology_provider   = .safe_chr(p[["Energy Storage Technology Provider"]]),
      developer             = .safe_chr(p[["Developer"]]),
      ownership_model       = .safe_chr(p[["Ownership Model"]]),
      capex_usd             = .safe_num(p[["Capital Expenditure - CAPEX (USD)"]]),
      interconnection_level = .safe_chr(p[["Grid Interconnection Level"]]),
      data_source           = .safe_chr(p[["Data Source"]]),
      validated             = .safe_chr(p[["Project Data Validated?"]])
    )
  })

  bind_rows(rows)
}


# == Public functions ==========================================================

#' List all energy storage projects from DOE GESDB
#'
#' Returns all projects from the DOE Global Energy Storage Database (GESDB)
#' maintained by Sandia National Laboratories. The full dataset (3,000+
#' projects worldwide) is cached in-session after the first call.
#'
#' @param refresh Logical. If \code{TRUE}, re-downloads from the API
#'   instead of using the in-session cache. Default \code{FALSE}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Integer. GESDB project ID.}
#'     \item{subsystem_id}{Numeric. Subsystem identifier.}
#'     \item{name}{Character. Project or plant name.}
#'     \item{status}{Character. Project status (e.g. \code{"Operational"}, \code{"Announced"}, \code{"Under construction"}, \code{"De-Commissioned"}).}
#'     \item{rated_power_kw}{Numeric. Rated power in kilowatts.}
#'     \item{duration_hrs}{Numeric. Discharge duration at rated power in hours.}
#'     \item{capacity_kwh}{Numeric. Storage capacity in kilowatt-hours.}
#'     \item{description}{Character. Project notes.}
#'     \item{url}{Character. Reference URL.}
#'     \item{country}{Character. Country name (e.g. \code{"United States"}, \code{"Germany"}).}
#'     \item{city}{Character. City name.}
#'     \item{state}{Character. State, province, or territory.}
#'     \item{latitude}{Numeric. Latitude coordinate.}
#'     \item{longitude}{Numeric. Longitude coordinate.}
#'     \item{commissioned_date}{Character. Date commissioned (text format).}
#'     \item{technology_provider}{Character. Energy storage technology provider.}
#'     \item{developer}{Character. Project developer.}
#'     \item{ownership_model}{Character. Ownership model description.}
#'     \item{capex_usd}{Numeric. Capital expenditure in USD.}
#'     \item{interconnection_level}{Character. Grid interconnection level.}
#'     \item{data_source}{Character. Source of the data.}
#'     \item{validated}{Character. Whether project data has been validated.}
#'   }
#' @examples
#' sandia_projects()
#' @export
sandia_projects <- function(refresh = FALSE) {
  .get_all_projects(refresh = refresh)
}

#' Search energy storage projects by keyword
#'
#' Performs a case-insensitive text search across project names and/or
#' descriptions in the GESDB.
#'
#' @param query Character. Search term (e.g. \code{"battery"}, \code{"solar"},
#'   \code{"pumped hydro"}).
#' @param field Character. Which fields to search: \code{"all"} (default,
#'   searches both name and description), \code{"name"} (project name only),
#'   or \code{"description"} (description/notes only).
#' @return A tibble of matching projects with the same columns as
#'   \code{sandia_projects()}.
#' @examples
#' sandia_search("battery")
#' sandia_search("solar", field = "name")
#' @export
sandia_search <- function(query, field = "all") {
  all_proj <- .get_all_projects()
  q <- tolower(as.character(query))

  matches <- switch(field,
    "name" = grepl(q, tolower(all_proj$name), fixed = TRUE),
    "description" = grepl(q, tolower(all_proj$description), fixed = TRUE),
    grepl(q, tolower(all_proj$name), fixed = TRUE) |
      grepl(q, tolower(all_proj$description), fixed = TRUE)
  )

  all_proj[matches, ]
}

#' Get a single energy storage project by ID
#'
#' Retrieves details for one project from the GESDB by its numeric ID.
#'
#' @param id Integer or numeric. GESDB project ID. Obtain from the
#'   \code{id} column of \code{sandia_projects()} or \code{sandia_search()}.
#' @return A tibble with one row and the same columns as
#'   \code{sandia_projects()}.
#' @examples
#' sandia_project(1)
#' @export
sandia_project <- function(id) {
  all_proj <- .get_all_projects()
  result <- all_proj |> filter(id == as.integer(!!id))
  if (nrow(result) == 0) {
    message("No project found with ID ", id)
    return(.schema_projects)
  }
  result
}

#' List energy storage projects by country
#'
#' Filters GESDB projects by country name with optional status filter.
#' Uses case-insensitive partial matching on country name.
#'
#' @param country_query Character. Country name or partial name
#'   (e.g. \code{"United States"}, \code{"Germany"}, \code{"Japan"}).
#'   Case-insensitive.
#' @param status Character or NULL. Optional status filter
#'   (e.g. \code{"Operational"}, \code{"Announced"},
#'   \code{"Under construction"}). NULL (default) returns all statuses.
#'   Use \code{sandia_statuses()} to see valid values.
#' @return A tibble of matching projects with the same columns as
#'   \code{sandia_projects()}.
#' @examples
#' sandia_by_country("Germany")
#' sandia_by_country("United States", status = "Operational")
#' @export
sandia_by_country <- function(country_query, status = NULL) {
  all_proj <- .get_all_projects()
  q <- tolower(as.character(country_query))
  result <- all_proj |>
    filter(grepl(q, tolower(country), fixed = TRUE))

  if (!is.null(status)) {
    s <- tolower(as.character(status))
    result <- result |>
      filter(grepl(s, tolower(.data$status), fixed = TRUE))
  }
  result
}

#' Summarize energy storage projects by country
#'
#' Returns aggregate statistics per country: project count, total rated
#' power, and total storage capacity from the full GESDB.
#'
#' @param top_n Integer or NULL. If provided, return only the top N
#'   countries ranked by project count. NULL (default) returns all countries.
#' @return A tibble with columns:
#'   \describe{
#'     \item{country}{Character. Country name.}
#'     \item{n_projects}{Integer. Number of projects in that country.}
#'     \item{total_power_kw}{Numeric. Sum of rated power in kilowatts.}
#'     \item{total_capacity_kwh}{Numeric. Sum of storage capacity in kilowatt-hours.}
#'   }
#' @examples
#' sandia_summary(top_n = 10)
#' @export
sandia_summary <- function(top_n = NULL) {
  all_proj <- .get_all_projects()
  result <- all_proj |>
    group_by(country) |>
    summarise(
      n_projects       = n(),
      total_power_kw   = sum(rated_power_kw, na.rm = TRUE),
      total_capacity_kwh = sum(capacity_kwh, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(n_projects))

  if (!is.null(top_n)) {
    result <- result |> head(as.integer(top_n))
  }
  result
}

#' List unique project statuses in the GESDB
#'
#' Returns all distinct project status values and their frequency. Useful
#' for filtering with \code{sandia_by_country()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{status}{Character. Project status (e.g. \code{"Operational"}, \code{"Announced"}, \code{"Under construction"}, \code{"De-Commissioned"}).}
#'     \item{n_projects}{Integer. Number of projects with that status.}
#'   }
#' @examples
#' sandia_statuses()
#' @export
sandia_statuses <- function() {
  all_proj <- .get_all_projects()
  all_proj |>
    filter(!is.na(status)) |>
    group_by(status) |>
    summarise(n_projects = n(), .groups = "drop") |>
    arrange(desc(n_projects))
}

#' List unique countries in the GESDB
#'
#' Returns all countries represented in the Global Energy Storage Database,
#' ranked by project count.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{country}{Character. Country name (e.g. \code{"United States"}, \code{"Germany"}).}
#'     \item{n_projects}{Integer. Number of projects in that country.}
#'   }
#' @examples
#' sandia_countries()
#' @export
sandia_countries <- function() {
  all_proj <- .get_all_projects()
  all_proj |>
    filter(!is.na(country)) |>
    group_by(country) |>
    summarise(n_projects = n(), .groups = "drop") |>
    arrange(desc(n_projects))
}


# == Context ===================================================================

#' Get sandia.gov client context for LLM use
#'
#' Reads this source file and prints roxygen blocks and function signatures
#' for every public function, providing a compact reference suitable for
#' pasting into an LLM prompt.
#'
#' @return Character string of formatted documentation (printed to console
#'   and returned invisibly).
#' @examples
#' sandia_context()
#' @export
sandia_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(sandia_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/sandia.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "sandia.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# sandia.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# sandia.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
