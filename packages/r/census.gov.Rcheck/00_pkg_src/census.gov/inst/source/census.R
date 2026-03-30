# census-gov.R
# Self-contained US Census Bureau client.
# All public functions return tibbles. All columns return as character —
# caller decides types (Census uses "-", "N", "(X)" for suppressed data).
#
# Dependencies: httr2, jsonlite, dplyr, tidyr, tibble
# Auth: optional API key (query param). Get one at api.census.gov/data/key_signup.html

library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.census_cache <- new.env(parent = emptyenv())
.census_base <- "https://api.census.gov/data"

# Max variables per Census API request (hard limit is 50)
.census_var_limit <- 49

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# -- Census response parser ----------------------------------------------------
# Census API returns JSON array-of-arrays: [[headers], [row1], [row2], ...]
# Returns ALL columns as character — no auto-type guessing.

.parse_census_response <- function(raw) {
  if (is.matrix(raw)) {
    if (nrow(raw) < 2) return(tibble())
    headers <- tolower(raw[1, ])
    mat <- raw[-1, , drop = FALSE]
    colnames(mat) <- headers
    as_tibble(as.data.frame(mat, stringsAsFactors = FALSE))
  } else if (is.list(raw) && length(raw) >= 2) {
    headers <- tolower(unlist(raw[[1]]))
    rows <- raw[-1]
    if (length(rows) == 0)
      return(tibble(!!!setNames(rep(list(character()), length(headers)), headers)))
    mat <- do.call(rbind, lapply(rows, function(r) {
      r[vapply(r, is.null, logical(1))] <- NA_character_
      unlist(r)
    }))
    colnames(mat) <- headers
    as_tibble(as.data.frame(mat, stringsAsFactors = FALSE))
  } else {
    tibble()
  }
}

# -- Variable caching ----------------------------------------------------------

.cached_variables <- function(dataset, year, key = NULL) {
  cache_key <- paste0("vars_", dataset, "_", year)
  if (!is.null(.census_cache[[cache_key]])) return(.census_cache[[cache_key]])
  result <- census_variables(dataset, year, key)
  .census_cache[[cache_key]] <- result
  result
}

# == Schemas ===================================================================

.schema_datasets <- tibble(
  title = character(), description = character(),
  dataset = character(), year = integer()
)

.schema_variables <- tibble(
  name = character(), label = character(), concept = character(),
  predicate_type = character(), group = character()
)

.schema_groups <- tibble(name = character(), description = character())

.schema_geography <- tibble(
  name = character(), geo_level = character(),
  requires = character(), wildcard = character()
)


# == Geography helpers =========================================================

#' Build a state-level geography spec
#' @param code FIPS code or "*" for all (default)
#' @return list(for_geo, in_geo) for use with census_get
census_geo_state <- function(code = "*") {
  list(for_geo = paste0("state:", code), in_geo = NULL)
}

#' Build a county-level geography spec
#' @param state State FIPS code (required)
#' @param code County FIPS or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_county <- function(state, code = "*") {
  list(for_geo = paste0("county:", code),
       in_geo = paste0("state:", state))
}

#' Build a tract-level geography spec
#' @param state State FIPS code (required)
#' @param county County FIPS code (required)
#' @param code Tract code or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_tract <- function(state, county, code = "*") {
  list(for_geo = paste0("tract:", code),
       in_geo = paste("state:", state, " county:", county, sep = ""))
}

#' Build a place-level geography spec
#' @param state State FIPS code (required)
#' @param code Place FIPS or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_place <- function(state, code = "*") {
  list(for_geo = paste0("place:", code),
       in_geo = paste0("state:", state))
}

#' Build a ZCTA geography spec
#' @param code ZCTA code or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_zcta <- function(code = "*") {
  list(for_geo = paste0("zip code tabulation area:", code), in_geo = NULL)
}

#' Build a block group geography spec
#' @param state State FIPS (required)
#' @param county County FIPS (required)
#' @param tract Tract code (required)
#' @param code Block group code or "*" for all (default)
#' @return list(for_geo, in_geo)
census_geo_blockgroup <- function(state, county, tract, code = "*") {
  list(for_geo = paste0("block group:", code),
       in_geo = paste0("state:", state, " county:", county, " tract:", tract))
}


# == Discovery =================================================================

#' List all available Census Bureau datasets
#'
#' Fetches the full dataset catalog. Cached per session.
#'
#' @return tibble: title, description, dataset (API path), year
census_datasets <- function() {
  if (!is.null(.census_cache$datasets)) return(.census_cache$datasets)
  raw <- .fetch_json(paste0(.census_base, ".json"))
  ds <- raw$dataset
  if (is.null(ds) || length(ds) == 0) return(.schema_datasets)

  df <- as_tibble(ds) |>
    mutate(
      dataset = vapply(c_dataset, function(x) paste(x, collapse = "/"), character(1)),
      year = suppressWarnings(as.integer(c_vintage)),
      title = as.character(title),
      description = as.character(description)
    ) |>
    select(title, description, dataset, year) |>
    filter(!is.na(year)) |>
    arrange(desc(year), dataset)

  .census_cache$datasets <- df
  df
}

#' List variables for a dataset/year
#'
#' @param dataset API dataset path (e.g. "acs/acs5")
#' @param year Integer year
#' @param key Optional API key
#' @return tibble: name, label, concept, predicate_type, group
census_variables <- function(dataset, year, key = NULL) {
  url <- sprintf("%s/%d/%s/variables.json", .census_base, year, dataset)
  if (!is.null(key)) url <- paste0(url, "?key=", key)

  raw <- .fetch_json(url)
  v <- raw$variables
  if (is.null(v) || length(v) == 0) return(.schema_variables)

  tibble(
    name = names(v),
    label = vapply(v, function(x) x$label %||% NA_character_, character(1)),
    concept = vapply(v, function(x) x$concept %||% NA_character_, character(1)),
    predicate_type = vapply(v, function(x) x$predicateType %||% NA_character_, character(1)),
    group = vapply(v, function(x) x$group %||% NA_character_, character(1))
  ) |> filter(!name %in% c("for", "in", "ucgid"))
}

#' List variable groups for a dataset/year
#'
#' @param dataset API dataset path
#' @param year Integer year
#' @param key Optional API key
#' @return tibble: name, description
census_groups <- function(dataset, year, key = NULL) {
  url <- sprintf("%s/%d/%s/groups.json", .census_base, year, dataset)
  if (!is.null(key)) url <- paste0(url, "?key=", key)

  raw <- .fetch_json(url)
  g <- raw$groups
  if (is.null(g) || length(g) == 0) return(.schema_groups)
  as_tibble(g) |> select(any_of(c("name", "description")))
}

#' List variables within a specific group
#'
#' @param dataset API dataset path
#' @param year Integer year
#' @param group Group name (e.g. "B01001")
#' @param key Optional API key
#' @return tibble: name, label, concept, predicate_type, group
census_group_variables <- function(dataset, year, group, key = NULL) {
  url <- sprintf("%s/%d/%s/groups/%s.json", .census_base, year, dataset, group)
  if (!is.null(key)) url <- paste0(url, "?key=", key)

  raw <- .fetch_json(url)
  v <- raw$variables
  if (is.null(v) || length(v) == 0) return(.schema_variables)

  tibble(
    name = names(v),
    label = vapply(v, function(x) x$label %||% NA_character_, character(1)),
    concept = vapply(v, function(x) x$concept %||% NA_character_, character(1)),
    predicate_type = vapply(v, function(x) x$predicateType %||% NA_character_, character(1)),
    group = group
  ) |> filter(!name %in% c("for", "in", "ucgid"))
}

#' List available geographies for a dataset/year
#'
#' @param dataset API dataset path
#' @param year Integer year
#' @param key Optional API key
#' @return tibble: name, geo_level, requires, wildcard
census_geography <- function(dataset, year, key = NULL) {
  url <- sprintf("%s/%d/%s/geography.json", .census_base, year, dataset)
  if (!is.null(key)) url <- paste0(url, "?key=", key)

  raw <- .fetch_json(url)
  fips <- raw$fips
  if (is.null(fips) || length(fips) == 0) return(.schema_geography)

  df <- as_tibble(fips)
  df |>
    transmute(
      name = as.character(name),
      geo_level = as.character(geoLevelDisplay),
      requires = vapply(requires, function(r) {
        if (is.null(r) || all(is.na(r))) NA_character_ else paste(r, collapse = ", ")
      }, character(1)),
      wildcard = vapply(wildcard, function(w) {
        if (is.null(w) || all(is.na(w))) NA_character_ else paste(w, collapse = ", ")
      }, character(1))
    )
}

#' Search Census variables by keyword
#'
#' Searches variable labels and concepts. Uses cached variable list.
#'
#' @param dataset API dataset path
#' @param year Integer year
#' @param query Search term (case-insensitive regex)
#' @param key Optional API key
#' @return tibble: matching variables with name, label, concept, group
census_search <- function(dataset, year, query, key = NULL) {
  vars <- .cached_variables(dataset, year, key)
  if (nrow(vars) == 0) return(vars)
  vars |> filter(
    grepl(query, label, ignore.case = TRUE) |
    grepl(query, concept, ignore.case = TRUE) |
    grepl(query, name, ignore.case = TRUE)
  )
}


# == S3 Dataset object =========================================================

#' Load a Census dataset with full metadata
#'
#' Fetches variables, groups, and geography info. Returns a lightweight
#' classed list (not the full variable catalog) with metadata as attrs.
#' Use census_variables() or census_search() to explore variables.
#'
#' @param dataset API dataset path (e.g. "acs/acs5")
#' @param year Integer year
#' @param key Optional API key
#' @return S3 object of class "census_dataset"
census_dataset <- function(dataset, year, key = NULL) {
  if (inherits(dataset, "census_dataset")) return(dataset)

  vars   <- tryCatch(.cached_variables(dataset, year, key),
                     error = function(e) .schema_variables)
  groups <- tryCatch(census_groups(dataset, year, key),
                     error = function(e) .schema_groups)
  geos   <- tryCatch(census_geography(dataset, year, key),
                     error = function(e) .schema_geography)

  # Summarize variables by concept for the print method
  var_summary <- vars |>
    filter(!is.na(concept)) |>
    count(concept, sort = TRUE)

  result <- tibble(
    dataset    = dataset,
    year       = as.integer(year),
    n_vars     = nrow(vars),
    n_groups   = nrow(groups),
    n_geos     = nrow(geos)
  )

  attr(result, "dataset")     <- dataset
  attr(result, "year")        <- as.integer(year)
  attr(result, "key")         <- key
  attr(result, "base_url")    <- sprintf("%s/%d/%s", .census_base, year, dataset)
  attr(result, "groups")      <- groups
  attr(result, "geography")   <- geos
  attr(result, "var_summary") <- var_summary
  class(result) <- c("census_dataset", class(result))
  result
}

#' @export
print.census_dataset <- function(x, ...) {
  dataset     <- attr(x, "dataset")
  year        <- attr(x, "year")
  groups      <- attr(x, "groups")
  geos        <- attr(x, "geography")
  var_summary <- attr(x, "var_summary")

  cat(strrep("\u2500", 70), "\n")
  cat(sprintf("Census Dataset: %s (%d)\n", dataset, year))
  cat(sprintf("%d variables | %d groups | %d geographies\n",
              x$n_vars, nrow(groups), nrow(geos)))
  cat(strrep("\u2500", 70), "\n")

  # Top concepts
  if (nrow(var_summary) > 0) {
    cat("\nTop concepts:\n")
    for (i in seq_len(min(5, nrow(var_summary))))
      cat(sprintf("  %s (%d vars)\n", var_summary$concept[i], var_summary$n[i]))
    if (nrow(var_summary) > 5) cat(sprintf("  ... and %d more\n", nrow(var_summary) - 5))
  }

  if (nrow(groups) > 0)
    cat(sprintf("\nGroups: %d (e.g. %s)\n", nrow(groups),
                paste(head(groups$name, 5), collapse = ", ")))

  if (nrow(geos) > 0) {
    cat("Geographies:", paste(head(geos$name, 8), collapse = ", "))
    if (nrow(geos) > 8) cat(", ...")
    cat("\n")
  }

  cat(sprintf('\nUsage: census_get("%s", %d, c("NAME", ...), "state:*")\n',
              dataset, year))
  cat(sprintf('Search: census_search("%s", %d, "median income")\n', dataset, year))

  invisible(x)
}


# == Core data fetching ========================================================

#' Fetch data from any Census API endpoint
#'
#' The universal engine. Builds the URL, fetches, and parses the
#' array-of-arrays JSON into a tibble. All columns return as character —
#' Census uses "-", "N", "(X)" for suppressed data, so auto-typing is
#' unsafe. Use mutate(across(..., as.numeric)) on the columns you need.
#'
#' Automatically chunks requests exceeding the Census API's 50-variable
#' limit and merges the results.
#'
#' @param dataset API dataset path (e.g. "acs/acs5", "dec/pl")
#' @param year Integer year
#' @param variables Character vector of variable names, or "group(B01001)"
#' @param for_geo Geography spec (e.g. "state:*", "county:*").
#'   Or pass a list from census_geo_*() helpers.
#' @param in_geo Parent geography constraint. String with space-separated
#'   components (e.g. "state:06 county:037") or NULL.
#' @param key Optional API key
#' @param label If TRUE, join variable labels from the variable catalog (cached)
#' @return tibble with one row per geography, all columns character.
census_get <- function(dataset, year, variables, for_geo,
                       in_geo = NULL, key = NULL, label = FALSE) {
  # Accept list from census_geo_*() helpers
  if (is.list(for_geo) && !is.null(for_geo$for_geo)) {
    in_geo <- in_geo %||% for_geo$in_geo
    for_geo <- for_geo$for_geo
  }

  # Always include NAME for join key when chunking
  has_name <- "NAME" %in% toupper(variables)
  if (!has_name) variables <- c("NAME", variables)

  # Chunk variables if > limit (Census hard caps at 50)
  if (length(variables) > .census_var_limit) {
    # Split into chunks, each with NAME for joining
    non_name <- setdiff(variables, "NAME")
    chunks <- split(non_name, ceiling(seq_along(non_name) / (.census_var_limit - 1)))
    chunks <- lapply(chunks, function(ch) c("NAME", ch))

    results <- lapply(seq_along(chunks), function(i) {
      message(sprintf("[%d/%d] Fetching %d variables...", i, length(chunks), length(chunks[[i]])))
      .census_get_single(dataset, year, chunks[[i]], for_geo, in_geo, key)
    })

    # Join all chunks on geography columns
    df <- results[[1]]
    geo_join_cols <- intersect(names(df),
      c("name", "state", "county", "tract", "place", "us", "region",
        "division", "zip code tabulation area", "zcta",
        "congressional district", "block group", "block"))
    for (i in seq_along(results)[-1]) {
      new_cols <- setdiff(names(results[[i]]), names(df))
      if (length(new_cols) > 0)
        df <- df |> left_join(results[[i]] |> select(all_of(c(geo_join_cols, new_cols))),
                              by = geo_join_cols)
    }
  } else {
    df <- .census_get_single(dataset, year, variables, for_geo, in_geo, key)
  }

  # Drop NAME if caller didn't ask for it
  if (!has_name && "name" %in% names(df)) df <- df |> select(-name)

  # Auto-join variable labels
  if (label && nrow(df) > 0) {
    vars_meta <- tryCatch(.cached_variables(dataset, year, key),
                          error = function(e) .schema_variables)
    if (nrow(vars_meta) > 0) {
      lookup <- vars_meta |> select(name, label) |> mutate(name = tolower(name))
      geo_cols_present <- intersect(names(df),
        c("name", "state", "county", "tract", "place", "us", "region",
          "division", "zip code tabulation area", "zcta",
          "congressional district", "block group", "block"))
      var_cols <- setdiff(names(df), geo_cols_present)
      label_map <- lookup |> filter(name %in% var_cols) |>
        mutate(label = gsub("^Estimate!!|^Total:!!", "", label))
      for (i in seq_len(nrow(label_map)))
        df[[paste0(label_map$name[i], "_label")]] <- label_map$label[i]
    }
  }
  df
}

# Single-request fetch (no chunking). URL-encodes for_geo and in_geo.
.census_get_single <- function(dataset, year, variables, for_geo,
                               in_geo = NULL, key = NULL) {
  vars_str <- paste(variables, collapse = ",")
  url <- sprintf("%s/%d/%s?get=%s&for=%s",
                 .census_base, year, dataset, vars_str,
                 utils::URLencode(for_geo))
  if (!is.null(in_geo))
    url <- paste0(url, "&in=", utils::URLencode(in_geo))
  if (!is.null(key))
    url <- paste0(url, "&key=", key)

  .parse_census_response(.fetch_json(url))
}


# == Convenience wrappers ======================================================

# -- ACS 5-Year ---------------------------------------------------------------

#' ACS 5-Year Detailed Tables
#' @param year Survey year (default 2022). Available 2009+.
#' @param variables Variable names (e.g. "B01001_001E", "B19013_001E")
#' @param for_geo Geography spec string or census_geo_*() list
#' @param in_geo Parent constraint (e.g. "state:06")
#' @param key API key
#' @param label If TRUE, add _label columns with variable descriptions
#' @return tibble (all columns character)
census_acs5 <- function(year = 2022, variables, for_geo,
                        in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs5", year, variables, for_geo, in_geo, key, label)
}

#' ACS 5-Year Subject Tables (S-prefixed variables)
#' @inheritParams census_acs5
#' @return tibble
census_acs5_subject <- function(year = 2022, variables, for_geo,
                                in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs5/subject", year, variables, for_geo, in_geo, key, label)
}

#' ACS 5-Year Data Profile Tables (DP-prefixed variables)
#' @inheritParams census_acs5
#' @return tibble
census_acs5_profile <- function(year = 2022, variables, for_geo,
                                in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs5/profile", year, variables, for_geo, in_geo, key, label)
}

#' ACS 5-Year Comparison Profile Tables (CP-prefixed variables)
#' @inheritParams census_acs5
#' @return tibble
census_acs5_comparison <- function(year = 2022, variables, for_geo,
                                   in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs5/cprofile", year, variables, for_geo, in_geo, key, label)
}

# -- ACS 1-Year ---------------------------------------------------------------

#' ACS 1-Year Detailed Tables (areas with 65,000+ population)
#' @inheritParams census_acs5
#' @return tibble
census_acs1 <- function(year = 2022, variables, for_geo,
                        in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs1", year, variables, for_geo, in_geo, key, label)
}

#' ACS 1-Year Subject Tables
#' @inheritParams census_acs5
#' @return tibble
census_acs1_subject <- function(year = 2022, variables, for_geo,
                                in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs1/subject", year, variables, for_geo, in_geo, key, label)
}

#' ACS 1-Year Data Profile Tables
#' @inheritParams census_acs5
#' @return tibble
census_acs1_profile <- function(year = 2022, variables, for_geo,
                                in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs1/profile", year, variables, for_geo, in_geo, key, label)
}

# -- Decennial ----------------------------------------------------------------

#' Decennial Census
#'
#' @param year Census year (2020, 2010, 2000)
#' @param variables Variable names
#' @param for_geo Geography spec string or census_geo_*() list
#' @param in_geo Parent constraint
#' @param table Summary file: "pl" (redistricting, default), "dhc", "dp", "sf1", "sf2"
#' @param key API key
#' @param label If TRUE, add label columns
#' @return tibble
census_decennial <- function(year = 2020, variables, for_geo,
                             in_geo = NULL, table = "pl",
                             key = NULL, label = FALSE) {
  dataset <- switch(table,
    pl  = "dec/pl",
    dhc = "dec/dhc",
    dp  = "dec/dp",
    sf1 = "dec/sf1",
    sf2 = "dec/sf2",
    paste0("dec/", table)
  )
  census_get(dataset, year, variables, for_geo, in_geo, key, label)
}

# -- Other programs -----------------------------------------------------------

#' Population Estimates Program
#' @inheritParams census_acs5
#' @return tibble
census_pep <- function(year = 2022, variables, for_geo,
                       in_geo = NULL, key = NULL, label = FALSE) {
  census_get("pep/population", year, variables, for_geo, in_geo, key, label)
}

#' Economic Census
#' @param year Economic Census year (2017, 2012, 2007)
#' @param variables Variable names (e.g. "NAICS2017", "EMP", "PAYANN")
#' @inheritParams census_acs5
#' @return tibble
census_economic <- function(year = 2017, variables, for_geo,
                            in_geo = NULL, key = NULL, label = FALSE) {
  census_get("ecnbasic", year, variables, for_geo, in_geo, key, label)
}

# -- PUMS microdata -----------------------------------------------------------

#' ACS PUMS (Public Use Microdata Sample)
#'
#' Person-level or housing-unit-level microdata.
#' Variables use different names than summary tables (e.g. AGEP, SEX, PWGTP).
#'
#' @param year ACS year
#' @param variables PUMS variable names (e.g. c("PWGTP", "AGEP", "SEX"))
#' @param for_geo Geography (typically "state:*" or "state:06")
#' @param in_geo Parent constraint
#' @param survey "acs5" (default) or "acs1"
#' @param key API key
#' @return tibble of microdata records
census_pums <- function(year = 2022, variables, for_geo = "state:*",
                        in_geo = NULL, survey = "acs5",
                        key = NULL) {
  dataset <- paste0("acs/", survey, "/pums")
  census_get(dataset, year, variables, for_geo, in_geo, key, label = FALSE)
}

# -- Migration flows ----------------------------------------------------------

#' ACS Migration/Flows data
#'
#' County-to-county or state-to-state migration flows.
#'
#' @param year ACS year
#' @param variables Flow variables (e.g. "MOVEDIN", "FULL1_NAME", "FULL2_NAME")
#' @param for_geo Geography (e.g. "county:*")
#' @param in_geo Parent constraint (e.g. "state:06")
#' @param key API key
#' @return tibble
census_flows <- function(year = 2022, variables, for_geo,
                         in_geo = NULL, key = NULL) {
  census_get("acs/flows", year, variables, for_geo, in_geo, key, label = FALSE)
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the census.gov package
#'
#' @param compact If TRUE (default), concise output
#' @return Character string (invisibly), also printed
census_context <- function(compact = TRUE) {
  fns <- list(
    list("census_datasets", "()",
         "List all Census Bureau datasets with years. Cached."),
    list("census_variables", "(dataset, year, key)",
         "List variables for a dataset/year with labels and concepts."),
    list("census_groups", "(dataset, year, key)",
         "List variable groups for a dataset/year."),
    list("census_group_variables", "(dataset, year, group, key)",
         "List variables within a specific group."),
    list("census_geography", "(dataset, year, key)",
         "List available geographies for a dataset/year."),
    list("census_search", "(dataset, year, query, key)",
         "Search variables by keyword in labels/concepts. Cached."),
    list("census_dataset", "(dataset, year, key)",
         "Load dataset metadata as S3 object. print() shows rich summary."),
    list("census_get", "(dataset, year, variables, for_geo, in_geo, key, label)",
         "Core engine: any Census endpoint. Auto-chunks >49 vars. label=TRUE joins descriptions."),
    list("census_acs5", "(year, variables, for_geo, in_geo, key, label)",
         "ACS 5-Year Detailed Tables (B-prefixed). Most common."),
    list("census_acs5_subject", "(year, variables, for_geo, in_geo, key, label)",
         "ACS 5-Year Subject Tables (S-prefixed). Pre-computed rates/percentages."),
    list("census_acs5_profile", "(year, variables, for_geo, in_geo, key, label)",
         "ACS 5-Year Data Profiles (DP-prefixed). Broad demographic snapshots."),
    list("census_acs5_comparison", "(year, variables, for_geo, in_geo, key, label)",
         "ACS 5-Year Comparison Profiles (CP-prefixed). Multi-year trends."),
    list("census_acs1", "(year, variables, for_geo, in_geo, key, label)",
         "ACS 1-Year Detailed Tables (areas 65k+ pop)."),
    list("census_acs1_subject", "(year, variables, for_geo, in_geo, key, label)",
         "ACS 1-Year Subject Tables."),
    list("census_acs1_profile", "(year, variables, for_geo, in_geo, key, label)",
         "ACS 1-Year Data Profiles."),
    list("census_decennial", "(year, variables, for_geo, in_geo, table, key, label)",
         "Decennial Census. table='pl'|'dhc'|'dp'|'sf1'|'sf2'."),
    list("census_pep", "(year, variables, for_geo, in_geo, key, label)",
         "Population Estimates Program."),
    list("census_economic", "(year, variables, for_geo, in_geo, key, label)",
         "Economic Census data."),
    list("census_pums", "(year, variables, for_geo, in_geo, survey, key)",
         "ACS PUMS microdata (person/housing-unit level)."),
    list("census_flows", "(year, variables, for_geo, in_geo, key)",
         "ACS migration/flows data (county-to-county or state-to-state)."),
    list("census_geo_state", "(code)", "Geography helper: state level."),
    list("census_geo_county", "(state, code)", "Geography helper: county within state."),
    list("census_geo_tract", "(state, county, code)", "Geography helper: tract within county."),
    list("census_geo_place", "(state, code)", "Geography helper: place within state."),
    list("census_geo_zcta", "(code)", "Geography helper: ZIP code tabulation area."),
    list("census_geo_blockgroup", "(state, county, tract, code)", "Geography helper: block group.")
  )

  lines <- c(
    "# census.gov - US Census Bureau Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tidyr, tibble",
    "# Auth: optional API key as `key` param (500 req/day without)",
    "# All columns return as character (Census uses '-', 'N', '(X)' for suppressed data).",
    "# Use mutate(across(..., as.numeric)) for the columns you need.",
    "#",
    "# Common variables:",
    "#   B01001_001E = Total population          S1701_C03_001E = Poverty rate %",
    "#   B19013_001E = Median household income   DP05_0001E = Total pop (profile)",
    "#   B25077_001E = Median home value          P1_001N = Total pop (decennial)",
    "#",
    "# Geography helpers (return list(for_geo, in_geo) for census_get):",
    "#   census_geo_state()                  all states",
    "#   census_geo_county('06')             all CA counties",
    "#   census_geo_tract('06', '037')       all LA County tracts",
    "#   census_geo_zcta()                   all ZCTAs",
    "#",
    "# == Functions ==",
    "#"
  )

  for (fn in fns) {
    lines <- c(lines,
      sprintf("# %s%s", fn[[1]], fn[[2]]),
      sprintf("#   %s", fn[[3]]),
      sprintf("#   Run `%s` to view source or `?%s` for help.", fn[[1]], fn[[1]]),
      "#"
    )
  }

  lines <- c(lines,
    "# == Quick examples ==",
    "#",
    "# census_acs5(2022, c('NAME', 'B01001_001E'), 'state:*')",
    "# census_acs5(2022, c('NAME', 'B19013_001E'), census_geo_county('06'))",
    "# census_acs5_subject(2022, c('NAME', 'S1701_C03_001E'), 'state:*')",
    "# census_decennial(2020, c('NAME', 'P1_001N'), 'state:*', table = 'dhc')",
    "# census_search('acs/acs5', 2022, 'median income')",
    "# census_dataset('acs/acs5', 2022)  # explore metadata",
    "#"
  )

  out <- paste(lines, collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
