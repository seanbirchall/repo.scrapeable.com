# census.gov.R
# Self-contained US Census Bureau client.
# All public functions return tibbles. All columns return as character.
#
# Dependencies: httr2, jsonlite, dplyr, tidyr, tibble
# Auth: optional API key (query param). Get one at api.census.gov/data/key_signup.html


# census-gov.R
# Self-contained US Census Bureau client.
# All public functions return tibbles. All columns return as character —
# caller decides types (Census uses "-", "N", "(X)" for suppressed data).
#
# Dependencies: httr2, jsonlite, dplyr, tidyr, tibble
# Auth: optional API key (query param). Get one at api.census.gov/data/key_signup.html


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
#'
#' Constructs a geography specification list for state-level Census queries.
#' Pass the result directly to \code{census_get()} or any convenience wrapper.
#'
#' @param code Character. Two-digit FIPS code (e.g. \code{"06"} for California,
#'   \code{"36"} for New York) or \code{"*"} for all states (default).
#' @return A list with elements \code{for_geo} and \code{in_geo} (NULL),
#'   suitable for passing to \code{census_get(for_geo = ...)}.
#' @examples
#' census_geo_state()       # all states
#' census_geo_state("06")   # California only
census_geo_state <- function(code = "*") {
  list(for_geo = paste0("state:", code), in_geo = NULL)
}

#' Build a county-level geography spec
#'
#' Constructs a geography specification for county-level Census queries.
#'
#' @param state Character. Two-digit state FIPS code (required). Example:
#'   \code{"06"} for California, \code{"36"} for New York.
#' @param code Character. Three-digit county FIPS code or \code{"*"} for
#'   all counties in the state (default).
#' @return A list with elements \code{for_geo} and \code{in_geo}.
#' @examples
#' census_geo_county("06")          # all California counties
#' census_geo_county("06", "037")   # Los Angeles County
census_geo_county <- function(state, code = "*") {
  list(for_geo = paste0("county:", code),
       in_geo = paste0("state:", state))
}

#' Build a tract-level geography spec
#'
#' Constructs a geography specification for census tract queries.
#'
#' @param state Character. Two-digit state FIPS code (required).
#' @param county Character. Three-digit county FIPS code (required).
#' @param code Character. Six-digit tract code or \code{"*"} for all
#'   tracts in the county (default).
#' @return A list with elements \code{for_geo} and \code{in_geo}.
#' @examples
#' census_geo_tract("06", "037")   # all tracts in LA County
census_geo_tract <- function(state, county, code = "*") {
  list(for_geo = paste0("tract:", code),
       in_geo = paste("state:", state, " county:", county, sep = ""))
}

#' Build a place-level geography spec
#'
#' Constructs a geography specification for place (city/town) queries.
#'
#' @param state Character. Two-digit state FIPS code (required).
#' @param code Character. Place FIPS code or \code{"*"} for all places
#'   in the state (default).
#' @return A list with elements \code{for_geo} and \code{in_geo}.
#' @examples
#' census_geo_place("06")           # all California places
#' census_geo_place("06", "44000")  # City of Los Angeles
census_geo_place <- function(state, code = "*") {
  list(for_geo = paste0("place:", code),
       in_geo = paste0("state:", state))
}

#' Build a ZCTA (ZIP Code Tabulation Area) geography spec
#'
#' @param code Character. Five-digit ZCTA code or \code{"*"} for all (default).
#' @return A list with elements \code{for_geo} and \code{in_geo} (NULL).
#' @examples
#' census_geo_zcta()         # all ZCTAs
#' census_geo_zcta("90210")  # Beverly Hills
census_geo_zcta <- function(code = "*") {
  list(for_geo = paste0("zip code tabulation area:", code), in_geo = NULL)
}

#' Build a block group geography spec
#'
#' Constructs a geography specification for block group queries (the
#' smallest Census geography with published data).
#'
#' @param state Character. Two-digit state FIPS code (required).
#' @param county Character. Three-digit county FIPS code (required).
#' @param tract Character. Six-digit tract code (required).
#' @param code Character. Single-digit block group code or \code{"*"} for
#'   all block groups in the tract (default).
#' @return A list with elements \code{for_geo} and \code{in_geo}.
#' @examples
#' census_geo_blockgroup("06", "037", "101110")
census_geo_blockgroup <- function(state, county, tract, code = "*") {
  list(for_geo = paste0("block group:", code),
       in_geo = paste0("state:", state, " county:", county, " tract:", tract))
}


# == Discovery =================================================================

#' List all available Census Bureau datasets
#'
#' Fetches the full dataset catalog from api.census.gov. Cached per session
#' for performance. Results are sorted by year descending. Use the
#' \code{dataset} column value as the first argument to \code{census_get()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{title}{character -- Dataset title (e.g. "American Community Survey:
#'       5-Year Data")}
#'     \item{description}{character -- Dataset description}
#'     \item{dataset}{character -- API path to use with \code{census_get()}
#'       (e.g. "acs/acs5", "dec/pl")}
#'     \item{year}{integer -- Vintage year (e.g. 2022)}
#'   }
#' @examples
#' census_datasets()
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
#' Fetches the full variable catalog for a specific Census dataset and
#' vintage year. Returns thousands of rows for ACS datasets. Use
#' \code{census_search()} for keyword filtering instead.
#'
#' @param dataset Character. API dataset path (e.g. \code{"acs/acs5"},
#'   \code{"dec/pl"}, \code{"pep/population"}).
#' @param year Integer. Vintage year (e.g. 2022).
#' @param key Character or NULL. Optional Census API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- Variable name (e.g. "B01001_001E")}
#'     \item{label}{character -- Human-readable label}
#'     \item{concept}{character -- Topic concept (e.g. "SEX BY AGE")}
#'     \item{predicate_type}{character -- Data type: "int", "float", "string"}
#'     \item{group}{character -- Table group (e.g. "B01001")}
#'   }
#' @examples
#' census_variables("acs/acs5", 2022)
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

#' List variable groups (tables) for a dataset/year
#'
#' Returns table groups available in a Census dataset. Each group
#' corresponds to a related set of variables (e.g. B01001 = Sex by Age).
#' Use group names with \code{census_group_variables()} or pass
#' \code{"group(B01001)"} to \code{census_get()}.
#'
#' @param dataset Character. API dataset path (e.g. \code{"acs/acs5"}).
#' @param year Integer. Vintage year.
#' @param key Character or NULL. Optional API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- Group/table name (e.g. "B01001")}
#'     \item{description}{character -- Table description}
#'   }
#' @examples
#' census_groups("acs/acs5", 2022)
census_groups <- function(dataset, year, key = NULL) {
  url <- sprintf("%s/%d/%s/groups.json", .census_base, year, dataset)
  if (!is.null(key)) url <- paste0(url, "?key=", key)

  raw <- .fetch_json(url)
  g <- raw$groups
  if (is.null(g) || length(g) == 0) return(.schema_groups)
  as_tibble(g) |> select(any_of(c("name", "description")))
}

#' List variables within a specific table group
#'
#' Returns all variables belonging to a specific table (e.g. B01001
#' "Sex by Age"). Useful for discovering the individual estimate and
#' margin-of-error variables within a table.
#'
#' @param dataset Character. API dataset path (e.g. \code{"acs/acs5"}).
#' @param year Integer. Vintage year.
#' @param group Character. Group/table name (e.g. \code{"B01001"},
#'   \code{"B19013"}, \code{"B25001"}).
#' @param key Character or NULL. Optional API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- Variable name (e.g. "B01001_001E")}
#'     \item{label}{character -- Human-readable label}
#'     \item{concept}{character -- Topic concept}
#'     \item{predicate_type}{character -- Data type}
#'     \item{group}{character -- Group name (same as input)}
#'   }
#' @examples
#' census_group_variables("acs/acs5", 2022, "B01001")
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
#' Returns the geographic summary levels supported by a Census dataset.
#' Use this to determine valid \code{for_geo} values for
#' \code{census_get()} and whether parent geographies are required.
#'
#' @param dataset Character. API dataset path (e.g. \code{"acs/acs5"}).
#' @param year Integer. Vintage year.
#' @param key Character or NULL. Optional API key.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- Geography name (e.g. "state", "county",
#'       "tract", "place")}
#'     \item{geo_level}{character -- Summary level code}
#'     \item{requires}{character -- Required parent geographies
#'       (comma-separated, or NA)}
#'     \item{wildcard}{character -- Fields that accept wildcard "*"
#'       (comma-separated, or NA)}
#'   }
#' @examples
#' census_geography("acs/acs5", 2022)
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
#' Searches variable names, labels, and concepts using case-insensitive
#' regex matching. Uses the cached variable list for fast repeated queries.
#' This is the recommended way to find variable codes for \code{census_get()}.
#'
#' @param dataset Character. API dataset path (e.g. \code{"acs/acs5"}).
#' @param year Integer. Vintage year (e.g. 2022).
#' @param query Character. Case-insensitive regex search term. Examples:
#'   \code{"median income"}, \code{"population"}, \code{"housing units"},
#'   \code{"poverty"}, \code{"B01001"}.
#' @param key Character or NULL. Optional API key.
#' @return A tibble with columns: name, label, concept, predicate_type, group
#'   (same schema as \code{census_variables()}, filtered to matches).
#' @examples
#' census_search("acs/acs5", 2022, "median income")
#' census_search("acs/acs5", 2022, "total population")
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
#' Fetches variables, groups, and geography info for a Census dataset and
#' returns a summary object. Printing the result shows top concepts,
#' available groups, geographies, and usage examples. The full variable
#' catalog is cached for use by \code{census_search()}.
#'
#' @param dataset Character. API dataset path (e.g. \code{"acs/acs5"},
#'   \code{"dec/pl"}).
#' @param year Integer. Vintage year (e.g. 2022).
#' @param key Character or NULL. Optional API key.
#' @return An S3 object of class \code{"census_dataset"} (a tibble with
#'   columns dataset, year, n_vars, n_groups, n_geos, plus metadata
#'   attributes). Print it for a summary.
#' @examples
#' ds <- census_dataset("acs/acs5", 2022)
#' print(ds)
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
#' The universal data-fetching engine for the Census Bureau API. Builds
#' the URL, fetches the JSON response, and parses the array-of-arrays
#' format into a tibble. All columns return as character because the
#' Census API uses special values ("-", "N", "(X)") for suppressed data.
#' Use \code{mutate(across(..., as.numeric))} on columns you need as
#' numbers.
#'
#' Automatically chunks requests exceeding the Census API's 50-variable
#' limit and merges results by geography columns.
#'
#' @param dataset Character. API dataset path. Common values:
#'   \code{"acs/acs5"} (ACS 5-year), \code{"acs/acs1"} (ACS 1-year),
#'   \code{"dec/pl"} (decennial redistricting), \code{"dec/dhc"} (decennial
#'   demographic), \code{"pep/population"} (population estimates).
#' @param year Integer. Vintage year (e.g. 2022, 2020).
#' @param variables Character vector. Variable names to fetch. Examples:
#'   \code{c("B01001_001E", "B19013_001E")}, \code{c("NAME", "P1_001N")},
#'   or group syntax \code{"group(B01001)"}. Use \code{census_search()} to
#'   find variable names.
#' @param for_geo Character string or list. Geography specification.
#'   String examples: \code{"state:*"}, \code{"county:*"}, \code{"place:*"}.
#'   Or pass a list from \code{census_geo_state()}, \code{census_geo_county()},
#'   etc.
#' @param in_geo Character or NULL. Parent geography constraint as
#'   space-separated string (e.g. \code{"state:06"},
#'   \code{"state:06 county:037"}). Usually set automatically when using
#'   \code{census_geo_*()} helpers.
#' @param key Character or NULL. Optional Census API key. Get one at
#'   \url{https://api.census.gov/data/key_signup.html}.
#' @param label Logical. If \code{TRUE}, appends \code{_label} columns with
#'   human-readable variable descriptions from the variable catalog (default
#'   \code{FALSE}).
#' @return A tibble with one row per geography unit. All columns are character.
#'   Geography columns (state, county, tract, etc.) appear alongside the
#'   requested variable columns.
#' @examples
#' # Total population by state
#' census_get("acs/acs5", 2022, "B01001_001E", "state:*")
#'
#' # Median income for California counties
#' census_get("acs/acs5", 2022, "B19013_001E",
#'            census_geo_county("06"))
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
#'
#' Convenience wrapper for the American Community Survey 5-Year estimates
#' (dataset \code{"acs/acs5"}). Covers all geographies including tracts
#' and block groups. Available from 2009 onward.
#'
#' @param year Integer. Survey year (default 2022). Available 2009+.
#' @param variables Character vector. Variable names (e.g.
#'   \code{"B01001_001E"} for total population, \code{"B19013_001E"} for
#'   median household income). Find names with \code{census_search()}.
#' @param for_geo Character or list. Geography spec (see \code{census_get()}).
#' @param in_geo Character or NULL. Parent geography constraint.
#' @param key Character or NULL. Optional API key.
#' @param label Logical. If \code{TRUE}, add label columns (default FALSE).
#' @return A tibble with all columns as character.
#' @examples
#' census_acs5(2022, "B01001_001E", "state:*")
#' census_acs5(2022, "B19013_001E", census_geo_county("06"))
census_acs5 <- function(year = 2022, variables, for_geo,
                        in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs5", year, variables, for_geo, in_geo, key, label)
}

#' ACS 5-Year Subject Tables (S-prefixed variables)
#'
#' Wrapper for ACS 5-Year Subject Tables (dataset \code{"acs/acs5/subject"}).
#' Variables use S-prefixes (e.g. \code{"S1701_C01_001E"} for poverty).
#'
#' @inheritParams census_acs5
#' @return A tibble with all columns as character.
#' @examples
#' census_acs5_subject(2022, "S1701_C01_001E", "state:*")
census_acs5_subject <- function(year = 2022, variables, for_geo,
                                in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs5/subject", year, variables, for_geo, in_geo, key, label)
}

#' ACS 5-Year Data Profile Tables (DP-prefixed variables)
#'
#' Wrapper for ACS 5-Year Data Profile (dataset \code{"acs/acs5/profile"}).
#' Variables use DP-prefixes (e.g. \code{"DP05_0001E"} for total population).
#'
#' @inheritParams census_acs5
#' @return A tibble with all columns as character.
census_acs5_profile <- function(year = 2022, variables, for_geo,
                                in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs5/profile", year, variables, for_geo, in_geo, key, label)
}

#' ACS 5-Year Comparison Profile Tables (CP-prefixed variables)
#'
#' Wrapper for ACS 5-Year Comparison Profile (dataset
#' \code{"acs/acs5/cprofile"}). Variables use CP-prefixes.
#'
#' @inheritParams census_acs5
#' @return A tibble with all columns as character.
census_acs5_comparison <- function(year = 2022, variables, for_geo,
                                   in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs5/cprofile", year, variables, for_geo, in_geo, key, label)
}

# -- ACS 1-Year ---------------------------------------------------------------

#' ACS 1-Year Detailed Tables (areas with 65,000+ population)
#'
#' Wrapper for ACS 1-Year estimates (dataset \code{"acs/acs1"}). Only
#' covers areas with 65,000+ population. Uses the same variable names
#' as ACS 5-Year.
#'
#' @inheritParams census_acs5
#' @return A tibble with all columns as character.
census_acs1 <- function(year = 2022, variables, for_geo,
                        in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs1", year, variables, for_geo, in_geo, key, label)
}

#' ACS 1-Year Subject Tables
#'
#' Wrapper for ACS 1-Year Subject Tables (dataset \code{"acs/acs1/subject"}).
#'
#' @inheritParams census_acs5
#' @return A tibble with all columns as character.
census_acs1_subject <- function(year = 2022, variables, for_geo,
                                in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs1/subject", year, variables, for_geo, in_geo, key, label)
}

#' ACS 1-Year Data Profile Tables
#'
#' Wrapper for ACS 1-Year Data Profile (dataset \code{"acs/acs1/profile"}).
#'
#' @inheritParams census_acs5
#' @return A tibble with all columns as character.
census_acs1_profile <- function(year = 2022, variables, for_geo,
                                in_geo = NULL, key = NULL, label = FALSE) {
  census_get("acs/acs1/profile", year, variables, for_geo, in_geo, key, label)
}

# -- Decennial ----------------------------------------------------------------

#' Decennial Census
#'
#' Wrapper for Decennial Census data. Supports multiple summary files
#' across census years 2000, 2010, and 2020.
#'
#' @param year Integer. Census year: 2020, 2010, or 2000.
#' @param variables Character vector. Variable names (e.g. \code{"P1_001N"}
#'   for total population in 2020 PL file).
#' @param for_geo Character or list. Geography spec.
#' @param in_geo Character or NULL. Parent constraint.
#' @param table Character. Summary file to query. Options: \code{"pl"}
#'   (redistricting data, default), \code{"dhc"} (demographic and housing),
#'   \code{"dp"} (demographic profile), \code{"sf1"} (2010/2000 summary
#'   file 1), \code{"sf2"} (2010/2000 summary file 2).
#' @param key Character or NULL. Optional API key.
#' @param label Logical. If \code{TRUE}, add label columns.
#' @return A tibble with all columns as character.
#' @examples
#' census_decennial(2020, "P1_001N", "state:*")
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

#' Population Estimates Program (PEP)
#'
#' Wrapper for the Census Bureau Population Estimates Program (dataset
#' \code{"pep/population"}). Provides annual population estimates
#' between decennial censuses.
#'
#' @inheritParams census_acs5
#' @return A tibble with all columns as character.
census_pep <- function(year = 2022, variables, for_geo,
                       in_geo = NULL, key = NULL, label = FALSE) {
  census_get("pep/population", year, variables, for_geo, in_geo, key, label)
}

#' Economic Census
#'
#' Wrapper for the Economic Census (dataset \code{"ecnbasic"}).
#' Provides economic data by industry for establishments.
#'
#' @param year Integer. Economic Census year: 2017, 2012, or 2007.
#' @param variables Character vector. Variable names (e.g. \code{"NAICS2017"},
#'   \code{"EMP"} for employment, \code{"PAYANN"} for annual payroll).
#' @param for_geo Character or list. Geography spec.
#' @param in_geo Character or NULL. Parent constraint.
#' @param key Character or NULL. Optional API key.
#' @param label Logical. If \code{TRUE}, add label columns.
#' @return A tibble with all columns as character.
census_economic <- function(year = 2017, variables, for_geo,
                            in_geo = NULL, key = NULL, label = FALSE) {
  census_get("ecnbasic", year, variables, for_geo, in_geo, key, label)
}

# -- PUMS microdata -----------------------------------------------------------

#' ACS PUMS (Public Use Microdata Sample)
#'
#' Person-level or housing-unit-level microdata from the ACS. Variables
#' use different names than summary tables (e.g. AGEP for age, SEX,
#' PWGTP for person weight).
#'
#' @param year Integer. ACS year (e.g. 2022).
#' @param variables Character vector. PUMS variable names. Common variables:
#'   \code{"PWGTP"} (person weight), \code{"AGEP"} (age), \code{"SEX"},
#'   \code{"RAC1P"} (race), \code{"SCHL"} (educational attainment),
#'   \code{"PINCP"} (personal income), \code{"WGTP"} (housing weight).
#' @param for_geo Character. Geography (typically \code{"state:*"} or
#'   \code{"state:06"}).
#' @param in_geo Character or NULL. Parent constraint.
#' @param survey Character. \code{"acs5"} (default, 5-year) or
#'   \code{"acs1"} (1-year).
#' @param key Character or NULL. API key.
#' @return A tibble of microdata records (all columns character).
#' @examples
#' census_pums(2022, c("PWGTP", "AGEP", "SEX"), "state:06")
census_pums <- function(year = 2022, variables, for_geo = "state:*",
                        in_geo = NULL, survey = "acs5",
                        key = NULL) {
  dataset <- paste0("acs/", survey, "/pums")
  census_get(dataset, year, variables, for_geo, in_geo, key, label = FALSE)
}

# -- Migration flows ----------------------------------------------------------

#' ACS Migration/Flows data
#'
#' County-to-county or state-to-state migration flows from the ACS
#' (dataset \code{"acs/flows"}).
#'
#' @param year Integer. ACS year (e.g. 2022).
#' @param variables Character vector. Flow variables. Common values:
#'   \code{"MOVEDIN"} (number who moved in), \code{"MOVEDOUT"},
#'   \code{"FULL1_NAME"} (origin name), \code{"FULL2_NAME"} (destination).
#' @param for_geo Character. Geography (e.g. \code{"county:*"}).
#' @param in_geo Character or NULL. Parent constraint (e.g. \code{"state:06"}).
#' @param key Character or NULL. API key.
#' @return A tibble with all columns as character.
census_flows <- function(year = 2022, variables, for_geo,
                         in_geo = NULL, key = NULL) {
  census_get("acs/flows", year, variables, for_geo, in_geo, key, label = FALSE)
}

# == Standalone Context ========================================================

#' Get census.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
census_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(census_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/census.gov.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "census.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# census.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# census.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
