# nycgovparks.org.R
# Self-contained NYC Parks client.
# Fetches facility directories, events, capital projects, and historical signs
# from NYC Department of Parks & Recreation JSON feeds.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required. All endpoints are public.

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.nycparks_base <- "https://www.nycgovparks.org"

.safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)

# -- JSON fetch helper ---------------------------------------------------------

.nycparks_fetch <- function(path) {
  url <- paste0(.nycparks_base, "/", path)
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_retry(max_tries = 2, backoff = function(...) 5) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = TRUE, flatten = TRUE)
}

# -- Type columns helper -------------------------------------------------------

.type_cols <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      vals <- df[[col]][!is.na(df[[col]])]
      # Dates
      if (grepl("date|Date|Start|Completion|Updated", col, ignore.case = TRUE) &&
          length(vals) > 0 && sum(grepl("^\\d{4}-\\d{2}-\\d{2}", vals)) > length(vals) * 0.3) {
        df[[col]] <- as.Date(substr(df[[col]], 1, 10))
        next
      }
      # Lat/Lon numerics
      if (col %in% c("lat", "lon", "latitude", "longitude") && length(vals) > 0 &&
          all(grepl("^-?[0-9]+\\.?[0-9]*$", vals))) {
        df[[col]] <- as.numeric(df[[col]])
        next
      }
    }
  }
  df
}


# == Directory Facilities ======================================================

# Known directory feeds: name -> JSON path
.nycparks_dirs <- list(
  pools_indoor    = "bigapps/DPR_Pools_indoor_001.json",
  pools_outdoor   = "bigapps/DPR_Pools_outdoor_001.json",
  dog_runs        = "bigapps/DPR_DogRuns_001.json",
  basketball      = "bigapps/DPR_Basketball_001.json",
  tennis          = "bigapps/DPR_Tennis_001.json",
  handball        = "bigapps/DPR_Handball_001.json",
  cricket         = "bigapps/DPR_Cricket_001.json",
  bocce           = "bigapps/DPR_Bocce_001.json",
  running_tracks  = "bigapps/DPR_RunningTracks_001.json",
  playgrounds     = "bigapps/DPR_Playgrounds_001.json",
  beaches         = "bigapps/DPR_Beaches_001.json",
  barbecue        = "bigapps/DPR_Barbecue_001.json",
  kayak           = "bigapps/DPR_Kayak_001.json",
  hiking          = "bigapps/DPR_Hiking_001.json",
  horseback       = "bigapps/DPR_Horseback_001.json",
  ice_skating     = "bigapps/DPR_IceSkating_001.json",
  rec_centers     = "bigapps/DPR_RecreationCenter_001.json",
  nature_centers  = "bigapps/DPR_NatureCenters_001.json",
  nature_preserves = "bigapps/DPR_NaturePreserves_001.json",
  zoos            = "bigapps/DPR_Zoos_001.json",
  historic_houses = "bigapps/DPR_HistoricHouses_001.json",
  eateries        = "bigapps/DPR_Eateries_001.json",
  concessions     = "bigapps/DPR_Concessions_001.json",
  computer_centers = "bigapps/DPR_PublicComputerResourceCenter_001.json"
)


#' List available NYC Parks facility directories
#'
#' Returns the names of all known directory feeds that can be
#' passed to nycparks_facilities().
#'
#' @return character vector of directory names
#' @export
nycparks_list <- function() {
  names(.nycparks_dirs)
}


#' Fetch a NYC Parks facility directory
#'
#' Retrieves a full directory of a specific facility type (pools, dog runs,
#' playgrounds, etc.) from the NYC Parks JSON feeds.
#'
#' @param directory Directory name (see nycparks_list() for options).
#'   Examples: "pools_indoor", "dog_runs", "playgrounds", "beaches"
#' @param borough Optional filter: "Bronx", "Brooklyn", "Manhattan",
#'   "Queens", "Staten_Island" (or partial match)
#' @return tibble with facility details (columns vary by directory type)
#' @export
nycparks_facilities <- function(directory, borough = NULL) {
  path <- .nycparks_dirs[[directory]]
  if (is.null(path)) {
    stop(sprintf("Unknown directory '%s'. Use nycparks_list() to see options.", directory))
  }
  raw <- .nycparks_fetch(path)
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  df <- .type_cols(df)

  if (!is.null(borough)) {
    # Try common borough column names
    for (bcol in c("borough", "Borough", "Location")) {
      if (bcol %in% names(df)) {
        df <- df |> filter(grepl(borough, .data[[bcol]], ignore.case = TRUE))
        break
      }
    }
  }
  df
}


#' Search NYC Parks facilities across all directories
#'
#' Searches facility names across all or selected directory types.
#'
#' @param query Text to search for in Name/name fields (case-insensitive)
#' @param directories Character vector of directory names to search
#'   (default: all). See nycparks_list().
#' @return tibble with matching facilities and a directory_type column
#' @export
nycparks_search <- function(query, directories = NULL) {
  if (is.null(directories)) directories <- names(.nycparks_dirs)
  all_results <- list()

  for (d in directories) {
    tryCatch({
      df <- nycparks_facilities(d)
      if (nrow(df) == 0) next

      # Find the name column
      name_col <- intersect(names(df), c("Name", "name", "title"))[1]
      if (is.na(name_col)) next

      matches <- df |> filter(grepl(query, .data[[name_col]], ignore.case = TRUE))
      if (nrow(matches) > 0) {
        matches$directory_type <- d
        all_results[[length(all_results) + 1]] <- matches
      }
    }, error = function(e) NULL)
  }

  if (length(all_results) == 0) return(tibble())
  bind_rows(all_results)
}


# == Events ====================================================================

#' Get upcoming NYC Parks events (next 14 days)
#'
#' Fetches public events from the NYC Parks events feed.
#'
#' @param category Optional category filter (e.g. "Fitness", "Dance",
#'   "Nature", "Volunteer"). Partial match.
#' @param borough Optional borough filter on park names/location
#' @return tibble: title, description, startdate, enddate, starttime, endtime,
#'   parknames, location, categories, link, registration_url, coordinates
#' @export
nycparks_events <- function(category = NULL, borough = NULL) {
  raw <- .nycparks_fetch("xml/events_300_rss.json")
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)

  # Type dates
  if ("startdate" %in% names(df)) df$startdate <- as.Date(df$startdate)
  if ("enddate" %in% names(df)) df$enddate <- as.Date(df$enddate)

  if (!is.null(category)) {
    df <- df |> filter(grepl(category, categories, ignore.case = TRUE))
  }
  if (!is.null(borough)) {
    df <- df |> filter(grepl(borough, parknames, ignore.case = TRUE) |
                       grepl(borough, location, ignore.case = TRUE))
  }
  df
}


# == Capital Projects ==========================================================

#' Fetch NYC Parks capital project tracker
#'
#' Returns all capital construction and renovation projects managed by
#' NYC Parks with timelines, phases, and funding info.
#'
#' @param phase Optional filter on current phase (e.g. "Construction",
#'   "Design", "Procurement", "Scope", "Completed")
#' @param borough Optional borough filter
#' @return tibble: TrackerID, Title, Summary, CurrentPhase,
#'   DesignPercentComplete, ConstructionPercentComplete, TotalFunding,
#'   dates, ProjectLiaison, Locations, Boroughs, ...
#' @export
nycparks_projects <- function(phase = NULL, borough = NULL) {
  raw <- .nycparks_fetch("bigapps/DPR_CapitalProjectTracker_001.json")
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  df <- .type_cols(df)

  if (!is.null(phase)) {
    df <- df |> filter(grepl(phase, CurrentPhase, ignore.case = TRUE))
  }
  if (!is.null(borough)) {
    df <- df |> filter(grepl(borough, Boroughs, ignore.case = TRUE))
  }
  df
}


# == Historical Signs ==========================================================

#' Fetch NYC Parks historical signs
#'
#' Returns the full text of historical signs installed in NYC Parks
#' properties, including park history and naming context.
#'
#' @param borough Optional filter: "Bronx", "Brooklyn", "Manhattan",
#'   "Queens", "Staten_Island"
#' @param query Optional text search in sign name or content
#' @return tibble: name, location, borough, content, propID
#' @export
nycparks_history <- function(borough = NULL, query = NULL) {
  raw <- .nycparks_fetch("bigapps/DPR_HistoricalSigns_001.json")
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)

  # Strip HTML tags from content for readability
  if ("content" %in% names(df)) {
    df$content <- gsub("<[^>]+>", "", df$content)
    df$content <- gsub("&[a-z]+;", " ", df$content)
    df$content <- gsub("\\s+", " ", trimws(df$content))
  }

  if (!is.null(borough)) {
    df <- df |> filter(grepl(borough, .data[["borough"]], ignore.case = TRUE))
  }
  if (!is.null(query)) {
    df <- df |> filter(grepl(query, name, ignore.case = TRUE) |
                       grepl(query, content, ignore.case = TRUE))
  }
  df
}


# == Press Releases ============================================================

#' Fetch NYC Parks press releases
#'
#' @return tibble with press release data
#' @export
nycparks_press <- function() {
  raw <- .nycparks_fetch("bigapps/DPR_PressReleases_001.json")
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  .type_cols(df)
}


# == Public Art ================================================================

#' Fetch NYC Parks temporary public art installations
#'
#' Directory of temporary art exhibitions and installations in NYC Parks
#' properties since 2000.
#'
#' @return tibble with art installation details
#' @export
nycparks_art <- function() {
  raw <- .nycparks_fetch("bigapps/DPR_PublicArt_001.json")
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  .type_cols(df)
}


# == Park Closures =============================================================

#' Fetch current NYC Parks closure notifications
#'
#' @return tibble with park/facility closure details
#' @export
nycparks_closures <- function() {
  raw <- .nycparks_fetch("bigapps/DPR_ParkClosure_001.json")
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  .type_cols(df)
}


# == Context ===================================================================

#' Generate LLM-friendly context for nycgovparks.org
#'
#' @return Character string with full function signatures
#' @export
nycparks_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/nycgovparks.org.R"
  if (!file.exists(src_file)) {
    cat("# nycgovparks.org context - source not found\n")
    return(invisible("# nycgovparks.org context - source not found"))
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
