# nycgovparks.org.R
# Self-contained NYC Parks client.
# Fetches facility directories, events, capital projects, and historical signs
# from NYC Department of Parks & Recreation JSON feeds.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: None required. All endpoints are public.


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
#' Returns the names of all 24 known directory feeds that can be
#' passed to \code{nycparks_facilities()}. Directories include pools,
#' dog runs, playgrounds, beaches, tennis, basketball, and more.
#'
#' @return A character vector of 24 directory names:
#'   \code{"pools_indoor"}, \code{"pools_outdoor"}, \code{"dog_runs"},
#'   \code{"basketball"}, \code{"tennis"}, \code{"handball"},
#'   \code{"cricket"}, \code{"bocce"}, \code{"running_tracks"},
#'   \code{"playgrounds"}, \code{"beaches"}, \code{"barbecue"},
#'   \code{"kayak"}, \code{"hiking"}, \code{"horseback"},
#'   \code{"ice_skating"}, \code{"rec_centers"}, \code{"nature_centers"},
#'   \code{"nature_preserves"}, \code{"zoos"}, \code{"historic_houses"},
#'   \code{"eateries"}, \code{"concessions"}, \code{"computer_centers"}.
#' @examples
#' nycparks_list()
#' @export
nycparks_list <- function() {
  names(.nycparks_dirs)
}


#' Fetch a NYC Parks facility directory
#'
#' Retrieves a full directory of a specific facility type from the
#' NYC Parks JSON feeds. Each directory returns a different set of
#' columns specific to that facility type.
#'
#' @param directory Character. Directory name -- must be one of the
#'   values returned by \code{nycparks_list()}. Common choices:
#'   \code{"pools_indoor"}, \code{"pools_outdoor"}, \code{"dog_runs"},
#'   \code{"playgrounds"}, \code{"beaches"}, \code{"tennis"},
#'   \code{"basketball"}, \code{"rec_centers"}.
#' @param borough Character or NULL. Borough filter (partial match,
#'   case-insensitive): \code{"Bronx"}, \code{"Brooklyn"},
#'   \code{"Manhattan"}, \code{"Queens"}, \code{"Staten Island"}.
#' @return A tibble with columns varying by directory type. Common
#'   columns include: Prop_ID, Name, Location, Phone, lat (numeric),
#'   lon (numeric), Accessible. Pool directories add Size and Type;
#'   playgrounds add ADA_Accessible_Flag, etc.
#' @examples
#' nycparks_facilities("pools_indoor")
#' nycparks_facilities("dog_runs", borough = "Manhattan")
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
#' Useful for finding a specific park, pool, or facility by name.
#'
#' @param query Character. Text to search for in Name fields
#'   (case-insensitive, e.g. \code{"Central Park"}, \code{"Flushing"}).
#' @param directories Character vector or NULL. Directory names to
#'   search (default: all 24). See \code{nycparks_list()}.
#' @return A tibble with matching facilities. Includes all columns
#'   from the matching directories plus a \code{directory_type} column
#'   indicating which directory each row came from.
#' @examples
#' nycparks_search("Central Park")
#' nycparks_search("Prospect", directories = c("pools_outdoor", "playgrounds"))
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

#' Get upcoming NYC Parks events
#'
#' Fetches the next ~300 public events from the NYC Parks events
#' feed. Covers fitness classes, nature walks, volunteer events,
#' concerts, and more across all five boroughs.
#'
#' @param category Character or NULL. Category filter (partial match,
#'   case-insensitive). Examples: \code{"Fitness"}, \code{"Dance"},
#'   \code{"Nature"}, \code{"Volunteer"}, \code{"Sports"},
#'   \code{"History"}.
#' @param borough Character or NULL. Borough filter applied to park
#'   names and location fields (e.g. \code{"Brooklyn"},
#'   \code{"Manhattan"}).
#' @return A tibble with columns including:
#'   \describe{
#'     \item{title}{character -- event title}
#'     \item{description}{character -- event description}
#'     \item{startdate}{Date -- event start date}
#'     \item{enddate}{Date -- event end date}
#'     \item{starttime}{character -- start time}
#'     \item{endtime}{character -- end time}
#'     \item{parknames}{character -- park(s) where event takes place}
#'     \item{location}{character -- specific location within park}
#'     \item{categories}{character -- event categories}
#'     \item{link}{character -- event detail URL}
#'     \item{registration_url}{character -- registration link}
#'     \item{coordinates}{character -- lat/lon coordinates}
#'   }
#' @examples
#' nycparks_events()
#' nycparks_events(category = "Fitness", borough = "Manhattan")
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
#' Returns all ~2,200 capital construction and renovation projects
#' managed by NYC Parks with timelines, phases, and funding info.
#'
#' @param phase Character or NULL. Current project phase filter
#'   (partial match, case-insensitive). Valid values:
#'   \code{"Construction"}, \code{"Design"}, \code{"Procurement"},
#'   \code{"Scope"}, \code{"Completed"}.
#' @param borough Character or NULL. Borough filter (partial match).
#'   Examples: \code{"Brooklyn"}, \code{"Queens"}.
#' @return A tibble with columns including:
#'   \describe{
#'     \item{TrackerID}{character -- unique project tracker ID}
#'     \item{FMSID}{character -- financial management system ID}
#'     \item{Title}{character -- project title}
#'     \item{Summary}{character -- project summary}
#'     \item{CurrentPhase}{character -- current phase name}
#'     \item{DesignPercentComplete}{character -- design completion pct}
#'     \item{ConstructionPercentComplete}{character -- construction pct}
#'     \item{Boroughs}{character -- borough(s)}
#'   }
#' @examples
#' nycparks_projects()
#' nycparks_projects(phase = "Construction", borough = "Manhattan")
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
#' Returns the full text of ~2,280 historical signs installed in NYC
#' Parks properties, including park history and naming context. HTML
#' tags are stripped from content.
#'
#' @param borough Character or NULL. Borough filter (partial match):
#'   \code{"Bronx"}, \code{"Brooklyn"}, \code{"Manhattan"},
#'   \code{"Queens"}, \code{"Staten Island"}.
#' @param query Character or NULL. Text search in sign name or
#'   content (case-insensitive).
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{character -- sign/park name}
#'     \item{location}{character -- location within the park}
#'     \item{borough}{character -- borough name}
#'     \item{content}{character -- full sign text (HTML stripped)}
#'     \item{propID}{character -- park property ID}
#'   }
#' @examples
#' nycparks_history()
#' nycparks_history(borough = "Manhattan", query = "Central Park")
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
#' Returns ~1,800 press releases from the NYC Department of Parks
#' and Recreation. Dates are auto-typed.
#'
#' @return A tibble with press release data including title, date,
#'   content, and URL columns (exact columns vary by feed version).
#' @examples
#' nycparks_press()
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
#' Directory of ~1,277 temporary art exhibitions and installations
#' in NYC Parks properties since 2000. Includes sculpture, murals,
#' performances, and multimedia works.
#'
#' @return A tibble with art installation details including title,
#'   artist, park, borough, and date columns (exact columns vary
#'   by feed version).
#' @examples
#' nycparks_art()
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
#' Returns ~5,500 current park and facility closure notifications.
#' Covers weather closures, construction, maintenance, and
#' special-event closures.
#'
#' @return A tibble with closure details including park name,
#'   reason, start/end dates, and location (exact columns vary
#'   by feed version).
#' @examples
#' nycparks_closures()
#' @export
nycparks_closures <- function() {
  raw <- .nycparks_fetch("bigapps/DPR_ParkClosure_001.json")
  if (length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  .type_cols(df)
}


# == Context ===================================================================

#' Get nycgovparks.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nycparks_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nycparks_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nycgovparks.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nycgovparks.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nycgovparks.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nycgovparks.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
