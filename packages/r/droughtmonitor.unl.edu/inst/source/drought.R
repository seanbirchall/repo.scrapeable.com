


# droughtmonitor-unl-edu.R
# Self-contained US Drought Monitor client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none known
# Note: API returns CSV format. Uses FIPS codes for states.
#   statisticsType=2 returns percent of area.


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.drought_base <- "https://usdmdataservices.unl.edu/api"

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url, ext = ".json"))

`%||%` <- function(a, b) if (is.null(a)) b else a

# State FIPS lookup
.state_fips <- c(
  AL="01", AK="02", AZ="04", AR="05", CA="06", CO="08", CT="09", DE="10",
  FL="12", GA="13", HI="15", ID="16", IL="17", IN="18", IA="19", KS="20",
  KY="21", LA="22", ME="23", MD="24", MA="25", MI="26", MN="27", MS="28",
  MO="29", MT="30", NE="31", NV="32", NH="33", NJ="34", NM="35", NY="36",
  NC="37", ND="38", OH="39", OK="40", OR="41", PA="42", RI="44", SC="45",
  SD="46", TN="47", TX="48", UT="49", VT="50", VA="51", WA="53", WV="54",
  WI="55", WY="56"
)

.to_fips <- function(state) {
  state <- toupper(state)
  if (nchar(state) == 2 && state %in% names(.state_fips)) return(.state_fips[state])
  if (grepl("^\\d{2}$", state)) return(state)
  state
}

# == Schemas ===================================================================

.schema_drought <- tibble(
  date = as.Date(character()), state = character(), none = numeric(),
  d0 = numeric(), d1 = numeric(), d2 = numeric(),
  d3 = numeric(), d4 = numeric()
)

.schema_drought_county <- tibble(
  date = as.Date(character()), county = character(), state = character(),
  none = numeric(), d0 = numeric(), d1 = numeric(),
  d2 = numeric(), d3 = numeric(), d4 = numeric()
)

# == Private helpers ===========================================================

.parse_drought_csv <- function(tmp, area_col = "StateAbbreviation") {
  raw_lines <- readLines(tmp, warn = FALSE)
  if (length(raw_lines) < 2) return(NULL)
  con <- textConnection(raw_lines)
  df <- tryCatch(read.csv(con, stringsAsFactors = FALSE), error = function(e) NULL)
  close(con)
  df
}

# == Public functions ==========================================================


#' Get state-level drought severity statistics (percent area)
#'
#' Returns weekly drought severity percentages for a given state from the
#' US Drought Monitor. D0=Abnormally Dry through D4=Exceptional Drought.
#'
#' @param state Two-letter state abbreviation (e.g., "CA", "TX") or FIPS code
#' @param start Start date as "M/D/YYYY" (e.g., "1/1/2024")
#' @param end End date as "M/D/YYYY" (e.g., "12/31/2024")
#' @return tibble: date, state, none, d0, d1, d2, d3, d4
#' @export
drought_state <- function(state, start = "1/1/2024", end = "12/31/2024") {
  fips <- .to_fips(state)
  url <- sprintf(
    "%s/StateStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=%s&startdate=%s&enddate=%s&statisticsType=2",
    .drought_base, fips, start, end
  )
  tmp <- .fetch(url, ext = ".csv")
  df <- .parse_drought_csv(tmp)
  if (is.null(df) || nrow(df) == 0) return(.schema_drought)

  as_tibble(df) |>
    transmute(
      date = as.Date(as.character(MapDate), format = "%Y%m%d"),
      state = as.character(StateAbbreviation),
      none = as.numeric(None),
      d0 = as.numeric(D0),
      d1 = as.numeric(D1),
      d2 = as.numeric(D2),
      d3 = as.numeric(D3),
      d4 = as.numeric(D4)
    )
}

#' Get county-level drought severity statistics (percent area)
#'
#' Returns weekly drought severity percentages for counties in a given state.
#'
#' @param state Two-letter state abbreviation (e.g., "CA", "TX") or FIPS code
#' @param start Start date as "M/D/YYYY" (e.g., "1/1/2024")
#' @param end End date as "M/D/YYYY" (e.g., "12/31/2024")
#' @return tibble: date, county, state, none, d0, d1, d2, d3, d4
#' @export
drought_county <- function(state, start = "1/1/2024", end = "12/31/2024") {
  fips <- .to_fips(state)
  url <- sprintf(
    "%s/CountyStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=%s&startdate=%s&enddate=%s&statisticsType=2",
    .drought_base, fips, start, end
  )
  tmp <- .fetch(url, ext = ".csv")
  df <- .parse_drought_csv(tmp)
  if (is.null(df) || nrow(df) == 0) return(.schema_drought_county)

  nms <- names(df)
  county_col <- if ("County" %in% nms) "County" else if ("CountyName" %in% nms) "CountyName" else if ("FIPS" %in% nms) "FIPS" else nms[2]

  as_tibble(df) |>
    transmute(
      date = as.Date(as.character(MapDate), format = "%Y%m%d"),
      county = as.character(.data[[county_col]]),
      state = as.character(if ("StateAbbreviation" %in% nms) StateAbbreviation else state),
      none = as.numeric(None),
      d0 = as.numeric(D0),
      d1 = as.numeric(D1),
      d2 = as.numeric(D2),
      d3 = as.numeric(D3),
      d4 = as.numeric(D4)
    )
}

# == Context ===================================================================

#' Generate LLM-friendly context for droughtmonitor.unl.edu
#'
#' @return Character string with full function signatures and bodies
#' @export
drought_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/droughtmonitor.unl.edu.R"
  if (!file.exists(src_file)) {
    cat("# droughtmonitor.unl.edu context - source not found\n")
    return(invisible("# droughtmonitor.unl.edu context - source not found"))
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

