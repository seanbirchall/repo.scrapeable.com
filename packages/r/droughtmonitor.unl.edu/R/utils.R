#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

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

.build_context <- function(pkg_name, src_file = NULL, header_lines = character()) {
  if (is.null(src_file)) {
    src_dir <- system.file("source", package = pkg_name)
    if (src_dir == "") return(paste(c(header_lines, "# Source not found."), collapse = "\n"))
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) return(paste(c(header_lines, "# No R source."), collapse = "\n"))
    src_file <- src_files[1]
  }
  lines <- readLines(src_file, warn = FALSE)
  n <- length(lines)
  fn_indices <- grep("^([a-zA-Z][a-zA-Z0-9_.]*) <- function[(]", lines)
  blocks <- list()
  for (fi in fn_indices) {
    fn_name <- sub(" <- function[(].*", "", lines[fi])
    if (startsWith(fn_name, ".")) next
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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

