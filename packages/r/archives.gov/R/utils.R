#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_options req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.nara_catalog_base <- "https://catalog.archives.gov/api/v2"
.nara_open_base <- "https://www.archives.gov"

# -- Core fetch helpers --------------------------------------------------------

.nara_fetch_json <- function(url, api_key = NULL) {
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, `Content-Type` = "application/json")
  key <- api_key %||% Sys.getenv("NARA_API_KEY", "")
  if (nzchar(key)) req <- req |> httr2::req_headers(`x-api-key` = key)
  tmp <- tempfile(fileext = ".json")
  httr2::req_perform(req, path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

.nara_fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_redirect(max = 5) |>
    httr2::req_perform(path = tmp)
  utils::read.csv(tmp, stringsAsFactors = FALSE) |> as_tibble()
}

# == Schemas ===================================================================

.schema_catalog <- tibble(
  naId = character(), title = character(), description = character(),
  type = character(), level = character(), date = character(),
  creators = character(), url = character()
)

.schema_amendments <- tibble(
  identifier = character(), source_code = character(),
  source_citation = character(), source_index_number = integer(),
  title_or_description_from_source = character(),
  date_approximation = character(), year = integer(),
  month = character(), day = character(),
  congress = character(), congressional_session = character(),
  joint_resolution_chamber = character(), joint_resolution_number = character(),
  sponsor_name = character(), sponsor_state_or_territory = character(),
  committee_of_referral = character(), last_modified = character()
)

.schema_grants <- tibble(
  grant_number = character(), grant_type = character(),
  first_name = character(), last_name = character(),
  institution = character(), city = character(), state = character(),
  year_awarded = integer(), project_title = character(),
  program = character(), division = character(),
  grant_amount_approved = character(), final_grant_award = character(),
  grant_start_date = character(), grant_end_date = character(),
  project_description = character()
)

.schema_tapes <- tibble(
  conversationTitle = character(), tapeNumber = integer(),
  conversationNumber = integer(), identifier = character(),
  startDateTime = character(), endDateTime = character(),
  participants = character(), description = character(),
  locationCode = character(), recordingDevice = character(),
  collection = character()
)

# -- Context generator ---------------------------------------------------------

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
