#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# energy.gov.R
# Self-contained Department of Energy data client.
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: API key for AFDC (DEMO_KEY for testing)
# Docs: https://developer.nrel.gov/docs/transportation/alt-fuel-stations-v1/

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.afdc_base <- "https://developer.nrel.gov/api/alt-fuel-stations/v1"

.doe_get_json <- function(url, params = list(), api_key = NULL) {
  key <- api_key %||% Sys.getenv("NREL_API_KEY", "DEMO_KEY")
  params$api_key <- key

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 2, max_seconds = 15) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

.doe_parse_stations <- function(stations) {
  if (length(stations) == 0) return(tibble::tibble())
  bind_rows(lapply(stations, function(s) {
    tibble::tibble(
      id                 = as.integer(s$id %||% NA),
      station_name       = s$station_name %||% NA_character_,
      fuel_type_code     = s$fuel_type_code %||% NA_character_,
      status_code        = s$status_code %||% NA_character_,
      street_address     = s$street_address %||% NA_character_,
      city               = s$city %||% NA_character_,
      state              = s$state %||% NA_character_,
      zip                = s$zip %||% NA_character_,
      latitude           = as.numeric(s$latitude %||% NA),
      longitude          = as.numeric(s$longitude %||% NA),
      access_code        = s$access_code %||% NA_character_,
      ev_network         = s$ev_network %||% NA_character_,
      ev_level1_evse_num = as.integer(s$ev_level1_evse_num %||% NA),
      ev_level2_evse_num = as.integer(s$ev_level2_evse_num %||% NA),
      ev_dc_fast_num     = as.integer(s$ev_dc_fast_num %||% NA),
      station_phone      = s$station_phone %||% NA_character_,
      open_date          = as.Date(s$open_date %||% NA_character_),
      date_last_confirmed = as.Date(s$date_last_confirmed %||% NA_character_),
      owner_type_code    = s$owner_type_code %||% NA_character_,
      facility_type      = s$facility_type %||% NA_character_
    )
  }))
}

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
