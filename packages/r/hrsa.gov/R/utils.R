#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode read.csv
#' @keywords internal
NULL

.ua <- "support@scrapeable.com"
.hrsa_hpsa_url <- "https://data.hrsa.gov/DataDownload/DD_Files/BCD_HPSA_FCT_DET_PC.csv"
.hrsa_hc_url <- "https://data.hrsa.gov/DataDownload/DD_Files/Health_Center_Service_Delivery_and_LookAlike_Sites.csv"

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.hrsa_cache <- new.env(parent = emptyenv())

.hrsa_load_hpsa <- function(force = FALSE) {
  if (!force && exists("hpsa", envir = .hrsa_cache)) return(get("hpsa", envir = .hrsa_cache))
  tmp <- .fetch_csv(.hrsa_hpsa_url)
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  raw <- raw[, nchar(trimws(names(raw))) > 0, drop = FALSE]
  df <- tibble::as_tibble(raw, .name_repair = "unique")
  df <- df |> transmute(
    hpsa_name = as.character(`HPSA Name`), hpsa_id = as.character(`HPSA ID`),
    designation_type = as.character(`Designation Type`),
    discipline_class = as.character(`HPSA Discipline Class`),
    hpsa_score = suppressWarnings(as.integer(`HPSA Score`)),
    state = as.character(`Primary State Abbreviation`),
    status = as.character(`HPSA Status`),
    designation_date = suppressWarnings(as.Date(`HPSA Designation Date`, format = "%m/%d/%Y")),
    metro_indicator = as.character(`Metropolitan Indicator`),
    designation_pop = suppressWarnings(as.numeric(`HPSA Designation Population`)),
    pct_poverty = suppressWarnings(as.numeric(`% of Population Below 100% Poverty`)),
    formal_ratio = as.character(`HPSA Formal Ratio`),
    population_type = as.character(`HPSA Population Type`),
    rural_status = as.character(`Rural Status`),
    longitude = suppressWarnings(as.numeric(Longitude)),
    latitude = suppressWarnings(as.numeric(Latitude)),
    county = as.character(`Common County Name`),
    state_name = as.character(`Common State Name`),
    provider_type = as.character(`Provider Type`)
  )
  assign("hpsa", df, envir = .hrsa_cache)
  df
}

.hrsa_load_hc <- function(force = FALSE) {
  if (!force && exists("hc", envir = .hrsa_cache)) return(get("hc", envir = .hrsa_cache))
  tmp <- .fetch_csv(.hrsa_hc_url)
  raw <- utils::read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  raw <- raw[, nchar(trimws(names(raw))) > 0, drop = FALSE]
  df <- tibble::as_tibble(raw, .name_repair = "unique")
  df <- df |> transmute(
    site_name = as.character(`Site Name`),
    health_center_name = as.character(`Health Center Name`),
    site_address = as.character(`Site Address`),
    site_city = as.character(`Site City`),
    site_state = as.character(`Site State Abbreviation`),
    site_zip = as.character(`Site Postal Code`),
    site_phone = as.character(`Site Telephone Number`),
    site_web = as.character(`Site Web Address`),
    hours_per_week = suppressWarnings(as.numeric(`Operating Hours per Week`)),
    health_center_type = as.character(`Health Center Type`),
    site_status = as.character(`Site Status Description`),
    location_type = as.character(`Health Center Location Type Description`),
    operator_type = as.character(`Health Center Operator Description`),
    longitude = suppressWarnings(as.numeric(`Geocoding Artifact Address Primary X Coordinate`)),
    latitude = suppressWarnings(as.numeric(`Geocoding Artifact Address Primary Y Coordinate`)),
    county = as.character(`Complete County Name`),
    state_name = as.character(`State Name`),
    hhs_region = as.character(`HHS Region Name`)
  )
  assign("hc", df, envir = .hrsa_cache)
  df
}

`%||%` <- function(x, y) if (is.null(x)) y else x

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
