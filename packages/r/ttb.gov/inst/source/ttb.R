# ttb.gov.R
# Self-contained Alcohol and Tobacco Tax and Trade Bureau (TTB) client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2
# Auth: none required
# Docs: https://www.ttb.gov/

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.ttb_base <- "https://www.ttb.gov"

# -- Fetch CSV from TTB -------------------------------------------------------
.ttb_fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning("TTB returned HTTP ", status, " for ", url)
    return(tibble())
  }
  as_tibble(read.csv(tmp, stringsAsFactors = FALSE, check.names = TRUE))
}

# -- Fetch JSON from TTB ------------------------------------------------------
.ttb_fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning("TTB returned HTTP ", status, " for ", url)
    return(list())
  }
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# -- Fetch XML from TTB -------------------------------------------------------
.ttb_fetch_xml <- function(url) {
  tmp <- tempfile(fileext = ".xml")
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform(path = tmp)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    warning("TTB returned HTTP ", status, " for ", url)
    return(NULL)
  }
  xml2::read_xml(tmp)
}

# == Dataset catalog ===========================================================

.ttb_datasets <- tibble(
  id = c("formula_stats", "cola_stats", "all_permits",
         "spirits_producers", "wine_producers", "importers",
         "wholesalers", "new_permits", "puerto_rico_permits"),
  name = c(
    "Formula Processing Times",
    "Label (COLA) Processing Times",
    "All Permits (JSON)",
    "Spirits Producers & Bottlers",
    "Wine Producers & Blenders",
    "Alcohol Importers",
    "Alcohol Wholesalers",
    "Newly Issued Basic Permits",
    "Puerto Rico Basic Permits"
  ),
  format = c("csv", "xml", "json", "csv", "csv", "csv", "csv", "csv", "csv"),
  description = c(
    "Current processing times for beverage alcohol formula applications",
    "Current processing times for COLA label applications by commodity",
    "Complete list of all TTB alcohol permits (83K+ records)",
    "DSP permit holders: distilled spirits producers and bottlers",
    "Wine producers and blender permit holders",
    "Alcohol importer permit holders",
    "Alcohol wholesaler permit holders",
    "Basic permits issued since last publication",
    "Puerto Rico basic permit holders"
  )
)

# == Discovery functions =======================================================

#' List available TTB datasets
#'
#' Returns a tibble describing the known TTB machine-readable datasets.
#'
#' @return tibble: id, name, format, description
#' @export
ttb_list <- function() {
  .ttb_datasets
}

#' Search TTB datasets by keyword
#'
#' @param query Character string to search in dataset names and descriptions
#' @return tibble: matching datasets
#' @export
ttb_search <- function(query) {
  if (missing(query) || is.null(query) || !nzchar(query)) return(.ttb_datasets)
  pattern <- tolower(query)
  .ttb_datasets |>
    filter(
      grepl(pattern, tolower(name)) | grepl(pattern, tolower(description))
    )
}

# == Formula Processing Times ==================================================

#' Fetch beverage alcohol formula processing times
#'
#' Returns current processing times for formula applications (distilled spirits,
#' malt beverages, wine, and lab analysis).
#'
#' @return tibble: application_type, processing_time (days),
#'   processing_new (date), creation_date
#' @export
ttb_formula_times <- function() {
  url <- paste0(.ttb_base, "/images/foia/form_stats.csv")
  df <- .ttb_fetch_csv(url)
  if (nrow(df) == 0) return(df)
  df |>
    mutate(
      processing_time = as.integer(processing_time)
    )
}

# == COLA Label Processing Times ===============================================

#' Fetch COLA (Certificate of Label Approval) processing times
#'
#' Returns current label application processing times by commodity type
#' (DS = distilled spirits, MB = malt beverages, Wine).
#'
#' @return tibble: commodity, turnaround_time_median (days), date
#' @export
ttb_cola_times <- function() {
  url <- paste0(.ttb_base, "/images/foia/cola_stats.xml")
  doc <- .ttb_fetch_xml(url)
  if (is.null(doc)) return(tibble())

  nodes <- xml2::xml_find_all(doc, "//FOR_XML2")
  if (length(nodes) == 0) return(tibble())

  bind_rows(lapply(nodes, function(node) {
    commodity <- trimws(xml2::xml_text(xml2::xml_find_first(node, "commodity")))
    median_time <- as.integer(trimws(xml2::xml_text(xml2::xml_find_first(node, "Turnaround_Time_Median"))))
    month <- trimws(xml2::xml_text(xml2::xml_find_first(node, "month")))
    day <- trimws(xml2::xml_text(xml2::xml_find_first(node, "day")))
    year <- trimws(xml2::xml_text(xml2::xml_find_first(node, "year")))
    now_time <- trimws(xml2::xml_text(xml2::xml_find_first(node, "now_time")))

    tibble(
      commodity = commodity,
      turnaround_time_median = median_time,
      date = as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d"),
      time = now_time
    )
  }))
}

# == Permit Data (All) =========================================================

#' Fetch all TTB permits
#'
#' Returns the complete list of TTB alcohol permit holders (83,000+ records).
#' This is a large download (~15MB JSON). Use state or industry_type params
#' to filter results after download.
#'
#' @param state Optional state abbreviation to filter (e.g. "CA", "NY")
#' @param industry_type Optional industry type filter (e.g. "Importer (Alcohol)")
#' @return tibble: permit_number, owner_name, operating_name, street, city,
#'   state, zip, county, industry_type, new_permit_flag
#' @export
ttb_permits <- function(state = NULL, industry_type = NULL) {
  # Try current month first, fall back to previous month
  current <- format(Sys.Date(), "%Y-%m")
  url <- paste0(.ttb_base, "/system/files/", current, "/FRL_All_Permits.json")
  raw <- .ttb_fetch_json(url)

  if (length(raw) == 0 || is.null(raw[["Permit Data"]])) {
    prev <- format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m")
    url <- paste0(.ttb_base, "/system/files/", prev, "/FRL_All_Permits.json")
    raw <- .ttb_fetch_json(url)
  }

  if (length(raw) == 0 || is.null(raw[["Permit Data"]])) {
    warning("Could not fetch TTB permits JSON")
    return(tibble())
  }

  permit_data <- raw[["Permit Data"]]
  cols <- c("permit_number", "owner_name", "operating_name", "street",
            "city", "state", "zip", "county", "industry_type", "new_permit_flag")

  df <- bind_rows(lapply(permit_data, function(row) {
    vals <- lapply(seq_along(cols), function(i) {
      v <- row[[i]]
      if (is.null(v)) NA_character_ else as.character(v)
    })
    names(vals) <- cols
    as_tibble(vals)
  }))

  if (!is.null(state)) {
    df <- df |> filter(toupper(.data$state) == toupper(!!state))
  }
  if (!is.null(industry_type)) {
    df <- df |> filter(grepl(!!industry_type, .data$industry_type, ignore.case = TRUE))
  }
  df |>
    mutate(new_permit_flag = as.integer(new_permit_flag))
}

# == Permit CSVs (by type) ====================================================

.ttb_permit_csv_url <- function(filename) {
  current <- format(Sys.Date(), "%Y-%m")
  paste0(.ttb_base, "/system/files/", current, "/", filename)
}

#' Fetch spirits producers and bottlers permits
#'
#' @return tibble: Permit_Number, Owner_Name, Operating_Name, Street, City,
#'   Prem_Zip, Prem_County, New_Permit_Flag
#' @export
ttb_spirits_producers <- function() {
  url <- .ttb_permit_csv_url("FRL_Spirits_Producers_and_Bottlers_List.csv")
  df <- .ttb_fetch_csv(url)
  if (nrow(df) == 0) {
    prev <- format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m")
    url2 <- paste0(.ttb_base, "/system/files/", prev, "/FRL_Spirits_Producers_and_Bottlers_List.csv")
    df <- .ttb_fetch_csv(url2)
  }
  df
}

#' Fetch wine producers and blenders permits
#'
#' @return tibble with permit columns
#' @export
ttb_wine_producers <- function() {
  url <- .ttb_permit_csv_url("FRL_Wine_Producer_and_Blender_Permit_List.csv")
  df <- .ttb_fetch_csv(url)
  if (nrow(df) == 0) {
    prev <- format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m")
    url2 <- paste0(.ttb_base, "/system/files/", prev, "/FRL_Wine_Producer_and_Blender_Permit_List.csv")
    df <- .ttb_fetch_csv(url2)
  }
  df
}

#' Fetch alcohol importer permits
#'
#' @return tibble with permit columns
#' @export
ttb_importers <- function() {
  url <- .ttb_permit_csv_url("FRL_Alcohol_Importer_Permit_List.csv")
  df <- .ttb_fetch_csv(url)
  if (nrow(df) == 0) {
    prev <- format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m")
    url2 <- paste0(.ttb_base, "/system/files/", prev, "/FRL_Alcohol_Importer_Permit_List.csv")
    df <- .ttb_fetch_csv(url2)
  }
  df
}

#' Fetch alcohol wholesaler permits
#'
#' @return tibble with permit columns
#' @export
ttb_wholesalers <- function() {
  url <- .ttb_permit_csv_url("FRL_Alcohol_Wholesaler_Permit_List.csv")
  df <- .ttb_fetch_csv(url)
  if (nrow(df) == 0) {
    prev <- format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m")
    url2 <- paste0(.ttb_base, "/system/files/", prev, "/FRL_Alcohol_Wholesaler_Permit_List.csv")
    df <- .ttb_fetch_csv(url2)
  }
  df
}

#' Fetch newly issued basic permits
#'
#' @return tibble with permit columns
#' @export
ttb_new_permits <- function() {
  url <- .ttb_permit_csv_url("FRL_Basic_Permits_Issued_Since_the_Last_Publication.csv")
  df <- .ttb_fetch_csv(url)
  if (nrow(df) == 0) {
    prev <- format(seq(Sys.Date(), by = "-1 month", length.out = 2)[2], "%Y-%m")
    url2 <- paste0(.ttb_base, "/system/files/", prev, "/FRL_Basic_Permits_Issued_Since_the_Last_Publication.csv")
    df <- .ttb_fetch_csv(url2)
  }
  df
}

# == Context ===================================================================

#' Return full source of all public functions
#'
#' @return character (invisibly); also printed to stdout
#' @export
ttb_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/ttb.gov.R"
  if (!file.exists(src_file)) {
    cat("# ttb.gov context - source not found\n")
    return(invisible("# ttb.gov context - source not found"))
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
    blocks[[length(blocks) + 1]] <- paste(c(rox, body), collapse = "\n")
  }
  txt <- paste(blocks, collapse = "\n\n")
  cat(txt, "\n")
  invisible(txt)
}
