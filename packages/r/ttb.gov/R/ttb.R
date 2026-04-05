# ttb.gov.R
# Self-contained Alcohol and Tobacco Tax and Trade Bureau (TTB) client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2
# Auth: none required
# Docs: https://www.ttb.gov/


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
#' Returns a tibble describing the known TTB (Alcohol and Tobacco Tax and
#' Trade Bureau) machine-readable datasets, including permit registries,
#' formula processing times, and label approval statistics.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Short identifier for the dataset.}
#'     \item{name}{Character. Human-readable dataset name.}
#'     \item{format}{Character. Data format: "csv", "json", or "xml".}
#'     \item{description}{Character. Brief description of dataset contents.}
#'   }
#'
#' @examples
#' ttb_list()
#'
#' @export
ttb_list <- function() {
  .ttb_datasets
}

#' Search TTB datasets by keyword
#'
#' Filters the catalog of TTB machine-readable datasets by matching a
#' query string against dataset names and descriptions (case-insensitive).
#' Call with no arguments to return all datasets.
#'
#' @param query Character. Search term to match against dataset names and
#'   descriptions. If missing or empty, all datasets are returned.
#'
#' @return A tibble with the same columns as [ttb_list()]: id, name, format,
#'   description. Only rows matching the query are included.
#'
#' @examples
#' ttb_search("spirits")
#' ttb_search("permit")
#'
#' @seealso [ttb_list()]
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
#' Retrieves current processing times (in business days) for TTB formula
#' applications. Covers distilled spirits, malt beverages, wine, and
#' laboratory analysis submissions.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{application_type}{Character. Type of formula application
#'       (e.g. "Distilled Spirits", "Wine", "Malt Beverages", "Lab Analysis").}
#'     \item{processing_time}{Integer. Current processing time in business days.}
#'     \item{processing_new}{Character. Date through which applications have
#'       been processed.}
#'     \item{creation_date}{Character. Timestamp when the data was generated.}
#'   }
#'
#' @examples
#' ttb_formula_times()
#'
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
#' Retrieves current label application turnaround times by commodity type
#' from the TTB COLA statistics XML feed. COLA applications are required
#' for all alcohol labels sold in the United States.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{commodity}{Character. Commodity type code: "DS" (distilled
#'       spirits), "MB" (malt beverages), or "Wine".}
#'     \item{turnaround_time_median}{Integer. Median turnaround time in
#'       calendar days for label approval.}
#'     \item{date}{Date. The reporting date for these statistics.}
#'     \item{time}{Character. Time of day the report was generated.}
#'   }
#'
#' @examples
#' ttb_cola_times()
#'
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
#' Downloads the complete list of TTB Federal Alcohol Administration Act
#' permit holders (83,000+ records). This is a large download (~15MB JSON).
#' Results can be filtered client-side by state or industry type after
#' download. Automatically tries the current month's file first, then
#' falls back to the previous month.
#'
#' @param state Character. Optional two-letter state abbreviation to filter
#'   results (e.g. "CA", "NY"). Applied after download.
#' @param industry_type Character. Optional industry type substring to
#'   filter results (e.g. "Importer", "Wholesaler", "Distiller").
#'   Matched case-insensitively via \code{grepl}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{permit_number}{Character. TTB permit number.}
#'     \item{owner_name}{Character. Permit holder's legal name.}
#'     \item{operating_name}{Character. Business operating name (DBA).}
#'     \item{street}{Character. Street address of the permitted premises.}
#'     \item{city}{Character. City.}
#'     \item{state}{Character. Two-letter state code.}
#'     \item{zip}{Character. ZIP code.}
#'     \item{county}{Character. County name.}
#'     \item{industry_type}{Character. Type of permit (e.g. "Importer
#'       (Alcohol)", "Distiller and/or Processor (DSP)").}
#'     \item{new_permit_flag}{Integer. 1 if this is a newly issued permit,
#'       0 otherwise.}
#'   }
#'
#' @examples
#' \dontrun{
#' # All permits (large download)
#' all <- ttb_permits()
#'
#' # Filter to California importers
#' ca_importers <- ttb_permits(state = "CA", industry_type = "Importer")
#' }
#'
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
#' Retrieves the current list of Distilled Spirits Plant (DSP) permit
#' holders who are authorized to produce and/or bottle distilled spirits.
#' Falls back to the previous month's file if the current month is not
#' yet published.
#'
#' @return A tibble with permit columns including Permit_Number,
#'   Owner_Name, Operating_Name, Street, City, Prem_Zip, Prem_County,
#'   and New_Permit_Flag.
#'
#' @examples
#' \dontrun{
#' ttb_spirits_producers()
#' }
#'
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
#' Retrieves the current list of bonded wine cellar permit holders
#' authorized to produce and/or blend wine. Falls back to the previous
#' month's file if the current month is not yet published.
#'
#' @return A tibble with permit columns including Permit_Number,
#'   Owner_Name, Operating_Name, Street, City, Prem_Zip, Prem_County,
#'   and New_Permit_Flag.
#'
#' @examples
#' \dontrun{
#' ttb_wine_producers()
#' }
#'
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
#' Retrieves the current list of TTB-permitted alcohol importers
#' authorized to import distilled spirits, wine, or malt beverages
#' into the United States. Falls back to the previous month's file
#' if the current month is not yet published.
#'
#' @return A tibble with permit columns including Permit_Number,
#'   Owner_Name, Operating_Name, Street, City, Prem_Zip, Prem_County,
#'   and New_Permit_Flag.
#'
#' @examples
#' \dontrun{
#' ttb_importers()
#' }
#'
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
#' Retrieves the current list of TTB-permitted alcohol wholesalers
#' authorized to distribute distilled spirits, wine, or malt beverages
#' in the United States. Falls back to the previous month's file if
#' the current month is not yet published.
#'
#' @return A tibble with permit columns including Permit_Number,
#'   Owner_Name, Operating_Name, Street, City, Prem_Zip, Prem_County,
#'   and New_Permit_Flag.
#'
#' @examples
#' \dontrun{
#' ttb_wholesalers()
#' }
#'
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
#' Retrieves the list of basic permits issued since the last monthly
#' publication. Useful for tracking new entrants to the alcohol
#' industry. Falls back to the previous month's file if the current
#' month is not yet published.
#'
#' @return A tibble with permit columns including Permit_Number,
#'   Owner_Name, Operating_Name, Street, City, Prem_Zip, Prem_County,
#'   and New_Permit_Flag.
#'
#' @examples
#' \dontrun{
#' ttb_new_permits()
#' }
#'
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

#' Get ttb.gov client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
ttb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(ttb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ttb.gov.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ttb.gov")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ttb.gov context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ttb.gov", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
