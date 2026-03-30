# == Core data fetching ========================================================

#' Fetch any BIS SDMX dataset
#'
#' Universal engine using the BIS SDMX v1 API.
#'
#' @param dataflow BIS dataflow ID (e.g. "WS_EER", "WS_CBPOL", "WS_SPP").
#'   Use bis_dataflows() to see all available.
#' @param key SDMX key (e.g. "M.N.B.US" for monthly US data).
#'   Use "." to wildcard a dimension.
#' @param start Start period (YYYY-MM-DD or YYYY-MM or YYYY)
#' @param end End period
#' @param last_n Last N observations only
#' @return tibble: full SDMX CSV columns including TIME_PERIOD, OBS_VALUE, TITLE_TS
#' @export
bis_get <- function(dataflow, key, start = NULL, end = NULL, last_n = NULL) {
  params <- list()
  if (!is.null(start))  params$startPeriod <- as.character(start)
  if (!is.null(end))    params$endPeriod <- as.character(end)
  if (!is.null(last_n)) params$lastNObservations <- last_n
  .bis_fetch(dataflow, key, params)
}

#' List all BIS dataflows (datasets)
#'
#' Parses the SDMX dataflow registry to show available datasets.
#'
#' @return tibble: id, name
#' @export
bis_dataflows <- function() {
  tmp <- tempfile(fileext = ".xml")
  httr2::request(paste0(.bis_base, "/dataflow/BIS")) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  doc <- xml2::read_xml(tmp)
  ns <- xml2::xml_ns(doc)
  flows <- xml2::xml_find_all(doc, ".//str:Dataflow", ns)
  if (length(flows) == 0) return(tibble(id = character(), name = character()))

  bind_rows(lapply(flows, function(f) {
    id <- xml2::xml_attr(f, "id")
    nm_node <- xml2::xml_find_first(f, ".//com:Name", ns)
    nm <- if (!is.na(nm_node)) xml2::xml_text(nm_node) else NA_character_
    tibble(id = id, name = nm)
  }))
}


# == Convenience: Central bank policy rates ====================================

#' Central bank policy rates (daily)
#'
#' Key policy interest rates set by central banks worldwide.
#'
#' @param country 2-letter ISO country code (e.g. "US", "GB", "JP", "EU").
#'   "." for all countries.
#' @param last_n Last N observations (default 60)
#' @return tibble: date, value, ref_area, title
#' @export
bis_policy_rate <- function(country = "US", last_n = 60) {
  key <- sprintf("D.%s", toupper(country))
  df <- bis_get("WS_CBPOL", key, last_n = last_n)
  if (nrow(df) == 0) return(.schema_bis)
  df |> transmute(
    date     = TIME_PERIOD,
    value    = OBS_VALUE,
    ref_area = REF_AREA,
    title    = if ("TITLE_TS" %in% names(df)) TITLE_TS else NA_character_
  )
}


# == Convenience: Effective exchange rates =====================================

#' BIS effective exchange rate indices
#'
#' Broad nominal and real effective exchange rates (monthly).
#'
#' @param country 2-letter country code (e.g. "US", "GB", "JP", "DE")
#' @param type "N" (nominal, default) or "R" (real)
#' @param basket "B" (broad, default) or "N" (narrow)
#' @param last_n Last N observations (default 60)
#' @return tibble: date, value, ref_area, title
#' @export
bis_exchange_rate <- function(country = "US", type = "N", basket = "B",
                              last_n = 60) {
  key <- sprintf("M.%s.%s.%s", toupper(type), toupper(basket), toupper(country))
  df <- bis_get("WS_EER", key, last_n = last_n)
  if (nrow(df) == 0) return(.schema_bis)
  df |> transmute(
    date     = TIME_PERIOD,
    value    = OBS_VALUE,
    ref_area = REF_AREA,
    title    = if ("TITLE_TS" %in% names(df)) TITLE_TS else NA_character_
  )
}


# == Convenience: Property prices ==============================================

#' BIS residential property price indices
#'
#' @param country 2-letter country code
#' @param last_n Last N observations (default 60)
#' @return tibble: date, value, ref_area, title
#' @export
bis_property_prices <- function(country = "US", last_n = 60) {
  key <- sprintf("Q.%s.N.628", toupper(country))
  df <- tryCatch(bis_get("WS_SPP", key, last_n = last_n),
                 error = function(e) tibble())
  if (nrow(df) == 0) return(.schema_bis)
  df |> transmute(
    date     = TIME_PERIOD,
    value    = OBS_VALUE,
    ref_area = REF_AREA,
    title    = if ("TITLE_TS" %in% names(df)) TITLE_TS else NA_character_
  )
}


# == Convenience: Debt securities ==============================================

#' BIS international debt securities outstanding
#'
#' @param country 2-letter country code (issuer nationality)
#' @param last_n Last N observations (default 40)
#' @return tibble: date, value, ref_area, title
#' @export
bis_debt_securities <- function(country = "US", last_n = 40) {
  key <- sprintf("Q.%s.A.A.A.A.TO1.A.A.A.A.I", toupper(country))
  df <- tryCatch(bis_get("WS_DBS", key, last_n = last_n),
                 error = function(e) tibble())
  if (nrow(df) == 0) return(.schema_bis)
  df |> transmute(
    date     = TIME_PERIOD,
    value    = OBS_VALUE,
    ref_area = REF_AREA,
    title    = if ("TITLE_TS" %in% names(df)) TITLE_TS else NA_character_
  )
}


# == Convenience: Central bank total assets ====================================

#' Central bank total assets (monthly)
#'
#' @param country 2-letter country code
#' @param last_n Last N observations (default 60)
#' @return tibble: date, value, ref_area, title
#' @export
bis_cb_assets <- function(country = "US", last_n = 60) {
  key <- sprintf("M.%s", toupper(country))
  df <- tryCatch(bis_get("WS_CBTA", key, last_n = last_n),
                 error = function(e) tibble())
  if (nrow(df) == 0) return(.schema_bis)
  df |> transmute(
    date     = TIME_PERIOD,
    value    = OBS_VALUE,
    ref_area = REF_AREA,
    title    = if ("TITLE_TS" %in% names(df)) TITLE_TS else NA_character_
  )
}


# == Context ===================================================================

#' Generate LLM-friendly context for the bis.org package
#'
#' @return Character string (invisibly), also printed
#' @export
bis_context <- function() {
  .build_context("bis.org", header_lines = c(
    "# bis.org - Bank for International Settlements Data Client for R",
    "# Dependencies: httr2, xml2, dplyr, tibble",
    "# Auth: none (public SDMX REST API at stats.bis.org)",
    "# All functions return tibbles.",
    "#",
    "# Key dataflows (use bis_dataflows() for full list):",
    "#   WS_CBPOL = Central bank policy rates",
    "#   WS_EER   = Effective exchange rates",
    "#   WS_SPP   = Property prices",
    "#   WS_DBS   = Debt securities",
    "#   WS_CBTA  = Central bank total assets",
    "#   WS_CBS   = Consolidated banking statistics",
    "#",
    "# Country codes: US, GB, JP, DE, FR, CN, EU (euro area), etc."
  ))
}
