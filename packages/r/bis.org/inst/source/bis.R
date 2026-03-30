# bis-org.R
# Self-contained Bank for International Settlements (BIS) data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, xml2, dplyr, tibble
# Auth: none required (public SDMX REST API)
# Rate limits: undocumented, be polite
# Docs: https://data.bis.org/topics

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.bis_base <- "https://stats.bis.org/api/v1"

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

# -- SDMX CSV fetch engine ----------------------------------------------------

.bis_fetch <- function(dataflow, key, params = list()) {
  params$format <- "csv"
  query <- paste(names(params), params, sep = "=", collapse = "&")
  url <- sprintf("%s/data/%s/%s?%s", .bis_base, dataflow, key, query)

  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  df <- read.csv(tmp, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(tibble())

  as_tibble(df) |>
    mutate(
      TIME_PERIOD = {
        tp <- TIME_PERIOD
        tp <- gsub("-Q1$", "-01-01", tp)
        tp <- gsub("-Q2$", "-04-01", tp)
        tp <- gsub("-Q3$", "-07-01", tp)
        tp <- gsub("-Q4$", "-10-01", tp)
        tp <- ifelse(grepl("^\\d{4}$", tp), paste0(tp, "-01-01"), tp)
        tp <- ifelse(grepl("^\\d{4}-\\d{2}$", tp), paste0(tp, "-01"), tp)
        as.Date(tp)
      },
      OBS_VALUE = as.numeric(OBS_VALUE)
    )
}


# == Schemas ===================================================================

.schema_bis <- tibble(
  date = as.Date(character()), value = numeric(),
  ref_area = character(), title = character()
)


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
