# bis.org.R - Self-contained bis.org client

library(httr2)
library(xml2)
library(dplyr)
library(tibble)



# bis-org.R
# Self-contained Bank for International Settlements (BIS) data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, xml2, dplyr, tibble
# Auth: none required (public SDMX REST API)
# Rate limits: undocumented, be polite
# Docs: https://data.bis.org/topics


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.bis_base <- "https://stats.bis.org/api/v1"
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

#' Fetch any BIS SDMX dataset by dataflow and key
#'
#' Universal engine for the BIS SDMX v1 REST API. Returns raw SDMX CSV
#' data as a tibble. Use \code{\link{bis_dataflows}} to discover available
#' datasets, then construct an SDMX key string to filter dimensions.
#'
#' @param dataflow Character. BIS dataflow ID (e.g. \code{"WS_EER"},
#'   \code{"WS_CBPOL"}, \code{"WS_SPP"}). See \code{bis_dataflows()} for
#'   the full list of ~29 datasets.
#' @param key Character. SDMX dimension key (e.g. \code{"D.US"} for daily
#'   US data, \code{"M.N.B.US"} for monthly nominal broad US exchange rate).
#'   Use \code{"."} to wildcard a dimension position.
#' @param start Character or Date. Start period in \code{YYYY-MM-DD},
#'   \code{YYYY-MM}, or \code{YYYY} format. \code{NULL} for no constraint.
#' @param end Character or Date. End period (same formats as \code{start}).
#' @param last_n Integer. Return only the last N observations per series.
#'   \code{NULL} returns all available data.
#' @return A tibble with full SDMX CSV columns. Key columns include:
#'   \describe{
#'     \item{TIME_PERIOD}{Date. Observation date (quarters/years converted to Date).}
#'     \item{OBS_VALUE}{Numeric. The observation value.}
#'     \item{REF_AREA}{Character. 2-letter ISO country code.}
#'     \item{TITLE_TS}{Character. Human-readable series title (when available).}
#'   }
#'   Additional columns vary by dataflow (e.g. FREQ, COLLECTION, UNIT_MEASURE).
#' @examples
#' # Central bank policy rates for the US, last 10 observations
#' bis_get("WS_CBPOL", "D.US", last_n = 10)
#'
#' # Effective exchange rates, monthly, nominal, broad, Japan
#' bis_get("WS_EER", "M.N.B.JP", last_n = 12)
#' @export
bis_get <- function(dataflow, key, start = NULL, end = NULL, last_n = NULL) {
  params <- list()
  if (!is.null(start))  params$startPeriod <- as.character(start)
  if (!is.null(end))    params$endPeriod <- as.character(end)
  if (!is.null(last_n)) params$lastNObservations <- last_n
  .bis_fetch(dataflow, key, params)
}

#' List all BIS dataflows (available datasets)
#'
#' Queries the BIS SDMX dataflow registry and returns a catalog of all
#' available statistical datasets. Typically returns ~29 dataflows covering
#' exchange rates, banking statistics, debt securities, property prices, etc.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Dataflow identifier (e.g. \code{"WS_CBPOL"},
#'       \code{"WS_EER"}). Pass to \code{\link{bis_get}}.}
#'     \item{name}{Character. Human-readable dataset name (e.g.
#'       \code{"Central bank policy rates"}).}
#'   }
#' @examples
#' bis_dataflows()
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

#' Get central bank policy interest rates (daily)
#'
#' Returns the key policy interest rate set by a country's central bank.
#' Data from the BIS WS_CBPOL dataflow, updated daily.
#'
#' @param country Character. 2-letter ISO country code (e.g. \code{"US"},
#'   \code{"GB"}, \code{"JP"}, \code{"EU"}). Use \code{"."} for all countries.
#' @param last_n Integer. Number of most recent observations to return
#'   (default 60).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{value}{Numeric. Policy rate in percent.}
#'     \item{ref_area}{Character. 2-letter ISO country code.}
#'     \item{title}{Character. Series description (may be NA).}
#'   }
#' @examples
#' bis_policy_rate("US", last_n = 10)
#' bis_policy_rate("EU", last_n = 30)
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

#' Get BIS effective exchange rate indices (monthly)
#'
#' Returns nominal or real effective exchange rate indices for a country.
#' Indices are trade-weighted and available in broad (64 economies) or
#' narrow baskets. Data from the BIS WS_EER dataflow.
#'
#' @param country Character. 2-letter ISO country code (e.g. \code{"US"},
#'   \code{"GB"}, \code{"JP"}, \code{"DE"}).
#' @param type Character. \code{"N"} for nominal (default) or \code{"R"} for
#'   real (CPI-deflated) exchange rate.
#' @param basket Character. \code{"B"} for broad basket (64 economies, default)
#'   or \code{"N"} for narrow basket.
#' @param last_n Integer. Number of most recent monthly observations (default 60).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. First day of the observation month.}
#'     \item{value}{Numeric. Index value (2020 = 100).}
#'     \item{ref_area}{Character. 2-letter ISO country code.}
#'     \item{title}{Character. Series title (e.g. \code{"United States - Nominal - Broad (64 economies)"}).}
#'   }
#' @examples
#' bis_exchange_rate("US", last_n = 12)
#' bis_exchange_rate("JP", type = "R")
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

#' Get BIS residential property price indices (quarterly)
#'
#' Returns residential property price index data from the BIS WS_SPP
#' dataflow. Indices track house price changes over time.
#'
#' @param country Character. 2-letter ISO country code (e.g. \code{"US"},
#'   \code{"GB"}, \code{"DE"}, \code{"JP"}).
#' @param last_n Integer. Number of most recent quarterly observations
#'   (default 60).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. First day of the observation quarter.}
#'     \item{value}{Numeric. Property price index value.}
#'     \item{ref_area}{Character. 2-letter ISO country code.}
#'     \item{title}{Character. Series description (may be NA).}
#'   }
#' @examples
#' bis_property_prices("US", last_n = 20)
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

#' Get BIS international debt securities outstanding (quarterly)
#'
#' Returns outstanding international debt securities data from the BIS
#' WS_DBS dataflow. Covers bonds and notes issued by residents of a country
#' in international markets.
#'
#' @param country Character. 2-letter ISO country code for issuer nationality
#'   (e.g. \code{"US"}, \code{"GB"}, \code{"DE"}).
#' @param last_n Integer. Number of most recent quarterly observations
#'   (default 40).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. First day of the observation quarter.}
#'     \item{value}{Numeric. Outstanding amount (in USD billions).}
#'     \item{ref_area}{Character. 2-letter ISO country code.}
#'     \item{title}{Character. Series description (may be NA).}
#'   }
#' @examples
#' bis_debt_securities("US", last_n = 10)
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

#' Get central bank total assets (monthly)
#'
#' Returns total assets held by a central bank from the BIS WS_CBTA
#' dataflow. Useful for tracking balance sheet expansion/contraction
#' (quantitative easing/tightening).
#'
#' @param country Character. 2-letter ISO country code (e.g. \code{"US"},
#'   \code{"GB"}, \code{"JP"}, \code{"EU"}).
#' @param last_n Integer. Number of most recent monthly observations
#'   (default 60).
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. First day of the observation month.}
#'     \item{value}{Numeric. Total assets value.}
#'     \item{ref_area}{Character. 2-letter ISO country code.}
#'     \item{title}{Character. Series description (may be NA).}
#'   }
#' @examples
#' bis_cb_assets("US", last_n = 24)
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

#' Get bis.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
bis_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(bis_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bis.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bis.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bis.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bis.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
