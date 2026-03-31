
# == Observations ==============================================================

#' Fetch observations for a Riksbank SWEA series
#'
#' @param series Series ID (e.g. "SEKUSDPMI", "SEKEURPMI", "SECBREPOEFF")
#' @param from Start date ("YYYY-MM-DD")
#' @param to End date ("YYYY-MM-DD")
#' @return tibble: date (Date), value (numeric), series (character)
#' @export
riksbank_observations <- function(series, from = "2024-01-01",
                                  to = format(Sys.Date(), "%Y-%m-%d")) {
  url <- sprintf("%s/Observations/%s/%s/%s", .riksbank_base, series, from, to)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_observations)
  # API returns a list of objects with date and value
  if (is.data.frame(raw)) {
    df <- as_tibble(raw)
  } else {
    df <- bind_rows(lapply(raw, as_tibble))
  }
  date_col <- intersect(c("date", "Date", "period"), names(df))
  value_col <- intersect(c("value", "Value", "average"), names(df))
  if (length(date_col) == 0 || length(value_col) == 0) return(.schema_observations)
  tibble(
    date = as.Date(df[[date_col[1]]]),
    value = suppressWarnings(as.numeric(df[[value_col[1]]])),
    series = series
  )
}

# == Series listing ============================================================

#' List all available Riksbank SWEA series
#'
#' @return tibble: seriesid (character), name (character), description (character)
#' @export
riksbank_series <- function() {
  url <- sprintf("%s/Series", .riksbank_base)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_series)
  if (is.data.frame(raw)) {
    df <- as_tibble(raw)
  } else {
    df <- bind_rows(lapply(raw, as_tibble))
  }
  # Normalize column names
  names(df) <- tolower(names(df))
  id_col <- intersect(c("seriesid", "id", "series_id", "key"), names(df))
  name_col <- intersect(c("name", "seriesname", "title", "longnameen"), names(df))
  desc_col <- intersect(c("description", "descriptionen", "source"), names(df))
  tibble(
    seriesid = if (length(id_col) > 0) as.character(df[[id_col[1]]]) else NA_character_,
    name = if (length(name_col) > 0) as.character(df[[name_col[1]]]) else NA_character_,
    description = if (length(desc_col) > 0) as.character(df[[desc_col[1]]]) else NA_character_
  )
}

# == Context ===================================================================

#' Show Riksbank SWEA API context for LLMs
#'
#' Displays package overview, common series codes, and function signatures.
#' @return Invisibly returns the context string
#' @export
riksbank_context <- function() {
  .build_context(
    "riksbank.se",
    header_lines = c(
      "# riksbank.se",
      "# Riksbank (Swedish central bank) SWEA API client",
      "# Auth: none required",
      "# Rate limits: unknown, be courteous",
      "#",
      "# Common series: SEKUSDPMI (SEK/USD), SEKEURPMI (SEK/EUR),",
      "#   SECBREPOEFF (repo rate), SEKINFLCPIF (CPIF inflation)"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
