# stratosphere.R
# Stratosphere.io financial KPI data client.
# All functions return data.frames. No database dependencies.
#
# Note: Stratosphere uses Next.js with a build ID that changes on deploy.
# The build_id parameter may need updating periodically.

# == Setup =====================================================================

if (!exists("http_get", mode = "function")) source("clients/_helpers.R")

.strat_get <- function(url, ext = ".json") http_get(url, ext)


# == KPI data ==================================================================

#' Fetch financial KPI data from Stratosphere.io
#'
#' Returns annual and quarterly financial data (revenue, margins, ratios, etc.)
#' for a given stock symbol.
#'
#' @param symbol Stock ticker symbol
#' @param build_id Next.js build ID (changes when Stratosphere deploys).
#'   You can find this in the page source of stratosphere.io.
#' @return data.frame with columns: date, name, value, label, view
#'   (view = "annual" or "quarterly")
strat_kpis <- function(symbol, build_id) {
  url <- paste0(
    "https://www.stratosphere.io/_next/data/", build_id,
    "/company/", tolower(symbol), "/kpis.json?symbol=", tolower(symbol)
  )
  path <- .strat_get(url, ".json")
  raw <- jsonlite::fromJSON(path)

  financials <- raw$pageProps$financials
  labels <- raw$pageProps$labels
  if (is.null(financials)) return(data.frame())

  views <- intersect(c("annual", "quarterly"), names(financials))
  if (length(views) == 0) return(data.frame())

  # Build label lookup
  label_map <- if (!is.null(labels)) {
    data.frame(
      name = names(labels),
      label = unname(unlist(labels)),
      stringsAsFactors = FALSE
    )
  } else NULL

  results <- lapply(views, function(v) {
    df <- financials[[v]]
    if (is.null(df) || length(df) == 0) return(NULL)

    # Each element is a named list of date -> value
    rows <- lapply(names(df), function(metric) {
      vals <- df[[metric]]
      if (is.null(vals) || length(vals) == 0) return(NULL)
      data.frame(
        date = names(vals),
        name = metric,
        value = as.character(unlist(vals)),
        view = v,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, Filter(Negate(is.null), rows))
  })

  result <- do.call(rbind, Filter(Negate(is.null), results))
  if (is.null(result) || nrow(result) == 0) return(data.frame())

  # Join labels
  if (!is.null(label_map)) {
    result <- merge(result, label_map, by = "name", all.x = TRUE)
  } else {
    result$label <- NA_character_
  }

  result$symbol <- toupper(symbol)
  result$value <- suppressWarnings(as.numeric(result$value))
  result
}
