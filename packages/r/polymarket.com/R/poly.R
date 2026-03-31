# == Markets ===================================================================

#' Fetch Polymarket prediction markets
#'
#' Returns a tibble of prediction markets from Polymarket.
#'
#' @param limit Number of markets to return (default 100, max 100)
#' @param active If TRUE, return only active markets (default TRUE)
#' @param closed If TRUE, include closed markets (default FALSE)
#' @return tibble: id, question, slug, end_date, active, closed,
#'   volume, liquidity, outcome_yes, outcome_no, description
#' @export
poly_markets <- function(limit = 100, active = TRUE, closed = FALSE) {
  url <- sprintf("%s/markets?limit=%d&active=%s&closed=%s",
                 .poly_base, limit, tolower(active), tolower(closed))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_markets)
  df <- as_tibble(raw)

  prices <- tryCatch({
    p <- df$outcomePrices
    parsed <- lapply(p, function(x) {
      v <- jsonlite::fromJSON(x)
      as.numeric(v)
    })
    do.call(rbind, parsed)
  }, error = function(e) NULL)

  result <- df |>
    transmute(
      id = as.character(id),
      question = as.character(question),
      slug = as.character(slug),
      end_date = as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      active = as.logical(active),
      closed = as.logical(closed),
      volume = as.numeric(volumeNum),
      liquidity = as.numeric(liquidityNum),
      description = as.character(description)
    )

  if (!is.null(prices) && nrow(prices) == nrow(result)) {
    result$outcome_yes <- prices[, 1]
    result$outcome_no  <- prices[, 2]
  } else {
    result$outcome_yes <- NA_real_
    result$outcome_no  <- NA_real_
  }

  result
}

#' Fetch a single Polymarket market by slug
#'
#' @param slug Market slug (e.g. "will-bitcoin-hit-100000-in-2025")
#' @return tibble: single row with same columns as poly_markets
#' @return tibble: id, question, slug, end_date, active, closed,
#'   volume, liquidity, outcome_yes, outcome_no, description
#' @export
poly_market <- function(slug) {
  url <- sprintf("%s/markets?slug=%s", .poly_base, slug)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_markets)
  df <- as_tibble(raw)

  prices <- tryCatch({
    p <- df$outcomePrices
    parsed <- lapply(p, function(x) {
      v <- jsonlite::fromJSON(x)
      as.numeric(v)
    })
    do.call(rbind, parsed)
  }, error = function(e) NULL)

  result <- df |>
    transmute(
      id = as.character(id),
      question = as.character(question),
      slug = as.character(slug),
      end_date = as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      active = as.logical(active),
      closed = as.logical(closed),
      volume = as.numeric(volumeNum),
      liquidity = as.numeric(liquidityNum),
      description = as.character(description)
    )

  if (!is.null(prices) && nrow(prices) == nrow(result)) {
    result$outcome_yes <- prices[, 1]
    result$outcome_no  <- prices[, 2]
  } else {
    result$outcome_yes <- NA_real_
    result$outcome_no  <- NA_real_
  }

  result
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the polymarket package
#'
#' @return Character string (invisibly), also printed
#' @export
poly_context <- function() {
  .build_context("polymarket.com", header_lines = c(
    "# polymarket.com - Polymarket Prediction Markets Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: unknown (be polite)",
    "# All functions return tibbles with typed columns."
  ))
}
