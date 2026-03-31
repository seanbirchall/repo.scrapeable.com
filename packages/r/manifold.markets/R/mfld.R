# == Public functions ==========================================================

#' List Manifold Markets prediction markets
#'
#' @param limit Max results (default 10, max 1000)
#' @param term Search term to filter markets
#' @return tibble: id, question, url, probability, volume, creator_username,
#'   mechanism, created_time, close_time, is_resolved
#' @export
mfld_markets <- function(limit = 10, term = NULL) {
  params <- list(limit = limit)
  if (!is.null(term)) params$term <- utils::URLencode(term, reserved = TRUE)
  qstr <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.mfld_base, "/search-markets?", qstr)
  raw <- .fetch_json(url)
  if (length(raw) == 0 || (is.data.frame(raw) && nrow(raw) == 0)) return(.schema_markets)

  tibble(
    id = as.character(raw$id),
    question = as.character(raw$question),
    url = as.character(raw$url %||% NA_character_),
    probability = as.numeric(raw$probability %||% NA_real_),
    volume = as.numeric(raw$volume %||% NA_real_),
    creator_username = as.character(raw$creatorUsername %||% NA_character_),
    mechanism = as.character(raw$mechanism %||% NA_character_),
    created_time = as.POSIXct(as.numeric(raw$createdTime %||% NA_real_) / 1000, origin = "1970-01-01"),
    close_time = as.POSIXct(as.numeric(raw$closeTime %||% NA_real_) / 1000, origin = "1970-01-01"),
    is_resolved = as.logical(raw$isResolved %||% NA)
  )
}

#' Get a single Manifold market by ID
#'
#' @param id Market ID or slug
#' @return tibble: one row with id, question, url, probability, volume,
#'   creator_username, mechanism, description, is_resolved, total_liquidity
#' @export
mfld_market <- function(id) {
  url <- paste0(.mfld_base, "/market/", id)
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_market)

  tibble(
    id = as.character(raw$id),
    question = as.character(raw$question),
    url = as.character(raw$url %||% NA_character_),
    probability = as.numeric(raw$probability %||% NA_real_),
    volume = as.numeric(raw$volume %||% NA_real_),
    creator_username = as.character(raw$creatorUsername %||% NA_character_),
    mechanism = as.character(raw$mechanism %||% NA_character_),
    description = as.character(raw$textDescription %||% NA_character_),
    is_resolved = as.logical(raw$isResolved %||% NA),
    total_liquidity = as.numeric(raw$totalLiquidity %||% NA_real_)
  )
}

#' Show Manifold Markets client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
mfld_context <- function() {
  .build_context(
    pkg_name = "manifold.markets",
    header_lines = c(
      "# manifold.markets -- Manifold Markets Prediction Markets Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required for public markets",
      "# Search terms: politics, AI, technology, sports, economics"
    )
  )
}

