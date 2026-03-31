# == Protocols =================================================================

#' Fetch all DeFi protocols from DeFi Llama
#'
#' Returns a tibble of all tracked DeFi protocols with current TVL.
#'
#' @return tibble: id, name, symbol, category, chains, tvl, change_1h,
#'   change_1d, change_7d, url, description, slug
#' @export
defi_protocols <- function() {
  raw <- .fetch_json(sprintf("%s/protocols", .defi_base))
  if (is.null(raw) || length(raw) == 0) return(.schema_protocols)
  df <- as_tibble(raw)

  chains_col <- if ("chains" %in% names(df)) {
    vapply(df$chains, function(x) paste(x, collapse = ", "), character(1))
  } else {
    rep(NA_character_, nrow(df))
  }

  df |>
    transmute(
      id = as.character(id),
      name = as.character(name),
      symbol = as.character(if ("symbol" %in% names(df)) symbol else NA_character_),
      category = as.character(if ("category" %in% names(df)) category else NA_character_),
      chains = chains_col,
      tvl = as.numeric(tvl),
      change_1h = as.numeric(if ("change_1h" %in% names(df)) change_1h else NA_real_),
      change_1d = as.numeric(if ("change_1d" %in% names(df)) change_1d else NA_real_),
      change_7d = as.numeric(if ("change_7d" %in% names(df)) change_7d else NA_real_),
      url = as.character(if ("url" %in% names(df)) url else NA_character_),
      description = as.character(if ("description" %in% names(df)) description else NA_character_),
      slug = as.character(slug)
    )
}

#' Fetch detailed TVL history for a single DeFi protocol
#'
#' @param slug Protocol slug (e.g. "aave", "uniswap", "lido")
#' @return tibble: date (Date), tvl (numeric)
#' @export
defi_protocol <- function(slug) {
  raw <- .fetch_json(sprintf("%s/protocol/%s", .defi_base, slug))
  if (is.null(raw)) return(.schema_protocol_detail)

  tvls <- raw$tvl
  if (is.null(tvls) || length(tvls) == 0) return(.schema_protocol_detail)

  tvl_df <- as_tibble(tvls)
  tvl_df |>
    transmute(
      date = as.Date(as.POSIXct(date, origin = "1970-01-01", tz = "UTC")),
      tvl = as.numeric(totalLiquidityUSD)
    )
}

#' Fetch historical total TVL across all DeFi protocols
#'
#' @return tibble: date (Date), tvl (numeric)
#' @export
defi_tvl_history <- function() {
  raw <- .fetch_json(sprintf("%s/v2/historicalChainTvl", .defi_base))
  if (is.null(raw) || length(raw) == 0) return(.schema_tvl_history)
  df <- as_tibble(raw)

  df |>
    transmute(
      date = as.Date(as.POSIXct(date, origin = "1970-01-01", tz = "UTC")),
      tvl = as.numeric(tvl)
    )
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the defillama package
#'
#' @return Character string (invisibly), also printed
#' @export
defi_context <- function() {
  .build_context("defillama.com", header_lines = c(
    "# defillama.com - DeFi Llama TVL Data Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: none known",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Popular protocol slugs: aave, uniswap, lido, makerdao, curve-dex,",
    "#   compound, sushiswap, pancakeswap, balancer, yearn-finance"
  ))
}
