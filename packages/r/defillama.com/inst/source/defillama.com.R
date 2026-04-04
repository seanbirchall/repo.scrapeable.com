# defillama.com.R - Self-contained defillama.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# defillama-com.R
# Self-contained DeFi Llama client for DeFi protocol TVL data.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none known


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.defi_base <- "https://api.llama.fi"
# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_protocols <- tibble(
  id = character(), name = character(), symbol = character(),
  category = character(), chains = character(),
  tvl = numeric(), change_1h = numeric(), change_1d = numeric(),
  change_7d = numeric(), url = character(), description = character(),
  slug = character()
)

.schema_protocol_detail <- tibble(
  date = as.Date(character()), tvl = numeric()
)

.schema_tvl_history <- tibble(
  date = as.Date(character()), tvl = numeric()
)


# == Protocols =================================================================

#' Fetch all DeFi protocols from DeFi Llama
#'
#' Returns a tibble of all tracked DeFi protocols with current Total Value
#' Locked (TVL) and recent percentage changes. Typically returns 4000+ protocols.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Internal DeFi Llama protocol ID.}
#'     \item{name}{Character. Protocol display name (e.g., "Aave V3", "Uniswap V3").}
#'     \item{symbol}{Character. Token ticker symbol (e.g., "AAVE", "UNI").}
#'     \item{category}{Character. Protocol category (e.g., "Lending", "Dexes", "CEX",
#'       "Liquid Staking", "Bridge", "CDP", "Yield").}
#'     \item{chains}{Character. Comma-separated blockchain names where the protocol
#'       operates (e.g., "Ethereum, Polygon, Avalanche").}
#'     \item{tvl}{Numeric. Current Total Value Locked in USD.}
#'     \item{change_1h}{Numeric. Percentage TVL change over the last 1 hour.}
#'     \item{change_1d}{Numeric. Percentage TVL change over the last 1 day.}
#'     \item{change_7d}{Numeric. Percentage TVL change over the last 7 days.}
#'     \item{url}{Character. Protocol website URL.}
#'     \item{description}{Character. Brief description of the protocol.}
#'     \item{slug}{Character. URL slug used for the \code{defi_protocol()} detail endpoint.}
#'   }
#' @examples
#' protocols <- defi_protocols()
#' # Top 10 by TVL
#' head(protocols[order(-protocols$tvl), ], 10)
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
#' Returns a daily time series of Total Value Locked for one protocol,
#' from inception to the current date.
#'
#' @param slug Character. Protocol slug identifier, e.g. \code{"aave"},
#'   \code{"uniswap"}, \code{"lido"}, \code{"makerdao"}. Use the \code{slug}
#'   column from \code{defi_protocols()} to discover valid slugs.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Calendar date of the TVL snapshot.}
#'     \item{tvl}{Numeric. Total Value Locked in USD on that date.}
#'   }
#' @examples
#' defi_protocol("aave")
#' defi_protocol("uniswap")
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
#' Returns a daily time series of aggregate Total Value Locked across the
#' entire DeFi ecosystem, from September 2017 to the present.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Calendar date of the TVL snapshot.}
#'     \item{tvl}{Numeric. Total Value Locked in USD across all tracked protocols.}
#'   }
#' @examples
#' tvl <- defi_tvl_history()
#' tail(tvl, 10)  # most recent 10 days
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

#' Get defillama.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
defi_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(defi_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/defillama.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "defillama.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# defillama.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# defillama.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
