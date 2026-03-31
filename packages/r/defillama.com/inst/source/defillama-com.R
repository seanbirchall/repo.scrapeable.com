# defillama-com.R
# Self-contained DeFi Llama client for DeFi protocol TVL data.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none known

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.defi_base <- "https://api.llama.fi"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

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
#' Returns a tibble of all tracked DeFi protocols with current TVL.
#'
#' @return tibble: id, name, symbol, category, chains, tvl, change_1h,
#'   change_1d, change_7d, url, description, slug
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
