#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# coingecko-com.R
# Self-contained CoinGecko cryptocurrency market data client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none for free tier. Pro key via x-cg-pro-api-key header.
# Rate limits: Free = 10-30 req/min. Pro = higher.
# Docs: https://www.coingecko.com/en/api/documentation


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.cg_base <- "https://api.coingecko.com/api/v3"

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

# -- Fetch helpers -------------------------------------------------------------

.cg_get <- function(endpoint, params = list()) {
  query <- if (length(params) > 0) {
    paste(names(params), params, sep = "=", collapse = "&")
  } else ""
  url <- if (query != "") paste0(.cg_base, "/", endpoint, "?", query)
         else paste0(.cg_base, "/", endpoint)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}

.cg_get_list <- function(endpoint, params = list()) {
  query <- if (length(params) > 0) {
    paste(names(params), params, sep = "=", collapse = "&")
  } else ""
  url <- if (query != "") paste0(.cg_base, "/", endpoint, "?", query)
         else paste0(.cg_base, "/", endpoint)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


# == Schemas ===================================================================

.schema_markets <- tibble(
  id = character(), symbol = character(), name = character(),
  current_price = numeric(), market_cap = numeric(),
  market_cap_rank = integer(), total_volume = numeric(),
  price_change_24h = numeric(), price_change_pct_24h = numeric()
)

.schema_history <- tibble(
  timestamp = as.POSIXct(character()), price = numeric(),
  market_cap = numeric(), volume = numeric(), coin = character()
)

.schema_search <- tibble(
  id = character(), name = character(), symbol = character(),
  market_cap_rank = integer()
)


