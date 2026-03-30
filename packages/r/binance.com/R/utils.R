#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @keywords internal
NULL

# binance-com.R
# Self-contained Binance/Binance.US crypto exchange client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none for public market data endpoints.
# Rate limits: 1200 req/min (general), 10 req/sec (orders).
# Docs: https://binance-docs.github.io/apidocs/spot/en/
#
# Note: Binance.com blocks US IPs (HTTP 451). Use base = "us" for US access.


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"

.binance_base <- function(region = "us") {
  switch(region,
    us      = "https://api.binance.us/api/v3",
    global  = "https://api.binance.com/api/v3",
    "https://api.binance.us/api/v3"
  )
}

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

# -- Fetch helper --------------------------------------------------------------

.bn_get <- function(endpoint, params = list(), base = "us") {
  query <- if (length(params) > 0)
    paste(names(params), params, sep = "=", collapse = "&") else ""
  url <- if (query != "") paste0(.binance_base(base), "/", endpoint, "?", query)
         else paste0(.binance_base(base), "/", endpoint)

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp)
}


# == Schemas ===================================================================

.schema_klines <- tibble(
  open_time = as.POSIXct(character()), open = numeric(), high = numeric(),
  low = numeric(), close = numeric(), volume = numeric(),
  close_time = as.POSIXct(character()), quote_volume = numeric(),
  trades = integer(), symbol = character()
)

.schema_ticker <- tibble(
  symbol = character(), price = numeric()
)


