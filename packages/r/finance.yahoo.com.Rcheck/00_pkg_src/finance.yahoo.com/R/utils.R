#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform req_error resp_headers resp_body_string resp_status
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# finance-yahoo.R
# Self-contained Yahoo Finance client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (cookie/crumb handled internally)
# Rate limits: undocumented, be polite (~2 req/sec max)


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
.yf_cache <- new.env(parent = emptyenv())
.yf_base_chart   <- "https://query2.finance.yahoo.com/v8/finance/chart"
.yf_base_summary <- "https://query2.finance.yahoo.com/v10/finance/quoteSummary"
.yf_base_quote   <- "https://query2.finance.yahoo.com/v7/finance/quote"
.yf_base_search  <- "https://query2.finance.yahoo.com/v1/finance/search"
.yf_base_options <- "https://query2.finance.yahoo.com/v7/finance/options"

# -- Auth (cookie + crumb, cached per session) --------------------------------

.yf_auth <- function() {
  if (!is.null(.yf_cache$crumb) && !is.null(.yf_cache$cookie)) {
    return(list(crumb = .yf_cache$crumb, cookie = .yf_cache$cookie))
  }

  resp <- httr2::request("https://fc.yahoo.com/") |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  cookie <- paste(httr2::resp_headers(resp, "set-cookie"), collapse = "; ")

  crumb <- httr2::request("https://query2.finance.yahoo.com/v1/test/getcrumb") |>
    httr2::req_headers(`User-Agent` = .ua, Cookie = cookie) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  .yf_cache$crumb <- crumb
  .yf_cache$cookie <- cookie
  list(crumb = crumb, cookie = cookie)
}

# -- Authenticated fetch -------------------------------------------------------

.yf_get <- function(url) {
  auth <- .yf_auth()
  sep <- if (grepl("\\?", url)) "&" else "?"
  full_url <- paste0(url, sep, "crumb=", utils::URLencode(auth$crumb))

  httr2::request(full_url) |>
    httr2::req_headers(`User-Agent` = .ua, Cookie = auth$cookie) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(simplifyVector = FALSE)
}

# -- Extract raw value from Yahoo's {raw, fmt, longFmt} objects ---------------

.yf_raw <- function(x, default = NA_real_) {
  if (is.null(x)) return(default)
  if (is.list(x) && !is.null(x$raw)) return(x$raw)
  if (is.numeric(x) || is.character(x)) return(x)
  default
}

.yf_chr <- function(x, default = NA_character_) {
  if (is.null(x)) return(default)
  if (is.list(x) && !is.null(x$fmt)) return(as.character(x$fmt))
  if (is.character(x)) return(x)
  as.character(x)
}

# -- quoteSummary module fetch ------------------------------------------------

.yf_summary <- function(ticker, modules) {
  modules_str <- paste(modules, collapse = ",")
  url <- sprintf("%s/%s?modules=%s", .yf_base_summary, ticker, modules_str)
  raw <- .yf_get(url)
  result <- raw$quoteSummary$result
  if (is.null(result) || length(result) == 0) return(NULL)
  result[[1]]
}


# == Schemas ===================================================================

.schema_history <- tibble(
  date = as.Date(character()), open = numeric(), high = numeric(),
  low = numeric(), close = numeric(), adj_close = numeric(),
  volume = numeric(), ticker = character()
)

.schema_quote <- tibble(
  ticker = character(), name = character(), price = numeric(),
  change = numeric(), change_pct = numeric(), volume = numeric(),
  market_cap = numeric(), exchange = character(), type = character()
)

.schema_search <- tibble(
  symbol = character(), name = character(), type = character(),
  exchange = character()
)

.schema_profile <- tibble(
  ticker = character(), name = character(), sector = character(),
  industry = character(), employees = integer(), country = character(),
  city = character(), website = character(), description = character()
)

.schema_financials <- tibble(
  ticker = character(), date = as.Date(character()),
  period = character(), metric = character(), value = numeric()
)

.schema_statistics <- tibble(
  ticker = character(), metric = character(), value = numeric()
)

.schema_holders <- tibble(
  ticker = character(), holder = character(), shares = numeric(),
  date_reported = as.Date(character()), pct_held = numeric(),
  value = numeric(), type = character()
)

.schema_recommendations <- tibble(
  ticker = character(), firm = character(), to_grade = character(),
  from_grade = character(), action = character(),
  date = as.Date(character())
)

.schema_earnings <- tibble(
  ticker = character(), date = as.Date(character()),
  eps_actual = numeric(), eps_estimate = numeric(),
  revenue_actual = numeric(), revenue_estimate = numeric()
)

.schema_options <- tibble(
  ticker = character(), expiration = as.Date(character()),
  type = character(), strike = numeric(), last_price = numeric(),
  bid = numeric(), ask = numeric(), volume = integer(),
  open_interest = integer(), implied_volatility = numeric()
)


