# bankofcanada.ca.R - Self-contained bankofcanada.ca client



# bankofcanada-ca.R
# Self-contained Bank of Canada Valet API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.boc_base <- "https://www.bankofcanada.ca/valet"
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

.schema_observations <- tibble(
  date = as.Date(character()), value = numeric(), series = character()
)

.schema_series <- tibble(
  name = character(), label = character(), description = character()
)

.schema_groups <- tibble(
  name = character(), label = character(), description = character()
)


# == Observations ==============================================================

#' Fetch observations for a Bank of Canada Valet series
#'
#' Retrieves time-series observations from the Bank of Canada Valet API for a
#' given series code. Supports filtering by date range or fetching recent
#' observations. Returns a long-format tibble with one row per date per series.
#'
#' @param series Character. Series code. Use \code{boc_series()} to discover
#'   available codes within a group. Examples: \code{"FXUSDCAD"} (USD/CAD
#'   exchange rate), \code{"FXEURCAD"} (EUR/CAD), \code{"V39079"} (Bank Rate).
#' @param recent Integer or NULL. Number of most recent observations to return.
#'   When provided, \code{start} and \code{end} are ignored by the API.
#' @param start Character or NULL. Start date in \code{"YYYY-MM-DD"} format
#'   (e.g. \code{"2024-01-01"}).
#' @param end Character or NULL. End date in \code{"YYYY-MM-DD"} format.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. Observation date.}
#'     \item{value}{Numeric. Observed value (e.g. exchange rate, interest
#'       rate). \code{NA} for bank holidays or missing data.}
#'     \item{series}{Character. Series code (matches \code{series} argument).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Last 5 USD/CAD exchange rates
#' boc_observations("FXUSDCAD", recent = 5)
#'
#' # EUR/CAD for a date range
#' boc_observations("FXEURCAD", start = "2024-01-01", end = "2024-03-31")
#' }
boc_observations <- function(series, recent = NULL, start = NULL, end = NULL) {
  url <- sprintf("%s/observations/%s/json", .boc_base, series)
  params <- list()
  if (!is.null(recent)) params[["recent"]] <- recent
  if (!is.null(start)) params[["start_date"]] <- start
  if (!is.null(end)) params[["end_date"]] <- end
  if (length(params) > 0) {
    qs <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste0(url, "?", qs)
  }
  raw <- .fetch_json(url)
  obs <- raw$observations
  if (is.null(obs) || length(obs) == 0) return(.schema_observations)
  df <- as_tibble(obs)
  # The data has a "d" column for date and the series name as a column
  series_col <- setdiff(names(df), "d")
  if (length(series_col) == 0) return(.schema_observations)
  result <- lapply(series_col, function(s) {
    tibble(
      date = as.Date(df[["d"]]),
      value = suppressWarnings(as.numeric(df[[s]][["v"]])),
      series = s
    )
  })
  bind_rows(result)
}

# == Series listing ============================================================

#' List available series in a Bank of Canada Valet group
#'
#' Retrieves all series codes within a specified Valet group. Use the
#' \code{name} column values as the \code{series} argument to
#' \code{boc_observations()}.
#'
#' @param group Character. Group name from \code{boc_groups()}. Examples:
#'   \code{"FX_RATES_DAILY"} (daily exchange rates),
#'   \code{"FX_RATES_MONTHLY"} (monthly exchange rates),
#'   \code{"BOND_YIELDS"}, \code{"TBILL"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Series code (e.g. "FXUSDCAD", "FXEURCAD").
#'       Pass to \code{boc_observations()}.}
#'     \item{label}{Character. Human-readable label (e.g. "USD/CAD",
#'       "EUR/CAD").}
#'     \item{description}{Character. Longer description, often \code{NA} for
#'       exchange rate series.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' boc_series("FX_RATES_DAILY")
#' boc_series("FX_RATES_MONTHLY")
#' }
boc_series <- function(group) {
  url <- sprintf("%s/groups/%s/json", .boc_base, group)
  raw <- .fetch_json(url)
  detail <- raw$groupDetails$groupSeries
  if (is.null(detail) || length(detail) == 0) return(.schema_series)
  df <- bind_rows(lapply(names(detail), function(nm) {
    s <- detail[[nm]]
    tibble(
      name = nm,
      label = as.character(s$label %||% NA_character_),
      description = as.character(s$description %||% NA_character_)
    )
  }))
  df
}

# == Groups listing ============================================================

#' List available groups on the Bank of Canada Valet API
#'
#' Retrieves all available data groups from the Bank of Canada Valet API.
#' Typically returns ~2300 groups covering exchange rates, interest rates,
#' monetary aggregates, and research datasets. Use the \code{name} column
#' values as the \code{group} argument to \code{boc_series()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{name}{Character. Group identifier (e.g. "FX_RATES_DAILY",
#'       "BOND_YIELDS"). Pass to \code{boc_series()}.}
#'     \item{label}{Character. Human-readable label
#'       (e.g. "Staff Discussion Paper 2012-8").}
#'     \item{description}{Character. Longer description of the group contents.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' groups <- boc_groups()
#' groups
#' # Find exchange rate groups
#' groups[grepl("FX|exchange", groups$label, ignore.case = TRUE), ]
#' }
boc_groups <- function() {
  url <- sprintf("%s/lists/groups/json", .boc_base)
  raw <- .fetch_json(url)
  groups <- raw$groups
  if (is.null(groups) || length(groups) == 0) return(.schema_groups)
  # API returns a named list of lists, not a data frame
  bind_rows(lapply(names(groups), function(nm) {
    g <- groups[[nm]]
    tibble(
      name = nm,
      label = as.character(g$label %||% NA_character_),
      description = as.character(g$description %||% NA_character_)
    )
  }))
}

# == Context ===================================================================

#' Get bankofcanada.ca client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
boc_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(boc_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/bankofcanada.ca.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "bankofcanada.ca")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# bankofcanada.ca context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# bankofcanada.ca", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
