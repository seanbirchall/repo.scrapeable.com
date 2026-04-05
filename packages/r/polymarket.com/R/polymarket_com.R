# gamma-api.polymarket.com.R - Self-contained gamma-api.polymarket.com client


.ua <- "support@scrapeable.com"
.poly_base <- "https://gamma-api.polymarket.com"

`%||%` <- function(a, b) if (is.null(a)) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

#' Get Polymarket prediction markets
#'
#' Returns a tibble of prediction markets from the Polymarket Gamma API.
#' Each market represents a binary yes/no question with associated trading
#' volume. Markets are listed in API default order (generally by creation
#' date).
#'
#' @param limit Integer. Number of markets to return (default 25).
#' @param active Logical. If \code{TRUE} (default), return only markets
#'   currently open for trading; if \code{FALSE}, include resolved markets.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Unique market identifier.}
#'     \item{question}{Character. The market question text.}
#'     \item{end_date}{Character (ISO 8601). Resolution deadline.}
#'     \item{volume}{Numeric. Total trading volume in USD.}
#'     \item{active}{Logical. Whether the market is currently active.}
#'   }
#'
#' @examples
#' poly_markets(limit = 5)
#' poly_markets(limit = 10, active = FALSE)
#'
#' @seealso \code{\link{poly_market}}, \code{\link{poly_events}}
#' @export
poly_markets <- function(limit = 25L, active = TRUE) {
  url <- sprintf("%s/markets?limit=%d&active=%s", .poly_base, limit, tolower(as.character(active)))
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(tibble::tibble(id = character(), question = character()))
  tibble::tibble(
    id = vapply(raw, function(x) x$id %||% NA_character_, character(1)),
    question = vapply(raw, function(x) x$question %||% NA_character_, character(1)),
    end_date = vapply(raw, function(x) x$endDate %||% NA_character_, character(1)),
    volume = vapply(raw, function(x) as.numeric(x$volume %||% NA), numeric(1)),
    active = vapply(raw, function(x) x$active %||% NA, logical(1))
  )
}

#' Get details for a specific Polymarket market
#'
#' Fetches full metadata for a single prediction market by its ID or slug.
#' Returns a single-row tibble with the market question, description,
#' trading volume, and resolution status.
#'
#' @param market_id Character. Market ID (numeric string) or URL slug
#'   (obtained from \code{poly_markets()} or \code{poly_events()}).
#'
#' @return A single-row tibble with columns:
#'   \describe{
#'     \item{id}{Character. Market identifier.}
#'     \item{question}{Character. The market question.}
#'     \item{description}{Character. Detailed market description and
#'       resolution criteria.}
#'     \item{end_date}{Character (ISO 8601). Resolution deadline.}
#'     \item{volume}{Numeric. Total trading volume in USD.}
#'     \item{active}{Logical. Whether the market is active.}
#'   }
#'
#' @examples
#' poly_market("12")
#'
#' @seealso \code{\link{poly_markets}}, \code{\link{poly_events}}
#' @export
poly_market <- function(market_id) {
  url <- sprintf("%s/markets/%s", .poly_base, market_id)
  raw <- .fetch_json(url)
  tibble::tibble(
    id = raw$id %||% NA_character_,
    question = raw$question %||% NA_character_,
    description = raw$description %||% NA_character_,
    end_date = raw$endDate %||% NA_character_,
    volume = as.numeric(raw$volume %||% NA),
    active = raw$active %||% NA
  )
}

# == Events ====================================================================

#' Get Polymarket events (groups of related markets)
#'
#' Returns a tibble of events from the Polymarket Gamma API. An event
#' groups related markets together (e.g. "2024 US Presidential Election"
#' may contain multiple outcome markets). Only active events are returned.
#'
#' @param limit Integer. Number of events to return (default 25).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Unique event identifier.}
#'     \item{title}{Character. Event title.}
#'     \item{slug}{Character. URL-friendly slug.}
#'     \item{end_date}{Character (ISO 8601). Event resolution deadline.}
#'     \item{volume}{Numeric. Aggregate trading volume in USD.}
#'     \item{active}{Logical. Whether the event is active.}
#'   }
#'
#' @examples
#' poly_events(limit = 5)
#'
#' @seealso \code{\link{poly_markets}}, \code{\link{poly_market}}
#' @export
poly_events <- function(limit = 25L) {
  schema <- tibble(id = character(), title = character(), slug = character(),
                   end_date = character(), volume = numeric(), active = logical())
  url <- sprintf("%s/events?limit=%d&active=true", .poly_base, limit)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(schema)

  tibble(
    id = vapply(raw, function(x) x$id %||% NA_character_, character(1)),
    title = vapply(raw, function(x) x$title %||% NA_character_, character(1)),
    slug = vapply(raw, function(x) x$slug %||% NA_character_, character(1)),
    end_date = vapply(raw, function(x) x$endDate %||% NA_character_, character(1)),
    volume = vapply(raw, function(x) as.numeric(x$volume %||% NA), numeric(1)),
    active = vapply(raw, function(x) x$active %||% NA, logical(1))
  )
}

# == Context ===================================================================

#' Get polymarket.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
poly_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(poly_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/polymarket.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "polymarket.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# polymarket.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# polymarket.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
