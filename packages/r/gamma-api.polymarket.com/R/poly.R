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
#' Fetches a list of prediction markets from the Polymarket Gamma API.
#' Returns market questions, end dates, trading volume, and active status.
#'
#' @param limit Number of markets to return (default 25, integer).
#' @param active Logical; if TRUE (default), return only active markets.
#'   Set to FALSE for all markets including resolved ones.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Unique market identifier}
#'     \item{question}{The prediction market question text}
#'     \item{end_date}{Market resolution/end date (ISO 8601 string)}
#'     \item{volume}{Total trading volume in USD}
#'     \item{active}{Whether the market is currently active (logical)}
#'   }
#' @examples
#' poly_markets(limit = 5)
#' poly_markets(limit = 10, active = FALSE)
#' @seealso [poly_market()], [poly_events()], [poly_context()]
#' @source <https://gamma-api.polymarket.com>
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
#' Fetches detailed metadata for a single Polymarket prediction market,
#' including its full description, resolution date, volume, and status.
#'
#' @param market_id Market ID (numeric string) or URL slug. IDs are returned
#'   by \code{\link{poly_markets}}.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Market identifier}
#'     \item{question}{Market question text}
#'     \item{description}{Full market description and resolution criteria}
#'     \item{end_date}{Resolution/end date (ISO 8601 string)}
#'     \item{volume}{Total trading volume in USD}
#'     \item{active}{Whether the market is currently active (logical)}
#'   }
#' @examples
#' poly_market("12")
#' @seealso [poly_markets()], [poly_events()], [poly_context()]
#' @source <https://gamma-api.polymarket.com>
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

#' Get Polymarket events (groups of markets)
#'
#' Fetches events from the Polymarket Gamma API. Events are higher-level
#' groupings that contain one or more related prediction markets (e.g.,
#' an election event may contain multiple outcome markets).
#'
#' @param limit Number of events to return (default 25, integer).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Unique event identifier}
#'     \item{title}{Event title}
#'     \item{slug}{URL slug for the event}
#'     \item{end_date}{Event end date (ISO 8601 string)}
#'     \item{volume}{Total trading volume across all markets in the event}
#'     \item{active}{Whether the event is currently active (logical)}
#'   }
#' @examples
#' poly_events(limit = 5)
#' @seealso [poly_markets()], [poly_market()], [poly_context()]
#' @source <https://gamma-api.polymarket.com>
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

#' Get gamma-api.polymarket.com client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the Polymarket prediction markets client. Designed for
#' LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' poly_context()
#' @export
poly_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(poly_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gamma-api.polymarket.com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gamma-api.polymarket.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gamma-api.polymarket.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gamma-api.polymarket.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
