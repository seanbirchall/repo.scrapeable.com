# steamspy.com.R - Self-contained steamspy.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)


# steamspy-com.R
# Self-contained SteamSpy API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: 4 requests per second


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.steam_base <- "https://steamspy.com/api.php"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

# -- Parser for named-list (top100/genre) responses ---------------------------

.parse_app_list <- function(raw) {
  if (is.null(raw) || length(raw) == 0) return(.schema_app)
  rows <- lapply(raw, function(x) {
    tibble(
      appid          = as.integer(x$appid %||% NA),
      name           = as.character(x$name %||% NA),
      developer      = as.character(x$developer %||% NA),
      publisher      = as.character(x$publisher %||% NA),
      positive       = as.integer(x$positive %||% NA),
      negative       = as.integer(x$negative %||% NA),
      owners         = as.character(x$owners %||% NA),
      average_forever = as.integer(x$average_forever %||% NA),
      average_2weeks = as.integer(x$average_2weeks %||% NA),
      price          = as.integer(x$price %||% NA),
      initialprice   = as.integer(x$initialprice %||% NA),
      discount       = as.integer(x$discount %||% NA),
      ccu            = as.integer(x$ccu %||% NA)
    )
  })
  bind_rows(rows)
}

# == Schemas ===================================================================

.schema_app <- tibble(
  appid = integer(), name = character(), developer = character(),
  publisher = character(), positive = integer(), negative = integer(),
  owners = character(), average_forever = integer(),
  average_2weeks = integer(), price = integer(),
  initialprice = integer(), discount = integer(), ccu = integer()
)

.schema_app_detail <- tibble(
  appid = integer(), name = character(), developer = character(),
  publisher = character(), positive = integer(), negative = integer(),
  owners = character(), average_forever = integer(),
  average_2weeks = integer(), median_forever = integer(),
  median_2weeks = integer(), price = integer(), initialprice = integer(),
  discount = integer(), ccu = integer(), genre = character(),
  languages = character()
)


# == Public functions ==========================================================

#' Get detailed information for a Steam app
#'
#' Retrieves comprehensive metadata and player statistics for a single Steam
#' application from the SteamSpy API. Includes review counts, estimated
#' ownership ranges, playtime statistics, pricing, genre tags, and supported
#' languages.
#'
#' @param appid Integer. Steam application ID (e.g. \code{730} for CS:GO,
#'   \code{570} for Dota 2, \code{440} for Team Fortress 2). Find app IDs on
#'   \url{https://store.steampowered.com}.
#' @return A single-row tibble with 17 columns:
#' \describe{
#'   \item{appid}{Integer. Steam application identifier.}
#'   \item{name}{Character. Game title.}
#'   \item{developer}{Character. Developer studio name.}
#'   \item{publisher}{Character. Publisher name.}
#'   \item{positive}{Integer. Count of positive Steam reviews.}
#'   \item{negative}{Integer. Count of negative Steam reviews.}
#'   \item{owners}{Character. Estimated ownership range (e.g. "10,000,000 .. 20,000,000").}
#'   \item{average_forever}{Integer. Average total playtime in minutes (all time).}
#'   \item{average_2weeks}{Integer. Average playtime in minutes over last 2 weeks.}
#'   \item{median_forever}{Integer. Median total playtime in minutes (all time).}
#'   \item{median_2weeks}{Integer. Median playtime in minutes over last 2 weeks.}
#'   \item{price}{Integer. Current price in cents (e.g. 999 = $9.99).}
#'   \item{initialprice}{Integer. Original price in cents before any discount.}
#'   \item{discount}{Integer. Current discount percentage (0-100).}
#'   \item{ccu}{Integer. Peak concurrent users in the last 24 hours.}
#'   \item{genre}{Character. Comma-separated genre tags.}
#'   \item{languages}{Character. Comma-separated supported languages.}
#' }
#' @examples
#' \dontrun{
#' steam_app(730)          # Counter-Strike 2
#' steam_app(570)          # Dota 2
#' }
#' @export
steam_app <- function(appid) {
  url <- sprintf("%s?request=appdetails&appid=%s", .steam_base, appid)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_app_detail)

  tibble(
    appid          = as.integer(raw$appid %||% NA),
    name           = as.character(raw$name %||% NA),
    developer      = as.character(raw$developer %||% NA),
    publisher      = as.character(raw$publisher %||% NA),
    positive       = as.integer(raw$positive %||% NA),
    negative       = as.integer(raw$negative %||% NA),
    owners         = as.character(raw$owners %||% NA),
    average_forever = as.integer(raw$average_forever %||% NA),
    average_2weeks = as.integer(raw$average_2weeks %||% NA),
    median_forever = as.integer(raw$median_forever %||% NA),
    median_2weeks  = as.integer(raw$median_2weeks %||% NA),
    price          = as.integer(raw$price %||% NA),
    initialprice   = as.integer(raw$initialprice %||% NA),
    discount       = as.integer(raw$discount %||% NA),
    ccu            = as.integer(raw$ccu %||% NA),
    genre          = as.character(raw$genre %||% NA),
    languages      = as.character(raw$languages %||% NA)
  )
}

#' Get top 100 games by current players in last 2 weeks
#'
#' Returns the top 100 most-played Steam games ranked by concurrent users
#' over the past two weeks. Useful for tracking trending games, market
#' analysis, and identifying popular titles.
#'
#' @return A tibble with up to 100 rows and 13 columns:
#' \describe{
#'   \item{appid}{Integer. Steam application identifier.}
#'   \item{name}{Character. Game title.}
#'   \item{developer}{Character. Developer studio name.}
#'   \item{publisher}{Character. Publisher name.}
#'   \item{positive}{Integer. Positive review count.}
#'   \item{negative}{Integer. Negative review count.}
#'   \item{owners}{Character. Estimated ownership range.}
#'   \item{average_forever}{Integer. Average total playtime in minutes.}
#'   \item{average_2weeks}{Integer. Average playtime (last 2 weeks) in minutes.}
#'   \item{price}{Integer. Current price in cents.}
#'   \item{initialprice}{Integer. Original price in cents.}
#'   \item{discount}{Integer. Current discount percentage.}
#'   \item{ccu}{Integer. Peak concurrent users (last 24h).}
#' }
#' @examples
#' \dontrun{
#' steam_top100()
#' }
#' @export
steam_top100 <- function() {
  url <- sprintf("%s?request=top100in2weeks", .steam_base)
  raw <- .fetch_json(url)
  .parse_app_list(raw)
}

#' Get games by genre
#'
#' Retrieves Steam games filtered by genre tag. Returns summary statistics
#' for each game including ownership estimates, playtime, pricing, and
#' review counts. Note: the SteamSpy API may return an empty result for
#' some genre queries due to rate limiting or data availability.
#'
#' @param genre Character. Genre name to filter by. Common values include
#'   \code{"Action"}, \code{"RPG"}, \code{"Strategy"}, \code{"Indie"},
#'   \code{"Adventure"}, \code{"Simulation"}, \code{"Racing"}, \code{"Sports"},
#'   \code{"Puzzle"}, \code{"Casual"}.
#' @return A tibble with 13 columns (same schema as \code{\link{steam_top100}}):
#'   appid, name, developer, publisher, positive, negative, owners,
#'   average_forever, average_2weeks, price, initialprice, discount, ccu.
#'   Returns an empty tibble with correct column types if no results found.
#' @examples
#' \dontrun{
#' steam_genre("RPG")
#' steam_genre("Indie")
#' }
#' @seealso \code{\link{steam_top100}} for the most-played games across all genres.
#' @export
steam_genre <- function(genre) {
  url <- sprintf("%s?request=genre&genre=%s", .steam_base,
                 utils::URLencode(genre, reserved = TRUE))
  raw <- .fetch_json(url)
  .parse_app_list(raw)
}

# == Context ===================================================================

#' Get steamspy.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
steam_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(steam_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/steamspy.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "steamspy.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# steamspy.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# steamspy.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
