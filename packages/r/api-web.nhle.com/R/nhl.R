# api-web.nhle.com.R - Self-contained api-web.nhle.com client




.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://api-web.nhle.com/v1"

#' Get current NHL standings
#'
#' Retrieves league standings for all 32 NHL teams on a given date,
#' including win/loss records, points, and goal differentials.
#'
#' @param date Character. Date in \code{"YYYY-MM-DD"} format. Defaults to
#'   today's date (\code{Sys.Date()}). Use past dates for historical standings,
#'   e.g. \code{"2024-03-15"}.
#' @return A tibble with one row per team and 8 columns:
#' \describe{
#'   \item{team}{Character. Full team name (e.g. \code{"Colorado Avalanche"}).}
#'   \item{abbrev}{Character. Three-letter team abbreviation (e.g. \code{"COL"}).}
#'   \item{wins}{Integer. Number of wins.}
#'   \item{losses}{Integer. Number of regulation losses.}
#'   \item{ot_losses}{Integer. Number of overtime/shootout losses.}
#'   \item{points}{Integer. Total standings points (2 * wins + ot_losses).}
#'   \item{goals_for}{Integer. Total goals scored.}
#'   \item{goals_against}{Integer. Total goals allowed.}
#' }
#' @examples
#' nhl_standings()
#' nhl_standings("2025-01-15")
#' @export
nhl_standings <- function(date = format(Sys.Date())) {
  url <- sprintf("%s/standings/%s", .base, date)
  raw <- .fetch_json(url)
  standings <- raw$standings
  if (length(standings) == 0) return(tibble::tibble(team = character(), wins = integer()))
  tibble::tibble(
    team = vapply(standings, function(x) x$teamName$default %||% NA_character_, character(1)),
    abbrev = vapply(standings, function(x) x$teamAbbrev$default %||% NA_character_, character(1)),
    wins = vapply(standings, function(x) x$wins %||% NA_integer_, integer(1)),
    losses = vapply(standings, function(x) x$losses %||% NA_integer_, integer(1)),
    ot_losses = vapply(standings, function(x) x$otLosses %||% NA_integer_, integer(1)),
    points = vapply(standings, function(x) x$points %||% NA_integer_, integer(1)),
    goals_for = vapply(standings, function(x) x$goalFor %||% NA_integer_, integer(1)),
    goals_against = vapply(standings, function(x) x$goalAgainst %||% NA_integer_, integer(1))
  )
}

#' Get NHL game schedule
#'
#' Retrieves the weekly game schedule centered on the given date. Returns
#' all games across the surrounding week, including game IDs, matchups,
#' and game types.
#'
#' @param date Character. Date in \code{"YYYY-MM-DD"} format. Defaults to
#'   today's date. The API returns the full week of games containing this date.
#' @return A tibble with one row per scheduled game and 5 columns:
#' \describe{
#'   \item{game_id}{Integer. Unique NHL game identifier (e.g. \code{2025021207}).}
#'   \item{date}{Character. Game date in \code{"YYYY-MM-DD"} format.}
#'   \item{home_team}{Character. Three-letter abbreviation of the home team (e.g. \code{"NYR"}).}
#'   \item{away_team}{Character. Three-letter abbreviation of the away team (e.g. \code{"DET"}).}
#'   \item{game_type}{Integer. Game type code: \code{1} = preseason, \code{2} = regular season, \code{3} = playoffs.}
#' }
#' @examples
#' nhl_schedule()
#' nhl_schedule("2025-02-01")
#' @export
nhl_schedule <- function(date = format(Sys.Date())) {
  url <- sprintf("%s/schedule/%s", .base, date)
  raw <- .fetch_json(url)
  days <- raw$gameWeek
  if (length(days) == 0) return(tibble::tibble(game_id = integer(), home_team = character(), away_team = character()))
  rows <- list()
  for (day in days) {
    for (game in day$games) {
      rows[[length(rows) + 1]] <- tibble::tibble(
        game_id = game$id %||% NA_integer_,
        date = day$date %||% NA_character_,
        home_team = game$homeTeam$abbrev %||% NA_character_,
        away_team = game$awayTeam$abbrev %||% NA_character_,
        game_type = as.integer(game$gameType %||% NA)
      )
    }
  }
  if (length(rows) == 0) return(tibble::tibble(game_id = integer(), home_team = character(), away_team = character()))
  dplyr::bind_rows(rows)
}

# == Context ===================================================================

#' Get api-web.nhle.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
nhl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nhl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api-web.nhle.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api-web.nhle.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api-web.nhle.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api-web.nhle.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
