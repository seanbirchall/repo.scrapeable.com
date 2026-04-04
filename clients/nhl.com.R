# api-web.nhle.com.R - Self-contained api-web.nhle.com client

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)



.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


.base <- "https://api-web.nhle.com/v1"

#' Get NHL standings
#'
#' Fetches current or historical NHL team standings from the official
#' NHL Stats API. Includes wins, losses, overtime losses, points,
#' and goal differential.
#'
#' @param date Date in YYYY-MM-DD format (default: today's date).
#'   Use historical dates for past standings.
#' @return A tibble with columns:
#'   \describe{
#'     \item{team}{Full team name (character)}
#'     \item{abbrev}{Three-letter team abbreviation (character)}
#'     \item{wins}{Number of wins (integer)}
#'     \item{losses}{Number of regulation losses (integer)}
#'     \item{ot_losses}{Number of overtime/shootout losses (integer)}
#'     \item{points}{Standings points (integer)}
#'     \item{goals_for}{Total goals scored (integer)}
#'     \item{goals_against}{Total goals allowed (integer)}
#'   }
#' @examples
#' nhl_standings()
#' nhl_standings("2025-01-15")
#' @seealso [nhl_schedule()], [nhl_context()]
#' @source <https://api-web.nhle.com/v1>
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

#' Get NHL schedule
#'
#' Fetches the NHL game schedule for a given week, centered on the
#' specified date. Returns all games with home/away teams.
#'
#' @param date Date in YYYY-MM-DD format (default: today's date)
#' @return A tibble with columns:
#'   \describe{
#'     \item{game_id}{Unique game identifier (integer)}
#'     \item{date}{Game date in YYYY-MM-DD format (character)}
#'     \item{home_team}{Home team abbreviation (character)}
#'     \item{away_team}{Away team abbreviation (character)}
#'     \item{game_type}{Game type code: 1=preseason, 2=regular, 3=playoff (integer)}
#'   }
#' @examples
#' nhl_schedule()
#' nhl_schedule("2025-03-15")
#' @seealso [nhl_standings()], [nhl_context()]
#' @source <https://api-web.nhle.com/v1>
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

#' Get nhl.com client context for LLM use
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
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/nhl.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "nhl.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# nhl.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# nhl.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
