# api-web-nhle-com.R
# Self-contained NHL API client (api-web.nhle.com).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: be polite

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nhl_base <- "https://api-web.nhle.com/v1"

`%||%` <- function(a, b) if (is.null(a)) b else a

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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

.safe_chr <- function(x) as.character(x %||% NA)
.safe_int <- function(x) as.integer(x %||% NA)
.safe_num <- function(x) as.numeric(x %||% NA)
.safe_default <- function(x) as.character((x$default) %||% NA)

# == Schemas ===================================================================

.schema_standings <- tibble(
  team_abbrev = character(), team_name = character(),
  conference = character(), division = character(),
  games_played = integer(), wins = integer(), losses = integer(),
  ot_losses = integer(), points = integer(), point_pctg = numeric(),
  goal_for = integer(), goal_against = integer(), goal_differential = integer(),
  streak_code = character(), streak_count = integer()
)

.schema_player <- tibble(
  player_id = integer(), first_name = character(), last_name = character(),
  team = character(), position = character(), sweater_number = integer(),
  height_cm = integer(), weight_kg = integer(),
  birth_date = as.Date(character()), birth_country = character()
)

.schema_schedule <- tibble(
  game_id = integer(), date = as.Date(character()),
  home_team = character(), away_team = character(),
  home_score = integer(), away_score = integer(),
  game_state = character(), period = integer()
)

# == Standings =================================================================

#' Fetch current NHL standings
#'
#' Retrieves the full NHL standings table for a given date, including win/loss
#' records, points, goal differentials, and current streaks for all 32 teams.
#'
#' @param date Date string "YYYY-MM-DD" (default: today). Can also be a
#'   \code{Date} object.
#' @return A tibble with one row per team and columns:
#'   \describe{
#'     \item{team_abbrev}{Three-letter team abbreviation (e.g. "TOR", "EDM")}
#'     \item{team_name}{Full team name}
#'     \item{conference}{Conference name ("Eastern" or "Western")}
#'     \item{division}{Division name (e.g. "Atlantic", "Pacific")}
#'     \item{games_played}{Total games played}
#'     \item{wins, losses, ot_losses}{Win/loss/overtime-loss counts}
#'     \item{points}{Standings points (2 per win, 1 per OT loss)}
#'     \item{point_pctg}{Points percentage (0-1 scale)}
#'     \item{goal_for, goal_against, goal_differential}{Goal statistics}
#'     \item{streak_code, streak_count}{Current streak type ("W"/"L"/"OT") and length}
#'   }
#' @examples
#' nhl_standings()
#' nhl_standings("2025-01-15")
#' @seealso [nhl_schedule()], [nhl_scores()], [nhl_player()]
#' @source <https://api-web.nhle.com>
#' @export
nhl_standings <- function(date = Sys.Date()) {
  url <- paste0(.nhl_base, "/standings/", as.character(date))
  raw <- .fetch_json(url)
  s <- raw$standings
  if (is.null(s) || length(s) == 0) return(.schema_standings)

  bind_rows(lapply(s, function(t) {
    tibble(
      team_abbrev     = .safe_chr(t$teamAbbrev$default),
      team_name       = .safe_chr(t$teamName$default),
      conference      = .safe_chr(t$conferenceName),
      division        = .safe_chr(t$divisionName),
      games_played    = .safe_int(t$gamesPlayed),
      wins            = .safe_int(t$wins),
      losses          = .safe_int(t$losses),
      ot_losses       = .safe_int(t$otLosses),
      points          = .safe_int(t$points),
      point_pctg      = .safe_num(t$pointPctg),
      goal_for        = .safe_int(t$goalFor),
      goal_against    = .safe_int(t$goalAgainst),
      goal_differential = .safe_int(t$goalDifferential),
      streak_code     = .safe_chr(t$streakCode),
      streak_count    = .safe_int(t$streakCount)
    )
  }))
}

#' Fetch NHL schedule for a date
#'
#' Returns the game schedule for the week containing the specified date,
#' including matchups, scores (if games have started or finished), game state,
#' and current period.
#'
#' @param date Date string "YYYY-MM-DD" (default: today). Can also be a
#'   \code{Date} object.
#' @return A tibble with one row per game and columns:
#'   \describe{
#'     \item{game_id}{Unique NHL game identifier (integer)}
#'     \item{date}{Game date (Date)}
#'     \item{home_team, away_team}{Three-letter team abbreviations}
#'     \item{home_score, away_score}{Current or final scores (NA if not started)}
#'     \item{game_state}{State code: "FUT" (future), "LIVE", "OFF" (final), "FINAL"}
#'     \item{period}{Current or final period number}
#'   }
#' @examples
#' nhl_schedule()
#' nhl_schedule("2025-02-14")
#' @seealso [nhl_scores()], [nhl_standings()]
#' @source <https://api-web.nhle.com>
#' @export
nhl_schedule <- function(date = Sys.Date()) {
  url <- paste0(.nhl_base, "/schedule/", as.character(date))
  raw <- .fetch_json(url)
  weeks <- raw$gameWeek
  if (is.null(weeks) || length(weeks) == 0) return(.schema_schedule)

  games <- list()
  for (w in weeks) {
    for (g in (w$games %||% list())) {
      games[[length(games) + 1]] <- tibble(
        game_id    = .safe_int(g$id),
        date       = tryCatch(as.Date(.safe_chr(g$gameDate)), error = function(e) NA),
        home_team  = .safe_chr(g$homeTeam$abbrev),
        away_team  = .safe_chr(g$awayTeam$abbrev),
        home_score = .safe_int(g$homeTeam$score),
        away_score = .safe_int(g$awayTeam$score),
        game_state = .safe_chr(g$gameState),
        period     = .safe_int(g$period)
      )
    }
  }
  if (length(games) == 0) return(.schema_schedule)
  bind_rows(games)
}

#' Fetch NHL scores for today
#'
#' Returns live and final scores for all games on a given date. Unlike
#' \code{nhl_schedule()}, this endpoint is optimized for score updates and
#' returns only games for the specified date (not the full week).
#'
#' @param date Date string "YYYY-MM-DD" (default: today). Can also be a
#'   \code{Date} object.
#' @return A tibble with the same columns as \code{\link{nhl_schedule}}: game_id,
#'   date, home_team, away_team, home_score, away_score, game_state, period.
#' @examples
#' nhl_scores()
#' nhl_scores("2025-03-01")
#' @seealso [nhl_schedule()], [nhl_standings()]
#' @source <https://api-web.nhle.com>
#' @export
nhl_scores <- function(date = Sys.Date()) {
  url <- paste0(.nhl_base, "/score/", as.character(date))
  raw <- .fetch_json(url)
  g_list <- raw$games
  if (is.null(g_list) || length(g_list) == 0) return(.schema_schedule)

  bind_rows(lapply(g_list, function(g) {
    tibble(
      game_id    = .safe_int(g$id),
      date       = tryCatch(as.Date(.safe_chr(g$gameDate)), error = function(e) NA),
      home_team  = .safe_chr(g$homeTeam$abbrev),
      away_team  = .safe_chr(g$awayTeam$abbrev),
      home_score = .safe_int(g$homeTeam$score),
      away_score = .safe_int(g$awayTeam$score),
      game_state = .safe_chr(g$gameState),
      period     = .safe_int(g$period)
    )
  }))
}

#' Fetch NHL player profile
#'
#' Retrieves biographical and roster information for a single NHL player,
#' including name, team, position, physical measurements, and birth details.
#'
#' @param id Player ID (integer). For example, 8478402 for Connor McDavid,
#'   8471675 for Sidney Crosby. Player IDs can be found in game data or on
#'   nhl.com player pages.
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{player_id}{NHL player ID (integer)}
#'     \item{first_name, last_name}{Player name}
#'     \item{team}{Current team abbreviation (e.g. "EDM")}
#'     \item{position}{Position code: "C", "L", "R", "D", "G"}
#'     \item{sweater_number}{Jersey number}
#'     \item{height_cm, weight_kg}{Physical measurements}
#'     \item{birth_date}{Date of birth (Date)}
#'     \item{birth_country}{Three-letter country code (e.g. "CAN", "USA")}
#'   }
#' @examples
#' nhl_player(8478402)
#' @seealso [nhl_standings()], [nhl_scores()]
#' @source <https://api-web.nhle.com>
#' @export
nhl_player <- function(id) {
  url <- paste0(.nhl_base, "/player/", id, "/landing")
  raw <- .fetch_json(url)
  if (is.null(raw$playerId)) return(.schema_player)
  tibble(
    player_id      = .safe_int(raw$playerId),
    first_name     = .safe_default(raw$firstName),
    last_name      = .safe_default(raw$lastName),
    team           = .safe_chr(raw$currentTeamAbbrev),
    position       = .safe_chr(raw$position),
    sweater_number = .safe_int(raw$sweaterNumber),
    height_cm      = .safe_int(raw$heightInCentimeters),
    weight_kg      = .safe_int(raw$weightInKilograms),
    birth_date     = tryCatch(as.Date(.safe_chr(raw$birthDate)), error = function(e) NA),
    birth_country  = .safe_chr(raw$birthCountry)
  )
}

#' Get api-web-nhle-com client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the NHL API client. Designed for LLM tool-use: shows each
#' function's purpose, parameters, and return type without implementation
#' details. Use \code{function_name} (no parens) to see full source, or
#' \code{?function_name} for rendered help.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' nhl_context()
#' @export
nhl_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(nhl_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/api-web-nhle-com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "api-web-nhle-com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# api-web-nhle-com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# api-web-nhle-com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
