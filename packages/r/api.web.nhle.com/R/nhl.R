
#' Fetch current NHL standings
#'
#' @param date Date string "YYYY-MM-DD" (default: today)
#' @return tibble: team_abbrev, team_name, conference, division, games_played,
#'   wins, losses, ot_losses, points, point_pctg, goal_for, goal_against,
#'   goal_differential, streak_code, streak_count
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
#' @param date Date string "YYYY-MM-DD" (default: today)
#' @return tibble: game_id, date, home_team, away_team, home_score,
#'   away_score, game_state, period
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
#' @param date Date string "YYYY-MM-DD" (default: today)
#' @return tibble: same as nhl_schedule
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
#' @param id Player ID (integer, e.g. 8478402 for Connor McDavid)
#' @return tibble: player_id, first_name, last_name, team, position,
#'   sweater_number, height_cm, weight_kg, birth_date, birth_country
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

#' NHL API context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
nhl_context <- function() {
  .build_context("api.web.nhle.com", header_lines = c(
    "# api.web.nhle.com - NHL API Client",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limits: be polite",
    "#",
    "# Notable player IDs: 8478402 (McDavid), 8479318 (Matthews),",
    "#   8477934 (Draisaitl), 8478483 (MacKinnon), 8480800 (Makar)",
    "# nhl_standings() requires a date - /standings/now may be empty"
  ))
}
