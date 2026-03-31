# == Public functions ==========================================================

#' Get current scoreboard for a sport/league
#'
#' @param sport Sport: "football", "basketball", "baseball", "hockey", "soccer"
#' @param league League: "nfl", "nba", "mlb", "nhl", "eng.1" (Premier League)
#' @param dates Optional date filter "YYYYMMDD" (default: current)
#' @return tibble: game_id, status, home_team, home_score, away_team, away_score, date, venue
#' @export
espn_scores <- function(sport = "football", league = "nfl", dates = NULL) {
  url <- sprintf("%s/%s/%s/scoreboard", .espn_base, sport, league)
  if (!is.null(dates)) url <- paste0(url, "?dates=", dates)
  raw <- .fetch_json(url)
  events <- raw$events
  if (is.null(events) || length(events) == 0) return(.schema_scores)

  rows <- lapply(events, function(ev) {
    comps <- ev$competitions[[1]]
    competitors <- comps$competitors
    home <- NULL; away <- NULL
    for (c in competitors) {
      if (identical(c$homeAway, "home")) home <- c
      if (identical(c$homeAway, "away")) away <- c
    }
    data.frame(
      game_id    = as.character(ev$id %||% NA_character_),
      status     = as.character(ev$status$type$description %||% NA_character_),
      home_team  = as.character(home$team$displayName %||% NA_character_),
      home_score = as.integer(home$score %||% NA_integer_),
      away_team  = as.character(away$team$displayName %||% NA_character_),
      away_score = as.integer(away$score %||% NA_integer_),
      date       = as.POSIXct(ev$date %||% NA_character_, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
      venue      = as.character(comps$venue$fullName %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })
  as_tibble(do.call(rbind, rows))
}

#' Get team standings for a sport/league
#'
#' @param sport Sport: "football", "basketball", "baseball", "hockey", "soccer"
#' @param league League: "nfl", "nba", "mlb", "nhl", "eng.1"
#' @return tibble: team, wins, losses, pct, streak, division
#' @export
espn_standings <- function(sport = "football", league = "nfl") {
  url <- sprintf("https://site.api.espn.com/apis/v2/sports/%s/%s/standings", sport, league)
  raw <- .fetch_json(url)
  children <- raw$children
  if (is.null(children) || length(children) == 0) return(.schema_standings)

  rows <- list()
  for (grp in children) {
    div_name <- grp$name %||% NA_character_
    standings <- grp$standings$entries
    if (is.null(standings)) next
    for (entry in standings) {
      team_name <- entry$team$displayName %||% NA_character_
      stats <- entry$stats
      stat_map <- list()
      if (!is.null(stats)) {
        for (s in stats) {
          stat_map[[s$name]] <- s$value
        }
      }
      rows[[length(rows) + 1]] <- data.frame(
        team     = as.character(team_name),
        wins     = as.integer(stat_map$wins %||% NA_integer_),
        losses   = as.integer(stat_map$losses %||% NA_integer_),
        pct      = as.numeric(stat_map$winPercent %||% stat_map$winPct %||% NA_real_),
        streak   = as.character(stat_map$streak %||% NA_character_),
        division = as.character(div_name),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(.schema_standings)
  as_tibble(do.call(rbind, rows))
}

#' Get teams for a sport/league
#'
#' @param sport Sport: "football", "basketball", "baseball", "hockey", "soccer"
#' @param league League: "nfl", "nba", "mlb", "nhl", "eng.1"
#' @return tibble: id, name, abbreviation, display_name, location, logo
#' @export
espn_teams <- function(sport = "football", league = "nfl") {
  url <- sprintf("%s/%s/%s/teams?limit=100", .espn_base, sport, league)
  raw <- .fetch_json(url)
  teams_list <- raw$sports[[1]]$leagues[[1]]$teams
  if (is.null(teams_list) || length(teams_list) == 0) return(.schema_teams)

  rows <- lapply(teams_list, function(t) {
    tm <- t$team
    data.frame(
      id           = as.character(tm$id %||% NA_character_),
      name         = as.character(tm$name %||% NA_character_),
      abbreviation = as.character(tm$abbreviation %||% NA_character_),
      display_name = as.character(tm$displayName %||% NA_character_),
      location     = as.character(tm$location %||% NA_character_),
      logo         = as.character(tm$logos[[1]]$href %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })
  as_tibble(do.call(rbind, rows))
}

#' Show ESPN package context for LLM use
#'
#' @return Invisible string with full context
#' @export
espn_context <- function() {
  .build_context("espn.com", header_lines = c(
    "# espn.com -- ESPN public sports API",
    "# Deps: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limits: none documented",
    "#",
    "# Sport/league combos:",
    "#   football/nfl, basketball/nba, baseball/mlb, hockey/nhl,",
    "#   soccer/eng.1 (Premier League), soccer/usa.1 (MLS)"
  ))
}
