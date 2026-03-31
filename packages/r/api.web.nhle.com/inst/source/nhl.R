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
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
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
