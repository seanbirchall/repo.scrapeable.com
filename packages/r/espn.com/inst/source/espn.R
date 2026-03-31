# espn-com.R
# Self-contained ESPN API client for sports scores, standings, and teams.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: none documented (public API)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.espn_base <- "https://site.api.espn.com/apis/site/v2/sports"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

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
    j <- fi - 1; rox_start <- fi
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

# == Schemas ===================================================================

.schema_scores <- tibble(
  game_id = character(), status = character(), home_team = character(),
  home_score = integer(), away_team = character(), away_score = integer(),
  date = as.POSIXct(character()), venue = character()
)

.schema_teams <- tibble(
  id = character(), name = character(), abbreviation = character(),
  display_name = character(), location = character(), logo = character()
)

.schema_standings <- tibble(
  team = character(), wins = integer(), losses = integer(),
  pct = numeric(), streak = character(), division = character()
)

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
