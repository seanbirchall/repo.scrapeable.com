# site.api.espn.com.R - Self-contained site.api.espn.com client




.ua <- "support@scrapeable.com"
.base <- "https://site.api.espn.com/apis/site/v2/sports"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


#' Get ESPN scores for a sport
#'
#' Fetches the current or most recent scoreboard for a given sport and league
#' from the ESPN public API. Returns one row per game event with scores and
#' completion status.
#'
#' @param sport Character. Sport category: \code{"football"}, \code{"basketball"},
#'   \code{"baseball"}, \code{"hockey"}, \code{"soccer"}.
#' @param league Character. League within the sport: \code{"nfl"}, \code{"nba"},
#'   \code{"mlb"}, \code{"nhl"}, \code{"college-football"}, \code{"mens-college-basketball"},
#'   \code{"wnba"}, \code{"eng.1"} (Premier League).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. ESPN event identifier.}
#'     \item{name}{Character. Full event name (e.g. "Seattle Seahawks at New England Patriots").}
#'     \item{short_name}{Character. Abbreviated matchup (e.g. "SEA VS NE").}
#'     \item{date}{Character. ISO 8601 event date/time.}
#'     \item{status}{Character. Game status description (e.g. "Final", "In Progress", "Scheduled").}
#'   }
#' @export
#' @examples
#' \dontrun{
#' espn_scores("football", "nfl")
#' espn_scores("basketball", "nba")
#' espn_scores("baseball", "mlb")
#' }
espn_scores <- function(sport = "football", league = "nfl") {
  url <- sprintf("%s/%s/%s/scoreboard", .base, sport, league)
  raw <- .fetch_json(url)
  events <- raw$events
  if (length(events) == 0) return(tibble::tibble(id = character(), name = character(), date = character()))
  tibble::tibble(
    id = vapply(events, function(x) x$id %||% NA_character_, character(1)),
    name = vapply(events, function(x) x$name %||% NA_character_, character(1)),
    short_name = vapply(events, function(x) x$shortName %||% NA_character_, character(1)),
    date = vapply(events, function(x) x$date %||% NA_character_, character(1)),
    status = vapply(events, function(x) {
      tryCatch(x$status$type$description %||% NA_character_, error = function(e) NA_character_)
    }, character(1))
  )
}

#' Get ESPN standings
#'
#' Retrieves current league standings grouped by division or conference.
#' Returns win/loss records for each team.
#'
#' @param sport Character. Sport category: \code{"football"}, \code{"basketball"},
#'   \code{"baseball"}, \code{"hockey"}.
#' @param league Character. League: \code{"nfl"}, \code{"nba"}, \code{"mlb"},
#'   \code{"nhl"}, \code{"college-football"}, \code{"mens-college-basketball"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{team}{Character. Full team display name.}
#'     \item{wins}{Integer. Number of wins.}
#'     \item{losses}{Integer. Number of losses.}
#'     \item{group}{Character. Division or conference name.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' espn_standings("football", "nfl")
#' espn_standings("basketball", "nba")
#' }
espn_standings <- function(sport = "football", league = "nfl") {
  url <- sprintf("%s/%s/%s/standings", .base, sport, league)
  raw <- .fetch_json(url)
  children <- raw$children
  if (length(children) == 0) return(tibble::tibble(team = character(), wins = integer(), losses = integer()))
  rows <- list()
  for (group in children) {
    standings <- group$standings$entries
    if (is.null(standings)) next
    for (entry in standings) {
      team_name <- tryCatch(entry$team$displayName, error = function(e) NA_character_)
      stats_list <- entry$stats
      wins <- losses <- NA_integer_
      if (!is.null(stats_list)) {
        for (s in stats_list) {
          if (!is.null(s$name) && s$name == "wins") wins <- as.integer(s$value)
          if (!is.null(s$name) && s$name == "losses") losses <- as.integer(s$value)
        }
      }
      rows[[length(rows) + 1]] <- tibble::tibble(
        team = team_name %||% NA_character_,
        wins = wins, losses = losses,
        group = group$name %||% NA_character_
      )
    }
  }
  if (length(rows) == 0) return(tibble::tibble(team = character(), wins = integer(), losses = integer()))
  dplyr::bind_rows(rows)
}

#' Get ESPN teams
#'
#' Lists all teams in a given sport and league with their identifiers
#' and location metadata.
#'
#' @param sport Character. Sport category: \code{"football"}, \code{"basketball"},
#'   \code{"baseball"}, \code{"hockey"}.
#' @param league Character. League: \code{"nfl"}, \code{"nba"}, \code{"mlb"},
#'   \code{"nhl"}, \code{"college-football"}, \code{"mens-college-basketball"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. ESPN team identifier.}
#'     \item{name}{Character. Full team display name (e.g. "Arizona Diamondbacks").}
#'     \item{abbreviation}{Character. Team abbreviation (e.g. "ARI").}
#'     \item{location}{Character. Team city or region (e.g. "Arizona").}
#'   }
#' @export
#' @examples
#' \dontrun{
#' espn_teams("baseball", "mlb")
#' espn_teams("hockey", "nhl")
#' }
espn_teams <- function(sport = "football", league = "nfl") {
  url <- sprintf("%s/%s/%s/teams", .base, sport, league)
  raw <- .fetch_json(url)
  sports <- raw$sports
  if (length(sports) == 0) return(tibble::tibble(id = character(), name = character()))
  teams <- list()
  for (s in sports) {
    for (lg in s$leagues) {
      for (t in lg$teams) {
        tm <- t$team
        teams[[length(teams) + 1]] <- tibble::tibble(
          id = tm$id %||% NA_character_,
          name = tm$displayName %||% NA_character_,
          abbreviation = tm$abbreviation %||% NA_character_,
          location = tm$location %||% NA_character_
        )
      }
    }
  }
  if (length(teams) == 0) return(tibble::tibble(id = character(), name = character()))
  dplyr::bind_rows(teams)
}

# == Context ===================================================================

#' Get espn.com client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
espn_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(espn_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/espn.com.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "espn.com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# espn.com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# espn.com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
