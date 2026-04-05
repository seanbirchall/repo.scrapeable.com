# football-data.co.uk.R - Self-contained football-data.co.uk client




.ua <- "support@scrapeable.com"

.fetch_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}


#' Get football match results from football-data.co.uk
#'
#' Downloads historical match results for a given league and season from
#' football-data.co.uk. Covers major European leagues back to the 1990s.
#'
#' @param league Character. League code (e.g. \code{"E0"} for English Premier
#'   League, \code{"D1"} for Bundesliga). Use \code{fbdata_leagues()} for
#'   all available codes.
#' @param season Character. Season code as four digits -- the last two digits
#'   of the start and end years (e.g. \code{"2324"} for 2023-2024).
#' @return A tibble with columns (when available):
#'   \describe{
#'     \item{Div}{Character. Division/league code.}
#'     \item{Date}{Character. Match date (DD/MM/YYYY).}
#'     \item{HomeTeam}{Character. Home team name.}
#'     \item{AwayTeam}{Character. Away team name.}
#'     \item{FTHG}{Integer. Full-time home goals.}
#'     \item{FTAG}{Integer. Full-time away goals.}
#'     \item{FTR}{Character. Full-time result: \code{"H"} (home win),
#'       \code{"A"} (away win), \code{"D"} (draw).}
#'   }
#' @examples
#' fbdata_results("E0", "2324")
#' fbdata_results("D1", "2324")
#' @export
fbdata_results <- function(league = "E0", season = "2324") {
  url <- sprintf("https://www.football-data.co.uk/mmz4281/%s/%s.csv", season, league)
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  df <- tryCatch(utils::read.csv(tmp, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (nrow(df) == 0) return(tibble::tibble(date = character(), home = character(), away = character()))
  cols <- intersect(c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR"), names(df))
  tibble::as_tibble(df[, cols, drop = FALSE])
}

#' Compute league standings from match results
#'
#' Downloads all match results for a league/season and computes the full
#' league table. Sorted by points descending, then goal difference.
#'
#' @param league Character. League code (e.g. \code{"E0"}). See
#'   \code{fbdata_leagues()}.
#' @param season Character. Season code (e.g. \code{"2324"} for 2023-2024).
#' @return A tibble with columns:
#'   \describe{
#'     \item{team}{Character. Team name.}
#'     \item{played}{Integer. Total matches played.}
#'     \item{won}{Integer. Matches won.}
#'     \item{drawn}{Integer. Matches drawn.}
#'     \item{lost}{Integer. Matches lost.}
#'     \item{goals_for}{Integer. Total goals scored.}
#'     \item{goals_against}{Integer. Total goals conceded.}
#'     \item{goal_diff}{Integer. Goal difference (goals_for - goals_against).}
#'     \item{points}{Integer. League points (3 per win, 1 per draw).}
#'   }
#' @examples
#' fbdata_standings("E0", "2324")
#' fbdata_standings("D1", "2324")
#' @export
fbdata_standings <- function(league = "E0", season = "2324") {
  results <- fbdata_results(league = league, season = season)
  if (nrow(results) == 0 || !all(c("HomeTeam", "AwayTeam", "FTHG", "FTAG") %in% names(results))) {
    return(tibble(team = character(), played = integer(), won = integer(),
                  drawn = integer(), lost = integer(), goals_for = integer(),
                  goals_against = integer(), goal_diff = integer(), points = integer()))
  }

  results <- results |> mutate(FTHG = as.integer(FTHG), FTAG = as.integer(FTAG))

  home <- results |>
    group_by(team = HomeTeam) |>
    summarise(p = n(), w = sum(FTHG > FTAG), d = sum(FTHG == FTAG),
              l = sum(FTHG < FTAG), gf = sum(FTHG), ga = sum(FTAG), .groups = "drop")
  away <- results |>
    group_by(team = AwayTeam) |>
    summarise(p = n(), w = sum(FTAG > FTHG), d = sum(FTAG == FTHG),
              l = sum(FTAG < FTHG), gf = sum(FTAG), ga = sum(FTHG), .groups = "drop")

  bind_rows(home, away) |>
    group_by(team) |>
    summarise(played = sum(p), won = sum(w), drawn = sum(d), lost = sum(l),
              goals_for = sum(gf), goals_against = sum(ga), .groups = "drop") |>
    mutate(goal_diff = goals_for - goals_against,
           points = won * 3L + drawn) |>
    arrange(desc(points), desc(goal_diff))
}

#' Get match results for a specific team
#'
#' Filters league results to only matches involving the specified team
#' (home or away). Team name is matched case-insensitively using partial
#' matching.
#'
#' @param team Character. Team name or partial name (e.g. \code{"Arsenal"},
#'   \code{"Man City"}, \code{"Liverpool"}).
#' @param league Character. League code (default \code{"E0"}). See
#'   \code{fbdata_leagues()}.
#' @param season Character. Season code (default \code{"2324"}).
#' @return A tibble with the same columns as \code{fbdata_results()},
#'   filtered to matches involving the specified team.
#' @examples
#' fbdata_team("Arsenal")
#' fbdata_team("Bayern", league = "D1")
#' @export
fbdata_team <- function(team, league = "E0", season = "2324") {
  results <- fbdata_results(league = league, season = season)
  if (nrow(results) == 0) return(results)
  results[grepl(team, results$HomeTeam, ignore.case = TRUE) |
          grepl(team, results$AwayTeam, ignore.case = TRUE), ]
}

#' List available leagues and their codes
#'
#' Returns a reference tibble of all league codes supported by
#' football-data.co.uk, covering major European leagues. Use these
#' codes with \code{fbdata_results()}, \code{fbdata_standings()}, etc.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{Character. League code for API use (e.g. \code{"E0"}).}
#'     \item{league}{Character. Full league name (e.g. \code{"Premier League"}).}
#'     \item{country}{Character. Country name.}
#'   }
#' @examples
#' fbdata_leagues()
#' @export
fbdata_leagues <- function() {
  tibble(
    code = c("E0", "E1", "E2", "E3", "EC",
             "SC0", "SC1", "SC2", "SC3",
             "D1", "D2", "I1", "I2",
             "SP1", "SP2", "F1", "F2",
             "N1", "B1", "P1", "T1", "G1"),
    league = c("Premier League", "Championship", "League One", "League Two", "Conference",
               "Scottish Premiership", "Scottish Championship", "Scottish League One", "Scottish League Two",
               "Bundesliga", "2. Bundesliga", "Serie A", "Serie B",
               "La Liga", "Segunda Division", "Ligue 1", "Ligue 2",
               "Eredivisie", "Jupiler League", "Liga Portugal", "Super Lig", "Super League Greece"),
    country = c(rep("England", 5), rep("Scotland", 4), rep("Germany", 2),
                rep("Italy", 2), rep("Spain", 2), rep("France", 2),
                "Netherlands", "Belgium", "Portugal", "Turkey", "Greece")
  )
}

# == Context ===================================================================

#' Get football-data.co.uk client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
fbdata_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fbdata_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/football-data.co.uk.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "football-data.co.uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# football-data.co.uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# football-data.co.uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
