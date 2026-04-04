



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
#' @param sport Sport ("football", "basketball", "baseball", "hockey")
#' @param league League ("nfl", "nba", "mlb", "nhl")
#' @return tibble of recent scores
#' @export
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
#' @param sport Sport ("football", "basketball", "baseball", "hockey")
#' @param league League ("nfl", "nba", "mlb", "nhl")
#' @return tibble of standings
#' @export
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
#' @param sport Sport ("football", "basketball", "baseball", "hockey")
#' @param league League ("nfl", "nba", "mlb", "nhl")
#' @return tibble of teams
#' @export
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

#' Generate LLM-friendly context for site.api.espn.com
#'
#' @return Character string with full function signatures and bodies
#' @export
espn_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/site.api.espn.com.R"
  if (!file.exists(src_file)) {
    cat("# site.api.espn.com context - source not found\n")
    return(invisible("# site.api.espn.com context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

