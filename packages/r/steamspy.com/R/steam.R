# == Public functions ==========================================================

#' Get detailed information for a Steam app
#'
#' @param appid Steam application ID (e.g. 730 for CS:GO, 570 for Dota 2)
#' @return tibble with one row: appid, name, developer, publisher, positive,
#'   negative, owners, average_forever, average_2weeks, median_forever,
#'   median_2weeks, price, initialprice, discount, ccu, genre, languages
#' @export
steam_app <- function(appid) {
  url <- sprintf("%s?request=appdetails&appid=%s", .steam_base, appid)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_app_detail)

  tibble(
    appid          = as.integer(raw$appid %||% NA),
    name           = as.character(raw$name %||% NA),
    developer      = as.character(raw$developer %||% NA),
    publisher      = as.character(raw$publisher %||% NA),
    positive       = as.integer(raw$positive %||% NA),
    negative       = as.integer(raw$negative %||% NA),
    owners         = as.character(raw$owners %||% NA),
    average_forever = as.integer(raw$average_forever %||% NA),
    average_2weeks = as.integer(raw$average_2weeks %||% NA),
    median_forever = as.integer(raw$median_forever %||% NA),
    median_2weeks  = as.integer(raw$median_2weeks %||% NA),
    price          = as.integer(raw$price %||% NA),
    initialprice   = as.integer(raw$initialprice %||% NA),
    discount       = as.integer(raw$discount %||% NA),
    ccu            = as.integer(raw$ccu %||% NA),
    genre          = as.character(raw$genre %||% NA),
    languages      = as.character(raw$languages %||% NA)
  )
}

#' Get top 100 games by current players in last 2 weeks
#'
#' @return tibble: appid, name, developer, publisher, positive, negative,
#'   owners, average_forever, average_2weeks, price, initialprice, discount, ccu
#' @export
steam_top100 <- function() {
  url <- sprintf("%s?request=top100in2weeks", .steam_base)
  raw <- .fetch_json(url)
  .parse_app_list(raw)
}

#' Get games by genre
#'
#' @param genre Genre name (e.g. "Action", "RPG", "Strategy", "Indie",
#'   "Adventure", "Simulation", "Racing", "Sports")
#' @return tibble: appid, name, developer, publisher, positive, negative,
#'   owners, average_forever, average_2weeks, price, initialprice, discount, ccu
#' @export
steam_genre <- function(genre) {
  url <- sprintf("%s?request=genre&genre=%s", .steam_base,
                 utils::URLencode(genre, reserved = TRUE))
  raw <- .fetch_json(url)
  .parse_app_list(raw)
}

# == Context ===================================================================

#' Generate LLM-friendly context for the steamspy.com package
#'
#' @return Character string (invisibly), also printed
#' @export
steam_context <- function() {
  .build_context("steamspy.com", header_lines = c(
    "# steamspy.com - SteamSpy Game Statistics Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# Rate limit: 4 requests per second",
    "# All functions return tibbles with typed columns.",
    "#",
    "# Common appids: 730 (CS:GO), 570 (Dota 2), 440 (TF2),",
    "#   578080 (PUBG), 271590 (GTA V), 1172470 (Apex Legends)",
    "# Genres: Action, RPG, Strategy, Indie, Adventure, Simulation,",
    "#   Racing, Sports, Free To Play, Early Access"
  ))
}
