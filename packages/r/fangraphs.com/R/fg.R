# == Public functions ==========================================================

#' Fetch FanGraphs leaderboard data
#'
#' NOTE: FanGraphs API is behind Cloudflare bot protection and may not be
#' accessible from automated/server environments.
#'
#' @param season Season year (default 2024)
#' @param stats "bat" (default) or "pit"
#' @param qual "y" for qualified, "0" for all
#' @param page Page in format "page_perpage" (default "1_30")
#' @return tibble of player statistics
#' @export
fg_leaders <- function(season = 2024, stats = "bat", qual = "y",
                       page = "1_30") {
  stop("FanGraphs API is behind Cloudflare bot protection. ",
       "Automated access is blocked. Visit https://www.fangraphs.com for data.")
}

# == Context ===================================================================

#' Generate LLM-friendly context for the fangraphs.com package
#'
#' @return Character string (invisibly), also printed
#' @export
fg_context <- function() {
  .build_context("fangraphs.com", header_lines = c(
    "# fangraphs.com - FanGraphs Baseball Statistics Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none",
    "# NOTE: API is behind Cloudflare bot protection - blocked from servers",
    "# All functions return tibbles with typed columns."
  ))
}
