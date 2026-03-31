# fangraphs-com.R
# Self-contained FanGraphs baseball statistics client.
# NOTE: FanGraphs API is behind Cloudflare bot protection.
# This client will fail unless accessed from a browser-like environment.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (but Cloudflare challenge blocks automated access)
# Rate limits: unknown

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fg_base <- "https://www.fangraphs.com/api"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_leaders <- tibble(
  player_name = character(), team = character(), season = integer(),
  g = integer(), pa = integer(), ab = integer(), h = integer(),
  hr = integer(), r = integer(), rbi = integer(), sb = integer(),
  bb_pct = numeric(), k_pct = numeric(), avg = numeric(),
  obp = numeric(), slg = numeric(), ops = numeric(), war = numeric()
)

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
