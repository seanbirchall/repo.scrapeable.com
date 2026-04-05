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
    while (j > 0 && grepl("^#\047", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#\047 @export|^#\047 @keywords", rox)]
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
#' Attempts to fetch batting or pitching leaderboard data from the FanGraphs
#' API. **Important**: The FanGraphs API is behind Cloudflare bot protection
#' and will reject automated/server requests. This function currently raises
#' an error with a descriptive message.
#'
#' @param season Season year (default 2024).
#' @param stats Stat type: "bat" for batting (default), "pit" for pitching.
#' @param qual Qualification filter: "y" for qualified players only (default),
#'   "0" for all players.
#' @param page Pagination in format "page_perpage" (default "1_30").
#'   For example, "2_50" returns page 2 with 50 results per page.
#' @return Would return a tibble with columns: player_name, team, season, g,
#'   pa, ab, h, hr, r, rbi, sb, bb_pct, k_pct, avg, obp, slg, ops, war.
#'   Currently raises an error due to Cloudflare protection.
#' @seealso [fg_context()]
#' @source <https://www.fangraphs.com>
#' @export
fg_leaders <- function(season = 2024, stats = "bat", qual = "y",
                       page = "1_30") {
  stop("FanGraphs API is behind Cloudflare bot protection. ",
       "Automated access is blocked. Visit https://www.fangraphs.com for data.")
}

# == Context ===================================================================

#' Get fangraphs-com client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the FanGraphs client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' fg_context()
#' @export
fg_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fg_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/fangraphs-com.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "fangraphs-com")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# fangraphs-com context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# fangraphs-com", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
