# football-data-co-uk.R
# Self-contained football-data.co.uk match results client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none
# Rate limits: none - static CSV files


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.fb_base <- "https://www.football-data.co.uk"

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

.fetch_csv <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_results <- tibble(
  div = character(), date = as.Date(character()), home_team = character(),
  away_team = character(), fthg = integer(), ftag = integer(),
  ftr = character(), hthg = integer(), htag = integer(), htr = character(),
  referee = character(), hs = integer(), as_ = integer(),
  hst = integer(), ast = integer(), hf = integer(), af = integer(),
  hc = integer(), ac = integer(), hy = integer(), ay = integer(),
  hr = integer(), ar = integer()
)

# == Match results =============================================================

#' Fetch football match results from football-data.co.uk
#'
#' Downloads historical match results for European football (soccer) leagues
#' from football-data.co.uk. Returns full-time and half-time scores, match
#' statistics (shots, fouls, corners, cards), and referee assignments.
#'
#' @param league League code string. Common codes:
#'   \describe{
#'     \item{"E0"}{English Premier League}
#'     \item{"E1"}{English Championship}
#'     \item{"E2"}{English League 1}
#'     \item{"E3"}{English League 2}
#'     \item{"SP1"}{Spanish La Liga}
#'     \item{"D1"}{German Bundesliga}
#'     \item{"I1"}{Italian Serie A}
#'     \item{"F1"}{French Ligue 1}
#'     \item{"N1"}{Dutch Eredivisie}
#'     \item{"P1"}{Portuguese Liga}
#'     \item{"SC0"}{Scottish Premiership}
#'     \item{"B1"}{Belgian Jupiler League}
#'   }
#' @param season Two-digit season code: "2324" = 2023/24, "2425" = 2024/25.
#'   Format is last two digits of start year concatenated with end year.
#' @return A tibble with columns:
#'   \describe{
#'     \item{div}{Division code}
#'     \item{date}{Match date (Date)}
#'     \item{home_team, away_team}{Team names}
#'     \item{fthg, ftag}{Full-time home/away goals}
#'     \item{ftr}{Full-time result: "H" (home win), "D" (draw), "A" (away win)}
#'     \item{hthg, htag}{Half-time home/away goals}
#'     \item{htr}{Half-time result}
#'     \item{referee}{Referee name}
#'     \item{hs, as_}{Home/away total shots}
#'     \item{hst, ast}{Home/away shots on target}
#'     \item{hf, af}{Home/away fouls committed}
#'     \item{hc, ac}{Home/away corners}
#'     \item{hy, ay}{Home/away yellow cards}
#'     \item{hr, ar}{Home/away red cards}
#'   }
#' @examples
#' fbdata_results("E0", "2324")
#' fbdata_results("SP1", "2324")
#' @seealso [fbdata_context()]
#' @source <https://www.football-data.co.uk>
#' @export
fbdata_results <- function(league = "E0", season = "2425") {
  url <- sprintf("%s/mmz4281/%s/%s.csv", .fb_base, season, league)
  f <- .fetch_csv(url)
  raw <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(raw) == 0) return(.schema_results)

  # Date format varies: DD/MM/YYYY or DD/MM/YY
  parse_date <- function(x) {
    d <- tryCatch(as.Date(x, format = "%d/%m/%Y"), error = function(e) NA)
    ifelse(is.na(d), tryCatch(as.Date(x, format = "%d/%m/%y"), error = function(e) NA), d)
    d2 <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
    d3 <- suppressWarnings(as.Date(x, format = "%d/%m/%y"))
    as.Date(ifelse(is.na(d2), d3, d2), origin = "1970-01-01")
  }

  raw |>
    as_tibble() |>
    transmute(
      div       = as.character(Div),
      date      = parse_date(Date),
      home_team = as.character(HomeTeam),
      away_team = as.character(AwayTeam),
      fthg      = as.integer(if ("FTHG" %in% names(raw)) FTHG else NA),
      ftag      = as.integer(if ("FTAG" %in% names(raw)) FTAG else NA),
      ftr       = as.character(if ("FTR" %in% names(raw)) FTR else NA),
      hthg      = as.integer(if ("HTHG" %in% names(raw)) HTHG else NA),
      htag      = as.integer(if ("HTAG" %in% names(raw)) HTAG else NA),
      htr       = as.character(if ("HTR" %in% names(raw)) HTR else NA),
      referee   = as.character(if ("Referee" %in% names(raw)) Referee else NA),
      hs        = as.integer(if ("HS" %in% names(raw)) HS else NA),
      as_       = as.integer(if ("AS" %in% names(raw)) AS else NA),
      hst       = as.integer(if ("HST" %in% names(raw)) HST else NA),
      ast       = as.integer(if ("AST" %in% names(raw)) AST else NA),
      hf        = as.integer(if ("HF" %in% names(raw)) HF else NA),
      af        = as.integer(if ("AF" %in% names(raw)) AF else NA),
      hc        = as.integer(if ("HC" %in% names(raw)) HC else NA),
      ac        = as.integer(if ("AC" %in% names(raw)) AC else NA),
      hy        = as.integer(if ("HY" %in% names(raw)) HY else NA),
      ay        = as.integer(if ("AY" %in% names(raw)) AY else NA),
      hr        = as.integer(if ("HR" %in% names(raw)) HR else NA),
      ar        = as.integer(if ("AR" %in% names(raw)) AR else NA)
    )
}

#' Get football-data-co-uk client context for LLM use
#'
#' Prints roxygen documentation and function signatures for all public
#' functions in the football-data.co.uk client. Designed for LLM tool-use.
#'
#' @return Character string of context documentation (printed to console and
#'   returned invisibly).
#' @examples
#' fbdata_context()
#' @export
fbdata_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(fbdata_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/football-data-co-uk.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "football-data-co-uk")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# football-data-co-uk context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# football-data-co-uk", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
