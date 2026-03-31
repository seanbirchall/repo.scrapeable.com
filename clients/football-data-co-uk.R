# football-data-co-uk.R
# Self-contained football-data.co.uk match results client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none
# Rate limits: none - static CSV files

library(dplyr, warn.conflicts = FALSE)
library(tibble)

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
#' Downloads CSV results for a given league and season.
#'
#' @param league League code: "E0" (EPL), "E1" (Championship), "E2" (League 1),
#'   "E3" (League 2), "SP1" (La Liga), "D1" (Bundesliga), "I1" (Serie A),
#'   "F1" (Ligue 1), "N1" (Eredivisie), "P1" (Liga Portugal),
#'   "SC0" (Scottish Premiership), "B1" (Jupiler League)
#' @param season Two-digit season code: "2324" = 2023/24, "2425" = 2024/25.
#'   Format is last two digits of start and end year.
#' @return tibble: div, date, home_team, away_team, fthg (full-time home goals),
#'   ftag (full-time away goals), ftr (FT result H/D/A), hthg, htag, htr,
#'   referee, hs (home shots), as_ (away shots), hst, ast, hf, af, hc, ac,
#'   hy, ay, hr, ar
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

#' Football-data.co.uk context for LLM integration
#'
#' @return Prints package documentation; returns invisibly
#' @export
fbdata_context <- function() {
  .build_context("football.data.co.uk", header_lines = c(
    "# football.data.co.uk - Football Match Results Client",
    "# Deps: httr2, dplyr, tibble",
    "# Auth: none",
    "#",
    "# League codes: E0=EPL, E1=Championship, SP1=La Liga, D1=Bundesliga,",
    "#   I1=Serie A, F1=Ligue 1, N1=Eredivisie, P1=Liga Portugal",
    "# Season format: 2324 = 2023/24, 2425 = 2024/25",
    "# FTR: H=Home win, D=Draw, A=Away win"
  ))
}
