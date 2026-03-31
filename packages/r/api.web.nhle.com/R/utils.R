#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# api-web-nhle-com.R
# Self-contained NHL API client (api-web.nhle.com).
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: be polite


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.nhl_base <- "https://api-web.nhle.com/v1"

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

.safe_chr <- function(x) as.character(x %||% NA)
.safe_int <- function(x) as.integer(x %||% NA)
.safe_num <- function(x) as.numeric(x %||% NA)
.safe_default <- function(x) as.character((x$default) %||% NA)

# == Schemas ===================================================================

.schema_standings <- tibble(
  team_abbrev = character(), team_name = character(),
  conference = character(), division = character(),
  games_played = integer(), wins = integer(), losses = integer(),
  ot_losses = integer(), points = integer(), point_pctg = numeric(),
  goal_for = integer(), goal_against = integer(), goal_differential = integer(),
  streak_code = character(), streak_count = integer()
)

.schema_player <- tibble(
  player_id = integer(), first_name = character(), last_name = character(),
  team = character(), position = character(), sweater_number = integer(),
  height_cm = integer(), weight_kg = integer(),
  birth_date = as.Date(character()), birth_country = character()
)

.schema_schedule <- tibble(
  game_id = integer(), date = as.Date(character()),
  home_team = character(), away_team = character(),
  home_score = integer(), away_score = integer(),
  game_state = character(), period = integer()
)

# == Standings =================================================================
