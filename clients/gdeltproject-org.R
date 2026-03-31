# gdeltproject-org.R
# Self-contained GDELT Project client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: 1 request per 5 seconds

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gdelt_base <- "https://api.gdeltproject.org/api/v2"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_articles <- tibble(
  url = character(), title = character(), seendate = character(),
  socialimage = character(), domain = character(), language = character(),
  sourcecountry = character()
)

.schema_timeline <- tibble(
  date = as.Date(character()), value = numeric(), series = character()
)

# == Public functions ==========================================================

#' Search GDELT for news articles
#'
#' Queries the GDELT DOC 2.0 API for news articles matching a search query.
#' Rate limit: 1 request per 5 seconds.
#'
#' @param query Search query string (e.g., "climate change", "Ukraine")
#' @param maxrecords Maximum number of articles to return. Default 10, max 250.
#' @param timespan Timespan to search (e.g., "15min", "1h", "1d", "7d"). Default "7d".
#' @return tibble: url, title, seendate, socialimage, domain, language, sourcecountry
#' @export
gdelt_articles <- function(query, maxrecords = 10, timespan = "7d") {
  url <- sprintf(
    "%s/doc/doc?query=%s&mode=artlist&maxrecords=%d&timespan=%s&format=json",
    .gdelt_base, utils::URLencode(query, reserved = TRUE),
    as.integer(maxrecords), timespan
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_articles)

  articles <- raw$articles
  if (is.null(articles) || nrow(articles) == 0) return(.schema_articles)

  as_tibble(articles) |>
    transmute(
      url = as.character(url),
      title = as.character(title),
      seendate = as.character(seendate),
      socialimage = as.character(if ("socialimage" %in% names(articles)) socialimage else NA_character_),
      domain = as.character(domain),
      language = as.character(language),
      sourcecountry = as.character(sourcecountry)
    )
}

#' Get GDELT timeline data for a query
#'
#' Returns a timeline of volume for news articles matching a search query.
#' Rate limit: 1 request per 5 seconds.
#'
#' @param query Search query string
#' @param timespan Timespan (e.g., "7d", "30d", "1y"). Default "30d".
#' @return tibble: date, value, series
#' @export
gdelt_timeline <- function(query, timespan = "30d") {
  url <- sprintf(
    "%s/doc/doc?query=%s&mode=timelinevol&timespan=%s&format=json",
    .gdelt_base, utils::URLencode(query, reserved = TRUE), timespan
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_timeline)

  tl <- raw$timeline
  if (is.null(tl) || length(tl) == 0) return(.schema_timeline)

  rows <- lapply(seq_along(tl), function(i) {
    series_name <- if (!is.null(names(tl))) names(tl)[i] else paste0("series_", i)
    entry <- tl[[i]]
    if (is.null(entry) || !is.data.frame(entry) || nrow(entry) == 0) return(NULL)
    nms <- names(entry)
    date_col <- nms[1]
    val_col <- nms[2]
    tibble(
      date = tryCatch(as.Date(entry[[date_col]]), error = function(e) as.Date(NA)),
      value = as.numeric(entry[[val_col]]),
      series = series_name
    )
  })
  bind_rows(rows)
}

#' Print GDELT context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
gdelt_context <- function() {
  .build_context(
    pkg_name = "gdeltproject.org",
    header_lines = c(
      "# Package: gdeltproject.org",
      "# GDELT Project DOC 2.0 API - global news monitoring",
      "# Auth: none",
      "# Rate limits: 1 request per 5 seconds",
      "#",
      "# Modes: artlist (articles), timelinevol (volume timeline)",
      "# Covers: 65+ languages, 150+ countries, updated every 15 min"
    )
  )
}
