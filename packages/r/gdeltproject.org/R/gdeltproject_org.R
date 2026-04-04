# gdeltproject.org.R - Self-contained gdeltproject.org client



# gdeltproject-org.R
# Self-contained GDELT Project client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: 1 request per 5 seconds


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.gdelt_base <- "https://api.gdeltproject.org/api/v2"

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
#' Queries the GDELT DOC 2.0 API for worldwide news articles matching a search
#' query. GDELT monitors print, broadcast, and online news from nearly every
#' country in over 100 languages. Results include the article URL, title,
#' publication date, source domain, language, and country of origin.
#'
#' Rate limit: 1 request per 5 seconds.
#'
#' @param query Character. Search query string (e.g., \code{"climate change"},
#'   \code{"Ukraine"}). Supports GDELT query syntax including boolean operators.
#' @param maxrecords Integer. Maximum number of articles to return.
#'   Default 10, maximum 250.
#' @param timespan Character. Timespan to search backwards from now. Accepts
#'   shorthand: \code{"15min"}, \code{"1h"}, \code{"1d"}, \code{"7d"} (default),
#'   \code{"30d"}, etc.
#' @return A tibble with columns:
#'   \describe{
#'     \item{url}{Character. Full URL of the article.}
#'     \item{title}{Character. Article headline.}
#'     \item{seendate}{Character. Date/time article was first seen (YYYYMMDDHHmmSS).}
#'     \item{socialimage}{Character. URL of the article's social sharing image.}
#'     \item{domain}{Character. Source domain name (e.g. "bbc.co.uk").}
#'     \item{language}{Character. Article language code.}
#'     \item{sourcecountry}{Character. Country of the source outlet.}
#'   }
#' @export
#' @family gdelt functions
#' @seealso [gdelt_timeline()] for article volume over time
#' @examples
#' \dontrun{
#' # Search for recent climate articles
#' gdelt_articles("climate change", maxrecords = 20)
#'
#' # Articles from the last hour
#' gdelt_articles("earthquake", timespan = "1h")
#' }
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
#' Returns a time series of article volume for news matching a search query.
#' Each row represents a time bin with a normalized volume score, useful for
#' tracking how media attention to a topic changes over time.
#'
#' Rate limit: 1 request per 5 seconds.
#'
#' @param query Character. Search query string (same syntax as
#'   \code{\link{gdelt_articles}}).
#' @param timespan Character. Timespan to chart: \code{"7d"}, \code{"30d"}
#'   (default), \code{"1y"}, etc.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Date. The date of the time bin.}
#'     \item{value}{Numeric. Normalized article volume.}
#'     \item{series}{Character. Series label (e.g. query term).}
#'   }
#' @export
#' @family gdelt functions
#' @seealso [gdelt_articles()] to retrieve actual article records
#' @examples
#' \dontrun{
#' # 30-day media attention for "inflation"
#' gdelt_timeline("inflation")
#'
#' # One-year trend
#' gdelt_timeline("artificial intelligence", timespan = "1y")
#' }
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

# == Context ===================================================================

#' Get gdeltproject.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
gdelt_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(gdelt_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/gdeltproject.org.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "gdeltproject.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# gdeltproject.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# gdeltproject.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
