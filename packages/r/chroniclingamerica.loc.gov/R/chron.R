#' Search historic newspaper pages on Chronicling America
#'
#' Full-text search across digitized newspaper pages from the Library of
#' Congress Chronicling America collection via loc.gov API.
#'
#' @param text Search text
#' @param page Page number of results (default 1, 40 results per page)
#' @return tibble: title, date, newspaper, id, image_url, url
#' @export
chron_search <- function(text, page = 1) {
  url <- sprintf(
    "%s/collections/chronicling-america/?q=%s&sp=%d&fo=json",
    .chron_base, utils::URLencode(text, reserved = TRUE), as.integer(page)
  )
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_pages)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_pages)

  rows <- lapply(results, function(r) {
    tibble(
      title = as.character(r$title %||% NA_character_),
      date = tryCatch(as.Date(r$date %||% NA_character_), error = function(e) as.Date(NA)),
      newspaper = as.character(if (!is.null(r$partof)) paste(sapply(r$partof, function(x) if(is.list(x)) x$title %||% x else x), collapse="; ") else NA_character_),
      id = as.character(r$id %||% NA_character_),
      image_url = as.character(if (!is.null(r$image_url)) paste(r$image_url, collapse="; ") else NA_character_),
      url = as.character(r$url %||% r$id %||% NA_character_)
    )
  })
  bind_rows(rows)
}

#' List newspapers in Chronicling America
#'
#' Returns a tibble of digitized newspapers available via the LOC API.
#'
#' @param state Optional two-letter state abbreviation to filter (e.g., "NY")
#' @return tibble: lccn, title, place, start_year, end_year, url
#' @export
chron_newspapers <- function(state = NULL) {
  url <- sprintf("%s/collections/chronicling-america/?fo=json&fa=original-format:newspaper", .chron_base)
  if (!is.null(state)) url <- paste0(url, "&fa=location:", utils::URLencode(state))
  url <- paste0(url, "&c=100")

  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw)) return(.schema_newspapers)

  results <- raw$results
  if (is.null(results) || length(results) == 0) return(.schema_newspapers)

  rows <- lapply(results, function(r) {
    # Extract LCCN from aka list
    lccn <- NA_character_
    if (!is.null(r$aka)) {
      lccn_match <- grep("lccn.loc.gov", unlist(r$aka), value = TRUE)
      if (length(lccn_match) > 0) lccn <- sub(".*/", "", lccn_match[1])
    }

    dates <- r$dates %||% list()
    tibble(
      lccn = as.character(lccn),
      title = as.character(r$title %||% NA_character_),
      place = as.character(if (!is.null(r$location)) paste(unlist(r$location), collapse="; ") else NA_character_),
      start_year = as.character(if (length(dates) > 0) dates[[1]] else NA_character_),
      end_year = as.character(if (length(dates) > 1) dates[[length(dates)]] else NA_character_),
      url = as.character(r$id %||% r$url %||% NA_character_)
    )
  })
  bind_rows(rows)
}

#' Print Chronicling America context for LLM integration
#'
#' @return Invisibly returns the context string
#' @export
chron_context <- function() {
  .build_context(
    pkg_name = "chroniclingamerica.loc.gov",
    header_lines = c(
      "# Package: chroniclingamerica.loc.gov",
      "# Chronicling America - Library of Congress Historic Newspapers",
      "# Auth: none",
      "# Rate limits: none documented",
      "#",
      "# Full-text search of digitized US newspapers (1770-1963)",
      "# Over 20 million pages from 3000+ newspaper titles",
      "# Uses loc.gov collections API"
    )
  )
}
