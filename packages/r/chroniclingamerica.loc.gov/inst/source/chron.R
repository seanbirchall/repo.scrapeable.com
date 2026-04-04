


# chroniclingamerica-loc-gov.R
# Self-contained Chronicling America (Library of Congress) client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none (public data)
# Rate limits: none documented
# Note: Uses loc.gov collections API (chroniclingamerica.loc.gov redirects)


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.chron_base <- "https://www.loc.gov"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# == Schemas ===================================================================

.schema_pages <- tibble(
  title = character(), date = as.Date(character()),
  newspaper = character(), id = character(),
  image_url = character(), url = character()
)

.schema_newspapers <- tibble(
  lccn = character(), title = character(), place = character(),
  start_year = character(), end_year = character(), url = character()
)

# == Public functions ==========================================================


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

# == Context ===================================================================

#' Generate LLM-friendly context for chroniclingamerica.loc.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
chron_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/chroniclingamerica.loc.gov.R"
  if (!file.exists(src_file)) {
    cat("# chroniclingamerica.loc.gov context - source not found\n")
    return(invisible("# chroniclingamerica.loc.gov context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

