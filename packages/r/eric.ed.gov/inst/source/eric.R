


# eric.R
# Self-contained ERIC (Education Resources Information Center) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.eric_base <- "https://api.ies.ed.gov/eric/"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_search <- tibble(
  id = character(), title = character(), author = character(),
  source = character(), publicationdate = character(),
  description = character(), subject = character(),
  url = character()
)

# == Search ====================================================================

#' Search ERIC education research database
#'
#' @param query Search query (e.g. "mathematics instruction", "STEM education")
#' @param rows Number of results (default 10, max 200)
#' @param start Starting record offset for pagination (default 0)
#' @return tibble: id, title, author, source, publicationdate,
#'   description, subject, url
#' @export
eric_search <- function(query, rows = 10, start = 0) {
  url <- paste0(.eric_base, "?search=", utils::URLencode(query),
                "&rows=", rows, "&start=", start, "&format=json")
  raw <- .fetch_json(url)
  d <- raw$response$docs
  if (is.null(d) || length(d) == 0) return(.schema_search)

  # author and subject may be list columns
  .collapse_col <- function(x) {
    if (is.list(x)) vapply(x, function(v) paste(v, collapse = "; "), character(1))
    else as.character(x)
  }

  as_tibble(d) |>
    transmute(
      id = as.character(id),
      title = as.character(title),
      author = .collapse_col(if ("author" %in% names(d)) author else NA_character_),
      source = as.character(if ("source" %in% names(d)) source else NA_character_),
      publicationdate = as.character(if ("publicationdate" %in% names(d)) publicationdate else NA_character_),
      description = as.character(if ("description" %in% names(d)) description else NA_character_),
      subject = .collapse_col(if ("subject" %in% names(d)) subject else NA_character_),
      url = paste0("https://eric.ed.gov/?id=", id)
    )
}

# == Context ===================================================================

#' Generate LLM-friendly context for eric.ed.gov
#'
#' @return Character string with full function signatures and bodies
#' @export
eric_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/eric.ed.gov.R"
  if (!file.exists(src_file)) {
    cat("# eric.ed.gov context - source not found\n")
    return(invisible("# eric.ed.gov context - source not found"))
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

