


# api-osf-io.R
# Self-contained Open Science Framework (OSF) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public data
# Rate limits: none documented for read-only access


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.osf_base <- "https://api.osf.io/v2"
# -- Fetch helpers -------------------------------------------------------------

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

.schema_preprints <- tibble(
  id = character(), title = character(), description = character(),
  date_created = as.POSIXct(character()), date_published = as.POSIXct(character()),
  doi = character(), provider = character()
)

.schema_nodes <- tibble(
  id = character(), title = character(), description = character(),
  category = character(), date_created = as.POSIXct(character()),
  date_modified = as.POSIXct(character()), public = logical(),
  tags = character()
)



# == Preprints =================================================================

#' Fetch preprints from the Open Science Framework
#'
#' Returns recent preprints, optionally filtered by provider.
#' Uses the JSON:API format from OSF v2 API.
#'
#' @param provider Preprint provider filter (default "osf"). Other values:
#'   "socarxiv", "psyarxiv", "engrxiv", "biohackrxiv"
#' @param page_size Number of results per page (default 25, max 100)
#' @return tibble: id, title, description, date_created, date_published,
#'   doi, provider
#' @export
osf_preprints <- function(provider = "osf", page_size = 25) {
  url <- sprintf("%s/preprints/?filter[provider]=%s&page[size]=%d",
                 .osf_base, provider, page_size)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_preprints)

  rows <- lapply(data, function(item) {
    a <- item$attributes
    tibble(
      id             = as.character(item$id %||% NA),
      title          = as.character(a$title %||% NA),
      description    = as.character(a$description %||% NA),
      date_created   = as.POSIXct(a$date_created %||% NA,
                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date_published = as.POSIXct(a$date_published %||% NA,
                                   format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      doi            = as.character(a$doi %||% NA),
      provider       = provider
    )
  })
  bind_rows(rows)
}


# == Nodes (projects) ==========================================================

#' Search OSF nodes (projects and components)
#'
#' Returns public OSF nodes matching a title filter.
#'
#' @param query Title search filter
#' @param page_size Number of results per page (default 25, max 100)
#' @return tibble: id, title, description, category, date_created,
#'   date_modified, public, tags
#' @export
osf_nodes <- function(query, page_size = 25) {
  url <- sprintf("%s/nodes/?filter[title]=%s&page[size]=%d",
                 .osf_base, utils::URLencode(query), page_size)
  raw <- .fetch_json(url)
  data <- raw$data
  if (is.null(data) || length(data) == 0) return(.schema_nodes)

  rows <- lapply(data, function(item) {
    a <- item$attributes
    tags_str <- paste(unlist(a$tags %||% list()), collapse = ", ")
    tibble(
      id            = as.character(item$id %||% NA),
      title         = as.character(a$title %||% NA),
      description   = as.character(a$description %||% NA),
      category      = as.character(a$category %||% NA),
      date_created  = as.POSIXct(a$date_created %||% NA,
                                  format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date_modified = as.POSIXct(a$date_modified %||% NA,
                                  format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      public        = as.logical(a$public %||% NA),
      tags          = tags_str
    )
  })
  bind_rows(rows)
}


# == Context ===================================================================

#' Generate LLM-friendly context for api.osf.io
#'
#' @return Character string with full function signatures and bodies
#' @export
osf_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/api.osf.io.R"
  if (!file.exists(src_file)) {
    cat("# api.osf.io context - source not found\n")
    return(invisible("# api.osf.io context - source not found"))
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

