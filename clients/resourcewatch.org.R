# resourcewatch.org.R - Self-contained resourcewatch.org client

library(httr2)
library(jsonlite)
library(tibble)


# resourcewatch.R
# Self-contained Resource Watch API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: not documented, be respectful


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.rw_base <- "https://api.resourcewatch.org/v1"

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_datasets <- tibble(
  id = character(), name = character(), subtitle = character(),
  provider = character(), connectorType = character(),
  published = logical(), env = character()
)

.schema_dataset_detail <- tibble(
  id = character(), name = character(), subtitle = character(),
  description = character(), provider = character(),
  connectorType = character(), tableName = character(),
  published = logical()
)

# == Dataset listing ===========================================================

#' List Resource Watch datasets
#'
#' Queries the Resource Watch API for environmental and sustainability
#' datasets covering forests, water, climate, energy, and more.
#'
#' @param page_size Number of results per page (default 10)
#' @param page Page number for pagination (default 1)
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Dataset UUID (character)}
#'     \item{name}{Dataset name (character)}
#'     \item{subtitle}{Dataset subtitle (character)}
#'     \item{provider}{Data provider, e.g. "cartodb", "gee" (character)}
#'     \item{connectorType}{Connector type, e.g. "rest", "document" (character)}
#'     \item{published}{Whether dataset is published (logical)}
#'     \item{env}{Environment: "production", "staging" (character)}
#'   }
#' @examples
#' rw_datasets()
#' rw_datasets(page_size = 5, page = 2)
#' @seealso [rw_dataset()], [rw_context()]
#' @source <https://api.resourcewatch.org/v1>
#' @export
rw_datasets <- function(page_size = 10, page = 1) {
  url <- paste0(.rw_base, "/dataset?page[size]=", page_size, "&page[number]=", page)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || length(d) == 0) return(.schema_datasets)

  attrs <- d$attributes
  if (is.null(attrs)) return(.schema_datasets)

  tibble(
    id = as.character(d$id),
    name = as.character(attrs$name %||% NA_character_),
    subtitle = as.character(if ("subtitle" %in% names(attrs)) attrs$subtitle else NA_character_),
    provider = as.character(if ("provider" %in% names(attrs)) attrs$provider else NA_character_),
    connectorType = as.character(if ("connectorType" %in% names(attrs)) attrs$connectorType else NA_character_),
    published = as.logical(if ("published" %in% names(attrs)) attrs$published else NA),
    env = as.character(if ("env" %in% names(attrs)) attrs$env else NA_character_)
  )
}

# == Dataset detail ============================================================

#' Fetch a single Resource Watch dataset by ID
#'
#' Returns full metadata for a specific Resource Watch dataset
#' including description, connector info, and table name.
#'
#' @param id Dataset UUID string (from rw_datasets results)
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Dataset UUID (character)}
#'     \item{name}{Dataset name (character)}
#'     \item{subtitle}{Subtitle (character)}
#'     \item{description}{Full description (character)}
#'     \item{provider}{Data provider (character)}
#'     \item{connectorType}{Connector type (character)}
#'     \item{tableName}{Underlying table name (character)}
#'     \item{published}{Whether published (logical)}
#'   }
#' @examples
#' \dontrun{
#' ds <- rw_datasets(page_size = 1)
#' rw_dataset(ds$id[1])
#' }
#' @seealso [rw_datasets()], [rw_context()]
#' @source <https://api.resourcewatch.org/v1>
#' @export
rw_dataset <- function(id) {
  url <- paste0(.rw_base, "/dataset/", id)
  raw <- .fetch_json(url)
  d <- raw$data
  if (is.null(d) || is.null(d$id)) return(.schema_dataset_detail)
  a <- d$attributes

  tibble(
    id = as.character(d$id),
    name = as.character(a$name %||% NA_character_),
    subtitle = as.character(a$subtitle %||% NA_character_),
    description = as.character(a$description %||% NA_character_),
    provider = as.character(a$provider %||% NA_character_),
    connectorType = as.character(a$connectorType %||% NA_character_),
    tableName = as.character(a$tableName %||% NA_character_),
    published = as.logical(a$published %||% NA)
  )
}

# == Context ===================================================================

#' Get resourcewatch.org client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
rw_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(rw_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/resourcewatch.org.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "resourcewatch.org")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# resourcewatch.org context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# resourcewatch.org", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
