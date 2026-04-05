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
    j <- fi - 1; rox_start <- fi
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
#' Browse datasets on Resource Watch, a platform for environmental and
#' socioeconomic data layers covering forests, water, climate, and more.
#' Use \code{rw_dataset()} to get full details for a specific dataset.
#'
#' @param page_size Number of results per page (default 10).
#' @param page Page number for pagination (default 1).
#' @return A tibble with one row per dataset:
#'   \describe{
#'     \item{id}{\code{character} -- Resource Watch dataset UUID.}
#'     \item{name}{\code{character} -- Dataset name.}
#'     \item{subtitle}{\code{character} -- Short subtitle.}
#'     \item{provider}{\code{character} -- Data provider type (e.g. \code{"featureservice"}, \code{"csv"}).}
#'     \item{connectorType}{\code{character} -- Connector type (e.g. \code{"rest"}, \code{"document"}).}
#'     \item{published}{\code{logical} -- Whether the dataset is published.}
#'     \item{env}{\code{character} -- Environment (e.g. \code{"production"}).}
#'   }
#' @examples
#' \dontrun{
#' rw_datasets(page_size = 20)
#' rw_datasets(page_size = 5, page = 3)
#' }
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
#' Retrieve full metadata for one Resource Watch dataset, including its
#' description and underlying table name.
#'
#' @param id Dataset UUID string. Obtain from the \code{id} column of
#'   \code{rw_datasets()} results.
#' @return A single-row tibble:
#'   \describe{
#'     \item{id}{\code{character} -- Dataset UUID.}
#'     \item{name}{\code{character} -- Dataset name.}
#'     \item{subtitle}{\code{character} -- Short subtitle.}
#'     \item{description}{\code{character} -- Full description.}
#'     \item{provider}{\code{character} -- Data provider type.}
#'     \item{connectorType}{\code{character} -- Connector type.}
#'     \item{tableName}{\code{character} -- Underlying table or layer name.}
#'     \item{published}{\code{logical} -- Whether the dataset is published.}
#'   }
#' @examples
#' \dontrun{
#' datasets <- rw_datasets(page_size = 5)
#' rw_dataset(datasets$id[1])
#' }
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

# == Context (LLM injection) ==================================================

#' Get Resource Watch client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' rw_context()
#' }
#' @export
rw_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(rw_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/resourcewatch.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "resourcewatch")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# resourcewatch context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# resourcewatch", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
