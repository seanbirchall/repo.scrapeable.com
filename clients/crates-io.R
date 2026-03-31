# crates-io.R
# Self-contained crates.io (Rust package registry) API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (User-Agent header required)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.crates_base <- "https://crates.io/api/v1"

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
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

.schema_search <- tibble(
  name = character(), description = character(), max_version = character(),
  downloads = integer(), created_at = as.POSIXct(character()),
  updated_at = as.POSIXct(character()), homepage = character(),
  repository = character(), categories = character()
)

.schema_crate <- tibble(
  name = character(), description = character(), max_version = character(),
  downloads = integer(), created_at = as.POSIXct(character()),
  updated_at = as.POSIXct(character()), homepage = character(),
  repository = character(), license = character(),
  max_stable_version = character()
)

# == Search ====================================================================

#' Search crates.io for Rust crates
#'
#' @param query Search query string (e.g. "serde", "async runtime")
#' @param per_page Number of results per page (default 20, max 100)
#' @return tibble: name, description, max_version, downloads, created_at,
#'   updated_at, homepage, repository, categories
crates_search <- function(query, per_page = 20) {
  url <- sprintf("%s/crates?q=%s&per_page=%d",
                 .crates_base, utils::URLencode(query), as.integer(per_page))
  raw <- .fetch_json(url)
  crates <- raw$crates
  if (is.null(crates) || nrow(crates) == 0) return(.schema_search)

  as_tibble(crates) |>
    transmute(
      name        = as.character(name),
      description = as.character(description %||% NA_character_),
      max_version = as.character(max_version %||% NA_character_),
      downloads   = as.integer(downloads %||% NA_integer_),
      created_at  = as.POSIXct(created_at, format = "%Y-%m-%dT%H:%M:%OS"),
      updated_at  = as.POSIXct(updated_at, format = "%Y-%m-%dT%H:%M:%OS"),
      homepage    = as.character(homepage %||% NA_character_),
      repository  = as.character(repository %||% NA_character_),
      categories  = as.character(NA)
    )
}

# == Crate detail ==============================================================

#' Get crate details from crates.io
#'
#' @param name Crate name (e.g. "serde", "tokio", "rand")
#' @return tibble: name, description, max_version, downloads, created_at,
#'   updated_at, homepage, repository, license, max_stable_version
crates_crate <- function(name) {
  url <- sprintf("%s/crates/%s", .crates_base, utils::URLencode(name))
  raw <- tryCatch(.fetch_json(url), error = function(e) {
    message("crates.io fetch failed: ", e$message)
    return(NULL)
  })
  if (is.null(raw)) return(.schema_crate)

  cr <- raw$crate
  tibble(
    name               = as.character(cr$name %||% NA_character_),
    description        = as.character(cr$description %||% NA_character_),
    max_version        = as.character(cr$max_version %||% NA_character_),
    downloads          = as.integer(cr$downloads %||% NA_integer_),
    created_at         = as.POSIXct(cr$created_at %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    updated_at         = as.POSIXct(cr$updated_at %||% NA_character_, format = "%Y-%m-%dT%H:%M:%OS"),
    homepage           = as.character(cr$homepage %||% NA_character_),
    repository         = as.character(cr$repository %||% NA_character_),
    license            = as.character(cr$license %||% NA_character_),
    max_stable_version = as.character(cr$max_stable_version %||% NA_character_)
  )
}

# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the crates.io package
#'
#' @return Character string (invisibly), also printed
crates_context <- function() {
  .build_context("crates.io", header_lines = c(
    "# crates.io - Rust Crate Registry API Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required (User-Agent header sent automatically)",
    "# All functions return tibbles with typed columns."
  ))
}
