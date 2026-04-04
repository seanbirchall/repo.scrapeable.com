# huggingface.R
# Self-contained HuggingFace Hub API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public endpoints
# Rate limits: generous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hf_base <- "https://huggingface.co/api"

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

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url), simplifyVector = TRUE)

# == Schemas ===================================================================

.schema_models <- tibble(
  id = character(), author = character(), sha = character(),
  pipeline_tag = character(), downloads = integer(), likes = integer(),
  last_modified = character()
)

.schema_datasets <- tibble(
  id = character(), author = character(), sha = character(),
  downloads = integer(), likes = integer(), last_modified = character()
)

.schema_model_detail <- tibble(
  id = character(), author = character(), pipeline_tag = character(),
  downloads = integer(), likes = integer(), library_name = character(),
  tags = character(), last_modified = character()
)

# == Public functions ==========================================================

#' Search HuggingFace models
#'
#' @param search Search query (e.g. "bert", "gpt2", "text-generation")
#' @param author Filter by author/organization
#' @param limit Max results (default 30)
#' @return tibble: id, author, sha, pipeline_tag, downloads, likes, last_modified
#' @export
hf_models <- function(search = NULL, author = NULL, limit = 30) {
  params <- list(search = search, author = author, limit = limit)
  params <- params[!vapply(params, is.null, logical(1))]
  qstr <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.hf_base, "/models?", qstr)
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_models)

  tibble(
    id = as.character(raw$id %||% NA_character_),
    author = as.character(raw$author %||% NA_character_),
    sha = as.character(raw$sha %||% NA_character_),
    pipeline_tag = as.character(raw$pipeline_tag %||% NA_character_),
    downloads = as.integer(raw$downloads %||% NA_integer_),
    likes = as.integer(raw$likes %||% NA_integer_),
    last_modified = as.character(raw$lastModified %||% NA_character_)
  )
}

#' Search HuggingFace datasets
#'
#' @param search Search query
#' @param author Filter by author/organization
#' @param limit Max results (default 30)
#' @return tibble: id, author, sha, downloads, likes, last_modified
#' @export
hf_datasets <- function(search = NULL, author = NULL, limit = 30) {
  params <- list(search = search, author = author, limit = limit)
  params <- params[!vapply(params, is.null, logical(1))]
  qstr <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.hf_base, "/datasets?", qstr)
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_datasets)

  tibble(
    id = as.character(raw$id %||% NA_character_),
    author = as.character(raw$author %||% NA_character_),
    sha = as.character(raw$sha %||% NA_character_),
    downloads = as.integer(raw$downloads %||% NA_integer_),
    likes = as.integer(raw$likes %||% NA_integer_),
    last_modified = as.character(raw$lastModified %||% NA_character_)
  )
}

#' Get details for a specific HuggingFace model
#'
#' @param id Model ID (e.g. "bert-base-uncased", "google/flan-t5-base")
#' @return tibble: one row with id, author, pipeline_tag, downloads, likes,
#'   library_name, tags, last_modified
#' @export
hf_model <- function(id) {
  url <- paste0(.hf_base, "/models/", id)
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_model_detail)

  tibble(
    id = as.character(raw$id),
    author = as.character(raw$author %||% NA_character_),
    pipeline_tag = as.character(raw$pipeline_tag %||% NA_character_),
    downloads = as.integer(raw$downloads %||% NA_integer_),
    likes = as.integer(raw$likes %||% NA_integer_),
    library_name = as.character(raw$library_name %||% NA_character_),
    tags = paste(raw$tags %||% character(), collapse = ", "),
    last_modified = as.character(raw$lastModified %||% NA_character_)
  )
}

#' Show HuggingFace client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
hf_context <- function() {
  .build_context(
    pkg_name = "huggingface.co",
    header_lines = c(
      "# huggingface.co -- HuggingFace Hub API Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required for public endpoints",
      "# Popular searches: bert, gpt2, llama, t5, text-generation, text-classification",
      "# Pipeline tags: text-generation, text-classification, token-classification, etc."
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
