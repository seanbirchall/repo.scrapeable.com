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

