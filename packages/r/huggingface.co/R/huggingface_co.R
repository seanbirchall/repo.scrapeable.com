# huggingface.co.R - Self-contained huggingface.co client



# huggingface.R
# Self-contained HuggingFace Hub API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public endpoints
# Rate limits: generous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.hf_base <- "https://huggingface.co/api"

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


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Search HuggingFace models
#'
#' Search the HuggingFace Hub for machine-learning models by keyword or author.
#' Returns model metadata including download counts and pipeline tags.
#'
#' @param search Character. Search query such as \code{"bert"}, \code{"gpt2"},
#'   \code{"text-generation"}, or \code{"image-classification"}. NULL returns
#'   trending models.
#' @param author Character. Filter by author or organization (e.g.,
#'   \code{"google"}, \code{"meta-llama"}, \code{"openai"}). Default NULL.
#' @param limit Integer. Maximum number of results (default 30).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- model identifier, e.g. \code{"google-bert/bert-base-uncased"}}
#'     \item{author}{character -- model author or organization}
#'     \item{sha}{character -- latest commit SHA}
#'     \item{pipeline_tag}{character -- task type, e.g. \code{"fill-mask"}, \code{"text-generation"}}
#'     \item{downloads}{integer -- total download count}
#'     \item{likes}{integer -- number of likes}
#'     \item{last_modified}{character -- ISO 8601 timestamp of last update}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hf_models(search = "text-generation", limit = 5)
#' hf_models(author = "meta-llama", limit = 10)
#' }
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
#' Search the HuggingFace Hub for datasets by keyword or author. Returns
#' dataset metadata including download counts and modification timestamps.
#'
#' @param search Character. Search query such as \code{"squad"}, \code{"imagenet"},
#'   or \code{"sentiment"}. NULL returns trending datasets.
#' @param author Character. Filter by author or organization (e.g.,
#'   \code{"rajpurkar"}, \code{"huggingface"}). Default NULL.
#' @param limit Integer. Maximum number of results (default 30).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{character -- dataset identifier, e.g. \code{"rajpurkar/squad"}}
#'     \item{author}{character -- dataset author or organization}
#'     \item{sha}{character -- latest commit SHA}
#'     \item{downloads}{integer -- total download count}
#'     \item{likes}{integer -- number of likes}
#'     \item{last_modified}{character -- ISO 8601 timestamp of last update}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hf_datasets(search = "squad", limit = 5)
#' }
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
#' Fetches full metadata for a single model on the HuggingFace Hub, including
#' its library, tags, and download statistics.
#'
#' @param id Character. Model identifier (e.g., \code{"bert-base-uncased"},
#'   \code{"google/flan-t5-base"}, \code{"meta-llama/Llama-2-7b"}).
#' @return A tibble (one row) with columns:
#'   \describe{
#'     \item{id}{character -- model identifier}
#'     \item{author}{character -- model author or organization}
#'     \item{pipeline_tag}{character -- task type, e.g. \code{"fill-mask"}}
#'     \item{downloads}{integer -- total download count}
#'     \item{likes}{integer -- number of likes}
#'     \item{library_name}{character -- ML library, e.g. \code{"transformers"}, \code{"diffusers"}}
#'     \item{tags}{character -- comma-separated tags}
#'     \item{last_modified}{character -- ISO 8601 timestamp of last update}
#'   }
#' @export
#' @examples
#' \dontrun{
#' hf_model("bert-base-uncased")
#' }
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

# == Context ===================================================================

#' Get huggingface.co client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
hf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/huggingface.co.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "huggingface.co")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# huggingface.co context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# huggingface.co", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
