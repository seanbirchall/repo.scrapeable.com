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
#' Search the HuggingFace Hub for machine learning models. Results can be
#' filtered by search term and/or author organization. Use \code{hf_model()}
#' for detailed metadata on a specific model.
#'
#' @param search Search query (e.g. \code{"bert"}, \code{"gpt2"},
#'   \code{"text-generation"}). Set to \code{NULL} to browse recent models.
#' @param author Filter by author or organization (e.g. \code{"google"},
#'   \code{"meta-llama"}).
#' @param limit Maximum number of results (default 30).
#' @return A tibble with one row per model:
#'   \describe{
#'     \item{id}{\code{character} -- Model identifier (e.g. \code{"google-bert/bert-base-uncased"}).}
#'     \item{author}{\code{character} -- Model author or organization.}
#'     \item{sha}{\code{character} -- Latest commit SHA.}
#'     \item{pipeline_tag}{\code{character} -- Pipeline task (e.g. \code{"fill-mask"}, \code{"text-generation"}).}
#'     \item{downloads}{\code{integer} -- Total download count.}
#'     \item{likes}{\code{integer} -- Number of likes.}
#'     \item{last_modified}{\code{character} -- Last modification timestamp.}
#'   }
#' @examples
#' \dontrun{
#' hf_models(search = "bert", limit = 10)
#' hf_models(author = "meta-llama", limit = 5)
#' }
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
#' Search the HuggingFace Hub for public datasets. Results include download
#' counts and can be filtered by author/organization.
#'
#' @param search Search query (e.g. \code{"squad"}, \code{"imagenet"}).
#'   Set to \code{NULL} to browse recent datasets.
#' @param author Filter by author or organization.
#' @param limit Maximum number of results (default 30).
#' @return A tibble with one row per dataset:
#'   \describe{
#'     \item{id}{\code{character} -- Dataset identifier (e.g. \code{"rajpurkar/squad"}).}
#'     \item{author}{\code{character} -- Dataset author or organization.}
#'     \item{sha}{\code{character} -- Latest commit SHA.}
#'     \item{downloads}{\code{integer} -- Total download count.}
#'     \item{likes}{\code{integer} -- Number of likes.}
#'     \item{last_modified}{\code{character} -- Last modification timestamp.}
#'   }
#' @examples
#' \dontrun{
#' hf_datasets(search = "squad", limit = 10)
#' }
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
#' Retrieve full metadata for a single model on HuggingFace Hub, including
#' library name, tags, and download/like counts.
#'
#' @param id Model identifier (e.g. \code{"bert-base-uncased"},
#'   \code{"google/flan-t5-base"}, \code{"meta-llama/Llama-2-7b"}).
#' @return A single-row tibble:
#'   \describe{
#'     \item{id}{\code{character} -- Model identifier.}
#'     \item{author}{\code{character} -- Author or organization.}
#'     \item{pipeline_tag}{\code{character} -- Pipeline task tag.}
#'     \item{downloads}{\code{integer} -- Total download count.}
#'     \item{likes}{\code{integer} -- Number of likes.}
#'     \item{library_name}{\code{character} -- ML library (e.g. \code{"transformers"}, \code{"diffusers"}).}
#'     \item{tags}{\code{character} -- All tags, comma-separated.}
#'     \item{last_modified}{\code{character} -- Last modification timestamp.}
#'   }
#' @examples
#' \dontrun{
#' hf_model("bert-base-uncased")
#' hf_model("google/flan-t5-base")
#' }
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

#' Get HuggingFace client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' hf_context()
#' }
#' @export
hf_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(hf_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/huggingface.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "huggingface")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# huggingface context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# huggingface", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
