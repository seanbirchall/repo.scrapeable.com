# manifold.R
# Self-contained Manifold Markets API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public endpoints
# Rate limits: generous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.mfld_base <- "https://api.manifold.markets/v0"

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

.schema_markets <- tibble(
  id = character(), question = character(), url = character(),
  probability = numeric(), volume = numeric(),
  creator_username = character(), mechanism = character(),
  created_time = as.POSIXct(character()), close_time = as.POSIXct(character()),
  is_resolved = logical()
)

.schema_market <- tibble(
  id = character(), question = character(), url = character(),
  probability = numeric(), volume = numeric(),
  creator_username = character(), mechanism = character(),
  description = character(), is_resolved = logical(),
  total_liquidity = numeric()
)

# == Public functions ==========================================================

#' List Manifold Markets prediction markets
#'
#' @param limit Max results (default 10, max 1000)
#' @param term Search term to filter markets
#' @return tibble: id, question, url, probability, volume, creator_username,
#'   mechanism, created_time, close_time, is_resolved
#' @export
mfld_markets <- function(limit = 10, term = NULL) {
  params <- list(limit = limit)
  if (!is.null(term)) params$term <- utils::URLencode(term, reserved = TRUE)
  qstr <- paste(names(params), params, sep = "=", collapse = "&")
  url <- paste0(.mfld_base, "/search-markets?", qstr)
  raw <- .fetch_json(url)
  if (length(raw) == 0 || (is.data.frame(raw) && nrow(raw) == 0)) return(.schema_markets)

  tibble(
    id = as.character(raw$id),
    question = as.character(raw$question),
    url = as.character(raw$url %||% NA_character_),
    probability = as.numeric(raw$probability %||% NA_real_),
    volume = as.numeric(raw$volume %||% NA_real_),
    creator_username = as.character(raw$creatorUsername %||% NA_character_),
    mechanism = as.character(raw$mechanism %||% NA_character_),
    created_time = as.POSIXct(as.numeric(raw$createdTime %||% NA_real_) / 1000, origin = "1970-01-01"),
    close_time = as.POSIXct(as.numeric(raw$closeTime %||% NA_real_) / 1000, origin = "1970-01-01"),
    is_resolved = as.logical(raw$isResolved %||% NA)
  )
}

#' Get a single Manifold market by ID
#'
#' @param id Market ID or slug
#' @return tibble: one row with id, question, url, probability, volume,
#'   creator_username, mechanism, description, is_resolved, total_liquidity
#' @export
mfld_market <- function(id) {
  url <- paste0(.mfld_base, "/market/", id)
  raw <- .fetch_json(url)
  if (length(raw) == 0) return(.schema_market)

  tibble(
    id = as.character(raw$id),
    question = as.character(raw$question),
    url = as.character(raw$url %||% NA_character_),
    probability = as.numeric(raw$probability %||% NA_real_),
    volume = as.numeric(raw$volume %||% NA_real_),
    creator_username = as.character(raw$creatorUsername %||% NA_character_),
    mechanism = as.character(raw$mechanism %||% NA_character_),
    description = as.character(raw$textDescription %||% NA_character_),
    is_resolved = as.logical(raw$isResolved %||% NA),
    total_liquidity = as.numeric(raw$totalLiquidity %||% NA_real_)
  )
}

#' Show Manifold Markets client context for LLM use
#'
#' @return Invisibly returns the context string
#' @export
mfld_context <- function() {
  .build_context(
    pkg_name = "manifold.markets",
    header_lines = c(
      "# manifold.markets -- Manifold Markets Prediction Markets Client",
      "# Deps: httr2, jsonlite, dplyr, tibble",
      "# Auth: none required for public markets",
      "# Search terms: politics, AI, technology, sports, economics"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
