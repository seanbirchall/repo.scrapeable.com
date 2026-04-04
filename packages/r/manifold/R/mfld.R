# manifold.R
# Self-contained Manifold Markets API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required for public endpoints
# Rate limits: generous


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

#' Search Manifold prediction markets
#'
#' Browse or search active prediction markets on Manifold Markets. Each market
#' represents a question with a community-estimated probability. Use
#' \code{mfld_market()} for full details on a specific market.
#'
#' @param limit Maximum results to return (default 10, max 1000).
#' @param term Optional search term to filter markets by question text
#'   (e.g. \code{"AI"}, \code{"election"}, \code{"climate"}).
#' @return A tibble with one row per market:
#'   \describe{
#'     \item{id}{\code{character} -- Market ID (pass to \code{mfld_market()}).}
#'     \item{question}{\code{character} -- Market question text.}
#'     \item{url}{\code{character} -- URL to the market page.}
#'     \item{probability}{\code{numeric} -- Current probability estimate (0--1).}
#'     \item{volume}{\code{numeric} -- Total trading volume in mana.}
#'     \item{creator_username}{\code{character} -- Username of market creator.}
#'     \item{mechanism}{\code{character} -- Market mechanism (e.g. \code{"cpmm-1"}).}
#'     \item{created_time}{\code{POSIXct} -- Market creation timestamp (UTC).}
#'     \item{close_time}{\code{POSIXct} -- Market close timestamp (UTC).}
#'     \item{is_resolved}{\code{logical} -- Whether the market has been resolved.}
#'   }
#' @examples
#' \dontrun{
#' mfld_markets(limit = 20, term = "AI")
#' mfld_markets(limit = 5)
#' }
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

#' Get a single Manifold market by ID or slug
#'
#' Retrieve full details for a specific prediction market, including the text
#' description and total liquidity.
#'
#' @param id Market ID or URL slug. Obtain from the \code{id} column of
#'   \code{mfld_markets()} results.
#' @return A single-row tibble:
#'   \describe{
#'     \item{id}{\code{character} -- Market ID.}
#'     \item{question}{\code{character} -- Market question text.}
#'     \item{url}{\code{character} -- URL to the market page.}
#'     \item{probability}{\code{numeric} -- Current probability estimate (0--1).}
#'     \item{volume}{\code{numeric} -- Total trading volume in mana.}
#'     \item{creator_username}{\code{character} -- Market creator.}
#'     \item{mechanism}{\code{character} -- Market mechanism type.}
#'     \item{description}{\code{character} -- Plain-text market description.}
#'     \item{is_resolved}{\code{logical} -- Whether the market has been resolved.}
#'     \item{total_liquidity}{\code{numeric} -- Total liquidity pool in mana.}
#'   }
#' @examples
#' \dontrun{
#' markets <- mfld_markets(term = "AI", limit = 5)
#' mfld_market(markets$id[1])
#' }
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

#' Get Manifold client context for LLM use
#'
#' Prints roxygen documentation and function signatures for every public
#' function in this client. Designed for injection into LLM prompts so an
#' assistant can discover available functions without reading full source.
#'
#' @return A character string (printed to the console and returned invisibly).
#' @examples
#' \dontrun{
#' mfld_context()
#' }
#' @export
mfld_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(mfld_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/manifold.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "manifold")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# manifold context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# manifold", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
