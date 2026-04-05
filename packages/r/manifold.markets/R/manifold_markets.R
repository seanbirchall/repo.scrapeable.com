# manifold.markets.R - Self-contained manifold.markets client



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


`%||%` <- function(x, y) if (is.null(x)) y else x

# == Public functions ==========================================================

#' Search or list Manifold prediction markets
#'
#' Queries the Manifold Markets API for prediction markets, optionally
#' filtered by a search term. Returns one row per market with the current
#' probability, trading volume, creator, mechanism type, and resolution
#' status. Markets can be binary (YES/NO) or multi-choice.
#'
#' @param limit Maximum number of results to return (default 10, max 1000).
#' @param term Optional free-text search term to filter markets
#'   (e.g. \code{"AI"}, \code{"2025 election"}).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Unique market identifier (character).}
#'     \item{question}{Market question text (character).}
#'     \item{url}{URL to the market page on Manifold (character).}
#'     \item{probability}{Current probability for binary markets, \code{NA}
#'       for multi-choice (numeric, 0--1).}
#'     \item{volume}{Total mana traded on this market (numeric).}
#'     \item{creator_username}{Manifold username of the market creator (character).}
#'     \item{mechanism}{Market mechanism, e.g. \code{"cpmm-1"} (character).}
#'     \item{created_time}{When the market was created (POSIXct).}
#'     \item{close_time}{When the market closes for trading (POSIXct).}
#'     \item{is_resolved}{Whether the market has been resolved (logical).}
#'   }
#' @export
#' @seealso \code{\link{mfld_market}}, \code{\link{mfld_groups}}
#' @examples
#' \dontrun{
#' mfld_markets(limit = 5, term = "AI")
#' mfld_markets(limit = 20)
#' }
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

#' Get detailed information for a single Manifold market
#'
#' Fetches the full record for one prediction market by its unique ID or
#' URL slug. Includes the text description and total liquidity in addition
#' to the fields returned by \code{\link{mfld_markets}}.
#'
#' @param id Market ID string or URL slug (e.g. from the \code{id} column
#'   of \code{\link{mfld_markets}}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{Market identifier (character).}
#'     \item{question}{Market question text (character).}
#'     \item{url}{URL to the market page (character).}
#'     \item{probability}{Current probability (numeric, 0--1).}
#'     \item{volume}{Total mana traded (numeric).}
#'     \item{creator_username}{Market creator's username (character).}
#'     \item{mechanism}{Market mechanism type (character).}
#'     \item{description}{Plain-text market description (character).}
#'     \item{is_resolved}{Whether the market has been resolved (logical).}
#'     \item{total_liquidity}{Mana in the liquidity pool (numeric).}
#'   }
#' @export
#' @seealso \code{\link{mfld_markets}}, \code{\link{mfld_user}}
#' @examples
#' \dontrun{
#' mfld_market("some-market-id")
#' }
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

#' List Manifold groups (topics / categories)
#'
#' Returns the public groups (also called topics or categories) defined on
#' Manifold Markets. Groups organise markets by theme and can be used to
#' browse related questions.
#'
#' @param available_to_anyone If \code{TRUE} (default), return only public
#'   groups; if \code{FALSE}, include private groups the caller has access to
#'   (requires authentication, which this client does not provide).
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Group identifier (character).}
#'     \item{name}{Group display name (character).}
#'     \item{slug}{URL-friendly slug (character).}
#'     \item{total_members}{Number of members in the group (integer).}
#'   }
#' @export
#' @seealso \code{\link{mfld_markets}}, \code{\link{mfld_user}}
#' @examples
#' \dontrun{
#' mfld_groups()
#' }
mfld_groups <- function(available_to_anyone = TRUE) {
  schema <- tibble(id = character(), name = character(), slug = character(),
                   total_members = integer())
  url <- paste0(.mfld_base, "/groups")
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(schema)
  if (!is.data.frame(raw)) return(schema)

  as_tibble(raw) |>
    transmute(
      id = as.character(id),
      name = as.character(if ("name" %in% names(raw)) name else NA_character_),
      slug = as.character(if ("slug" %in% names(raw)) slug else NA_character_),
      total_members = as.integer(if ("totalMembers" %in% names(raw)) totalMembers else NA_integer_)
    )
}

#' Get a Manifold user profile by username
#'
#' Fetches public profile information for a Manifold Markets user,
#' including their mana balance, total deposits, and cached all-time
#' profit.
#'
#' @param username Manifold username (e.g. \code{"JamesGrugett"}).
#' @return A tibble with one row and columns:
#'   \describe{
#'     \item{id}{User identifier (character).}
#'     \item{name}{Display name (character).}
#'     \item{username}{Manifold handle (character).}
#'     \item{created_time}{Account creation timestamp (POSIXct).}
#'     \item{balance}{Current mana balance (numeric).}
#'     \item{total_deposits}{Lifetime mana deposits (numeric).}
#'     \item{profit_cached}{Cached all-time profit in mana (numeric).}
#'   }
#' @export
#' @seealso \code{\link{mfld_markets}}, \code{\link{mfld_market}}
#' @examples
#' \dontrun{
#' mfld_user("JamesGrugett")
#' }
mfld_user <- function(username) {
  schema <- tibble(id = character(), name = character(), username = character(),
                   created_time = as.POSIXct(character()), balance = numeric(),
                   total_deposits = numeric(), profit_cached = numeric())
  url <- paste0(.mfld_base, "/user/", utils::URLencode(username))
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$id)) return(schema)

  tibble(
    id = as.character(raw$id),
    name = as.character(raw$name %||% NA_character_),
    username = as.character(raw$username %||% NA_character_),
    created_time = as.POSIXct(as.numeric(raw$createdTime %||% NA_real_) / 1000, origin = "1970-01-01"),
    balance = as.numeric(raw$balance %||% NA_real_),
    total_deposits = as.numeric(raw$totalDeposits %||% NA_real_),
    profit_cached = as.numeric(raw$profitCached %||% list(allTime = NA_real_)$allTime %||% NA_real_)
  )
}

# == Context ===================================================================

#' Get manifold.markets client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
mfld_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(mfld_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/manifold.markets.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "manifold.markets")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# manifold.markets context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# manifold.markets", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
