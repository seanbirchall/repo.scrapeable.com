# polymarket-com.R
# Self-contained Polymarket prediction markets client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none
# Rate limits: unknown (be polite)

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.poly_base <- "https://gamma-api.polymarket.com"

# -- Context generator (reads roxygen + signatures from inst/source/) ----------

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
    j <- fi - 1
    rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

# -- Fetch helpers -------------------------------------------------------------

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_markets <- tibble(
  id = character(), question = character(), slug = character(),
  end_date = as.POSIXct(character()),
  active = logical(), closed = logical(),
  volume = numeric(), liquidity = numeric(),
  outcome_yes = numeric(), outcome_no = numeric(),
  description = character()
)

# == Markets ===================================================================

#' Fetch Polymarket prediction markets
#'
#' Returns a tibble of prediction markets from Polymarket.
#'
#' @param limit Number of markets to return (default 100, max 100)
#' @param active If TRUE, return only active markets (default TRUE)
#' @param closed If TRUE, include closed markets (default FALSE)
#' @return tibble: id, question, slug, end_date, active, closed,
#'   volume, liquidity, outcome_yes, outcome_no, description
poly_markets <- function(limit = 100, active = TRUE, closed = FALSE) {
  url <- sprintf("%s/markets?limit=%d&active=%s&closed=%s",
                 .poly_base, limit, tolower(active), tolower(closed))
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_markets)
  df <- as_tibble(raw)

  prices <- tryCatch({
    p <- df$outcomePrices
    parsed <- lapply(p, function(x) {
      v <- jsonlite::fromJSON(x)
      as.numeric(v)
    })
    do.call(rbind, parsed)
  }, error = function(e) NULL)

  result <- df |>
    transmute(
      id = as.character(id),
      question = as.character(question),
      slug = as.character(slug),
      end_date = as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      active = as.logical(active),
      closed = as.logical(closed),
      volume = as.numeric(volumeNum),
      liquidity = as.numeric(liquidityNum),
      description = as.character(description)
    )

  if (!is.null(prices) && nrow(prices) == nrow(result)) {
    result$outcome_yes <- prices[, 1]
    result$outcome_no  <- prices[, 2]
  } else {
    result$outcome_yes <- NA_real_
    result$outcome_no  <- NA_real_
  }

  result
}

#' Fetch a single Polymarket market by slug
#'
#' @param slug Market slug (e.g. "will-bitcoin-hit-100000-in-2025")
#' @return tibble: single row with same columns as poly_markets
#' @return tibble: id, question, slug, end_date, active, closed,
#'   volume, liquidity, outcome_yes, outcome_no, description
poly_market <- function(slug) {
  url <- sprintf("%s/markets?slug=%s", .poly_base, slug)
  raw <- .fetch_json(url)
  if (is.null(raw) || length(raw) == 0) return(.schema_markets)
  df <- as_tibble(raw)

  prices <- tryCatch({
    p <- df$outcomePrices
    parsed <- lapply(p, function(x) {
      v <- jsonlite::fromJSON(x)
      as.numeric(v)
    })
    do.call(rbind, parsed)
  }, error = function(e) NULL)

  result <- df |>
    transmute(
      id = as.character(id),
      question = as.character(question),
      slug = as.character(slug),
      end_date = as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      active = as.logical(active),
      closed = as.logical(closed),
      volume = as.numeric(volumeNum),
      liquidity = as.numeric(liquidityNum),
      description = as.character(description)
    )

  if (!is.null(prices) && nrow(prices) == nrow(result)) {
    result$outcome_yes <- prices[, 1]
    result$outcome_no  <- prices[, 2]
  } else {
    result$outcome_yes <- NA_real_
    result$outcome_no  <- NA_real_
  }

  result
}


# == Context (LLM injection) ==================================================

#' Generate LLM-friendly context for the polymarket package
#'
#' @return Character string (invisibly), also printed
poly_context <- function() {
  .build_context("polymarket.com", header_lines = c(
    "# polymarket.com - Polymarket Prediction Markets Client for R",
    "# Dependencies: httr2, jsonlite, dplyr, tibble",
    "# Auth: none required",
    "# Rate limit: unknown (be polite)",
    "# All functions return tibbles with typed columns."
  ))
}
