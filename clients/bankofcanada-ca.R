# bankofcanada-ca.R
# Self-contained Bank of Canada Valet API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.boc_base <- "https://www.bankofcanada.ca/valet"

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

.schema_observations <- tibble(
  date = as.Date(character()), value = numeric(), series = character()
)

.schema_series <- tibble(
  name = character(), label = character(), description = character()
)

.schema_groups <- tibble(
  name = character(), label = character(), description = character()
)

# == Observations ==============================================================

#' Fetch observations for a Bank of Canada Valet series
#'
#' @param series Series code (e.g. "FXUSDCAD", "FXEURCAD", "V39079")
#' @param recent Number of recent observations to return (default 10)
#' @param start Optional start date ("YYYY-MM-DD")
#' @param end Optional end date ("YYYY-MM-DD")
#' @return tibble: date (Date), value (numeric), series (character)
boc_observations <- function(series, recent = NULL, start = NULL, end = NULL) {
  url <- sprintf("%s/observations/%s/json", .boc_base, series)
  params <- list()
  if (!is.null(recent)) params[["recent"]] <- recent
  if (!is.null(start)) params[["start_date"]] <- start
  if (!is.null(end)) params[["end_date"]] <- end
  if (length(params) > 0) {
    qs <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste0(url, "?", qs)
  }
  raw <- .fetch_json(url)
  obs <- raw$observations
  if (is.null(obs) || length(obs) == 0) return(.schema_observations)
  df <- as_tibble(obs)
  # The data has a "d" column for date and the series name as a column
  series_col <- setdiff(names(df), "d")
  if (length(series_col) == 0) return(.schema_observations)
  result <- lapply(series_col, function(s) {
    tibble(
      date = as.Date(df[["d"]]),
      value = suppressWarnings(as.numeric(df[[s]][["v"]])),
      series = s
    )
  })
  bind_rows(result)
}

# == Series listing ============================================================

#' List available series in a Bank of Canada Valet group
#'
#' @param group Group name (e.g. "FX_RATES_DAILY", "FX_RATES_MONTHLY")
#' @return tibble: name (character), label (character), description (character)
boc_series <- function(group) {
  url <- sprintf("%s/groups/%s/json", .boc_base, group)
  raw <- .fetch_json(url)
  detail <- raw$groupDetails$groupSeries
  if (is.null(detail) || length(detail) == 0) return(.schema_series)
  df <- bind_rows(lapply(names(detail), function(nm) {
    s <- detail[[nm]]
    tibble(
      name = nm,
      label = as.character(s$label %||% NA_character_),
      description = as.character(s$description %||% NA_character_)
    )
  }))
  df
}

# == Groups listing ============================================================

#' List available groups on the Bank of Canada Valet API
#'
#' @return tibble: name (character), label (character), description (character)
boc_groups <- function() {
  url <- sprintf("%s/lists/groups/json", .boc_base)
  raw <- .fetch_json(url)
  groups <- raw$groups
  if (is.null(groups) || length(groups) == 0) return(.schema_groups)
  # API returns a named list of lists, not a data frame
  bind_rows(lapply(names(groups), function(nm) {
    g <- groups[[nm]]
    tibble(
      name = nm,
      label = as.character(g$label %||% NA_character_),
      description = as.character(g$description %||% NA_character_)
    )
  }))
}

# == Context ===================================================================

#' Show Bank of Canada Valet API context for LLMs
#'
#' Displays package overview, common series codes, and function signatures.
#' @return Invisibly returns the context string
boc_context <- function() {
  .build_context(
    "bankofcanada.ca",
    header_lines = c(
      "# bankofcanada.ca",
      "# Bank of Canada Valet API client",
      "# Auth: none required",
      "# Rate limits: unknown, be courteous",
      "#",
      "# Common series: FXUSDCAD (USD/CAD), FXEURCAD (EUR/CAD),",
      "#   FXGBPCAD (GBP/CAD), FXJPYCAD (JPY/CAD)",
      "# Common groups: FX_RATES_DAILY, FX_RATES_MONTHLY",
      "#   BOND_YIELDS, INTEREST_RATES, MONEY_MARKET"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
