# riksbank-se.R
# Self-contained Riksbank (Swedish central bank) SWEA API client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous

library(dplyr, warn.conflicts = FALSE)
library(tibble)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.riksbank_base <- "https://api.riksbank.se/swea/v1"

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

.fetch <- function(url, ext = ".json") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua, Accept = "application/json") |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_json <- function(url) jsonlite::fromJSON(.fetch(url))

# == Schemas ===================================================================

.schema_observations <- tibble(
  date = as.Date(character()), value = numeric(), series = character()
)

.schema_series <- tibble(
  seriesid = character(), name = character(), description = character()
)

# == Observations ==============================================================

#' Fetch observations for a Riksbank SWEA series
#'
#' @param series Series ID (e.g. "SEKUSDPMI", "SEKEURPMI", "SECBREPOEFF")
#' @param from Start date ("YYYY-MM-DD")
#' @param to End date ("YYYY-MM-DD")
#' @return tibble: date (Date), value (numeric), series (character)
riksbank_observations <- function(series, from = "2024-01-01",
                                  to = format(Sys.Date(), "%Y-%m-%d")) {
  url <- sprintf("%s/Observations/%s/%s/%s", .riksbank_base, series, from, to)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_observations)
  # API returns a list of objects with date and value
  if (is.data.frame(raw)) {
    df <- as_tibble(raw)
  } else {
    df <- bind_rows(lapply(raw, as_tibble))
  }
  date_col <- intersect(c("date", "Date", "period"), names(df))
  value_col <- intersect(c("value", "Value", "average"), names(df))
  if (length(date_col) == 0 || length(value_col) == 0) return(.schema_observations)
  tibble(
    date = as.Date(df[[date_col[1]]]),
    value = suppressWarnings(as.numeric(df[[value_col[1]]])),
    series = series
  )
}

# == Series listing ============================================================

#' List all available Riksbank SWEA series
#'
#' @return tibble: seriesid (character), name (character), description (character)
riksbank_series <- function() {
  url <- sprintf("%s/Series", .riksbank_base)
  raw <- tryCatch(.fetch_json(url), error = function(e) NULL)
  if (is.null(raw) || length(raw) == 0) return(.schema_series)
  if (is.data.frame(raw)) {
    df <- as_tibble(raw)
  } else {
    df <- bind_rows(lapply(raw, as_tibble))
  }
  # Normalize column names
  names(df) <- tolower(names(df))
  id_col <- intersect(c("seriesid", "id", "series_id", "key"), names(df))
  name_col <- intersect(c("name", "seriesname", "title", "longnameen"), names(df))
  desc_col <- intersect(c("description", "descriptionen", "source"), names(df))
  tibble(
    seriesid = if (length(id_col) > 0) as.character(df[[id_col[1]]]) else NA_character_,
    name = if (length(name_col) > 0) as.character(df[[name_col[1]]]) else NA_character_,
    description = if (length(desc_col) > 0) as.character(df[[desc_col[1]]]) else NA_character_
  )
}

# == Context ===================================================================

#' Show Riksbank SWEA API context for LLMs
#'
#' Displays package overview, common series codes, and function signatures.
#' @return Invisibly returns the context string
riksbank_context <- function() {
  .build_context(
    "riksbank.se",
    header_lines = c(
      "# riksbank.se",
      "# Riksbank (Swedish central bank) SWEA API client",
      "# Auth: none required",
      "# Rate limits: unknown, be courteous",
      "#",
      "# Common series: SEKUSDPMI (SEK/USD), SEKEURPMI (SEK/EUR),",
      "#   SECBREPOEFF (repo rate), SEKINFLCPIF (CPIF inflation)"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
