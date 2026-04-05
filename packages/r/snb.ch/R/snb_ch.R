# data.snb.ch.R - Self-contained data.snb.ch client



# data-snb-ch.R
# Self-contained Swiss National Bank (SNB) data portal client.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, dplyr, tibble
# Auth: none required
# Rate limits: unknown, be courteous


# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.snb_base <- "https://data.snb.ch/api/cube"

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

.fetch_snb_csv <- function(url) {
  f <- .fetch(url, ext = ".csv")
  lines <- readLines(f, warn = FALSE)
  # SNB CSVs have metadata header lines (quoted); find the header row with "Date"
  header_idx <- which(grepl("^\"Date\"", lines) | grepl("^Date;", lines))
  if (length(header_idx) == 0) {
    return(utils::read.csv(f, stringsAsFactors = FALSE, sep = ";"))
  }
  data_lines <- lines[header_idx[1]:length(lines)]
  # Remove empty trailing lines
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]
  tc <- textConnection(data_lines)
  on.exit(close(tc))
  utils::read.csv(tc, stringsAsFactors = FALSE, sep = ";", quote = "\"")
}

# == Schemas ===================================================================

.schema_data <- tibble(
  date = character(), variable = character(), value = numeric()
)

.schema_dimensions <- tibble(
  dimension = character(), code = character(), label = character()
)


# == Data =====================================================================

#' Fetch data from an SNB data cube
#'
#' Downloads time-series data from the Swiss National Bank data portal. The
#' response is pivoted into a long-format tibble with date, variable label,
#' and numeric value columns.
#'
#' @param cube_id Character. Cube identifier. Common examples:
#'   \itemize{
#'     \item \code{"devkum"} -- Exchange rates (monthly)
#'     \item \code{"rendopar"} -- Interest rates
#'     \item \code{"snbbipo"} -- Balance of payments
#'     \item \code{"snbbista"} -- Balance sheet statistics
#'   }
#'   Use the SNB data portal to discover additional cube IDs.
#' @param lang Character. Language: \code{"en"} (default), \code{"de"},
#'   \code{"fr"}, \code{"it"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{date}{Character. Observation date or period (e.g. "2024-01", "2024-Q1").}
#'     \item{variable}{Character. Pipe-separated dimension labels identifying the series.}
#'     \item{value}{Numeric. Observation value (\code{NA} if missing).}
#'   }
#' @examples
#' snb_data("devkum")
#' snb_data("rendopar", lang = "de")
#' @export
snb_data <- function(cube_id, lang = "en") {
  url <- sprintf("%s/%s/data/csv/%s", .snb_base, cube_id, lang)
  df <- tryCatch(.fetch_snb_csv(url), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_data)
  df <- as_tibble(df)
  # SNB format: Date, D0, D1, ..., Value  (dimension columns + Value)
  # Build a variable label from dimension columns
  date_col <- intersect(c("Date", "date", "DATE"), names(df))
  value_col <- intersect(c("Value", "value"), names(df))
  if (length(date_col) > 0 && length(value_col) > 0) {
    dim_cols <- setdiff(names(df), c(date_col[1], value_col[1]))
    var_label <- if (length(dim_cols) > 0) {
      apply(df[, dim_cols, drop = FALSE], 1, paste, collapse = "|")
    } else {
      rep(cube_id, nrow(df))
    }
    return(tibble(
      date = as.character(df[[date_col[1]]]),
      variable = as.character(var_label),
      value = suppressWarnings(as.numeric(df[[value_col[1]]]))
    ))
  }
  # Fallback: melt non-date columns
  non_date <- setdiff(names(df), date_col)
  if (length(date_col) > 0 && length(non_date) > 0) {
    result <- lapply(non_date, function(col) {
      tibble(
        date = as.character(df[[date_col[1]]]),
        variable = col,
        value = suppressWarnings(as.numeric(df[[col]]))
      )
    })
    return(bind_rows(result))
  }
  df
}

# == Dimensions ================================================================

#' Fetch dimension metadata for an SNB data cube
#'
#' Returns the dimension codes and labels for a given SNB data cube. Useful for
#' understanding the structure of the data returned by \code{\link{snb_data}}.
#'
#' @param cube_id Character. Cube identifier (e.g. \code{"snbbipo"},
#'   \code{"rendopar"}, \code{"devkum"}).
#' @param lang Character. Language: \code{"en"} (default), \code{"de"},
#'   \code{"fr"}, \code{"it"}.
#' @return A tibble with columns:
#'   \describe{
#'     \item{dimension}{Character. Dimension name.}
#'     \item{code}{Character. Dimension value code.}
#'     \item{label}{Character. Human-readable label.}
#'   }
#' @examples
#' snb_dimensions("devkum")
#' @export
snb_dimensions <- function(cube_id, lang = "en") {
  url <- sprintf("%s/%s/dimensions/csv/%s", .snb_base, cube_id, lang)
  df <- tryCatch({
    f <- .fetch(url, ext = ".csv")
    lines <- readLines(f, warn = FALSE)
    # SNB dimension CSV can be semicolon-separated
    utils::read.csv(f, stringsAsFactors = FALSE, sep = ";")
  }, error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(.schema_dimensions)
  df <- as_tibble(df)
  names(df) <- tolower(names(df))
  dim_col <- intersect(c("dimension", "dim", "dimname"), names(df))
  code_col <- intersect(c("code", "key", "id"), names(df))
  label_col <- intersect(c("label", "name", "description", "value"), names(df))
  tibble(
    dimension = if (length(dim_col) > 0) as.character(df[[dim_col[1]]]) else NA_character_,
    code = if (length(code_col) > 0) as.character(df[[code_col[1]]]) else NA_character_,
    label = if (length(label_col) > 0) as.character(df[[label_col[1]]]) else NA_character_
  )
}
`%||%` <- function(x, y) if (is.null(x)) y else x

# == Context ===================================================================

#' Get snb.ch client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
snb_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(snb_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/snb.ch.R"
  if (!file.exists(src_file)) {
    pkg_src <- system.file("source", package = "snb.ch")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# snb.ch context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# snb.ch", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
