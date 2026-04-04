


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
#' @param cube_id Cube ID (e.g. "snbbipo" for balance of payments,
#'   "rendopar" for interest rates, "devkum" for exchange rates)
#' @param lang Language: "en" (default), "de", "fr", "it"
#' @return tibble with date and value columns (structure varies by cube)
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
#' @param cube_id Cube ID (e.g. "snbbipo", "rendopar", "devkum")
#' @param lang Language: "en" (default), "de", "fr", "it"
#' @return tibble: dimension (character), code (character), label (character)
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

#' Generate LLM-friendly context for data.snb.ch
#'
#' @return Character string with full function signatures and bodies
#' @export
snb_context <- function() {
  src_file <- NULL
  tryCatch(src_file <- sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) {
    tryCatch({
      f <- sys.frame(0)$ofile
      if (!is.null(f) && file.exists(f)) src_file <<- f
    }, error = function(e) NULL)
  }
  if (is.null(src_file)) src_file <- "clients/data.snb.ch.R"
  if (!file.exists(src_file)) {
    cat("# data.snb.ch context - source not found\n")
    return(invisible("# data.snb.ch context - source not found"))
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
    depth <- 0; end_line <- fi
    for (k in fi:n) {
      depth <- depth + nchar(gsub("[^{]", "", lines[k])) - nchar(gsub("[^}]", "", lines[k]))
      if (depth == 0 && k >= fi) { end_line <- k; break }
    }
    body <- lines[fi:end_line]
    blocks[[length(blocks) + 1]] <- c(rox, body, "")
  }
  out <- paste(unlist(blocks), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}

