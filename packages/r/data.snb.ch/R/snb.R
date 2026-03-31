
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

# == Context ===================================================================

#' Show SNB API context for LLMs
#'
#' Displays package overview, common cube IDs, and function signatures.
#' @return Invisibly returns the context string
#' @export
snb_context <- function() {
  .build_context(
    "data.snb.ch",
    header_lines = c(
      "# data.snb.ch",
      "# Swiss National Bank data portal client",
      "# Auth: none required",
      "#",
      "# Common cubes: snbbipo (balance of payments), rendopar (interest rates),",
      "#   devkum (exchange rates), snbbipc (current account),",
      "#   snbgeldmengen (monetary aggregates)"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
