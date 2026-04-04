# _helpers.R
# Shared utility functions used across all client files.
# Source this at the top of each client: source("clients/_helpers.R")
#
# Dependencies: httr2, xml2, jsonlite (only)

# == HTTP helpers ==============================================================

#' Download a URL to a temp file, returning the local path.
#' httr2 auto-errors on non-2xx status codes.
#'
#' @param url URL to fetch
#' @param ext File extension for the temp file
#' @param ua User-Agent string
#' @return Path to the downloaded temp file
#' @export
http_get <- function(url, ext = ".html", ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

#' POST JSON to a URL and save the response to a temp file
#'
#' @param url URL to POST to
#' @param body List to send as JSON body
#' @param ext File extension for the temp file
#' @param ua User-Agent string
#' @return Path to the downloaded temp file
#' @export
http_post <- function(url, body, ext = ".zip", ua = "support@scrapeable.com") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = ua) |>
    httr2::req_body_json(body) |>
    httr2::req_perform(path = tmp)
  tmp
}

#' Fetch a URL and parse JSON response
#'
#' @param url URL returning JSON
#' @param ua User-Agent string
#' @return Parsed R object (usually data.frame or list)
#' @export
http_json <- function(url, ua = "support@scrapeable.com") {
  path <- http_get(url, ext = ".json", ua = ua)
  jsonlite::fromJSON(path)
}


# == HTML helpers (xml2 only, no rvest) ========================================

#' Parse HTML tables from a document into a list of data.frames
#'
#' Replaces rvest::html_table(). Uses xml2 XPath only.
#'
#' @param doc An xml2 HTML document (from xml2::read_html)
#' @param xpath XPath to find table nodes (default: all tables)
#' @return List of data.frames, one per table
#' @export
html_tables <- function(doc, xpath = ".//table") {
  table_nodes <- xml2::xml_find_all(doc, xpath)
  lapply(table_nodes, function(tbl) {
    rows <- xml2::xml_find_all(tbl, ".//tr")
    if (length(rows) == 0) return(data.frame())

    cells <- lapply(rows, function(r) {
      tds <- xml2::xml_find_all(r, ".//td|.//th")
      xml2::xml_text(tds, trim = TRUE)
    })

    max_cols <- max(lengths(cells), 0L)
    if (max_cols == 0) return(data.frame())

    # Pad short rows with NA
    mat <- do.call(rbind, lapply(cells, function(r) {
      length(r) <- max_cols
      r
    }))

    df <- as.data.frame(mat, stringsAsFactors = FALSE)

    # Use first row as header
    if (nrow(df) > 1) {
      hdr <- as.character(df[1, ])
      hdr[is.na(hdr) | hdr == ""] <- paste0("X", which(is.na(hdr) | hdr == ""))
      names(df) <- make.unique(hdr)
      df <- df[-1, , drop = FALSE]
      rownames(df) <- NULL
    } else {
      names(df) <- paste0("X", seq_len(ncol(df)))
    }
    df
  })
}


# == Data reshaping (base R, no tidyr) =========================================

#' Simple pivot_wider using base R
#'
#' Replaces tidyr::pivot_wider() for the common case of spreading
#' name/value pairs into columns. Takes the first value on collision.
#'
#' @param df A data.frame
#' @param names_col Column whose values become new column names
#' @param values_col Column whose values fill the new columns
#' @param id_cols Columns that identify each row (default: all others)
#' @return Wide data.frame
#' @export
pivot_wider_base <- function(df, names_col, values_col, id_cols = NULL) {
  if (is.null(id_cols)) id_cols <- setdiff(names(df), c(names_col, values_col))

  unique_names <- unique(df[[names_col]])

  if (length(id_cols) == 0) {
    # No id columns - single row output
    row <- data.frame(row.names = 1)
    for (nm in unique_names) {
      vals <- df[[values_col]][df[[names_col]] == nm]
      row[[nm]] <- if (length(vals) > 0) vals[1] else NA
    }
    return(row)
  }

  # Build composite key for grouping
  id_key <- do.call(paste, c(df[id_cols], list(sep = "\x1F")))
  unique_keys <- unique(id_key)

  rows <- lapply(unique_keys, function(k) {
    mask <- id_key == k
    sub <- df[mask, ]
    row <- sub[1, id_cols, drop = FALSE]
    for (nm in unique_names) {
      vals <- sub[[values_col]][sub[[names_col]] == nm]
      row[[nm]] <- if (length(vals) > 0) vals[1] else NA
    }
    row
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}


# == Data helpers ==============================================================

#' Standardize column names: lowercase, dots/spaces to underscores
#' @export
fix_names <- function(df) {
  nms <- tolower(names(df))
  nms <- gsub("[. ]+", "_", nms)
  names(df) <- nms
  df
}

#' Trim whitespace and collapse internal runs
#' @export
clean_text <- function(x) {
  x <- trimws(x)
  gsub("\\s+", " ", x)
}

#' Normalize text for matching: lowercase, alphanum only, collapse spaces
#' @export
normalize_text <- function(x, remove_spaces = FALSE) {
  x <- gsub("[^[:alnum:]]", " ", x)
  x <- tolower(trimws(x))
  x <- gsub("\\s+", " ", x)
  if (remove_spaces) x <- gsub(" ", "", x)
  x
}

#' Zero-pad a CIK to 10 digits
#' @export
pad_cik <- function(cik) {
  sprintf("%010d", as.integer(cik))
}

#' Read a bz2-compressed CSV from a URL
#' @export
read_bz2_csv <- function(url) {
  tmp <- http_get(url, ext = ".csv.bz2")
  read.csv(tmp, stringsAsFactors = FALSE)
}

#' Prefix all column names in a data.frame
#' @export
prefix_cols <- function(df, prefix) {
  names(df) <- paste0(prefix, names(df))
  df
}
