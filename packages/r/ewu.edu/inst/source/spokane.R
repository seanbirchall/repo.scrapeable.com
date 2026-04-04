# ewu.edu.R
# Self-contained Spokane Community Indicators client (EWU).
# Provides access to 185+ community indicators for Spokane County.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2
# Auth: none required

library(httr2)
library(jsonlite)
library(tibble)
library(dplyr)

# == Private utilities =========================================================

.ua <- "support@scrapeable.com"
.ewu_base <- "http://www.spokanetrends.org"

`%||%` <- function(x, y) if (is.null(x)) y else x

.fetch <- function(url, ext = ".csv") {
  tmp <- tempfile(fileext = ext)
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)
  tmp
}

# == Schemas ===================================================================

.schema_indicators <- tibble(
  cat_id = integer(),
  sub_cat_id = integer(),
  ind_id = integer(),
  code = character(),
  name = character()
)

.schema_data <- tibble(
  indicator = character(),
  location = character(),
  measure = character(),
  year = integer(),
  value = character()
)

# == Public functions ==========================================================

#' List available Spokane Community Indicators
#'
#' Scrapes the A-to-Z index to return all available indicator names and IDs.
#'
#' @return tibble: cat_id, sub_cat_id, ind_id, code, name
spokane_indicators <- function() {
  url <- sprintf("%s/aToZ.cfm", .ewu_base)
  tmp <- .fetch(url, ext = ".html")
  html <- xml2::read_html(tmp)

  links <- xml2::xml_find_all(html, "//a[@class='azLinks']")
  if (length(links) == 0) return(.schema_indicators)

  hrefs <- xml2::xml_attr(links, "href")
  texts <- trimws(xml2::xml_text(links))

  rows <- lapply(seq_along(hrefs), function(i) {
    href <- hrefs[i]
    cat_m <- regmatches(href, regexpr("cat_id=([0-9]+)", href))
    sub_m <- regmatches(href, regexpr("sub_cat_id=([0-9]+)", href))
    ind_m <- regmatches(href, regexpr("ind_id=([0-9]+)", href))

    cat_id <- if (length(cat_m) > 0) as.integer(sub("cat_id=", "", cat_m)) else NA_integer_
    sub_cat_id <- if (length(sub_m) > 0) as.integer(sub("sub_cat_id=", "", sub_m)) else NA_integer_
    ind_id <- if (length(ind_m) > 0) as.integer(sub("ind_id=", "", ind_m)) else NA_integer_

    txt <- texts[i]
    code <- trimws(sub("^([0-9.]+).*", "\\1", txt))
    name <- trimws(sub("^[0-9.]+\\s*", "", txt))

    tibble(cat_id = cat_id, sub_cat_id = sub_cat_id, ind_id = ind_id,
           code = code, name = name)
  })
  bind_rows(rows)
}

#' Get data for a specific Spokane community indicator
#'
#' Downloads CSV data for a given indicator. Use spokane_indicators() to
#' find valid cat_id, sub_cat_id, and ind_id values.
#'
#' @param cat_id Category ID (integer)
#' @param sub_cat_id Sub-category ID (integer)
#' @param ind_id Indicator ID (integer)
#' @return tibble: indicator, location, measure, year, value (long format)
spokane_data <- function(cat_id, sub_cat_id, ind_id) {
  url <- sprintf("%s/templates/dataDownload.cfm?cat_id=%d&sub_cat_id=%d&ind_id=%d",
                 .ewu_base, cat_id, sub_cat_id, ind_id)
  tmp <- .fetch(url, ext = ".csv")
  lines <- readLines(tmp, warn = FALSE)
  if (length(lines) < 2) return(.schema_data)

  # First line is the indicator title
  indicator_title <- trimws(lines[1])

  # Read from line 2 onward as CSV
  raw <- utils::read.csv(textConnection(paste(lines[-1], collapse = "\n")),
                         stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(raw) == 0 || ncol(raw) < 3) return(.schema_data)

  # Pivot wide -> long: first two columns are Location and Measure,

  # rest are year columns
  loc_col <- names(raw)[1]
  meas_col <- names(raw)[2]
  year_cols <- names(raw)[3:ncol(raw)]

  rows <- lapply(seq_len(nrow(raw)), function(r) {
    lapply(year_cols, function(yr) {
      tibble(
        indicator = indicator_title,
        location = as.character(raw[r, loc_col]),
        measure = as.character(raw[r, meas_col]),
        year = suppressWarnings(as.integer(yr)),
        value = as.character(raw[r, yr])
      )
    })
  })
  result <- bind_rows(unlist(rows, recursive = FALSE))
  result |> filter(!is.na(year))
}

#' Search Spokane community indicators by name
#'
#' @param query Search string matched against indicator names (case-insensitive)
#' @return tibble: cat_id, sub_cat_id, ind_id, code, name
spokane_search <- function(query) {
  spokane_indicators() |>
    filter(grepl(query, name, ignore.case = TRUE))
}

#' Show context for the ewu.edu client
#'
#' @return Invisible string of context
spokane_context <- function() {
  src_dir <- system.file("source", package = "ewu.edu")
  if (src_dir == "") {
    this_file <- sys.frame(1)$ofile %||%
      attr(body(spokane_indicators), "srcfile")$filename %||%
      ""
    if (this_file != "" && file.exists(this_file)) {
      lines <- readLines(this_file, warn = FALSE)
    } else {
      cat("# ewu.edu - Spokane Community Indicators client\n")
      cat("# Source not found. Use ?spokane_indicators for help.\n")
      return(invisible(""))
    }
  } else {
    src_files <- list.files(src_dir, pattern = "[.]R$", full.names = TRUE)
    if (length(src_files) == 0) {
      cat("# No R source found.\n")
      return(invisible(""))
    }
    lines <- readLines(src_files[1], warn = FALSE)
  }

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
    sig <- lines[fi]
    k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) {
      k <- k + 1
      sig <- paste(sig, trimws(lines[k]))
    }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, "")
  }
  out <- paste(c(
    "# ewu.edu - Spokane Community Indicators",
    "# 185+ indicators for Spokane County",
    "#",
    "# == Functions ==",
    "#",
    unlist(blocks)
  ), collapse = "\n")
  cat(out, "\n")
  invisible(out)
}
