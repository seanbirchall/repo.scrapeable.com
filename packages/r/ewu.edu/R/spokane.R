# ewu.edu.R
# Self-contained Spokane Community Indicators client (EWU).
# Provides access to 185+ community indicators for Spokane County.
# All public functions return tibbles. All columns properly typed.
#
# Dependencies: httr2, jsonlite, dplyr, tibble, xml2
# Auth: none required


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
#' Scrapes the A-to-Z index from SpokaneTrends.org to return all 185+
#' community indicators for Spokane County. Each indicator has a unique
#' combination of category, sub-category, and indicator IDs used to
#' fetch data via \code{spokane_data()}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{cat_id}{Integer. Category ID (e.g. 0 = Demographics, 2 = Economy).}
#'     \item{sub_cat_id}{Integer. Sub-category ID within the category.}
#'     \item{ind_id}{Integer. Indicator ID within the sub-category.}
#'     \item{code}{Character. Hierarchical code (e.g. "2.5.1").}
#'     \item{name}{Character. Human-readable indicator name (e.g. "Total Population Living in Poverty").}
#'   }
#' @export
#' @examples
#' \dontrun{
#' spokane_indicators()
#' }
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
#' Downloads CSV data for a given indicator and pivots it into long format.
#' Use \code{spokane_indicators()} to find valid ID combinations, or
#' \code{spokane_search()} to search by name.
#'
#' @param cat_id Integer. Category ID (e.g. 0, 2, 5). See \code{spokane_indicators()}.
#' @param sub_cat_id Integer. Sub-category ID (e.g. 1, 5). See \code{spokane_indicators()}.
#' @param ind_id Integer. Indicator ID (e.g. 1, 2, 5). See \code{spokane_indicators()}.
#' @return A tibble in long format with columns:
#'   \describe{
#'     \item{indicator}{Character. Full indicator title (e.g. "2.5.1 Total and Share of Overall Population Living in Poverty").}
#'     \item{location}{Character. Geographic area (e.g. "Spokane County", "Washington State").}
#'     \item{measure}{Character. Measurement type (e.g. "Total Number", "Percent").}
#'     \item{year}{Integer. Calendar year of the observation.}
#'     \item{value}{Character. Observation value (character to handle mixed numeric/text).}
#'   }
#' @export
#' @examples
#' \dontrun{
#' spokane_data(cat_id = 2, sub_cat_id = 5, ind_id = 1)
#' }
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
#' Filters the full indicator list by a case-insensitive regex match on
#' indicator names. Wraps \code{spokane_indicators()} with a grep filter.
#'
#' @param query Character. Search string matched against indicator names
#'   (case-insensitive regex). Examples: \code{"poverty"}, \code{"education"},
#'   \code{"population"}, \code{"employment"}, \code{"housing"}.
#' @return A tibble with columns: cat_id, sub_cat_id, ind_id, code, name
#'   (same schema as \code{spokane_indicators()}).
#' @export
#' @examples
#' \dontrun{
#' spokane_search("poverty")
#' spokane_search("education")
#' }
spokane_search <- function(query) {
  spokane_indicators() |>
    filter(grepl(query, name, ignore.case = TRUE))
}

#' Get ewu.edu client context for LLM use
#'
#' Returns roxygen documentation and function signatures for all public
#' functions. Shows each function's purpose, parameters, and return type
#' without implementation. Use `function_name` (no parens) to see source,
#' or `?function_name` for help.
#'
#' @return Character string (printed and returned invisibly)
#' @export
spokane_context <- function() {
  src_file <- NULL
  tryCatch({
    env <- environment(spokane_context)
    src <- attr(env, "srcfile")
    if (!is.null(src)) src_file <<- src$filename
  }, error = function(e) NULL)
  if (is.null(src_file) || !file.exists(src_file)) src_file <- "clients/ewu.edu.R"
  if (is.null(src_file) || !file.exists(src_file)) {
    pkg_src <- system.file("source", package = "ewu.edu")
    if (nzchar(pkg_src)) {
      sf <- list.files(pkg_src, pattern = "[.]R$", full.names = TRUE)
      if (length(sf)) src_file <- sf[1]
    }
  }
  if (!file.exists(src_file)) { cat("# ewu.edu context - source not found\n"); return(invisible("")) }

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
  out <- paste(c("# ewu.edu", "# R API Client", "#", "# == Public Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}
