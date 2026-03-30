#' @import dplyr
#' @importFrom tibble tibble as_tibble
#' @importFrom httr2 request req_headers req_perform
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
NULL

# epa-gov.R
# Self-contained EPA Envirofacts API client.
# All public functions return tibbles with typed columns.
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# Auth: none required (public API)
# Docs: https://www.epa.gov/enviro/envirofacts-data-service-api


# == Private utilities =========================================================

`%||%` <- function(x, y) if (is.null(x)) y else x
.ua <- "support@scrapeable.com"
.epa_base <- "https://data.epa.gov/efservice"

# -- Context generator ---------------------------------------------------------

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
    j <- fi - 1; rox_start <- fi
    while (j > 0 && grepl("^#'", lines[j])) { rox_start <- j; j <- j - 1 }
    rox <- if (rox_start < fi) lines[rox_start:(fi - 1)] else character()
    rox <- rox[!grepl("^#' @export|^#' @keywords", rox)]
    sig <- lines[fi]; k <- fi
    while (!grepl("[{]\\s*$", sig) && k < min(fi + 15, n)) { k <- k + 1; sig <- paste(sig, trimws(lines[k])) }
    sig <- sub("\\s*[{]\\s*$", "", sig)
    blocks[[length(blocks) + 1]] <- c(rox, sig, sprintf("  Run `%s` to view source or `?%s` for help.", fn_name, fn_name), "")
  }
  out <- paste(c(header_lines, "#", "# == Functions ==", "#", unlist(blocks)), collapse = "\n")
  cat(out, "\n"); invisible(out)
}

# -- Envirofacts query builder -------------------------------------------------
# Pattern: {base}/{table}/{col}/{op}/{value}/rows/{start}:{end}/JSON
# Operators: = (equals), != (not equals), < > (comparison), BEGINNING (starts with)

.epa_query <- function(table, filters = list(), max_rows = 1000) {
  # Build filter path segments
  filter_path <- ""
  for (f in filters) {
    if (!is.null(f$value))
      filter_path <- paste0(filter_path, "/", f$column, "/", f$op %||% "=", "/",
                            utils::URLencode(as.character(f$value)))
  }

  url <- paste0(.epa_base, "/", table, filter_path, "/rows/0:", max_rows - 1, "/JSON")

  tmp <- tempfile(fileext = ".json")
  httr2::request(url) |>
    httr2::req_headers(`User-Agent` = .ua) |>
    httr2::req_perform(path = tmp)

  raw <- jsonlite::fromJSON(tmp)
  if (is.null(raw) || length(raw) == 0) return(tibble())
  df <- as_tibble(raw)
  names(df) <- tolower(names(df))

  # Auto-type: dates, numbers
  for (col in names(df)) {
    vals <- df[[col]]
    if (!is.character(vals)) next
    # Date columns (contain "date" in name)
    if (grepl("date", col)) {
      parsed <- suppressWarnings(as.Date(vals))
      if (sum(!is.na(parsed)) > sum(!is.na(vals)) * 0.5) { df[[col]] <- parsed; next }
    }
    # Numeric columns (lat, lon, amounts, quantities)
    if (grepl("latitude|longitude|amt|qty|quantity|amount|rate|num|count|year", col)) {
      parsed <- suppressWarnings(as.numeric(vals))
      if (sum(!is.na(parsed)) >= sum(vals != "" & !is.na(vals)) * 0.8) df[[col]] <- parsed
    }
  }
  df
}

# -- Pagination for large queries ----------------------------------------------

.epa_fetch_all <- function(table, filters = list(), max_rows = 10000,
                           chunk_size = 1000) {
  all_data <- list()
  offset <- 0

  repeat {
    end <- offset + chunk_size - 1
    filter_path <- ""
    for (f in filters) {
      if (!is.null(f$value))
        filter_path <- paste0(filter_path, "/", f$column, "/", f$op %||% "=", "/",
                              utils::URLencode(as.character(f$value)))
    }

    url <- paste0(.epa_base, "/", table, filter_path,
                  "/rows/", offset, ":", end, "/JSON")

    raw <- tryCatch({
      tmp <- tempfile(fileext = ".json")
      httr2::request(url) |>
        httr2::req_headers(`User-Agent` = .ua) |>
        httr2::req_perform(path = tmp)
      jsonlite::fromJSON(tmp)
    }, error = function(e) NULL)

    if (is.null(raw) || length(raw) == 0) break

    df <- as_tibble(raw)
    names(df) <- tolower(names(df))
    all_data[[length(all_data) + 1]] <- df

    n_so_far <- sum(vapply(all_data, nrow, integer(1)))
    if (nrow(df) < chunk_size || n_so_far >= max_rows) break
    offset <- offset + chunk_size

    if (length(all_data) %% 5 == 0)
      message(sprintf("  ...fetched %d rows", n_so_far))
  }

  if (length(all_data) == 0) return(tibble())
  result <- bind_rows(all_data)
  if (nrow(result) > max_rows) result <- head(result, max_rows)
  result
}


